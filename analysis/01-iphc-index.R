
#library
library(ggplot2)
library(dplyr)
library(sdmTMB)


# Retrieve survey data
s <- readRDS("data/IPHC_coastdata.rds") %>% #outside only, downloaded from website, expansion set and SOG removed
     dplyr::filter(iphc.reg.area == "2B")
d <- sdmTMB::add_utm_columns(s, ll_names = c("beginlon", "beginlat"), utm_crs = 32609)

glimpse(d)
sort(unique(d$year))
unique(d)

ggplot(d, aes(X, Y, size = cpue)) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  coord_fixed()

sum(is.na(d$depth_m))
d <- dplyr::filter(d, !is.na(depth_m_log))
d$log_hookobserved <- log(as.numeric(d$hooksobserved))

mesh <- make_mesh(d, c("X", "Y"), cutoff = 12)
plot(mesh)
mesh$mesh$n

#models
fit_iphc_tw <-  sdmTMB(
  cpue ~ 1 + poly(depth_m_log, 2L),
  family = tweedie(link = "log"),
  data = d,
  mesh = mesh,
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  #anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
  )

saveRDS(fit_iphc_tw, file = "data/generated/iphc-tw-sdmTMB.rds")

# fit_iphc_nb2 <- sdmTMB(
#   cpue ~ 1 + poly(depth_m_log, 2L),
#   family = nbinom2(link = "log"),
#   data = d,
#   mesh = mesh,
#   time = "year",
#   spatiotemporal = "rw",
#   spatial = "on",
#   silent = FALSE,
#   anisotropy = TRUE,
#   control = sdmTMBcontrol(newton_loops = 1L)
# )
# saveRDS(fit_iphc_nb2, file = "data/generated/iphc-nb2-sdmTMB.rds")
# fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB.rds")

sanity(fit_iphc_tw)
sanity(fit_iphc_nb2)

plot_anisotropy(fit_iphc_tw)
plot_anisotropy(fit_iphc_nb2)

fit_iphc_tw

fit_iphc_tw$sd_report
fit_iphc_nb2$sd_report

AIC(fit_iphc_tw, fit_iphc_nb2)



#grid if IPHC main fixed survey locations
s <- readRDS("data/raw/IPHC_coastdata.rds") %>% #outside only, downloaded from website, expansion set and SOG removed
  dplyr::filter(iphc.reg.area == "2B") %>% distinct(station, .keep_all = TRUE)
g <- s %>% dplyr::select(beginlon, beginlat, depth_m) %>% distinct(.keep_all = TRUE)
grid <- add_utm_columns(g, ll_names = c("beginlon", "beginlat"), utm_crs = 32609)

ggplot(grid, aes(X, Y), fill = depth_m_log, colour = depth_m_log) +
  geom_tile(width = 2, height = 2) +
  coord_fixed() +
  scale_fill_viridis_c(trans = "sqrt", direction = -1) +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)


year <- unique(d$year)
grid2 <- purrr::map_dfr(year, function(.x) {dplyr::mutate(grid, year = .x)})

p_ins_tw <- predict(fit_iphc_tw, newdata = grid2, return_tmb_object = TRUE)
ind_ins <- get_index(p_ins_tw, bias_correct = TRUE)

ggplot(ind_ins, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))

saveRDS(ind_ins_save, file = "data/generated/geostat-ind-iphc.rds")
ind_ins_save <- readRDS("data/generated/geostat-ind-iphc.rds")

ggplot(ind_ins_save, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))







both2 <- filter(both, survey_abbrev == "HBLL INS N" | survey_abbrev == "HBLL INS S")
both2 <- filter(both, survey_abbrev == "HBLL INS S")
both2 <- filter(both, survey_abbrev == "HBLL INS N" )


mesh100 <- sdmTMB::make_mesh(both2,
                             xy_cols = c("UTM.lon", "UTM.lat"),
                             n_knots = 100
)

plot(mesh100$mesh, asp = 1, main = "")
points(both2$UTM.lon, both2$UTM.lat, pch = ".", col = "red")

plot(both2$UTM.lon, both2$UTM.lat)

#saveRDS(mesh100, "output/mesh100_SOG.rds")
#mesh100 <- readRDS("output/mesh100_SOG.rds")

m_dog_sog <- sdmTMB(
  formula = catch_count ~ 0 +  offset  + as.factor(survey_abbrev) + poly(depth_m, 2) + poly(julian,2),
  data = both2,
  mesh = mesh100,
  spatiotemporal = "AR1",
  time = "year",
  silent = FALSE,
  family = poisson(link = "log"),
  spatial = TRUE
)


nd <- expand.grid(julian =
                    seq(min(both2$julian), max(both2$julian), length.out = 100))
nd$depth_m <- mean(both2$depth_m)
nd$offset <- mean(both2$offset)
nd$year <- 2021L # L: integer to match original data
nd$survey_abbrev = "HBLL INS S"
p <- predict(m_dog_sog4, newdata = nd, se_fit = TRUE, re_form = NA)
ggplot(p, aes(julian, est,
              ymin = I(est - 1.96 * est_se), ymax = I(est + 1.96 * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4)





