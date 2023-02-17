library(ggplot2)
library(dplyr)
library(sdmTMB)

s <- readRDS("data/raw/IPHC_coastdata.rds") %>%
  # outside only, downloaded from website, expansion set and SOG removed:
  dplyr::filter(iphc.reg.area == "2B")
d <- sdmTMB::add_utm_columns(s, ll_names = c("beginlon", "beginlat"), utm_crs = 32609)
d$hooksobserved <- as.numeric(d$hooksobserved)
names(d) <- gsub("\\.", "_", names(d))

glimpse(d)
sort(unique(d$year))

ggplot(d, aes(X, Y, size = cpue)) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  coord_fixed()

stopifnot(sum(is.na(d$depth_m)) == 0L)
stopifnot(sum(is.na(d$depth_m_log)) == 0L)
stopifnot(sum(is.na(d$number_observed)) == 0L)
stopifnot(sum(is.na(d$hooksobserved)) == 0L)
range(d$number_observed)
range(d$hooksobserved)
range(d$depth_m_log)
d$log_hookobserved <- log(d$hooksobserved)

mesh <- make_mesh(d, c("X", "Y"), cutoff = 15)
plot(mesh)
mesh$mesh$n

fit_iphc_nb2 <- sdmTMB(
  number_observed ~ 1 + poly(depth_m_log, 2L),
  family = nbinom2(link = "log"),
  data = d,
  mesh = mesh,
  time = "year",
  offset = "log_hookobserved",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
)
saveRDS(fit_iphc_nb2, file = "data/generated/iphc-nb2-sdmTMB.rds")
fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB.rds")

# fit_iphc_tw <- update(fit_iphc_nb2, family = tweedie())

# sanity(fit_iphc_tw)
sanity(fit_iphc_nb2)

# plot_anisotropy(fit_iphc_tw)
plot_anisotropy(fit_iphc_nb2)

# fit_iphc_tw

# fit_iphc_tw$sd_report
fit_iphc_nb2$sd_report

# AIC(fit_iphc_tw, fit_iphc_nb2)

# grid of IPHC main fixed survey locations
s <- readRDS("data/IPHC_coastdata.rds") %>%
  # outside only, downloaded from website, expansion set and SOG removed
  dplyr::filter(iphc.reg.area == "2B") %>%
  distinct(station, .keep_all = TRUE)
grid <- s %>% dplyr::select(beginlon, beginlat) %>%
  distinct(.keep_all = TRUE)
g <- add_utm_columns(grid, ll_names = c("beginlon", "beginlat"), utm_crs = 32609)
plot(g$beginlon, g$beginlat)

ggplot(g, aes(X, Y), fill = depth_m, colour = depth_m) +
  geom_tile(width = 2, height = 2) +
  coord_fixed() +
  scale_fill_viridis_c(trans = "sqrt", direction = -1) +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)

year <- sort(union(unique(d$year), fit_ins_nb2$extra_time))
grid <- purrr::map_dfr(year, function(.x) {dplyr::mutate(g, year = .x)})

p_ins <- predict(fit_iphc_nb2, newdata = grid, return_tmb_object = TRUE)
ind_ins <- get_index(p_ins, bias_correct = TRUE)
survs <- select(d, year, survey_abbrev) |> distinct()
ind_ins <- left_join(ind_ins, survs, by = join_by(year))

ggplot(ind_ins, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))

saveRDS(ind_ins, file = "data/generated/geostat-ind-iphc.rds")
ind_ins_save <- readRDS("data/generated/geostat-ind-iphc.rds")

ggplot(ind_ins_save, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
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





