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
nrow(d)

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

mesh <- make_mesh(d, c("X", "Y"), cutoff = 20)
plot(mesh)
mesh$mesh$n

fit_iphc_nb2 <- sdmTMB(
  number_observed ~ 0 + poly(depth_m_log, 2L),
  family = nbinom2(link = "log"),
  time_varying = ~ 1,
  data = d,
  mesh = mesh,
  time = "year",
  offset = "log_hookobserved",
  spatiotemporal = "ar1",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
)
saveRDS(fit_iphc_nb2, file = "data/generated/iphc-nb2-sdmTMB.rds")
fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB.rds")

fit_iphc_nb2
sanity(fit_iphc_nb2)
fit_iphc_nb2$sd_report
plot_anisotropy(fit_iphc_nb2)

# grid of IPHC main fixed survey locations
s <- readRDS("data/raw/IPHC_coastdata.rds") %>%
  # outside only, downloaded from website, expansion set and SOG removed
  dplyr::filter(iphc.reg.area == "2B") %>%
  distinct(station, .keep_all = TRUE)
grid <- s %>% dplyr::select(beginlon, beginlat, depth_m_log) %>%
  distinct(.keep_all = TRUE)
g <- add_utm_columns(grid, ll_names = c("beginlon", "beginlat"), utm_crs = 32609)
plot(g$beginlon, g$beginlat)

ggplot(g, aes(X, Y, colour = depth_m_log)) +
  geom_point() +
  coord_fixed() +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)

years <- sort(union(unique(d$year), fit_iphc_nb2$extra_time))
grid <- sdmTMB::replicate_df(g, "year", years)

p <- predict(fit_iphc_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))

saveRDS(ind, file = "data/generated/geostat-ind-iphc.rds")
ind <- readRDS("data/generated/geostat-ind-iphc.rds")

# both2 <- filter(both, survey_abbrev == "HBLL INS N" | survey_abbrev == "HBLL INS S")
# both2 <- filter(both, survey_abbrev == "HBLL INS S")
# both2 <- filter(both, survey_abbrev == "HBLL INS N" )
#
#
# mesh100 <- sdmTMB::make_mesh(both2,
#                              xy_cols = c("UTM.lon", "UTM.lat"),
#                              n_knots = 100
# )
#
# plot(mesh100$mesh, asp = 1, main = "")
# points(both2$UTM.lon, both2$UTM.lat, pch = ".", col = "red")
#
# plot(both2$UTM.lon, both2$UTM.lat)
#
# #saveRDS(mesh100, "output/mesh100_SOG.rds")
# #mesh100 <- readRDS("output/mesh100_SOG.rds")
#
# m_dog_sog <- sdmTMB(
#   formula = catch_count ~ 0 +  offset  + as.factor(survey_abbrev) + poly(depth_m, 2) + poly(julian,2),
#   data = both2,
#   mesh = mesh100,
#   spatiotemporal = "AR1",
#   time = "year",
#   silent = FALSE,
#   family = poisson(link = "log"),
#   spatial = TRUE
# )
#
#
# nd <- expand.grid(julian =
#                     seq(min(both2$julian), max(both2$julian), length.out = 100))
# nd$depth_m <- mean(both2$depth_m)
# nd$offset <- mean(both2$offset)
# nd$year <- 2021L # L: integer to match original data
# nd$survey_abbrev = "HBLL INS S"
# p <- predict(m_dog_sog4, newdata = nd, se_fit = TRUE, re_form = NA)
# ggplot(p, aes(julian, est,
#               ymin = I(est - 1.96 * est_se), ymax = I(est + 1.96 * est_se))) +
#   geom_line() + geom_ribbon(alpha = 0.4)
#
#
#
#
#
