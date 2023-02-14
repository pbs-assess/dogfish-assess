library(dplyr)
library(ggplot2)
library(sdmTMB)
theme_set(gfplot::theme_pbs())

s <- readRDS("data/raw/survey-sets.rds")
d <- dplyr::filter(s, grepl("HBLL OUT", survey_abbrev))
d <- sdmTMB::add_utm_columns(d, utm_crs = 32609)

ggplot(d, aes(X, Y, size = density_ppkm2)) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  coord_fixed()

sum(is.na(d$depth_m))
d <- dplyr::filter(d, !is.na(depth_m))
d$log_hook_count <- log(d$hook_count)

mesh <- make_mesh(d, c("X", "Y"), cutoff = 12)
plot(mesh)
mesh$mesh$n

fit_nb2 <- sdmTMB(
  catch_count ~ 1 + poly(log(depth_m), 2L),
  family = nbinom2(link = "log"),
  data = d,
  mesh = mesh,
  offset = "log_hook_count",
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  extra_time = 2013L,
  control = sdmTMBcontrol(newton_loops = 1L)
)
sanity(fit_nb2, gradient_thresh = 0.001)
max(fit_nb2$gradients)
plot_anisotropy(fit_nb2)
fit_nb2
fit_nb2$sd_report

# fit_dg <- update(fit_nb2, family = delta_gamma())
# sanity(fit_dg)
# plot_anisotropy(fit_dg)
# fit_dg
# fit_dg$sd_report
#
# AIC(fit_nb2, fit_dg)
# fit_dl <- update(fit_nb2, family = delta_lognormal())

g <- gfplot::hbll_grid$grid
g <- rename(g, latitude = Y, longitude = X, depth_m = depth)
g <- add_utm_columns(g, utm_crs = 32609)

ggplot(g, aes(X, Y, fill = depth_m, colour = depth_m)) +
  geom_tile(width = 2, height = 2) +
  coord_fixed() +
  scale_fill_viridis_c(trans = "sqrt", direction = -1) +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)

yrs <- sort(union(unique(d$year), fit_nb2$extra_time))
grid <- sdmTMB::replicate_df(g, time_name = "year", time_values = yrs)

p_nb2 <- predict(fit_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p_nb2, bias_correct = TRUE)
survs <- select(d, year, survey_abbrev) |> distinct()
ind <- left_join(ind, survs, by = join_by(year))

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))

ind_save <- dplyr::filter(ind, !is.na(survey_abbrev))
saveRDS(ind_save, file = "data/generated/geostat-ind-hbll-out.rds")

ggplot(ind_save, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))
