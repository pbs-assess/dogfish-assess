library(dplyr)
library(ggplot2)
library(sdmTMB)
theme_set(gfplot::theme_pbs())

s <- readRDS("data/raw/survey-sets.rds")
d <- dplyr::filter(s, grepl("SYN", survey_abbrev))
table(d$survey_abbrev)
d <- sdmTMB::add_utm_columns(d, utm_crs = 32609)

# used old version of gfdata...
d$area_swept1 <- d$doorspread_m * d$tow_length_m
d$area_swept2 <- d$doorspread_m * (d$speed_mpm * d$duration_min)
d$area_swept <- ifelse(!is.na(d$area_swept1), d$area_swept1, d$area_swept2)


table(d$year[is.na(d$doorspread_m)])
table(d$year[is.na(d$tow_length_m)])
table(d$year[is.na(d$tow_length_m)])
d$survey_abbrev[is.na(d$doorspread_m)]

d <- dplyr::filter(d, !is.na(area_swept))

ggplot(d, aes(X, Y, size = density_kgpm2)) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  coord_fixed()

sum(is.na(d$depth_m))
table(d$year[is.na(d$depth_m)])
d <- dplyr::filter(d, !is.na(depth_m))
d$log_area_swept <- log(d$area_swept)

mesh <- make_mesh(d, c("X", "Y"), cutoff = 15)
plot(mesh)
mesh$mesh$n

fit <- sdmTMB(
  catch_weight ~ 1 + poly(log(depth_m), 2L),
  family = delta_gamma(),
  data = d,
  mesh = mesh,
  offset = "log_area_swept",
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
)

saveRDS(fit, file = "data/generated/synoptic-sdmTMB.rds")
fit <- readRDS("data/generated/synoptic-sdmTMB.rds")
sanity(fit)
plot_anisotropy(fit)
fit
fit$sd_report

g <- gfplot::synoptic_grid |> dplyr::select(-survey_domain_year)
g <- rename(g, depth_m = depth)
# g <- add_utm_columns(g, utm_crs = 32609)

ggplot(g, aes(X, Y, fill = depth_m, colour = depth_m)) +
  geom_tile(width = 2, height = 2) +
  coord_fixed() +
  scale_fill_viridis_c(trans = "sqrt", direction = -1) +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)

yrs <- sort(unique(d$year))
grid <- sdmTMB::replicate_df(g, time_name = "year", time_values = yrs)

p <- predict(fit, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)
saveRDS(ind, file = "data/generated/geostat-ind-synoptic.rds")
ind <- readRDS("data/generated/geostat-ind-synoptic.rds")

survs <- select(d, year, survey_abbrev) |> distinct() |>
  group_by(year) |>
  summarise(survey_abbrev = paste(survey_abbrev, collapse = ", "))

ind <- left_join(ind, survs, by = join_by(year))

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))
