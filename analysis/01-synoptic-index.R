library(dplyr)
library(ggplot2)
library(sdmTMB)
library(cowplot)
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

## Figures for report ----
coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

gg <- ggplot(d, aes(longitude, latitude, colour = catch_weight/area_swept, fill = catch_weight/area_swept)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  theme(panel.spacing = unit(0, "in"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_colour_viridis_c(trans = "log", breaks = c(6e-6, 1e-4, 2.5e-3, 5e-2)) +
  scale_fill_viridis_c(trans = "log", breaks = c(6e-6, 1e-4, 2.5e-3, 5e-2)) +
  labs(x = "Longitude", y = "Latitude", colour = expression("CPUE (kg/"~m^2~")"), fill = expression("CPUE (kg/"~m^2~")"))
ggsave("figs/synoptic/syn_cpue.png", gg, height = 6, width = 5, dpi = 600)

pzero <- d %>%
  summarise(p = mean(catch_weight == 0) %>% round(2) %>% format(), .by = year)
gg <- d %>%
  mutate(cpue = 100 * catch_weight/area_swept) %>%
  filter(cpue <= 2e-4 * 100, cpue > 0) %>%
  ggplot(aes(x = cpue, y = after_stat(count))) +
  geom_label(data = pzero, x = Inf, y = Inf, hjust = "inward", vjust = "inward", aes(label = p), inherit.aes = FALSE) +
  geom_histogram(bins = 20, colour = 1, fill = "grey80") +
  facet_wrap(vars(year), ncol = 4) +
  theme(panel.spacing = unit(0, "in"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = expression("CPUE (g/"~m^2~")"), y = "Frequency")
ggsave("figs/synoptic/cpue_hist.png", gg, height = 6, width = 5)

## Fit sdm model ----
mesh <- make_mesh(d, c("X", "Y"), cutoff = 15)
plot(mesh)
mesh$mesh$n

# Plot mesh ----
g <- local({
  mesh_m <- mesh$mesh
  mesh_m$loc <- 1e3 * mesh_m$loc

  ggplot() +
    inlabru::gg(mesh_m, edge.color = "grey60") +
    geom_sf(data = coast %>% sf::st_transform(crs = 32609), inherit.aes = FALSE) +
    #geom_point(data = mesh$loc_xy %>% as.data.frame() %>% `*`(1e3), aes(X, Y), fill = "red", shape = 21, size = 1) +
    labs(x = "Longitude", y = "Latitude")
})
ggsave("figs/synoptic/syn_mesh.png", g, width = 5, height = 6)

# Call sdm
fit <- sdmTMB(
  catch_weight ~ 1 + poly(log(depth_m), 2L),
  family = delta_gamma(),
  data = d,
  mesh = mesh,
  offset = "log_area_swept",
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = TRUE,
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
grid <- sdmTMB::replicate_df(g, time_name = "year", time_values = yrs) %>%
  mutate(Xm = 1e3 * X, Ym = 1e3 * Y) %>%
  add_utm_columns(ll_names = c("Xm", "Ym"), ll_crs = 32609, utm_names = c("longitude", "latitude"), utm_crs = 4326, units = "m")

p <- predict(fit, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)
saveRDS(ind, file = "data/generated/geostat-ind-synoptic.rds")
ind <- readRDS("data/generated/geostat-ind-synoptic.rds")

survs <- select(d, year, survey_abbrev) |> distinct() |>
  group_by(year) |>
  summarise(survey_abbrev = paste(survey_abbrev, collapse = ", "))

ind <- left_join(ind, survs, by = join_by(year, survey_abbrev))

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))



## Plot figures in prediction grid ----
# Depth ----
gg <- ggplot(grid %>% filter(year == yrs[1]), aes(longitude, latitude, fill = depth_m, colour = depth_m)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_tile(width = 0.025, height = 0.025) +
  scale_fill_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750, 1250)) +
  scale_colour_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750, 1250)) +
  labs(x = "Longitude", y = "Latitude", colour = "Depth (m)", fill = "Depth (m)")
ggsave("figs/synoptic/prediction_grid_depth.png", gg, height = 4, width = 4, dpi = 600)

# Encounter probability ----
rb_fill2 <- scale_fill_gradient2(high = "red", low = "blue", mid = "grey90", midpoint = 0.5, labels = seq(0, 1, 0.25))
rb_col2 <- scale_colour_gradient2(high = "red", low = "blue", mid = "grey90", midpoint = 0.5, labels = seq(0, 1, 0.25))

gg <- ggplot(p$data, aes(longitude, latitude, fill = plogis(est1), colour = plogis(est1))) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_tile(width = 0.025, height = 0.025) +
  rb_fill2 + rb_col2 +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", colour = "Encounter\nprobability", fill = "Encounter\nprobability")
ggsave("figs/synoptic/prediction_grid_encounter.png", gg, height = 6, width = 5, dpi = 600)


# Omega ----
rb_fill <- scale_fill_gradient2(high = "red", low = "blue", mid = "grey90")
rb_col <- scale_colour_gradient2(high = "red", low = "blue", mid = "grey90")

gg <- ggplot(p$data %>% filter(year == yrs[1]), aes(longitude, latitude, fill = omega_s2, colour = omega_s2)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_tile(width = 0.025, height = 0.025) +
  rb_fill + rb_col +
  labs(x = "Longitude", y = "Latitude", colour = "Spatial effect", fill = "Spatial effect")
ggsave("figs/synoptic/prediction_grid_omega.png", gg, height = 4, width = 4, dpi = 600)

# Epsilon ----
gg <- ggplot(p$data, aes(longitude, latitude, fill = epsilon_st2, colour = epsilon_st2)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_tile(width = 0.025, height = 0.025) +
  rb_fill + rb_col +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", colour = "Spatiotemporal\neffect", fill = "Spatiotemporal\neffect")
ggsave("figs/synoptic/prediction_grid_eps.png", gg, height = 6, width = 5, dpi = 600)

# log-density ----
gg <- ggplot(p$data, aes(longitude, latitude, fill = est2, colour = est2)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_tile(width = 0.025, height = 0.025) +
  scale_colour_viridis_c(option = "C") +
  scale_fill_viridis_c(option = "C") +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", colour = "log density", fill = "log density")
ggsave("figs/synoptic/prediction_grid_density.png", gg, height = 6, width = 5, dpi = 600)

# Index ----
gg <- ggplot(ind, aes(year, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Year", y = "Synoptic Trawl Index") +
  expand_limits(y = 0)
ggsave("figs/synoptic/syn_index.png", gg, height = 3, width = 4)


# Marginal effect of depth ----
m1 <- visreg_delta(fit, xvar = "depth_m", breaks = seq(0, 1300, 50), scale = "response", plot = FALSE, model = 1)
m2 <- visreg_delta(fit, xvar = "depth_m", breaks = seq(0, 1300, 50), scale = "response", plot = FALSE, model = 2)
#saveRDS(list(m1 = m1, m2 = m2), file = 'data/generated/visreg_syn_depth.rds')

gg1 <- plot(m1, gg = TRUE,
            partial = FALSE, rug = FALSE,
            line.par = list(col = 1)) +
  coord_cartesian(xlim = c(0, 750), ylim = c(0, 1), expand = FALSE) +
  labs(x = "Depth (m)", y = "Encounter probability")

gg2 <- plot(m2, gg = TRUE,
            partial = FALSE, rug = FALSE,
            line.par = list(col = 1),
            points.par = list(alpha = 0.2)) +
  coord_cartesian(xlim = c(0, 750), ylim = c(0, 0.0005), expand = FALSE) +
  labs(x = "Depth (m)", y = "CPUE")

gg3 <- cowplot::plot_grid(gg1, gg2, ncol = 1, nrow = 2, align = "hv")
ggsave("figs/synoptic/depth_marginal.png", gg3, height = 6, width = 4)


# This used to work for sdmTMB
#m <- ggeffects::ggeffect(fit, terms = "depth_m [20:269, by=10]", offset = 0)

