library(dplyr)
library(ggplot2)
library(sdmTMB)
library(rnaturalearth)
theme_set(gfplot::theme_pbs())

# setwd("C:/Users/tcarruth/Documents/GitHub/dogfish-assess")

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

## Hook competition ----
# hookll <- gfdata::get_ll_hook_data(species = "north pacific spiny dogfish", ssid = c(22, 36)) %>%
#   dplyr::select(fishing_event_id, year, count_bait_only, count_empty_hooks, count_bent_broken, count_non_target_species, count_target_species)
# saveRDS(hookll, "data/raw/HBLL_OUT_hookinfo.rds")
hookll <- readRDS("data/raw/HBLL_OUT_hookinfo.rds")

hll <- hookll %>%
  rowwise() %>%
  mutate(hooksfishing = sum(count_target_species + count_non_target_species +
    count_empty_hooks + count_bait_only -
    count_bent_broken))

ggplot(hll, aes(count_bait_only)) +
  geom_histogram() +
  facet_wrap(~year)

ggplot(hll, aes(hooksfishing)) +
  geom_histogram() +
  facet_wrap(~year)

ggplot(
  filter(hll, year %in% c(2006, 2012, 2022)),
  aes(hooksfishing, group = as.factor(year), col = as.factor(year))
) +
  geom_density()

ggplot(
  filter(hll, year %in% c(2006, 2012, 2022)),
  aes(count_bait_only, group = as.factor(year), col = as.factor(year))
) +
  geom_density()

hll$count_bait_only[hll$count_bait_only == 0] <- 1
hll <- hll[hll$hooksfishing > 0, ]
hll <- hll %>%
  mutate(Pit = count_bait_only / hooksfishing) %>%
  mutate(Ait = -log(Pit) / (1 - Pit))

range(hll$Pit)
range(hll$Ait)

ggplot(hll, aes(Pit, Ait, group = year, col = year)) +
  geom_point()

d <- left_join(d, select(hll, c(Pit, Ait, fishing_event_id, year)),
  by = c("year" = "year", "fishing_event_id" = "fishing_event_id")
)

d$offset <- log(d$hook_count / d$Ait)
stopifnot(sum(is.na(d$offset)) == 0L)


## Figures for report - data and hook adjustment ----
coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

gg <- ggplot(d, aes(longitude, latitude, fill = Pit, colour = Pit)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  scale_fill_viridis_c(option = "C", limits = c(0, 1)) +
  scale_colour_viridis_c(option = "C", limits = c(0, 1)) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", fill = "Proportion baited hooks", colour = "Proportion baited hooks")
ggsave("figs/hbll_out/baited_hooks.png", gg, height = 6, width = 5, dpi = 600)

gg <- ggplot(d, aes(longitude, latitude, fill = catch_count/exp(offset), colour = catch_count/exp(offset))) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  theme(panel.spacing = unit(0, "in"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_colour_viridis_c(trans = "log", breaks = c(0.007, 0.05, 0.368, 2.72)) +
  scale_fill_viridis_c(trans = "log", breaks = c(0.007, 0.05, 0.368, 2.72)) +
  labs(x = "Longitude", y = "Latitude", fill = "Adjusted CPUE", colour = "Adjusted CPUE")
ggsave("figs/hbll_out/adjusted_cpue.png", gg, height = 6, width = 5, dpi = 600)

gg <- d %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  filter(cpue <= 1) %>%
  ggplot(aes(x = cpue, y = after_stat(count))) +
  geom_histogram(bins = 20, colour = 1, fill = "grey80") +
  facet_wrap(vars(year)) +
  theme(panel.spacing = unit(0, "in")) +
  labs(x = "Adjusted CPUE", y = "Frequency") +
  coord_cartesian(xlim = c(0, 1))
ggsave("figs/hbll_out/adjusted_cpue_hist.png", gg, height = 4, width = 5)



## Fit sdm model ----
mesh <- make_mesh(d, c("X", "Y"), cutoff = 12)
plot(mesh)
mesh$mesh$n
# Plot mesh ----

g <- local({
  mesh_m <- mesh$mesh
  mesh_m$loc <- 1e3 * mesh_m$loc

  ggplot() +
    inlabru::gg(mesh_m, edge.color = "grey61") +
    geom_sf(data = coast %>% sf::st_transform(crs = 32609), inherit.aes = FALSE) +
    #geom_point(data = mesh$loc_xy %>% as.data.frame() %>% `*`(1e3), aes(X, Y), fill = "red", shape = 21, size = 1) +
    labs(x = "Longitude", y = "Latitude")
})
ggsave("figs/hbll_out/hbll_out_mesh.png", g, width = 5, height = 6)

# Call sdm
fit_nb2 <- sdmTMB(
  catch_count ~ 1 + poly(log(depth_m), 2L),
  family = nbinom2(link = "log"),
  data = d,
  mesh = mesh,
  offset = "offset", # hook competition offset
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = TRUE,
  anisotropy = TRUE,
  extra_time = 2013L,
  control = sdmTMBcontrol(newton_loops = 1L)
)
saveRDS(fit_nb2, file = "data/generated/hbll-out-sdmTMB.rds")
fit_nb2 <- readRDS("data/generated/hbll-out-sdmTMB.rds")

sanity(fit_nb2)
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

## Get prediction grid ----
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
# grid <- purrr::map_dfr(yrs, ~ tibble(g, year = .x))

## Make index ----
p_nb2 <- predict(fit_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p_nb2, bias_correct = TRUE)
survs <- select(d, year, survey_abbrev) |> distinct()
ind <- left_join(ind, survs, by = join_by(year))
# ind <- left_join(ind, survs, by = "year")

ggplot() +
  geom_pointrange(data = ind_save, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
  coord_cartesian(ylim = c(0, NA))

ind_save <- dplyr::filter(ind, !is.na(survey_abbrev))

saveRDS(ind_save, file = "data/generated/geostat-ind-hbll-out.rds")
ind_save <- readRDS("data/generated/geostat-ind-hbll-out.rds")
#saveRDS(ind_save, file = "data/generated/geostat-ind-hbll-out-hook-compet.rds")
#ind_save_hk <- readRDS("data/generated/geostat-ind-hbll-out-hook-compet.rds")

# x <- ggplot(ind_save_hk, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
#   geom_pointrange() +
#   coord_cartesian(ylim = c(0, NA))
# x + geom_pointrange(data = ind_save_nohk, aes(year, est, ymin = lwr, ymax = upr), colour = "black")



## Plot figures in prediction grid ----
# Depth ----
gg <- ggplot(g, aes(longitude, latitude, fill = depth_m, colour = depth_m)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_tile(width = 0.025, height = 0.025) +
  scale_fill_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750)) +
  scale_colour_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750)) +
  labs(x = "Longitude", y = "Latitude", colour = "Depth (m)", fill = "Depth (m)")
ggsave("figs/hbll_out/prediction_grid_depth.png", gg, height = 4, width = 4, dpi = 600)

# Omega ----
rb_fill <- scale_fill_gradient2(high = "red", low = "blue", mid = "grey90")
rb_col <- scale_colour_gradient2(high = "red", low = "blue", mid = "grey90")

gg <- ggplot(p_nb2$data, aes(longitude, latitude, fill = omega_s, colour = omega_s)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_tile(width = 0.025, height = 0.025) +
  rb_fill + rb_col +
  labs(x = "Longitude", y = "Latitude", colour = "Spatial effect", fill = "Spatial effect")
ggsave("figs/hbll_out/prediction_grid_omega.png", gg, height = 4, width = 4, dpi = 600)

# Epsilon ----
gg <- ggplot(p_nb2$data, aes(longitude, latitude, fill = epsilon_st, colour = epsilon_st)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_tile(width = 0.025, height = 0.025) +
  rb_fill + rb_col +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", colour = "Spatiotemporal\neffect", fill = "Spatiotemporal\neffect")
ggsave("figs/hbll_out/prediction_grid_eps.png", gg, height = 6, width = 5, dpi = 600)

# log-density ----
gg <- ggplot(p_nb2$data, aes(longitude, latitude, fill = est, colour = est)) +
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
ggsave("figs/hbll_out/prediction_grid_density.png", gg, height = 6, width = 5, dpi = 600)

# Index ----
gg <- ggplot(ind_save, aes(year, est)) +
  geom_point() +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Year", y = "HBLL Index")
ggsave("figs/hbll_out/hbll_index.png", gg, height = 3, width = 4)

# Marginal effect of depth ----
marginal_depth <- visreg::visreg(fit_nb2, xvar = "depth_m", breaks = seq(0, 270, 10),
                                 data = fit_nb2$data,
                                 plot = FALSE)

gg <- plot(marginal_depth, gg = TRUE,
           line.par = list(col = 1),
           points.par = list(alpha = 0.2)) +
  coord_cartesian(xlim = c(0, 300), expand = FALSE) +
  labs(x = "Depth (m)", y = "log(CPUE)")
ggsave("figs/hbll_out/depth_marginal.png", gg, height = 3, width = 4)
