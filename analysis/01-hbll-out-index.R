library(dplyr)
library(ggplot2)
library(sdmTMB)
library(rnaturalearth)
theme_set(gfplot::theme_pbs())

# setwd("C:/Users/tcarruth/Documents/GitHub/dogfish-assess")

# built in data/raw/pull-raw-data.R
s <- readRDS("data/raw/survey-sets.rds")
d <- dplyr::filter(s, grepl("HBLL OUT", survey_abbrev))
d <- sdmTMB::add_utm_columns(d, utm_crs = 32609)
d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))

ggplot(d, aes(year, julian, colour = survey_abbrev)) +
  geom_jitter(pch = 21, alpha = 0.3)

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
range(hll$hooksfishing)

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

#d <- left_join(d, select(hll, c(Pit, Ait, fishing_event_id, year)),
#  by = c("year" = "year", "fishing_event_id" = "fishing_event_id")
#)

d <- left_join(d, hll,
               by = c("year" = "year", "fishing_event_id" = "fishing_event_id")
)

ggplot(d, aes(count_target_species, catch_count)) + geom_point()
ggplot(d, aes(hook_count, hooksfishing )) + geom_point() + facet_wrap(~year)
ggplot(d, aes(catch_count, count_target_species )) + geom_point() + facet_wrap(~year)
ggplot(d, aes(hook_count, count_bait_only  )) + geom_point() + facet_wrap(~year)
d |> filter(count_bait_only > hook_count)

d$offset_hk <- log(d$hook_count / d$Ait)
d$offset <- log(d$hook_count)
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
  labs(x = "Longitude", y = "Latitude", fill = "CPUE", colour = "CPUE")
ggsave("figs/hbll_out/cpue.png", gg, height = 6, width = 5, dpi = 600)

gg <- d %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  #filter(cpue <= 1) %>%
  ggplot(aes(x = cpue, y = after_stat(count))) +
  geom_histogram(binwidth = 0.025, linewidth = 0.1, colour = 1, fill = "grey80") +
  facet_wrap(vars(year)) +
  #theme(panel.spacing = unit(0, "in")) +
  labs(x = "CPUE", y = "Frequency") +
  coord_cartesian(xlim = c(-0.0125, 0.35), expand = FALSE)
ggsave("figs/hbll_out/cpue_hist.png", gg, height = 6, width = 6)

gg <- d %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  ggplot(aes(depth_m, log(cpue + 1))) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(year)) +
  labs(x = "Depth (m)", y = "log(CPUE + 1)") +
  theme(panel.spacing = unit(0, "in")) +
  coord_cartesian(xlim = c(0, 300))
ggsave("figs/hbll_out/cpue_depth_time.png", gg, height = 5, width = 6)


## Design-based index ----
# Some sets are missing strata area
strata <- d %>%
  filter(!is.na(area_km2)) %>%
  summarize(area_km2 = unique(area_km2), .by = grouping_code)

index_design <- d %>%
  select(-area_km2) %>%
  left_join(strata, by = "grouping_code") %>%
  select(catch_count, offset, offset_hk, hook_count, Ait, year, survey_abbrev, grouping_code, area_km2) %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  summarize(n = n(),
            nsamp = unique(area_km2)/sum(exp(offset)) * n,
            index_expand = mean(area_km2 * cpue),
            index_var = var(area_km2 * cpue),
            .by = c(year, grouping_code, survey_abbrev)) %>%
  summarize(Biomass = sum(index_expand),
            nset = sum(n),
            sampling_units = sum(nsamp),
            Var = sum(index_var)/n()/n(), # Divide by the number of strata
            #Var = sum(nsamp/n * index_var)/sum(nsamp)/sum(nsamp),
            .by = c(year, survey_abbrev)) %>%
  mutate(SE = sqrt(Var), CV = SE/Biomass)

g <- index_design %>%
  ggplot(aes(year, Biomass, linetype = survey_abbrev, shape = survey_abbrev)) +
  geom_linerange(aes(ymin = Biomass - 2 * SE, ymax = Biomass + 2 * SE)) +
  geom_point() +
  geom_line(linewidth = 0.1) +
  theme(panel.spacing = unit(0, "in"), legend.position = "bottom") +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Index of abundance", linetype = "Survey", shape = "Survey") +
  scale_shape_manual(values = c(16, 1))
ggsave("figs/hbll_out/hbll_index_design.png", g, height = 3, width = 4)




## Fit sdm model ----
mesh <- make_mesh(d, c("X", "Y"), cutoff = 12)
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
ggsave("figs/hbll_out/hbll_out_mesh.png", g, width = 5, height = 6)

range(d$julian)
d |> filter(year == 2006) |> summarize(mean = mean(julian))
d$julian_centre <- d$julian - 236

# Call sdm
fit_nb2 <- sdmTMB(
  #catch_count ~ 1 + poly(log(depth_m), 2L) + poly(julian_centre,2L),
  catch_count ~ 1 + poly(log(depth_m), 2L),
  family = nbinom2(link = "log"),
  data = d,
  mesh = mesh,
  offset = "offset_hk", # hook competition offset
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  extra_time = 2013L
)

fit_nb2_nohk <- update(fit_nb2, offset = "offset")
fit_nb2_julian <- update(fit_nb2, formula = catch_count ~ 1 + poly(log(depth_m), 2L) + poly(julian_centre,2L))

#with offset_hk
saveRDS(fit_nb2, file = "data/generated/hbll-out-sdmTMB.rds")
fit_nb2 <- readRDS("data/generated/hbll-out-sdmTMB.rds")

#without hk
saveRDS(fit_nb2_nohk, file = "data/generated/hbll-out-sdmTMB_nohk.rds")
fit_nb2_nohk <- readRDS("data/generated/hbll-out-sdmTMB_nohk.rds")

#with julian and no hk
saveRDS(fit_nb2_julian, file = "data/generated/hbll-out-sdmTMB-julian.rds")
fit_nb2_julian <- readRDS("data/generated/hbll-out-sdmTMB-julian.rds")

sanity(fit_nb2)
sanity(fit_nb2_nohk)
sanity(fit_nb2_julian)
AIC(fit_nb2_julian)
AIC(fit_nb2_nohk)
AIC(fit_nb2)

plot_anisotropy(fit_nb2)
plot_anisotropy(fit_nb2_julian)
plot_anisotropy(fit_nb2_nohk)
fit_nb2
fit_nb2$sd_report

# Censored Poisson -----
# Note on where pstar comes from:
# Needs gfsynopsis to run this, but pstar for dogfish = 1
# See Watson et al. 2023 Figure 6:
# pstar_list <- gfsynopsis::get_pstar(
#   survey_dat = d,
#   gam_formula = formula(catch ~ -1 + s(prop_removed) + fyear + offset(log(hook_count))),
#   survey_type = "hbll_outside",
#   prop_removed_min = NULL, h = 0.005,
#   pstar_cache = NULL, save_out = FALSE)
# if (nrow(pstar_list$pstar_df) == 0) {
#   pstar <- 1
# } else {
#   pstar <- pluck(pstar_list, 'pstar_df', 'pstar')
# }

for (pstar in c(0.8, 0.95, 1)) {
  #pstar <- 0.8
  #pstar <- 0.95
  # pstar <- 1
  mesh_cutoff <- 15
  d <- d |>
    mutate(obs_id = as.factor(seq(1, n()))) |> # Account for variance constraint when using Poisson
    mutate(prop_removed = (1 - count_bait_only / hook_count))
  # Provide upper bound on censored distribution
  d$upr <- sdmTMB:::get_censored_upper(
    prop_removed = d$prop_removed,
    n_catch = d$catch_count,
    n_hooks = d$hook_count,
    pstar = pstar)

  plot(d$upr, d$catch_count);abline(0,1)

  fit_cpois <- sdmTMB(
    #formula = catch_count ~ 1 + poly(log(depth_m), 2L) + (1 | obs_id),
    formula = catch_count ~ 1 + (1 | obs_id),
    family = sdmTMB::censored_poisson(link = "log"),
    data = d,
    mesh = make_mesh(d, c("X", "Y"), cutoff = mesh_cutoff),
    offset = d$log_hook_count,
    time = "year",
    spatiotemporal = "rw",
    spatial = "on",
    silent = FALSE,
    anisotropy = TRUE,
    extra_time = 2013L,
    control = sdmTMB::sdmTMBcontrol(censored_upper = d$upr)
  )

  saveRDS(fit_cpois, file = paste0("data/generated/hbll-out-sdmTMB_cpois-pstar=", pstar, "_mesh=", mesh_cutoff, ".rds"))
}
fit_cpois_1  <- readRDS(paste0("data/generated/hbll-out-sdmTMB_cpois-pstar=", 1, "_mesh=", mesh_cutoff, ".rds"))
fit_cpois_95 <- readRDS(paste0("data/generated/hbll-out-sdmTMB_cpois-pstar=", 0.95, "_mesh=", mesh_cutoff, ".rds"))
fit_cpois_80 <- readRDS(paste0("data/generated/hbll-out-sdmTMB_cpois-pstar=", 0.8, "_mesh=", mesh_cutoff, ".rds"))

sanity(fit_cpois_1)
sanity(fit_cpois_95)
sanity(fit_cpois_80)
AIC(fit_nb2)
AIC(fit_cpois_1)
AIC(fit_cpois_95)
# AIC(fit_cpois_80)

plot_anisotropy(fit_cpois_1)
fit_cpois_1$sd_report

AIC(fit_nb2, fit_nb2_julian, fit_nb2_nohk, fit_cpois) |>
  arrange(AIC)

# ----

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
g <- g |> mutate(julian_centre = 0)
g$obs_id <- 1L
ggplot(g, aes(X, Y, fill = depth_m, colour = depth_m)) +
  geom_tile(width = 2, height = 2) +
  coord_fixed() +
  scale_fill_viridis_c(trans = "sqrt", direction = -1) +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)

yrs <- sort(union(unique(d$year), fit_nb2$extra_time))
grid <- sdmTMB::replicate_df(g, time_name = "year", time_values = yrs)
grid$offset <- 0
grid$offset_hk <- 0
# grid <- purrr::map_dfr(yrs, ~ tibble(g, year = .x))

## Make index ----
p_nb2 <- predict(fit_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p_nb2, bias_correct = TRUE)
survs <- select(d, year, survey_abbrev) |> distinct()
ind <- left_join(ind, survs, by = join_by(year))
# ind <- left_join(ind, survs, by = "year")

p_nb2_nohk <- predict(fit_nb2_nohk, newdata = grid, return_tmb_object = TRUE)
ind_nohk <- get_index(p_nb2_nohk, bias_correct = TRUE)
survs <- select(d, year, survey_abbrev) |> distinct()
ind_nohk <- left_join(ind_nohk, survs, by = join_by(year))

# p_nb2_julian <- predict(fit_nb2_julian, newdata = grid, return_tmb_object = TRUE)
# ind_julian <- get_index(p_nb2_julian, bias_correct = TRUE)
# survs <- select(d, year, survey_abbrev) |> distinct()
# ind_julian <- left_join(ind_julian, survs, by = join_by(year))

p_cpois_1 <- predict(fit_cpois_1, newdata = grid, return_tmb_object = TRUE, re_form_iid = NA)
ind_cpois_1 <- get_index(p_cpois_1, bias_correct = TRUE)
survs <- select(d, year, survey_abbrev) |> distinct()
ind_cpois_1 <- left_join(ind_cpois_1, survs, by = join_by(year))

# p_cpois_95 <- predict(fit_cpois_95, newdata = grid, return_tmb_object = TRUE, re_form_iid = NA)
# ind_cpois_95 <- get_index(p_cpois_95, bias_correct = TRUE)
# survs <- select(d, year, survey_abbrev) |> distinct()
# ind_cpois_95 <- left_join(ind_cpois_95, survs, by = join_by(year))
#
# p_cpois_80 <- predict(fit_cpois_80, newdata = grid, return_tmb_object = TRUE, re_form_iid = NA)
# ind_cpois_80 <- get_index(p_cpois_80, bias_correct = TRUE)
# survs <- select(d, year, survey_abbrev) |> distinct()
# ind_cpois_80 <- left_join(ind_cpois_80, survs, by = join_by(year))
beepr::beep()
#saveRDS(ind_cpois_80, file = "data/generated/geostat-ind-hbll-out_ind_cpois_80.rds")

indexes <- bind_rows(list(
  mutate(ind, type = "NB2 ICR hook competition"),
  mutate(ind_nohk, type = "NB2 no hook competition"),
  #mutate(ind_julian, type = "NB2 hook competition + day of year"),
  mutate(ind_cpois_1, type = "Overdispersed Poisson\nno hook competition"))
)
gg <- group_by(indexes, type) |>
  mutate(lwr = lwr/est[1], upr = upr/est[1], est = est / est[1]) |>
  ggplot(aes(year, est, colour = type, fill = type)) +
  # geom_line() +
  # geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, colour = NA) + ylim(0, NA) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), alpha = 1, position = position_dodge(width = 0.7), pch = 21, fill = NA) +
  ylim(0, NA) +
  coord_cartesian(expand = F) +
  labs(colour = "Model", fill = "Model", y = "Scaled index", x = "Year") +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position.inside = c(0.7, 0.8))
ggsave("figs/hbll_out/index_model_comparison.png", width = 7, height = 4)

ggplot() +
  geom_pointrange(data = indexes, aes(year, est, ymin = lwr, ymax = upr, colour = type, group = type)) +
  coord_cartesian(ylim = c(0, NA))

ind_hk <- dplyr::filter(ind, !is.na(survey_abbrev))
ind_nohk <- dplyr::filter(ind_nohk, !is.na(survey_abbrev))
# ind_julian <- dplyr::filter(ind_julian, !is.na(survey_abbrev))

saveRDS(ind_nohk, file = "data/generated/geostat-ind-hbll-out.rds")
ind_nohk <- readRDS("data/generated/geostat-ind-hbll-out.rds")
saveRDS(ind_hk, file = "data/generated/geostat-ind-hbll-out-hook-compet.rds")
ind_hk <- readRDS("data/generated/geostat-ind-hbll-out-hook-compet.rds")
#saveRDS(ind_julian, file = "data/generated/geostat-ind-hbll-out-hook-compet-julian.rds")
#ind_julian <- readRDS("data/generated/geostat-ind-hbll-out-hook-compet-julian.rds")

x <- ggplot(ind_hk, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA))
x + geom_pointrange(data = ind_nohk, aes(year, est, ymin = lwr, ymax = upr), colour = "black")

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

gg <- ggplot(p_nb2_nohk$data, aes(longitude, latitude, fill = omega_s, colour = omega_s)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_tile(width = 0.025, height = 0.025) +
  rb_fill + rb_col +
  labs(x = "Longitude", y = "Latitude", colour = "Spatial effect", fill = "Spatial effect")
ggsave("figs/hbll_out/prediction_grid_omega.png", gg, height = 4, width = 4, dpi = 600)

# Epsilon ----
gg <- ggplot(p_nb2_nohk$data, aes(longitude, latitude, fill = epsilon_st, colour = epsilon_st)) +
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
gg <- ggplot(p_nb2_nohk$data, aes(longitude, latitude, fill = est, colour = est)) +
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
gg <- ggplot(ind_nohk, aes(year, est)) +
  geom_point() +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Year", y = "HBLL Index") +
  expand_limits(y = 0) +
  coord_cartesian(expand = FALSE, xlim = range(ind_nohk$year) + c(-0.5, 0.5))
ggsave("figs/hbll_out/hbll_index.png", gg, height = 3, width = 4)

# Marginal effect of depth ----
marginal_depth <- visreg::visreg(fit_nb2_nohk, xvar = "depth_m", breaks = seq(0, 270, 10),
                                 data = fit_nb2_nohk$data,
                                 plot = FALSE)

gg <- plot(marginal_depth, gg = TRUE,
           line.par = list(col = 1),
           points.par = list(alpha = 0.2)) +
  coord_cartesian(xlim = c(0, 300), expand = FALSE) +
  labs(x = "Depth (m)", y = "log(CPUE)")
ggsave("figs/hbll_out/depth_marginal.png", gg, height = 3, width = 4)
