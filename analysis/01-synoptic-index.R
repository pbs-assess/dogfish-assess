library(dplyr)
library(ggplot2)
library(sdmTMB)
library(cowplot)
theme_set(gfplot::theme_pbs())

coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

s <- readRDS("data/raw/survey-sets_2023.rds")
#s <- readRDS("data/raw/survey-sets.rds") #where did this file come from?
# SA: it's built in data/raw/pull-raw-data.R
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


library(rosettafish)
source("analysis/plot_multiyear_survey_sets.R")
dir.create("figs/synoptic", showWarnings = FALSE)
g <- plot_multiyear_survey_sets(d, "SYN WCVI")
ggsave("figs/synoptic/sets-wcvi.png", width = 10, height = 6, dpi = 160)
g <- plot_multiyear_survey_sets(d, "SYN QCS")
ggsave("figs/synoptic/sets-qcs.png", width = 10, height = 6, dpi = 160)
g <- plot_multiyear_survey_sets(d, "SYN HS")
ggsave("figs/synoptic/sets-hs.png", width = 10, height = 6, dpi = 160)
g <- plot_multiyear_survey_sets(d, "SYN WCHG")
ggsave("figs/synoptic/sets-wchg.png", width = 6, height = 7, dpi = 160)

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

gg <- d %>%
  mutate(cpue = 100 * catch_weight/area_swept) %>%
  filter(cpue < quantile(cpue, 0.99), .by = year) %>%
  filter(survey_abbrev == "SYN WCVI") %>%
  ggplot(aes(depth_m, log1p(cpue), colour = survey_abbrev, fill = survey_abbrev)) +
  facet_wrap(vars(year), scales = "free_y", ncol = 4) +
  geom_point(alpha = 0.25, shape = 21) +
  scale_x_continuous(trans = "log", breaks = c(20, 50, 150, 400)) +
  #scale_x_continuous(trans = "log", breaks = seq(3, 7, 1) %>% exp() %>% floor()) +
  theme_bw() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  labs(x = "Depth (m)", y = "log(CPUE + 1)") +
  ggtitle("SYN WCVI")
ggsave("figs/synoptic/cpue_depth_year_wcvi.png", gg, height = 6, width = 6)

gg <- d %>%
  mutate(cpue = 100 * catch_weight/area_swept) %>%
  #filter(cpue < quantile(cpue, 0.975), .by = year) %>%
  filter(survey_abbrev == "SYN WCVI") %>%
  ggplot(aes(depth_m, log1p(cpue), colour = survey_abbrev, fill = survey_abbrev)) +
  facet_wrap(vars(year), scales = "free_y", ncol = 4) +
  geom_point(alpha = 0.25, shape = 21) +
  scale_x_continuous(trans = "log", breaks = c(20, 50, 150, 400)) +
  #scale_x_continuous(trans = "log", breaks = seq(3, 7, 1) %>% exp() %>% floor()) +
  theme_bw() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  labs(x = "Depth (m)", y = "log(CPUE + 1)") +
  ggtitle("SYN WCVI")
ggsave("figs/synoptic/cpue_depth_year_wcvi.png", gg, height = 4, width = 6)

gg <- d %>%
  mutate(cpue = 100 * catch_weight/area_swept) %>%
  filter(cpue > 0) %>%
  filter(survey_abbrev != "SYN WCHG") %>%
  mutate(md = mean(depth_m), .by = c(year, survey_abbrev)) %>%
  ggplot(aes(year, md, colour = survey_abbrev)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Mean depth of positive sets", colour = "Survey")
ggsave("figs/synoptic/cpue_mean_depth.png", gg, height = 3, width = 5)

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
  silent = FALSE,
  anisotropy = TRUE
)

fit_lg <- update(fit, family = delta_lognormal())
fit_dgg <- update(fit, family = delta_gengamma())

AIC(fit, fit_lg, fit_dgg) |> arrange(AIC)

saveRDS(fit_lg, file = "data/generated/synoptic-sdmTMB-lognormal.rds")
saveRDS(fit, file = "data/generated/synoptic-sdmTMB.rds")
fit <- readRDS("data/generated/synoptic-sdmTMB.rds")
sanity(fit)
sanity(fit_lg)
plot_anisotropy(fit)
ggsave("figs/synoptic/aniso.png", width = 4, height = 4)
plot_anisotropy(fit_lg)
ggsave("figs/synoptic/aniso-lg.png", width = 4, height = 4)
fit
fit_lg
fit$sd_report
fit_lg$sd_report

set.seed(123)
r1 <- residuals(fit, model = 1, type = "mle-mvn")
qqnorm(r1);abline(0, 1)
set.seed(123)
r2 <- residuals(fit, model = 2, type = "mle-mvn")
qqnorm(r2, main = "Gamma");abline(0, 1)

set.seed(123)
r3 <- residuals(fit_lg, model = 2, type = "mle-mvn")
qqnorm(r3, main = "Lognormal");abline(0, 1)

set.seed(123)
r4 <- residuals(fit_dgg, model = 2, type = "mle-mvn")
qqnorm(r4, main = "Gengamma");abline(0, 1)

set.seed(123)
s <- simulate(fit, nsim = 300, type = "mle-mvn")
dr1 <- dharma_residuals(s, fit, test_uniformity = F)
ggplot(dr1, aes(expected, observed)) + geom_abline(intercept = 0, slope = 1, colour = "red") + geom_point() + xlab("Expected") + ylab("Observed")
ggsave("figs/synoptic/qq.png", width = 5, height = 5, dpi = 180)

set.seed(123)
s <- simulate(fit_lg, nsim = 300, type = "mle-mvn")
dr2 <- dharma_residuals(s, fit, test_uniformity = F)
ggplot(dr2, aes(expected, observed)) + geom_abline(intercept = 0, slope = 1, colour = "red") + geom_point() + xlab("Expected") + ylab("Observed")
ggsave("figs/synoptic/qq-lg.png", width = 5, height = 5, dpi = 180)

set.seed(123)
s <- simulate(fit_dgg, nsim = 300, type = "mle-mvn")
dr3 <- dharma_residuals(s, fit, test_uniformity = F)
ggplot(dr3, aes(expected, observed)) + geom_abline(intercept = 0, slope = 1, colour = "red") + geom_point() + xlab("Expected") + ylab("Observed")
ggsave("figs/synoptic/qq-gg.png", width = 5, height = 5, dpi = 180)

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

p_lg <- predict(fit_lg, newdata = grid, return_tmb_object = TRUE)
ind_lg <- get_index(p_lg, bias_correct = TRUE)
saveRDS(ind_lg, file = "data/generated/geostat-ind-synoptic-lg.rds")
ind_lg <- readRDS("data/generated/geostat-ind-synoptic-lg.rds")

p_gg <- predict(fit_dgg, newdata = grid, return_tmb_object = TRUE)
ind_gg <- get_index(p_gg, bias_correct = TRUE)
saveRDS(ind_gg, file = "data/generated/geostat-ind-synoptic-gg.rds")
ind_gg <- readRDS("data/generated/geostat-ind-synoptic-gg.rds")

bind_rows(
  mutate(ind, family = "delta gamma"),
  mutate(ind_lg, family = "delta lognormal"),
  mutate(ind_gg, family = "delta generalized gamma")
) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = family)) +
  geom_pointrange(position = position_dodge(width = 0.2)) +
  coord_cartesian(ylim = c(0, NA))

survs <- select(d, year, survey_abbrev) |> distinct() |>
  group_by(year) |>
  summarise(survey_abbrev = paste(survey_abbrev, collapse = ", "))

# ******** PICK INDEX HERE ************ #
ind <- ind_lg
p <- p_lg
fit <- fit_lg

ind <- left_join(ind, survs, by = join_by(year))

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
  scale_fill_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750, 1250), option = "G") +
  scale_colour_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750, 1250), option = "G") +
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
ggsave("figs/synoptic/prediction_grid_encounter.png", gg, height = 8, width = 6.2, dpi = 200)


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
ggsave("figs/synoptic/prediction_grid_eps.png", gg, height = 8, width = 6.2, dpi = 200)

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
        axis.text.x = element_text(angle = 45, vjust = 0.5)
        # axis.text = element_blank()
    ) +
  labs(x = "Longitude", y = "Latitude", colour = "log density", fill = "log density")
ggsave("figs/synoptic/prediction_grid_density.png", gg, height = 8, width = 6.2, dpi = 200)

# Index ----
gg <- ggplot(ind, aes(year, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Year", y = "Synoptic Trawl Index") +
  expand_limits(y = 0)
ggsave("figs/synoptic/syn_index.png", gg, height = 5, width = 6)

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

# Index by survey area

library(snowfall)
sfInit(parallel = TRUE, cpus = 4)
sfLibrary(sdmTMB)
sfLibrary(tidyverse)
sfExport(list = c("fit", "grid"))
ind_area <- sfLapply(unique(grid$survey), function(i) {
  grid_survey <- dplyr::filter(grid, survey == i)

  p <- predict(fit, newdata = grid_survey, return_tmb_object = TRUE)
  ind <- get_index(p, bias_correct = TRUE) %>%
    mutate(survey_abbrev = i)
  return(ind)
})
saveRDS(ind_area, file = "data/generated/geostat-ind-synoptic-area.rds")
sfStop()

ind_area <- readRDS(file = "data/generated/geostat-ind-synoptic-area.rds") %>%
  bind_rows()

g <- ind_area %>%
  mutate(value = est/sum(est), .by = year) %>%
  ggplot(aes(year, value, fill = survey_abbrev)) +
  geom_col(width = 1, colour = NA) +
  gfplot::theme_pbs() +
  coord_cartesian(expand = FALSE) +
  labs(x = "Year", y = "Proportion biomass", fill = "Survey") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")
g2 <- ind_area %>%
  mutate(value = est * 4 * 1000*1000) |> # 2km x 2km in t
  mutate(value = value / 1000) |> # from kg to t
  mutate(value = value / 1000) |> # from t to 1000 t
  ggplot(aes(year, value, fill = survey_abbrev)) +
  geom_col(width = 1, colour = NA) +
  gfplot::theme_pbs() +
  coord_cartesian(expand = FALSE) +
  labs(x = "Year", y = "Biomass estimate", fill = "Survey") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")
gout <- ggpubr::ggarrange(g2, g, ncol = 2, legend = "bottom", common.legend = TRUE)
ggsave("figs/synoptic/syn_index_area_biomass.png", gout, height = 3, width = 6)


# Compare with design-based index
## Design-based index ----
# QH: area_km2 should be the strata area (unique to each grouping code) so that I can calculate the area-weighted index
# The field was in survey-sets.rds but is missing in survey-sets_2023.rds
#index_design <- d %>%
#  left_join(
#    readRDS("data/raw/survey-sets.rds") %>%
#      select(grouping_code, area_km2) %>%
#      filter(!duplicated(grouping_code)),
#    by = "grouping_code"
#  ) %>%
#  mutate(cpue = catch_weight/area_swept,
#         catch_expand = area_km2 * cpue) %>% #where did this area_km2 value come from? I cahnged to area_swept
#  summarize(index_strat = mean(catch_expand),
#            var_strat = var(catch_expand),
#            n = n(),
#            nsamp = unique(area_km2)/sum(area_swept) * n * 1e3 * 1e3, # Number of sampling units per stratum
#            .by = c(year, grouping_code, survey_abbrev, area_km2)) %>%
#  mutate(area_total = sum(area_km2), .by = c(year, survey_abbrev)) %>%
#  summarize(Biomass = sum(index_strat),
#            Var = sum(nsamp * (nsamp - n)/n * var_strat)/sum(nsamp)/sum(nsamp), # See SimSurvey appendix
#            #Var = sum(var_strat * area_km2^2/area_total^2),
#            .by = c(year, survey_abbrev)) %>%
#  mutate(SE = sqrt(Var), CV = SE/Biomass,
#         lwr = Biomass - 2 * SE, upr = Biomass + 2 * SE)
#g <- index_design %>%
#  ggplot(aes(year, Biomass)) +
#  geom_linerange(aes(ymin = lwr, ymax = upr)) +
#  geom_point() +
#  theme(panel.spacing = unit(0, "in"), legend.position = "bottom") +
#  expand_limits(y = 0) +
#  facet_wrap(vars(survey_abbrev), scales = "free_y") +
#  labs(x = "Year", y = "Index of biomass")
#ggsave("figs/synoptic/syn_index_design.png", g, height = 4, width = 6)
#
#ind_compare <- rbind(
#  index_design %>% select(year, survey_abbrev, Biomass, lwr, upr) %>%
#    rename(est = Biomass) %>% mutate(type = "Design-based"),
#  ind_area %>%
#    select(year, survey_abbrev, est, lwr, upr) %>%
#    mutate(type = "Spatiotemporal model")
#)

index_design <- readRDS("data/raw/design-based-indices.rds") %>%
  filter(survey_abbrev %in% unique(ind_area$survey_abbrev)) %>%
  select(year, survey_abbrev, biomass, lowerci, upperci) %>%
  rename(est = biomass, lwr = lowerci, upr = upperci) %>%
  mutate(est = 1e-7 * est, lwr = 1e-7 * lwr, upr = 1e-7 * upr)

ind_compare <- rbind(
  index_design %>% mutate(type = "Design-based"),
  ind_area %>%
    select(year, survey_abbrev, est, lwr, upr) %>%
    mutate(type = "Spatiotemporal model")
)


yrs <- select(ind_compare, year, survey_abbrev, type) |>
  filter(type == "Design-based") |>
  distinct() |> select(-type)

g <- ind_compare %>%
  right_join(yrs) |>
  mutate(value = est/exp(mean(log(est))),
         lwr = lwr/exp(mean(log(est))),
         upr = upr/exp(mean(log(est))),
         .by = c(type, survey_abbrev)) %>%
  mutate(upr = ifelse(survey_abbrev == "SYN QCS", pmin(upr, 7), upr)) %>%
  mutate(upr = ifelse(survey_abbrev == "SYN WCHG", pmin(upr, 6), upr)) %>%
  ggplot(aes(year, value, colour = type, shape = type)) +
  geom_linerange(aes(ymin = lwr, ymax = upr),
                 position = position_dodge(0.5), linewidth = 0.25) +
  geom_point(position = position_dodge(0.5)) +
  theme(panel.spacing = unit(0.04, "in"),
        legend.position = "bottom") +
  expand_limits(y = 0) +
  facet_wrap(vars(survey_abbrev), scales = "free_y") +
  scale_shape_manual(values = c(1, 16)) +
  coord_cartesian(xlim = c(2000, 2024), expand = FALSE) +
  labs(x = "Year", y = "Relative Biomass Index", colour = "Method", shape = "Method") +
  scale_colour_brewer(palette = "Set2")
ggsave("figs/synoptic/syn_index_compare_design.png", g, height = 5, width = 6)

## 2024:

library(sdmTMB)
d <- readRDS("data/raw/syn-wcvi-dogfish-2024.rds")
d$area_swept1 <- d$doorspread_m * d$tow_length_m
d$area_swept2 <- d$doorspread_m * (d$speed_mpm * d$duration_min)
d$area_swept <- ifelse(!is.na(d$area_swept1), d$area_swept1, d$area_swept2)

d <- add_utm_columns(d)

d <- filter(d, !is.na(depth_m))
mesh <- make_mesh(d, c("X", "Y"), cutoff = 5)
plot(mesh)
mesh$mesh$n

m <- sdmTMB(catch_weight ~ 0 + poly(log(depth_m), 2L),
  time_varying = ~ 1,
  time_varying_type = "rw",
  family = delta_lognormal(), mesh = mesh, time = "year",
  anisotropy = TRUE,
  silent = FALSE,
  data = d
)
m
m$sd_report
sanity(m)

grid_survey <- dplyr::filter(grid, survey == "SYN WCVI") |> select(-year) |>
  distinct() |>
  replicate_df(time_name = "year", time_values = unique(d$year))

p2024 <- predict(m, newdata = grid_survey, return_tmb_object = TRUE)
ind2024 <- get_index(p2024, bias_correct = TRUE)

ind2024 |>
  mutate(
    value = est/exp(mean(log(est))),
    lwr = lwr/exp(mean(log(est))),
    upr = upr/exp(mean(log(est)))) |>
  ggplot(aes(year, value, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, xlim = c(2003, 2025)) +
  scale_x_continuous(breaks = seq(2000, 2025, 2)) +
  ylab("Biomass index") + xlab("")
ggsave("figs/synoptic/syn_2024.png", width = 5, height = 4)
