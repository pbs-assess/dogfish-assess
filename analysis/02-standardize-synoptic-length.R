# This script standardizes the synoptic length composition by imputing the length composition in missing strata

library(tidyverse)
theme_set(gfplot::theme_pbs())
#remotes::install_github("James-Thorson-NOAA/VAST")

source("analysis/utils-VAST.R")

## Biological samples ----
d <- readRDS("data/raw/survey-samples.rds") %>%
  filter(grepl("SYN", survey_abbrev)) %>%
  filter(sex != 0, !is.na(length)) %>%
  mutate(sex = ifelse(sex == 1, "M", "F"))

## Set data ----
dsets <- readRDS("data/raw/survey-sets.rds") %>%
  filter(grepl("SYN", survey_abbrev),
         !duplicated(fishing_event_id)) %>%
  mutate(area_swept1 = doorspread_m * tow_length_m,
         area_swept2 = doorspread_m * (speed_mpm * duration_min),
         area_swept = ifelse(!is.na(area_swept1), area_swept1, area_swept2),
         cpue_set = catch_weight/area_swept * 1e3 * 1e3) # kg/km^2








## Create length comp from expanded numbers ----
len_bin <- seq(35, 115, 5)
dbin2 <- expand_comp(d, dsets, len_bin = len_bin)
saveRDS(dbin2, file = "data/raw/synoptic_cpue_length.rds")
dbin2 <- readRDS("data/raw/synoptic_cpue_length.rds")


## Figures stratified index and individual sets by bin and sex ----
# By area
# "X" indicates years when there zero catch rates at a given size bin in a survey location
index_bin <- dbin2 %>%
  summarize(cpue_strat = mean(area_km2 * cpue),
            var_strat = var(area_km2 * cpue),
            n = n(),
            nsamp = unique(area_km2)/sum(area_swept) * n * 1e3 * 1e3, # Number of sampling units per stratum
            .by = c(year, sex, bin, grouping_code, survey_abbrev)) %>%
  #mutate(p_area = area_km2/sum(area_km2), .by = c(year, sex, bin, survey_abbrev)) %>%
  summarize(Abundance = sum(cpue_strat),
            Var = sum(nsamp * (nsamp - n)/n * var_strat)/sum(nsamp)/sum(nsamp), # See SimSurvey appendix
            .by = c(year, sex, bin, survey_abbrev)) %>%
  mutate(SE = sqrt(Var), CV = SE/Abundance) %>%
  mutate(est = Abundance/geom_mean(Abundance),
         lwr = (Abundance - 2 * SE)/geom_mean(Abundance),
         upr = (Abundance + 2 * SE)/geom_mean(Abundance),
         .by = bin) %>%
  mutate(est = log1p(est), lwr = log1p(pmax(lwr, 0)), upr = log1p(upr))

g <- index_bin %>%
  filter(Abundance > 0, bin %in% len_bin[1:9]) %>%
  ggplot(aes(year, est, shape = sex, colour = sex)) +
  geom_line(linewidth = 0.1) +
  geom_point() +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  geom_point(data = index_bin %>% filter(Abundance == 0, bin %in% len_bin[1:9]), shape = 4) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = "bottom") +
  facet_grid(vars(bin), vars(survey_abbrev)) +
  labs(x = "Year", y = "log(Relative abundance + 1)", colour = "Sex", shape = "Sex") +
  scale_shape_manual(values = c(1, 16)) +
  coord_cartesian(ylim = c(0, 4))
ggsave("figs/synoptic_length/strat_index_length1.png", g, height = 7, width = 6)

g <- index_bin %>%
  filter(Abundance > 0, !bin %in% len_bin[1:9]) %>%
  ggplot(aes(year, est, shape = sex, colour = sex)) +
  geom_line(linewidth = 0.1) +
  geom_point() +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  geom_point(data = index_bin %>% filter(Abundance == 0, !bin %in% len_bin[1:9]), shape = 4) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = "bottom") +
  facet_grid(vars(bin), vars(survey_abbrev)) +
  labs(x = "Year", y = "log(Relative abundance + 1)", colour = "Sex", shape = "Sex") +
  scale_shape_manual(values = c(1, 16)) +
  coord_cartesian(ylim = c(0, 4))
ggsave("figs/synoptic_length/strat_index_length2.png", g, height = 6.5, width = 6)


coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)
g <- ggplot(dbin2 %>% filter(cpue > 0), aes(longitude, latitude, colour = cpue)) +
  #geom_sf(data = coast, inherit.aes = FALSE) +
  #coord_sf(expand = FALSE) +
  geom_point(alpha = 0.5) +
  facet_grid(vars(bin), vars(sex)) +
  labs(x = "Longitude", y = "Latitude", colour = "CPUE") +
  scale_colour_viridis_c(trans = "log", direction = -1, breaks = c(54.9, 1096.6, 22026.4)) +
  theme(panel.spacing = unit(0, "in"))
ggsave("figs/synoptic_length/sets_bin_sex.png", g, height = 10, width = 6)

g <- ggplot(dbin2 %>% filter(cpue == 0), aes(longitude, latitude)) +
  #geom_sf(data = coast, inherit.aes = FALSE) +
  #coord_sf(expand = FALSE) +
  geom_point(alpha = 0.1, shape = 1, size = 0.5) +
  geom_point(data = dbin2 %>% filter(cpue > 0), colour = 2, alpha = 0.1, size = 0.5) +
  facet_grid(vars(bin), vars(sex)) +
  labs(x = "Longitude", y = "Latitude") +
  #scale_size_manual(values = c("TRUE" = 1, "FALSE" = 0.1)) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = "bottom")
ggsave("figs/synoptic_length/sets_bin_sex_presence.png", g, height = 10, width = 4)

# Positive sets with no length data (hopefully missing at random)
dsets_samp <- left_join(
  dsets,
  d %>% summarize(nlength = n(), .by = fishing_event_id)
) %>%
  mutate(not_samp = catch_weight > 0 & is.na(nlength))

pnot_samp <- dsets_samp %>%
  summarize(p = mean(not_samp) %>% round(2) %>% format(), .by = year)

g <- ggplot(dsets_samp, aes(longitude, latitude, colour = not_samp)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(alpha = 0.1) +
  geom_label(data = pnot_samp, inherit.aes = FALSE, aes(label = p), x = -Inf, y = -Inf,
             hjust = "inward", vjust = "inward") +
  facet_wrap(vars(year)) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_shape_manual(values = c("TRUE" = 1, "FALSE" = 4)) +
  labs(x = "Longitude", y = "Latitude", colour = "Unsampled") +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave("figs/synoptic_length/sets_unsampled.png", g, height = 8, width = 6, dpi = 600)

g <- dsets_samp %>%
  filter(!is.na(cpue_set)) %>%
  filter(cpue_set < 1000,
         cpue_set > 0) %>%
  ggplot(aes(cpue_set, after_stat(ndensity))) +
  geom_density(aes(fill = not_samp), alpha = 0.4) +
  #geom_histogram(aes(fill = not_samp), alpha = 0.75, position = "dodge") +
  facet_wrap(vars(survey_abbrev)) +
  #coord_cartesian(xlim = c(0, 600)) +
  labs(x = "CPUE", y = "Density of sets", fill = "Unsampled")
ggsave("figs/synoptic_length/sets_unsampled_hist.png", g, height = 4, width = 6)
















## Fit VAST model ----
# Grab saved dataframe and remove categories with zero catches
dbin2 <- readRDS("data/raw/synoptic_cpue_length.rds") %>%
  mutate(category = paste0(sex, "_", bin))
len_bin <- unique(dbin2$bin) %>% as.character() %>% as.numeric() # Factors to numeric
dbin2 %>% summarize(n = sum(cpue > 0), .by = category) %>% filter(n < 10)
dbin2 <- filter(dbin2, !category %in% c("M_100", "M_105", "M_110", "M_115"))

# Intermediate figures - presence by bin and year
bin_presence <- summarize(dbin2, p = mean(cpue > 0), .by = c(bin, sex, year))
g <- ggplot(bin_presence, aes(year, p, colour = sex)) +
  facet_wrap(vars(bin)) +
  geom_point() +
  geom_line()

# Create category factors
DF_categories <- c("F_", "M_") %>% rep(each = length(unique(dbin2$bin))) %>% paste0(unique(dbin2$bin))
#DF_error_struct <- local({ # Size bins <= 40 cm share same sigma, likewise with >= 95 cm
#  v <- strsplit(DF_categories, "_") %>% sapply(getElement, 2) %>% as.numeric()
#  ifelse(v <= 40, 40, ifelse(v >= 95, 95, DF_categories))
#})
DF <- dbin2 %>%
  mutate(
    category = factor(category, levels = DF_categories[DF_categories %in% unique(category)]),
    cat_int = category %>% as.integer() %>% `-`(1),
    err_int = cat_int, # Integers that group the error structure
    #err_int = DF_error_struct[match(category, DF_categories)] %>% factor() %>% as.integer() %>% `-`(1) # This crashes VAST
  )
table(DF$category, DF$cat_int)
table(DF$category, DF$err_int)

# Create prediction grid
input_grid <- gfplot::synoptic_grid[, c("X", "Y", "cell_area")] %>%
  mutate(X = 1e3 * X, Y = 1e3 * Y) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), ll_crs = 32609, utm_names = c("Lon", "Lat"), utm_crs = 4326, units = "m") %>%
  select(Lon, Lat, cell_area) %>%
  rename(Area_km2 = cell_area) %>%
  mutate(Area_km2 = 1) %>%
  as.matrix()

# Call VAST ----

#library(VAST)
tictoc::tic()

# Betas (intercepts) are constant over time, Epsilon is a random walk over time
RhoConfig <- c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 2, "Epsilon2" = 2)
fit <- fit_VAST_model(
  DF,
  input_grid,
  category_names = DF$category %>% table() %>% names(),
  Omega = c(2, 2),          # Number of spatial fields
  Epsilon = c(2, 2),        # Number of spatiotemporal fields
  Beta = c("IID", "IID"),   # Intercepts for each category
  RhoConfig = RhoConfig,
  Version = "VAST_v14_0_1", # FishStatsUtils::get_latest_version(package = "VAST")
  run_model = FALSE
)
tictoc::toc()

fit$parameter_estimates
fit$parameter_estimates$SD <- TMB::sdreport(fit$tmb_list$Obj, bias.correct = FALSE)
saveRDS(fit, file = "data/generated/VAST_2f_bin5.rds")













# Plot observed predicted data ----
fit <- readRDS("data/generated/VAST_3f_bin5.rds")
#fit_2f <- readRDS("data/generated/VAST_2f_bin5.rds")
#fit <- readRDS("D:/synoptic_length/VAST_3f_bin5.rds")
#fit_2f <- readRDS("D:/synoptic_length/VAST_2f_bin5.rds")

#AIC <- list(fit, fit_2f) %>% sapply(function(i) i[["parameter_estimates"]][["AIC"]])
#AIC - min(AIC) # The 3 factor model reduces AIC by 160 units, VAST does not count random effects

pred <- fit$data_frame %>%
  mutate(b_i = as.numeric(b_i), a_i = as.numeric(a_i),
         R1_i = fit$Report$R1_i,
         R2_i = fit$Report$R2_i,
         category = fit$category_names[c_iz + 1] %>% factor(levels = fit$category_names),
         D_i = fit$Report$D_i,
         Dobs_i = b_i/a_i,
         log_Dobs_i = log(Dobs_i))

# Observed positives and predicted mean encounter rate by category
#g <- pred %>%
#  summarize(pos_obs = mean(b_i > 0),
#            pos_pred = mean(R1_i),
#            .by = category) %>%
#  reshape2::melt(id.var = "category") %>%
#  ggplot(aes(category, value, colour = variable)) +
#  geom_point() +
#  geom_line()


# Next two figures show nominal log(Obs/Pred) for positive sets and second linear predictor
g <- pred %>%
  filter(b_i > 0) %>%
  ggplot(aes(log(b_i/a_i/R2_i))) +
  facet_wrap(vars(category)) +
  geom_histogram(colour = 1, fill = "grey80", linewidth = 0.1) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "log(Observed/Predicted)") +
  theme(panel.spacing = unit(0, "in")) +
  ggtitle("Positive sets with second linear predictor")
ggsave("figs/synoptic_length/resid_nominal_hist.png", g, height = 5, width = 6)

#g <- pred %>%
#  filter(b_i > 0) %>%
#  ggplot(aes(log(b_i/a_i), log(R2_i), colour = factor(t_i))) +
#  facet_wrap(vars(category)) +
#  geom_point(alpha = 0.4) +
#  geom_abline(slope = 1, intercept = 0, linetype = 2) +
#  labs(x = "log(Observed)", y = "log(Predicted)") +
#  ggtitle("Positive sets with second linear predictor")

# Predicted encounter probability by positive and negative sets
g <- pred %>%
  #filter(b_i == 0) %>%
  ggplot(aes(R1_i, after_stat(ndensity))) +
  facet_wrap(vars(category)) +
  geom_density(fill = NA, aes(linetype = b_i > 0)) +
  theme(legend.position = "bottom",
        panel.spacing = unit(0, "in")) +
  labs(x = "Predicted encounter probability", y = "Relative density", linetype = "Positive set")
ggsave("figs/synoptic_length/pred_encounter_hist.png", g, height = 5, width = 6)


# Plot factors, correlations, and loadings. Saved in working_dir
plot_VAST_factor(fit, working_dir = "figs/synoptic_length", save_figure = TRUE, rel_size = 2)

# Plot spatial density ----
g <- plot_spatial_cpue(fit, category = "F_80", type = "pred")
ggsave("figs/synoptic_length/pred_F_80.png", g, height = 8, width = 6)

g <- plot_spatial_cpue(dbin2 = dbin2, category = "F_80", type = "obs", cell_size = 0.25)
ggsave("figs/synoptic_length/obs_F_80.png", g, height = 8, width = 6)


#attr(fit$Report$D_gct, "units") <- NULL
#D_gct <- fit$Report$D_gct %>%
#  structure(class = "array") %>%
#  reshape2::melt() %>%
#  rename(Year = Time) %>%
#  left_join(fit$spatial_list$latlon_g %>% as.data.frame() %>% mutate(Site = 1:nrow(.)), by = "Site") %>%
#  left_join(fit$spatial_list$loc_g %>% as.data.frame() %>% mutate(Site = 1:nrow(.)), by = "Site")
#
#coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
#  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)
#g <- D_gct %>%
#  dplyr::filter(Year == max(D_gct$Year)) %>%
#  ggplot(aes(Lon, Lat)) +
#  geom_sf(data = coast, inherit.aes = FALSE) +
#  coord_sf(expand = FALSE) +
#  geom_tile(height = 0.025, width = 0.025, aes(fill = value, colour = value)) +
#  facet_wrap(vars(Category)) +
#  scale_fill_viridis_c(trans = "sqrt", direction = -1) +
#  scale_colour_viridis_c(trans = "sqrt", direction = -1) +
#  labs(x = "Longitude", y = "Latitude", colour = "Log-Density", fill = "Log-Density") +
#  theme(panel.spacing = unit(0, "in"))
#ggsave("figs/synoptic_length/Density_lastyear.png", g, height = 8, width = 6, dpi = 600)

## Compare standardized and nominal length comp ----

# Nominal len comp - get data frame from top of this script
len_bin <- unique(dbin2$bin) %>% as.character() %>% as.numeric()
len_nom <- d %>%
  mutate(length = ifelse(length < min(len_bin), min(len_bin), length),
         length = ifelse(length > max(len_bin), max(len_bin), length),
         bin = len_bin[findInterval(length, len_bin)]) %>%
  summarise(n = n(), .by = c(year, bin, sex)) %>%
  reshape2::dcast(year + sex ~ bin, value.var = "n", fill = 0) %>%
  reshape2::melt(id.vars = c("year", "sex"), variable.name = "bin", value.name = "n") %>%
  mutate(p = n/sum(n), .by = year) %>%
  mutate(bin = as.character(bin) %>% as.numeric(),
         type = "Nominal")

# Predicted length comp
# Make sure to do the bias correction for the final model !!!
attr(fit$Report$Index_ctl, "units") <- NULL
len_std <- fit$Report$Index_ctl[, , 1] %>%
  structure(class = "matrix") %>%
  reshape2::melt() %>%
  rename(year = Time) %>%
  mutate(p = value/sum(value), .by = year) %>%
  mutate(sex = substr(Category, 1, 1),
         bin = strsplit(Category %>% as.character(), "_") %>% sapply(getElement, 2) %>% as.numeric(),
         type = "Standardized")

g <- plot_lencomp(len_std, len_nom, y = 2003:2012) +
  coord_cartesian(xlim = c(35, 105), ylim = c(0, 0.3))
ggsave("figs/synoptic_length/compare_comp1.png", g, height = 8, width = 5)

g <- plot_lencomp(len_std, len_nom, y = 2013:2022) +
  coord_cartesian(xlim = c(35, 105), ylim = c(0, 0.3))
ggsave("figs/synoptic_length/compare_comp2.png", g, height = 8, width = 5)

# Length comp by survey area
len_nom_surv <- d %>%
  mutate(length = ifelse(length < min(len_bin), min(len_bin), length),
         length = ifelse(length > max(len_bin), max(len_bin), length),
         bin = len_bin[findInterval(length, len_bin)]) %>%
  summarise(n = n(), .by = c(year, bin, sex, survey_abbrev)) %>%
  reshape2::dcast(year + survey_abbrev + sex ~ bin, value.var = "n", fill = 0) %>%
  reshape2::melt(id.vars = c("year", "sex", "survey_abbrev"), variable.name = "bin", value.name = "n") %>%
  mutate(p = n/sum(n), .by = c(year, survey_abbrev)) %>%
  mutate(bin = as.character(bin) %>% as.numeric(),
         type = "Nominal")

attr(fit$Report$D_gct, "units") <- NULL
len_std_surv <- fit$Report$D_gct %>%
  structure(class = "array") %>%
  reshape2::melt() %>%
  left_join(gfplot::synoptic_grid %>% select(survey) %>% mutate(Site = 1:n()), by = "Site") %>%
  rename(survey_abbrev = survey, year = Time) %>%
  summarize(value = sum(value), .by = c(survey_abbrev, year, Category)) %>%
  mutate(p = value/sum(value), .by = c(year, survey_abbrev)) %>%
  mutate(sex = substr(Category, 1, 1),
         bin = strsplit(Category %>% as.character(), "_") %>% sapply(getElement, 2) %>% as.numeric(),
         type = "Standardized")

g <- plot_lencomp(len_std_surv, len_nom_surv, y = 2003:2012) +
  coord_cartesian(xlim = c(35, 105), ylim = c(-0.2, 0.2))
ggsave("figs/synoptic_length/compare_area_comp1.png", g, height = 8, width = 5)

g <- plot_lencomp(len_std_surv, len_nom_surv, y = 2013:2022) +
  coord_cartesian(xlim = c(35, 105), ylim = c(-0.2, 0.2))
ggsave("figs/synoptic_length/compare_area_comp2.png", g, height = 8, width = 5)

# Proportion abundance by area
g <- len_std_surv %>%
  mutate(parea = value/sum(value), .by = c(year, Category)) %>%
  ggplot(aes(year, parea, fill = survey_abbrev)) +
  geom_col(width = 1, colour = NA) +
  facet_wrap(vars(Category), ncol = 6) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Year", y = "Proportion abundance", fill = "Survey") +
  coord_cartesian(expand = FALSE)
ggsave("figs/synoptic_length/compare_area_prop.png", g, height = 6, width = 6)


# Mean length by area
mlen_obs <- len_nom_surv %>%
  summarize(value = weighted.mean(bin + 2.5, n), .by = c(year, sex, survey_abbrev, type))
mlen_pred <- len_std_surv %>%
  summarize(value = weighted.mean(bin + 2.5, value), .by = c(year, sex, survey_abbrev, type))

g <- rbind(mlen_obs, mlen_pred) %>%
  ggplot(aes(year, value, shape = type, linetype = type)) +
  geom_line() +
  geom_point() +
  facet_grid(vars(survey_abbrev), vars(sex)) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(50, 100)) +
  labs(x = "Year", y = "Mean length (cm)", colour = "Sex", shape = "Series", linetype = "Series") +
  scale_shape_manual(values = c(1, 16)) +
  scale_linetype_manual(values = c("Nominal" = 2, "Standardized" = 1))
ggsave("figs/synoptic_length/mean_length_area.png", g, height = 7, width = 6)

# Coastwide mean length
mlen_obs_cw <- len_nom_surv %>%
  summarize(value = weighted.mean(bin + 2.5, n), .by = c(year, sex, type))

mlen_pred_cw <- fit$Report$D_gct %>%
  structure(class = "array") %>%
  reshape2::melt() %>%
  left_join(gfplot::synoptic_grid %>% select(survey) %>% mutate(Site = 1:n()), by = "Site") %>%
  rename(survey_abbrev = survey, year = Time) %>%
  summarize(value = sum(value), .by = c(year, Category)) %>%
  mutate(p = value/sum(value), .by = c(year)) %>%
  mutate(sex = substr(Category, 1, 1),
         bin = strsplit(Category %>% as.character(), "_") %>% sapply(getElement, 2) %>% as.numeric(),
         type = "Standardized") %>%
  summarise(value = weighted.mean(bin + 2.5, value), .by = c(year, sex, type))
  select(year, sex, type, value)

g <- rbind(mlen_obs_cw, mlen_pred_cw) %>%
  ggplot(aes(year, value, shape = type, linetype = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(sex)) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(50, 100)) +
  labs(x = "Year", y = "Coastwide mean length (cm)", colour = "Sex", shape = "Series", linetype = "Series") +
  scale_shape_manual(values = c(1, 16)) +
  scale_linetype_manual(values = c("Nominal" = 2, "Standardized" = 1))
ggsave("figs/synoptic_length/mean_length_coastwide.png", g, height = 3, width = 6)



# Index by length bin
g <- len_std %>%
  #mutate(bin = paste(bin, "cm")) %>%
  ggplot(aes(year, log1p(value), colour = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(bin)) +
  labs(x = "Year", y = "log(Abundance + 1)", colour = "Sex") +
  expand_limits(y = 0)

# Relative depletion (value of 1 in first year of series)
g <- len_std %>%
  mutate(mu = value/value[1], .by = Category) %>%
  ggplot(aes(year, mu, colour = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(bin)) +
  labs(x = "Year", y = "Relative abundance", colour = "Sex") +
  coord_cartesian(ylim = c(0, 6))
ggsave("figs/synoptic_length/length_index_rel_year1.png", g, height = 5, width = 6)

# Relative series (relative to geometric mean of each size bin)
g <- len_std %>%
  mutate(mu = value/geom_mean(value), .by = bin) %>%
  ggplot(aes(year, mu, colour = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(bin)) +
  labs(x = "Year", y = "Relative abundance", colour = "Sex") +
  coord_cartesian(ylim = c(0, 6))
ggsave("figs/synoptic_length/length_index_rel_bin.png", g, height = 5, width = 6)

# Cluster analysis of length bins in VAST model ----
cspatial <- plot_dendrogram(fit, k = 3, Lvec = c("L_omega1_cf", "L_omega2_cf"), rel_size = 2)
cst <- plot_dendrogram(fit, k = 3, Lvec = c("L_epsilon1_cf", "L_epsilon2_cf"), rel_size = 2)

ggsave("figs/synoptic_length/ncluster_spatial.png",
       cspatial$g_nclust + ggtitle(NULL),
       height = 3, width = 4)
ggsave("figs/synoptic_length/ncluster_st.png",
       cst$g_nclust + ggtitle(NULL),
       height = 3, width = 4)

ggsave("figs/synoptic_length/corr_spatial.png",
       cspatial$g_corr + theme(axis.text.x = element_text(angle = 90)),
       height = 5, width = 6)
ggsave("figs/synoptic_length/den_spatial.png",
       cspatial$g_clust + ggtitle(NULL),
       height = 4, width = 6)

ggsave("figs/synoptic_length/corr_st.png",
       cst$g_corr + theme(axis.text.x = element_text(angle = 90)),
       height = 5, width = 6)
ggsave("figs/synoptic_length/den_st.png",
       cst$g_clust + ggtitle(NULL),
       height = 4, width = 6)

# Plot first two principal components of the total variance matrix...
plot_PCA(fit, L = c("L_epsilon1_cf", "L_epsilon2_cf"))
plot_PCA(fit, L = c("L_omega1_cf", "L_omega2_cf"))


# Variance calculations for proportions
comp_SE <- calculate_SE(fit)

# Effective sample size
tau <- comp_SE %>%
  summarize(Nmed = calculate_Neff(p, SE_p, median),
            Nmean = calculate_Neff(p, SE_p, mean),
            .by = Time)

# Make SS3 data frame
ss_comp <- left_join(len_std, tau, by = c("year" = "Time")) %>%
  mutate(value = round(Nmed * p, 2)) %>%
  mutate(month = 1, fleet = 6, partition = 0, total = round(Nmed, 1)) %>%
  reshape2::dcast(year + month + fleet + partition + total ~ Category, value.var = "value", fill = 0)
readr::write_csv(ss_comp, file = "data/ss3/ss3-length-synoptic-std.csv")

ss_comp_nom <- len_nom %>%
  mutate(total = sum(n), .by = year) %>%
  mutate(category = paste0(sex, "_", bin) %>% factor(levels = DF_categories),
         month = 1, fleet = 6, partition = 0) %>%
  reshape2::dcast(year + month + fleet + partition + total ~ category, value.var = "n", fill = 0)
readr::write_csv(ss_comp_nom, file = "data/ss3/ss3-length-synoptic-nom.csv")

