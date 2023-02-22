library(dplyr)
library(ggplot2)
library(sdmTMB)
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

# hook competition
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

mesh <- make_mesh(d, c("X", "Y"), cutoff = 12)
plot(mesh)
mesh$mesh$n

fit_nb2 <- sdmTMB(
  catch_count ~ 1 + poly(log(depth_m), 2L),
  family = nbinom2(link = "log"),
  data = d,
  mesh = mesh,
  offset = "offset", # hook competition offset
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
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

p_nb2 <- predict(fit_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p_nb2, bias_correct = TRUE)
survs <- select(d, year, survey_abbrev) |> distinct()
ind <- left_join(ind, survs, by = join_by(year))
# ind <- left_join(ind, survs, by = "year")

ggplot() +
  geom_pointrange(data = ind, aes(year, est, ymin = lwr, ymax = upr, colour = survey_abbrev)) +
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
