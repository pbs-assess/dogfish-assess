dir.create("figs/msa/", showWarnings = FALSE)
# calculate a Hecate Strait multi species

# Hecate Strait multi species trawl extends back to 1984
# here I create an index for the HS MS trawl
# add a multiplier to add standard error to the coast wide index

# library -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(cowplot)
theme_set(gfplot::theme_pbs())
# library(gfplot)
# library(gfdata)
library(sf)

# map objects -------------------------------------------------------------
CRS <- 32609
coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

# load data ---------------------------------------------------------------

# surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
surveysets <- readRDS("data/raw/survey-sets.rds")
d <- dplyr::filter(surveysets, survey_abbrev == "HS MSA")
d <- sdmTMB::add_utm_columns(d, utm_crs = CRS)
unique(d$year)
unique(d$survey_abbrev)

# summary plots -----------------------------------------------------------

d |>
  ggplot() +
  geom_point(aes(Y, X, colour = log(catch_weight)), size = 2) +
  facet_wrap(~year)

d |>
  ggplot() +
  geom_jitter(aes(year, log(catch_weight), colour = log(catch_weight), size = catch_weight))

d |>
  ggplot() +
  geom_jitter(aes(year, depth_m, colour = catch_weight, size = catch_weight))

d |>
  ggplot() +
  geom_jitter(aes(year, area_swept, colour = catch_weight, size = catch_weight))

d |>
  ggplot() +
  geom_point(aes(year, doorspread_m, colour = catch_weight), size = 2)

d |>
  ggplot() +
  geom_point(aes(year, tow_length_m, colour = catch_weight), size = 2)

range(d$doorspread_m)
d |> filter(is.na(doorspread_m) == TRUE)
range(d$tow_length_m)
d |>
  filter(is.na(tow_length_m) == TRUE) |>
  tally()
d |> filter(doorspread_m == 0)
d |>
  ggplot() +
  geom_point(aes(year, tow_length_m, colour = catch_weight), size = 2)



# data cleaning -----------------------------------------------------------
d <- d |>
  mutate(UTM.lon = X, UTM.lat = Y)
d$offset <- log(d$area_swept)
d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))
d <- d |> mutate(logbot_depth = log(depth_m), logbot_depth2 = log(depth_m) * log(depth_m))
meanlogbotdepth <- mean(d$logbot_depth)
d <- d |> mutate(logbot_depthc = logbot_depth - meanlogbotdepth)
d$julian_small <- d$julian / 100
d$offset_sm <- log(d$area_swept / 1000)

d |>
  group_by(year) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch))

d |>
  group_by(year) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue))

d |>
  ggplot() +
  geom_point(aes(year, area_swept, colour = catch_weight), size = 2)

d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))

d |>
  ggplot() +
  geom_point(aes(year, julian, colour = catch_weight), size = 2)

# create grid -------------------------------------------------------------

g <- gfplot::synoptic_grid
# drop year and then add the HS years in
g <- g |> dplyr::select(-survey_domain_year)
g$logbot_depth <- log(g$depth)
g$logbot_depth2 <- log(g$depth) * log(g$depth)
g <- g |> mutate(UTM.lon = X, UTM.lat = Y)
g$offset <- 0

plot(g$X, g$Y)
g_sf <- st_as_sf(g, coords = c("X", "Y"), crs = 32609)
plot(st_geometry(g_sf))

# convex polygon
d_sf <- st_as_sf(d, coords = c("X", "Y"), crs = 32609)
plot(st_geometry(d_sf))
hull <- concaveman::concaveman(d_sf)
# I am going to create a buffer
hull_buf <- st_buffer(hull, 8)
plot(hull_buf)

grid_hs <- st_intersection(hull_buf, g_sf)

plot(st_geometry(grid_hs))
plot(st_geometry(d_sf), col = "red", add = TRUE)

grid_sf <- st_as_sf(g, coords = c("X", "Y"), crs = 32609)
grid_hull <- concaveman::concaveman(grid_sf)

d_sf2 <- st_intersection(grid_hull, d_sf)
plot(st_geometry(grid_hs))
plot(st_geometry(d_sf2), col = "red", add = TRUE)

# what are those three points?? pull them out
test <- st_difference(d_sf, grid_hull)
plot(st_geometry(grid_hs))
plot(st_geometry(test), col = "red", add = TRUE)

d_sf2 <- d_sf |> filter(!fishing_event_id %in% test$fishing_event_id)

plot(st_geometry(grid_hs), pch = ".")

# create mesh -------------------------------------------------------------
st_geometry(d_sf2) <- NULL
glimpse(d_sf2)
mesh <- make_mesh(d_sf2, c("UTM.lon", "UTM.lat"), cutoff = 10)
plot(mesh)
mesh$mesh$n

# HS MSM only model -------------------------------------------------------------------
# grid
# years in the survey only
all_years <- seq(min(d_sf2$year), max(d_sf2$year))
missing_years <- sort(setdiff(all_years, unique(d_sf2$year)))
missing_years

plot(grid_hs)
grid_hs$year <- NULL
grid_hs_yrs <- purrr::map_dfr(all_years, ~ tibble(grid_hs, year = .x))
# st_geometry(grid_hs_yrs) <- NULL
grid_hs_yrs$julian <- mean(d_sf2$julian)
grid_hs_yrs$julian_small <- grid_hs_yrs$julian / 100
grid_hs_yrs$offset_sm <- 0
grid_hs_yrs$logbot_depthc <- grid_hs_yrs$logbot_depth - meanlogbotdepth


ggplot(grid_hs, aes(UTM.lon, UTM.lat, fill = exp(logbot_depth), colour = exp(logbot_depth))) +
  geom_tile(width = 2.1, height = 2.1) +
  scale_fill_viridis_c(option = "G", direction = -1) +
  scale_colour_viridis_c(option = "G", direction = -1) +
  labs(colour = "Depth (m)", fill = "Depth (m)") +
  coord_fixed()
ggsave("figs/msa/depth-grid.png", width = 4, height = 5)

d_sf2 |>
  group_by(year) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue))

m <- sdmTMB(
  catch_weight ~ 1 + logbot_depthc + I(logbot_depthc^2),
  # catch_weight ~ 1,
  data = d_sf2,
  time = "year",
  mesh = mesh,
  spatiotemporal = "rw",
  silent = FALSE,
  offset = d_sf2$offset_sm,
  spatial = TRUE,
  do_index = TRUE,
  share_range = TRUE,
  anisotropy = TRUE,
  extra_time = missing_years,
  predict_args = list(newdata = grid_hs_yrs),
  index_args = list(area = grid_hs_yrs$cell_area),
  family = delta_lognormal()
)

set.seed(1234)
s <- simulate(m, nsim = 500, type = "mle-mvn")
dh <- dharma_residuals(s, m, test_uniformity = T)
ggplot(dh, aes(expected, observed)) + geom_abline(intercept = 0, slope = 1, colour = "red") + geom_point(size = 2) + xlab("Expected") + ylab("Observed")
ggsave("figs/msa/qq.png", width = 5, height = 5, dpi = 180)

reduced_grid <- filter(grid_hs_yrs, year %in% d_sf2$year)

print(m)
sanity(m)
plot_anisotropy(m)
ggsave("figs/msa/aniso.png", width = 4, height = 4)

saveRDS(m, "data/generated/m_HSMSdl.rds")
m <- readRDS("data/generated/m_HSMSdl.rds")
m$sd_report

ind_dl <- get_index(m, bias_correct = TRUE)
saveRDS(ind_dl, "data/generated/m_HSMSdl-index.rds")
ind_dl <- readRDS("data/generated/m_HSMSdl-index.rds")

had_data <- select(d_sf2, year) |> distinct() |> mutate(surveyed = TRUE)
ind_dl_filtered <- left_join(ind_dl, had_data) |>
  mutate(surveyed = ifelse(!is.na(surveyed), TRUE, FALSE)) |>
  filter(surveyed) |>
  select(-surveyed)

ind_dl_filtered |>
  ggplot(aes(year, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ylab("Relative biomass index") + xlab("Year") +
  coord_cartesian(expand = FALSE, ylim = c(0, max(ind_dl$upr) * 1.02),
    xlim = c(range(ind_dl$year) + c(-0.5, 0.5))) +
  scale_x_continuous(breaks = seq(1984, 2002, 2))
  # ggtitle("RW fields")
ggsave("figs/msa/index.png", width = 5, height = 4)
saveRDS(ind_dl_filtered, "data/generated/hs-msa-index.rds")

d_sf2$julian_scaled <- (d_sf2$julian - mean(d_sf2$julian))/sd(d_sf2$julian)
reduced_grid$julian_scaled <- (reduced_grid$julian - mean(d_sf2$julian))/sd(d_sf2$julian)

miid <- sdmTMB(
  catch_weight ~ 0 + as.factor(year) + logbot_depthc + I(logbot_depthc^2) + julian_scaled,
  data = d_sf2,
  time = "year",
  mesh = mesh,
  silent = FALSE,
  offset = d_sf2$offset_sm,
  do_index = TRUE,
  anisotropy = TRUE,
  predict_args = list(newdata = reduced_grid),
  index_args = list(area = reduced_grid$cell_area),
  family = delta_lognormal()
)
miid
sanity(miid)

miid_no_jul <- sdmTMB(
  catch_weight ~ 0 + as.factor(year) + logbot_depthc + I(logbot_depthc^2),
  data = d_sf2,
  time = "year",
  mesh = mesh,
  spatiotemporal = "iid",
  silent = FALSE,
  offset = d_sf2$offset_sm,
  do_index = TRUE,
  anisotropy = TRUE,
  predict_args = list(newdata = reduced_grid),
  index_args = list(area = reduced_grid$cell_area),
  family = delta_lognormal()
)
miid_no_jul
miid_no_jul$sd_report
sanity(miid_no_jul)

AIC(miid, miid_no_jul)

ind_iid <- get_index(miid_no_jul, bias_correct = TRUE)

ind_iid |>
  ggplot(aes(year, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ylab("Relative biomass index") + xlab("Year") +
  coord_cartesian(expand = FALSE, ylim = c(0, max(ind_iid$upr) * 1.02),
    xlim = c(range(ind_iid$year) + c(-0.5, 0.5))) +
  scale_x_continuous(breaks = seq(1984, 2002, 2)) +
  ggtitle("IID")
ggsave("figs/msa/index-iid.png", width = 5, height = 4)
d_sf2 |>
  ggplot() +
  geom_point(aes(year, julian, colour = log(catch_weight/exp(d_sf2$offset_sm))), size = 2,
    position = position_jitter(width = 0.4)) +
  scale_colour_viridis_c(option = "A", direction = 1)

plot(d_sf2$julian_small, d_sf2$catch_weight)

ggplot(d_sf2, aes(UTM.lon, UTM.lat, size = catch_weight/exp(offset_sm),
  colour = log(catch_weight/exp(offset_sm)))) +
  geom_point(pch = 21) +
  facet_wrap(~year) +
  scale_size_area(max_size = 10) +
  scale_colour_viridis_c(option = "D", direction = 1) +
  coord_fixed() +
  labs(colour = "CPUE", size = "CPUE")
ggsave("figs/msa/cpue-map.png", width = 7, height = 9)

xx <- group_by(d_sf2, year) |> summarise(mj = mean(julian))
left_join(ind_iid, xx) |> ggplot(aes(mj, est)) + geom_point() + xlab("Julian day") +
  ylab("Index value")

saveRDS(ind_iid, "data/generated/hs-msa-index-iid.rds")
#
# ## Plot figures in prediction grid ----
# # Depth ----
# gg <- ggplot(reduced_grid, aes(UTM.lon, UTM.lat, fill = exp(logbot_depth), colour = exp(logbot_depth))) +
#   geom_sf(data = coast, inherit.aes = FALSE) +
#   coord_sf(expand = FALSE) +
#   geom_tile(width = 0.025, height = 0.025) +
#   scale_fill_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750)) +
#   scale_colour_viridis_c(trans = "sqrt", direction = -1, breaks = c(50, 250, 750)) +
#   labs(x = "Longitude", y = "Latitude", colour = "Depth (m)", fill = "Depth (m)")
# ggsave("figs/hbll_out/prediction_grid_depth.png", gg, height = 4, width = 4, dpi = 200)
#
# # Omega ----
# rb_fill <- scale_fill_gradient2(high = "red", low = "blue", mid = "grey90")
# rb_col <- scale_colour_gradient2(high = "red", low = "blue", mid = "grey90")
#
# gg <- ggplot(p_nb2_nohk$data, aes(longitude, latitude, fill = omega_s, colour = omega_s)) +
#   geom_sf(data = coast, inherit.aes = FALSE) +
#   coord_sf(expand = FALSE) +
#   geom_tile(width = 0.025, height = 0.025) +
#   rb_fill + rb_col +
#   labs(x = "Longitude", y = "Latitude", colour = "Spatial effect", fill = "Spatial effect")
# ggsave("figs/hbll_out/prediction_grid_omega.png", gg, height = 4, width = 4, dpi = 200)
#
# # Epsilon ----
# gg <- ggplot(p_nb2_nohk$data, aes(longitude, latitude, fill = epsilon_st, colour = epsilon_st)) +
#   geom_sf(data = coast, inherit.aes = FALSE) +
#   coord_sf(expand = FALSE) +
#   facet_wrap(vars(year)) +
#   geom_tile(width = 0.025, height = 0.025) +
#   rb_fill + rb_col +
#   theme(panel.spacing = unit(0, "in"),
#     legend.position = 'bottom',
#     axis.text.x = element_text(angle = 45, vjust = 0.5)) +
#   labs(x = "Longitude", y = "Latitude", colour = "Spatiotemporal\neffect", fill = "Spatiotemporal\neffect")
# ggsave("figs/hbll_out/prediction_grid_eps.png", gg, height = 9, width = 7, dpi = 200)
#
# log-density ----

pp <- predict(m, newdata = grid_hs_yrs)

pp <- dplyr::filter(pp, year %in% unique(d_sf2$year))

gg <- ggplot(pp, aes(UTM.lon, UTM.lat, fill = plogis(est1) * exp(est2), colour = plogis(est1) * exp(est2))) +
  # geom_sf(data = coast, inherit.aes = FALSE) +
  # coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_tile(width = 2, height = 2) +
  scale_colour_viridis_c(option = "C", trans = "sqrt") +
  scale_fill_viridis_c(option = "C", trans = "sqrt") +
  theme(panel.spacing = unit(0, "in"),
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "UTM", y = "UTM", colour = "Density", fill = "Density")
ggsave("figs/msa/prediction_grid_density.png", gg, height = 9, width = 7, dpi = 200)

