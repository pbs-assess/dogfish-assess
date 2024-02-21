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

grid_hs$year <- NULL
grid_hs_yrs <- purrr::map_dfr(all_years, ~ tibble(grid_hs, year = .x))
st_geometry(grid_hs_yrs) <- NULL
grid_hs_yrs$julian <- mean(d_sf2$julian)
grid_hs_yrs$julian_small <- grid_hs_yrs$julian / 100
grid_hs_yrs$offset_sm <- 0
grid_hs_yrs$logbot_depthc <- grid_hs_yrs$logbot_depth - meanlogbotdepth

d_sf2 |>
  group_by(year) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue))

m <- sdmTMB(
  catch_weight ~ 1 + logbot_depthc + I(logbot_depthc^2),
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
print(m)
sanity(m)
plot_anisotropy(m)
saveRDS(m, "data/generated/m_HSMSdl.rds")
m <- readRDS("data/generated/m_HSMSdl.rds")

ind_dl <- get_index(m, bias_correct = TRUE)

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

saveRDS(ind_dl_filtered, "data/generated/hs-msa-index.rds")