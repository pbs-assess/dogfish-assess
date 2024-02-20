#calculate a Hecate Strait multi species

#Hecate Strait multi species trawl extends back to 1984
#here I create an index for the HS MS trawl
#add a multiplier to add standard error to the coast wide index

# library -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(cowplot)
theme_set(gfplot::theme_pbs())
library(gfplot)
library(gfdata)
library(sf)

# map objects -------------------------------------------------------------
CRS = 32609
coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)


# load data ---------------------------------------------------------------

#surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
surveysets <- readRDS('data/raw/survey-sets.rds')
d <- dplyr::filter(surveysets, survey_abbrev ==  "HS MSA")
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
  geom_point(aes(year, doorspread_m   , colour = catch_weight), size = 2)

d |>
  ggplot() +
  geom_point(aes(year, tow_length_m   , colour = catch_weight), size = 2)

range(d$doorspread_m)
d |> filter(is.na(doorspread_m) == TRUE)
range(d$tow_length_m)
d |> filter(is.na(tow_length_m) == TRUE) |> tally()
d |> filter(doorspread_m == 0)
d |>
  ggplot() +
  geom_point(aes(year, tow_length_m   , colour = catch_weight), size = 2)



# data cleaning -----------------------------------------------------------
d <- d |>
  mutate(UTM.lon = X, UTM.lat = Y)
d$offset <- log(d$area_swept)
d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))
d <- d |>
  mutate(logbotdepth_m = log(depth_m))
d <- d |> mutate(logbot_depth = log(depth_m), logbot_depth2 = log(depth_m) * log(depth_m))
d$julian_small <- d$julian/100

d |>
  group_by(year) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch/effort) |>
  ggplot() +
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch))

d |>
  group_by(year) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch/effort) |>
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
#drop year and then add the HS years in
g <- g |> dplyr::select(-survey_domain_year)

#continuous year or years in the survey only
#g <- purrr::map_dfr(unique(d$year), ~ tibble(g, year = .x))
year <- seq(min(d$year), max(d$year), 1)
g <- purrr::map_dfr(year, ~tibble(g, year = .x))

g$logbot_depth <- log(g$depth)
g$logbot_depth2 <- log(g$depth) * log(g$depth)
g <- g |> mutate(UTM.lon = X, UTM.lat = Y)
g$offset <- 0

plot(g$X, g$Y)
g_sf <- st_as_sf(g, coords = c("X", "Y"), crs = 32609)
plot(st_geometry(g_sf))

#convex polygon
d_sf <- st_as_sf(d, coords = c("X", "Y"), crs = 32609)
plot(st_geometry(d_sf))
hull <- concaveman::concaveman(d_sf)
#I am going to create a buffer
hull_buf = st_buffer(hull, 8)
plot(hull_buf)

grid_hs <- st_intersection(hull_buf, g_sf)

plot(st_geometry(grid_hs))
plot(st_geometry(d_sf), col = "red", add = TRUE)

grid_sf <- st_as_sf(g, coords = c("X", "Y"), crs = 32609)
grid_hull <- concaveman::concaveman(grid_sf)

d_sf2 <- st_intersection(grid_hull, d_sf)
plot(st_geometry(grid_hs))
plot(st_geometry(d_sf2), col = "red", add = TRUE)

#what are those three points?? pull them out
test <- st_difference(d_sf, grid_hull)
plot(st_geometry(grid_hs))
plot(st_geometry(test), col = "red", add = TRUE)

d_sf2 <- d_sf |> filter(!fishing_event_id %in% test$fishing_event_id)


# create mesh -------------------------------------------------------------
st_geometry(d_sf2) <- NULL
glimpse(d_sf2)
mesh <- make_mesh(d_sf2, c("UTM.lon", "UTM.lat"), cutoff = 15)
plot(mesh)
mesh$mesh$n

# HS MSM only model -------------------------------------------------------------------
#data
d_sf2
#grid
st_geometry(grid_hs) <- NULL
grid_hs
grid_hs$julian <- mean(d_sf2$julian)
grid_hs$julian_small <- grid_hs$julian/100
plot(grid_hs$UTM.lon, grid_hs$UTM.lat)
#mesh
mesh


d_sf2 |>
  group_by(year) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch/effort) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue))

unique(d_sf2$year)

m <- sdmTMB::sdmTMB(
  #catch_weight ~ 1 + survey_type +  poly(log_depth_c, 2) + poly(days_to_solstice, 2),
  catch_weight ~ 1 + logbot_depth + logbot_depth2,
  #catch_weight ~ 1 + logbot_depth + logbot_depth2 + poly(julian_small,2),
    data = d_sf2,
    time = "year",
    mesh = mesh,
    spatiotemporal = "rw",
    silent = FALSE,
    offset = d_sf2$offset,
    spatial = TRUE,
    #do_index = FALSE,
    do_index = TRUE,
    share_range = FALSE,
    #extra_time = c(1985, 1986, 1988, 1990, 1992, 1994, 1997, 1999, 2001),
    predict_args = list(newdata = grid_hs),
    control = sdmTMBcontrol(newton_loops = 1L),
    index_args = list(area = grid_hs$cell_area),
    #family = tweedie()
    #family = delta_gamma()
    family = delta_lognormal() #sensitive to distribution, delta gamma

  )
sanity(m)

saveRDS(m, "data/generated/m_tweedie")
saveRDS(m, "data/generated/m_HSMSdg")
saveRDS(m, "data/generated/m_HSMSdl.rds")
saveRDS(m, "data/generated/m_HSMSdl_julian.rds")
saveRDS(m, "data/generated/m_HSMSdl_julianex.rds")

m <- readRDS("data/generated/m_tweedie")
m1 <- readRDS("data/generated/m_HSMSdg")
m2 <- readRDS("data/generated/m_HSMSdl.rds")
m3 <- readRDS("data/generated/m_HSMSdl_julian.rds")

index <- get_index(m, bias_correct = TRUE)
indexdg <- get_index(m1, bias_correct = TRUE)
indexdl <- get_index(m2, bias_correct = TRUE)
indexdljulian <- get_index(m3, bias_correct = TRUE)


#pred <- predict(m, grid_hs, return_tmb_object = TRUE, response = TRUE)

tweedie <- index |> mutate(type = "tweedie")
dg <- indexdg |> mutate(type = "dg")
dl <- indexdl |> mutate(type = "dl")
dl_julian <- indexdljulian |> mutate(type = "dl_julian")

all <-rbind(tweedie, dl, dl_julian, dg)

ggplot(all, aes(year, est, colour = type)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999") +
  facet_wrap(~type, scales = "free")

dl <- indexdl |> mutate(type = "dl")
dl_julian <- indexdljulian |> mutate(type = "dl_julian")

dl <-rbind(dl, dl_julian)

ggplot(dl, aes(year, est, colour = type)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999")

