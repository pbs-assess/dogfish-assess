#calculate a coastwide index using the Hecate Strait Multi Species trawl survey

#Hecate Strait multi species trawl extends back to 1984
#can either predict along the whole coast to create one index OR
#add a multiplier to add standard error to the coast wide index

#the grid can be a convex polygon with a 2 by 2 grid or
#overlay the current grid with a convex polygon

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
  geom_point(aes(year, log(catch_weight), colour = log(catch_weight), size = catch_weight))

d |>
  ggplot() +
  geom_point(aes(year, depth_m, colour = catch_weight), size = 2)

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



# data cleaning ignore for now -----------------------------------------------------------
glimpse(d)
#effort mutate(area_swept1_m2 = doorspread_m * tow_length_m) |>
  drop_na(doorspread_m)ignore

#included some sets unsually not deemed usable
meandoors <- dat %>%
  filter(usability_code == 1 &
           doorspread_m != 0) %>%
  group_by(survey_id) %>% summarise(
    mean_doorspread = mean(doorspread_m, na.rm = TRUE)
  )

dat <- dat %>%
  filter(usability_code %in% c(1, 22, 16, 6)) %>%
  filter(duration_min >= 10) %>%
  left_join(meandoors) %>%
  mutate(
    DOY = as.numeric(strftime(time_deployed, format = "%j")),
    days_to_solstice = DOY - 172,
    survey_type = as.factor(ifelse(survey_abbrev == "HS MSA", "MSA", "SYN")),
    log_depth = log(depth_m),
    log_depth_c = log_depth - 5, # mean and median for whole data set
    doorspread_m = ifelse(doorspread_m == 0, mean_doorspread, doorspread_m),
    area_swept = ifelse(is.na(tow_length_m), doorspread_m * duration_min * speed_mpm, doorspread_m * tow_length_m)
  )




# data cleaning -----------------------------------------------------------
d <- d |>
  mutate(area_swept_calc = ifelse(is.na(tow_length_m), doorspread_m * duration_min * speed_mpm,
                                  doorspread_m * tow_length_m))
d$offset <- log(d$area_swept_calc)

d |>
  ggplot() +
  geom_point(aes(year, area_swept_calc, colour = catch_weight), size = 2)

d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))

d |>
  ggplot() +
  geom_point(aes(year, julian, colour = catch_weight), size = 2)



# create mesh -------------------------------------------------------------

mesh <- make_mesh(d, c("X", "Y"), cutoff = 15)
plot(mesh)
mesh$mesh$n

# create grid -------------------------------------------------------------

g <- gfplot::synoptic_grid
plot(g$X, g$Y)
g_sf <- st_as_sf(g, coords = c("X", "Y"), crs = 32609)
plot(g_sf)

plot(mesh)
points(g$X, g$Y) #grid
points(d$X, d$Y, col = "red") #HS points

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

#what are those three points?? they are outside of the grid so maybe I should remove them
plot(st_geometry(g_sf))
plot(st_geometry(d_sf), col = "red", add = TRUE)

#what are those three points?? they are outside of the grid so maybe I should remove them
plot(st_geometry(g_sf))
plot(st_geometry(d_sf), col = "red", add = TRUE)

grid_sf <- st_as_sf(g, coords = c("X", "Y"), crs = 32609)
plot(st_geometry(grid_sf))
grid_hull <- concaveman::concaveman(grid_sf)

d_sf2 <- st_intersection(grid_hull, d_sf)
plot(st_geometry(grid_hs))
plot(st_geometry(d_sf2), col = "red", add = TRUE)

#what are those three points?? pull them out
test <- st_difference(d_sf, grid_hull)
plot(st_geometry(grid_hs))
plot(st_geometry(test), col = "red", add = TRUE)

d_sf2 <- d_sf |> filter(!fishing_event_id %in% test$fishing_event_id)

# model -------------------------------------------------------------------
#philina's model has survey type, depths, and DOY centered on the solstice
#unshared ranges and priors
#use grid_hs
#use mesh
d_sf2

m <- sdmTMB::sdmTMB(
  catch_weight ~ 1 + survey_type +
  poly(log_depth_c, 2) +
  poly(days_to_solstice, 2)
share_range = FALSE,
priors = sdmTMBpriors(
  matern_s = pc_matern(range_gt = cutoff_distance * 1.5,
                       sigma_lt = 2),
  matern_st = pc_matern(range_gt = cutoff_distance * 1.5,
                        sigma_lt = 2)
)
