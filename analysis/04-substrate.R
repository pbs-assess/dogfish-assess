
library(raster)
library(tidyverse)

# https://www.gis-hub.ca/dataset/substrate100m-data

#SOURCE: This dataset was downloaded from: https://www.gis-hub.ca/dataset/substrate100m-data
#TITLE: Deep substrate model (100m) of the Pacific Canadian shelf
#SUMMARY:
#  This deep water substrate bottom type model was created to aid in habitat modeling, and to complement the nearshore bottom patches.
#It was created from a combination of bathymetrically-derived layers in addition to bottom type observations. Using random forest classification, the relationship between observed substrates and bathymetric derivatives was estimated across the entire area of interest.

#The raster is categorized into:
#1 - Rock
#2 - Mixed
#3 - Sand
#4 - Mud


#r <- raster::raster("data/substrate/Ranger_RF_Substrate_100m.tif") %>%
#  raster::projectRaster(crs = 32609)

# https://www.r-bloggers.com/2021/06/conversions-between-different-spatial-classes-in-r/
# Takes a while
#r2 <- terra::rast(rnew) %>%
#  terra::as.polygons()

# Convert to sf object
#r3 <- sf::st_as_sf(r2)
#saveRDS(r3, file = "data/substrate/substrate_sf.rds")

r3 <- readRDS("data/substrate/substrate_sf.rds")

# Plot
g <- ggplot(r3) +
  geom_sf() +
  coord_sf()
ggsave("figs/substrate/map2.png", g, height = 7, width = 7)


substrate_key <- data.frame(
  substrate = 1:4,
  type = c("Rock", "Mixed", "Sand", "Mud")
)

# HBLL
hbll <- readRDS("data/generated/hbll-out-sdmTMB_nohk.rds")

hbll_sf <- hbll$data %>%
  dplyr::select(latitude, longitude, depth_m, year, X, Y, catch_count, offset, survey_abbrev) %>%
  mutate(Xm = X * 1e3, Ym = Y * 1e3) %>%
  sf::st_as_sf(coords = c("Xm", "Ym"), crs = 32609)
hbll_intersects <- sf::st_intersects(hbll_sf, r3)

hbll_out <- hbll$data %>%
  dplyr::select(latitude, longitude, depth_m, year, X, Y, catch_count, offset, survey_abbrev) %>%
  mutate(substrate = sapply(hbll_intersects, function(x) if(!length(x)) NA else x)) %>%
  left_join(substrate_key, by = "substrate") %>%
  filter(year != 2013)

g <- hbll_out %>%
  filter(!is.na(substrate)) %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  ggplot(aes(depth_m, log1p(cpue), colour = type)) +
  geom_point(alpha = 0.7, shape = 21) +
  gfplot::theme_pbs() +
  facet_wrap(vars(year)) +
  theme(legend.position = "bottom") +
  labs(x = "Depth (m)", y = "log(CPUE + 1)", colour = "Substrate") +
  ggtitle("HBLL")
ggsave("figs/substrate/hbll_substrate_year.png", g, height = 6, width = 6)

g <- hbll_out %>%
  filter(!is.na(substrate)) %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  ggplot(aes(depth_m, log1p(cpue), colour = type)) +
  geom_point(alpha = 0.7, shape = 21) +
  gfplot::theme_pbs() +
  facet_wrap(vars(type)) +
  guides(colour = "none") +
  labs(x = "Depth (m)", y = "log(CPUE + 1)", colour = "Substrate") +
  ggtitle("HBLL")
ggsave("figs/substrate/hbll_substrate.png", g, height = 4, width = 6)

# Annual mean CPUE by substrate
g <- hbll_out %>%
  filter(!is.na(substrate)) %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  summarise(cpue = mean(cpue), .by = c(year, type)) %>%
  ggplot(aes(year, cpue, colour = type)) +
  geom_point() +
  geom_line() +
  gfplot::theme_pbs() +
  facet_wrap(vars(type)) +
  guides(colour = "none") +
  labs(x = "Year", y = "log(CPUE + 1)", colour = "Substrate") +
  ggtitle("HBLL")
ggsave("figs/substrate/hbll_substrate_year.png", g, height = 4, width = 6)


# Substrate of HBLL grid
hbll_grid_sf <- gfplot::hbll_grid$grid %>%
  rename(longitude = X, latitude = Y) %>%
  sdmTMB::add_utm_columns(units = "m", utm_crs = 32609) %>%
  sf::st_as_sf(coords = c("X", "Y"), crs = 32609)
hbll_grid_intersects <- sf::st_intersects(hbll_grid_sf, r3)

hbll_grid <- gfplot::hbll_grid$grid %>%
  rename(longitude = X, latitude = Y) %>%
  sdmTMB::add_utm_columns(units = "km", utm_crs = 32609) %>%
  mutate(substrate = sapply(hbll_grid_intersects, function(x) if(!length(x)) NA else x)) %>%
  left_join(substrate_key, by = "substrate") %>%
  filter(!is.na(type)) %>%
  rename(depth_m = depth)

coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

g <- ggplot(hbll_grid, aes(X, Y, colour = type)) +
  geom_point(size = 0.5, alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude", colour = "Substrate") +
  gfplot::theme_pbs()
ggsave("figs/substrate/hbll_grid_substrate.png", g, height = 6, width = 6)

yrs <- sort(union(unique(hbll$data$year), hbll$extra_time))
grid <- sdmTMB::replicate_df(hbll_grid, time_name = "year", time_values = yrs)

# Index of mud habitat
p <- predict(hbll, newdata = grid %>% filter(type == "Mud"), return_tmb_object = TRUE)
ind_mud <- get_index(p, bias_correct = TRUE)
readr::write_csv(ind_mud, file = "data/generated/hbll_index_mud.csv")

g <- ggplot(ind, aes(year, est)) +
  #geom_linerange(aes(ymin = lwr, ymax = upr)) +
  geom_point() +
  #facet_wrap(vars(name), ncol = 1) +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Index")

# Index of non-mud habitat
p <- predict(hbll, newdata = grid %>% filter(type != "Mud"), return_tmb_object = TRUE)
ind_nonmud <- get_index(p, bias_correct = TRUE)
readr::write_csv(ind_nonmud, file = "data/generated/hbll_index_nonmud.csv")

g <- rbind(
  ind_mud %>% mutate(type = "Mud"),
  ind_nonmud %>% mutate(type = "Non-mud")
) %>%
  ggplot(aes(year, est)) +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  geom_point() +
  facet_wrap(vars(type), ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Index")




# IPHC
iphc <- readRDS("data/generated/iphc-nb2-sdmTMB_gfdata.rds")

iphc_sf <- iphc$data %>%
  select(latitude, longitude, depth_m, year, numobs, offset) %>%
  sdmTMB::add_utm_columns(ll_names = c("longitude", "latitude"), utm_crs = 32609, units = "m") %>%
  sf::st_as_sf(coords = c("X", "Y"), crs = 32609)
iphc_intersects <- sf::st_intersects(iphc_sf, r3)

iphc_out <- iphc$data %>%
  select(latitude, longitude, depth_m, year, numobs, offset) %>%
  mutate(substrate = sapply(iphc_intersects, function(x) if(!length(x)) NA else x)) %>%
  left_join(substrate_key, by = "substrate")

g <- iphc_out %>%
  filter(!is.na(substrate)) %>%
  mutate(cpue = numobs/exp(offset)) %>%
  ggplot(aes(depth_m, log1p(cpue), colour = type)) +
  geom_point(alpha = 0.7, shape = 21) +
  gfplot::theme_pbs() +
  facet_wrap(vars(year)) +
  theme(legend.position = "bottom") +
  labs(x = "Depth (m)", y = "log(CPUE + 1)", colour = "Substrate") +
  ggtitle("IPHC")
ggsave("figs/substrate/iphc_substrate_year.png", g, height = 6, width = 6)

g <- iphc_out %>%
  filter(!is.na(substrate)) %>%
  mutate(cpue = numobs/exp(offset)) %>%
  ggplot(aes(depth_m, log1p(cpue), colour = type)) +
  geom_point(alpha = 0.7, shape = 21) +
  gfplot::theme_pbs() +
  facet_wrap(vars(type)) +
  guides(colour = "none") +
  labs(x = "Depth (m)", y = "log(CPUE + 1)", colour = "Substrate") +
  ggtitle("IPHC")
ggsave("figs/substrate/iphc_substrate.png", g, height = 4, width = 6)


g <- rbind(
  iphc_out %>% mutate(survey_abbrev = "IPHC") %>% select(year, depth_m, numobs, offset, type, survey_abbrev) %>% rename(catch_count = numobs),
  hbll_out %>% select(year, depth_m, offset, catch_count, type, survey_abbrev)
) %>%
  filter(!is.na(type)) %>%
  mutate(cpue = catch_count/exp(offset)) %>%
  ggplot(aes(depth_m, log1p(cpue), colour = survey_abbrev)) +
  geom_point(alpha = 0.7, shape = 1) +
  gfplot::theme_pbs() +
  facet_wrap(vars(type)) +
  labs(x = "Depth (m)", y = "log(CPUE + 1)", colour = "Survey")
ggsave("figs/substrate/substrate_cpue.png", g, height = 4, width = 6)

