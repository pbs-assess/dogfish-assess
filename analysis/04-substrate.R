
library(raster)

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
#  raster::projectRaster(scrs = 32609)

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
  select(latitude, longitude, depth_m, year, X, Y, catch_count, offset, survey_abbrev) %>%
  mutate(Xm = X * 1e3, Ym = Y * 1e3) %>%
  sf::st_as_sf(coords = c("Xm", "Ym"), crs = 32609)
hbll_intersects <- sf::st_intersects(hbll_sf, r3)

hbll_out <- hbll$data %>%
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





# IPHC
iphc <- readRDS("data/generated/iphc-nb2-sdmTMB_gfdata.rds")

iphc_sf <- iphc$data %>%
  select(latitude, longitude, depth_m, year, numobs, offset) %>%
  sdmTMB::add_utm_columns(ll_names = c("longitude", "latitude"), utm_crs = 32609, units = "m") %>%
  sf::st_as_sf(coords = c("X", "Y"), crs = 32609)
iphc_intersects <- sf::st_intersects(iphc_sf, r3)

iphc_out <- iphc$data %>%
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

