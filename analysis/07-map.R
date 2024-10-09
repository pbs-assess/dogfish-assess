library(dplyr)
library(ggplot2)
library(sf)

map_data <- rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf", continent = "north america"
)

bc_coast <- suppressWarnings(suppressMessages(
  st_crop(
    map_data,
    c(xmin = -134, ymin = 40, xmax = -120, ymax = 56)
  )
))

utm_zone9 <- 3156
bc_coast_proj <- sf::st_transform(bc_coast, crs = utm_zone9)

data("major", package = "PBSdata")
major <- sf::st_as_sf(major, coords = c("X", "Y"), crs = "WGS84")
major <- sf::st_transform(major, crs = utm_zone9)
coords <- sf::st_coordinates(major)
major_df <- data.frame(group = major$PID, X = coords[, 1], Y = coords[, 2])

data("eez.bc", package = "PBSdata")
eez <- sf::st_as_sf(eez.bc, coords = c("X", "Y"), crs = "WGS84")
eez <- sf::st_transform(eez, crs = utm_zone9)
coords <- sf::st_coordinates(eez)
eez_df <- data.frame(group = eez$PID, X = coords[, 1], Y = coords[, 2])

data("major", package = "PBSdata")
labels <- attributes(major)$PolyData
labels <- sf::st_as_sf(labels, coords = c("X", "Y"), crs = "WGS84")
labels <- sf::st_transform(labels, crs = utm_zone9)
lab_xy <- sf::st_coordinates(labels)
lab_xy_df <- select(labels, label)
lab_xy_df$X <- lab_xy[, 1]
lab_xy_df$Y <- lab_xy[, 2]
lab_xy_df <- filter(lab_xy_df, !grepl("4B", label))

blues <- RColorBrewer::brewer.pal(5, "Blues")

ggplot() +
  geom_polygon(data = filter(major_df, group != 1), mapping = aes(X, Y, group = group, fill = factor(group)), colour = "grey40") +
  geom_text(data = lab_xy_df, mapping = aes(X, Y, label = label), colour = "grey20") +
  geom_polygon(data = eez_df, mapping = aes(X, Y, group = group), colour = "grey40", fill = NA, lwd = 0.8) +
  geom_sf(data = bc_coast_proj, fill = "grey90", colour = "grey30") +
  coord_sf(xlim = c(227739 - 30000, 1157998.9 - 200000), ylim = c(5364575, 6131904 - 50000)) +
  theme_light() +
  xlab("Longitude (°)") +
  ylab("Latitude (°)") +
  annotate("text", x = 227739 + 600000, y = 6131904 - 200000, label = "British\nColumbia", size = 5, colour = "grey30") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.05, "in"),
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  ) +
  theme(axis.title = element_blank()) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = "none")

source("ss3/99-utils.R")
ggsave_optipng("figs/map-sar.png", width = 4.2, height = 3.8)
