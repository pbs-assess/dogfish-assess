#Create index using IPHC points that overlap with the HBLL survey domain


# libraries ---------------------------------------------------------------
library(gfdata)
library(sf)
library(tidyverse)
library(ggplot2)
library(gfplot)
library(sdmTMB)

# Pull IPHC and HBLL data -------------------------------------------------
d <- readRDS("data/generated/IPHC_coastdata_nosog_gfdata_hk.rds")
d$numobs <- ifelse(is.na(d$N_it) == TRUE, d$N_it20, d$N_it)
d$offset <- log(d$hooksobserved)
d.sf <- st_as_sf(d,
                 coords = c("UTM_lon_m", "UTM_lat_m"),
                 crs = 32609
)

grid <- gfplot::hbll_grid$grid
grid <- rename(grid, latitude = Y, longitude = X, depth_m = depth)
grid <- add_utm_columns(grid, utm_crs = 32609) |>
  rename(UTM_lon = X, UTM_lat = Y) |>
  mutate(UTM_lon_m = UTM_lon*1000, UTM_lat_m = UTM_lat*1000)
grid$area_km2 <- 4
grid$depth_m_log <- log(grid$depth_m)
grid$offset <- 0
grid.sf <- st_as_sf(grid,
                 coords = c("UTM_lon_m", "UTM_lat_m"),
                 crs = 32609
)

x <- ggplot(grid, aes(UTM_lon, UTM_lat)) + geom_point()
x + geom_point(data = d, aes(UTM_lon, UTM_lat), col= "red")

x <- ggplot() + geom_sf(data = grid.sf)
x + geom_sf(data = d.sf, col= "red")

#hbll data
s <- readRDS("data/raw/survey-sets.rds")
h <- dplyr::filter(s, grepl("HBLL OUT", survey_abbrev))
h <- sdmTMB::add_utm_columns(h, utm_crs = 32609)
h <- h |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))
range(h$depth_m)

# Intersect IPHC points with HBLL grid ------------------------------------
#create a polygon, if you intersect with the grid points it has to overlap perfectly
hulls <- concaveman::concaveman(grid.sf)
plot(st_geometry(hulls), col = "red", lwd = 2) #doesnt work well
plot(st_geometry(d.sf), add = TRUE)

#make a buffer around points then dissolve
buff <- st_buffer(grid.sf, dist = 2000) #2 km buffer
diss <- buff %>% st_union() %>% st_cast("POLYGON")
ggplot() + geom_sf(data=diss, aes(fill="red"))

#use the buffer to intersect with the IPHC points
plot(st_geometry(diss), col = "red", lwd = 2)
plot(st_geometry(d.sf), add = TRUE)

df.int <- st_intersection(
  d.sf,
  diss
)

x <- ggplot() + geom_sf(data = grid.sf)
y <- x + geom_sf(data = d.sf, col= "red")
y + geom_sf(data = df.int, col = "blue")

x <- ggplot() + geom_point(data = h, aes(longitude, latitude), col= "red")
x + geom_point(data = df.int, aes(longitude, latitude, colour = depth_m))

range(grid$depth_m)
range(h$depth_m)
range(df.int$depth_m)

ggplot(df.int, aes(year, depth_m)) + geom_point()
df.int <- st_drop_geometry(df.int)
df.int <- filter(df.int , depth_m > 20 & depth_m < 270)

range(grid$depth_m)
range(h$depth_m)
range(df.int$depth_m)

# Intersect IPHC points with HBLL grid - WCVI only ------------------------------------
#make a buffer around points then dissolve
grid.wcvi <- filter(grid, latitude < 51)
grid.sf.wcvi <- filter(grid.sf, latitude < 51)
buff <- st_buffer(grid.sf.wcvi, dist = 5000) #10 km buffer
diss <- buff %>% st_union() %>% st_cast("POLYGON")

x <- ggplot() + geom_sf(data=diss, fill="red")
x + geom_sf(data = grid.sf.wcvi, col = "black")

#usegeom_sf()#use the buffer to intersect with the IPHC points
plot(st_geometry(diss), col = "red", lwd = 2)
plot(st_geometry(d.sf), add = TRUE)

df.int <- st_intersection(
  d.sf,
  diss
)

x <- ggplot() + geom_sf(data = grid.sf.wcvi)
y <- x + geom_sf(data = d.sf, col= "red")
y + geom_sf(data = df.int, col = "blue")

x <- ggplot() + geom_point(data = h, aes(longitude, latitude), col= "red")
x + geom_point(data = df.int, aes(longitude, latitude, colour = depth_m))

range(grid$depth_m)
range(h$depth_m)
range(df.int$depth_m)

ggplot(df.int, aes(year, depth_m)) + geom_point()
df.int <- st_drop_geometry(df.int)
df.int <- filter(df.int , depth_m > 20 & depth_m < 270)

range(grid.wcvi$depth_m)
range(h$depth_m)
range(df.int$depth_m)

# Run model with the IPHC points that overlap --------------------------------------
mesh <- make_mesh(df.int, c("UTM_lon", "UTM_lat"), cutoff = 12)
plot(mesh)
mesh$mesh$n

years <- unique(df.int$year)
grid <- sdmTMB::replicate_df(grid, "year", years)
#grid <- sdmTMB::replicate_df(grid.wcvi, "year", years)

iphc_trim <- sdmTMB(
  numobs ~ 0 + poly(depth_m_log, 2L), #this is both N_it20 or N_it, offset accounts for hooks observed
  family = nbinom2(link = "log"),
  time_varying = ~1,
  data = df.int,
  mesh = mesh,
  time = "year",
  offset = "offset",
  #offset = "offset_hk",
  spatiotemporal = "ar1",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE
  # predict_args = list(newdata = grid, re_form_iid = NA),
  # index_args = list(area = grid$area_km),
  # do_index = TRUE
)
iphc_trim
sanity(iphc_trim)
plot_anisotropy(iphc_trim)
tidy(iphc_trim, conf.int = TRUE)
tidy(iphc_trim, effects = "ran_pars", conf.int = TRUE)

saveRDS(iphc_trim, file = "data/generated/iphc-nb2-hblloverlap.rds")
iphc_trim <- readRDS("data/generated/iphc-nb2-hblloverlap.rds")
##saveRDS(iphc_trim, file = "data/generated/iphc-nb2-wcvi-hblloverlap.rds")
##iphc_trim <- readRDS("data/generated/iphc-nb2-wcvi-hblloverlap.rds")

set.seed(1)
# r <- residuals(iphc_trim, type = "mle-mvn")
# qqnorm(r);abline(0, 1)
s <- simulate(iphc_trim, nsim = 500, type = "mle-mvn")
dharma_residuals(s, iphc_trim)


p <- predict(iphc_trim, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)
ind_orig <- readRDS("data/generated/geostat-ind-iphc_gfdata.rds")

bind_rows(mutate(ind, type = "IPHC trimmed to HBLL"), mutate(ind_orig, type = "Full IPHC")) |>
  group_by(type) |>
  mutate(upr = upr / est[year == 1998], lwr = lwr / est[year == 1998], est = est / est[year == 1998]) |>
  ggplot(aes(year, est, colour = type, fill = type)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  scale_x_continuous(breaks = c(years))


