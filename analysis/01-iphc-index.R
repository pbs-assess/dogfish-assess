library(ggplot2)
library(dplyr)
library(sdmTMB)
library(tidyr)
library(sf)

# catch <- read.csv("~/Downloads/Non-Pacific halibut data_raw.csv")
# stations <- read.csv("~/Downloads/Map select_standardgrid.csv")
# latlongs <- read.csv("~/Downloads/Set and Pacific halibut data_raw.csv") |>
#   dplyr::filter(`IPHC.Reg.Area` %in% "2B")
# saveRDS(catch, file = "data/raw/Non-Pacific halibut data_raw.rds")
# saveRDS(stations, file = "data/raw/Map select_standardgrid.rds")
# saveRDS(latlongs, file = "data/raw/Set and Pacific halibut data_raw.rds")

iphc_stations <- readRDS("data/raw/Map select_standardgrid.rds")
iphc_coast <- readRDS("data/raw/Non-Pacific halibut data_raw.rds")
iphc_latlongs <- readRDS("data/raw/Set and Pacific halibut data_raw.rds") %>%
  dplyr::select(IPHC.Reg.Area, Date, Eff, Ineffcde, BeginLat, BeginLon, AvgDepth..fm., Stlkey)

glimpse(iphc_latlongs)
glimpse(iphc_coast)
glimpse(iphc_stations)

iphc_coast2 <- iphc_coast %>%
  inner_join(iphc_stations) %>%
  inner_join(iphc_latlongs, by = "Stlkey")
names(iphc_coast2) <- tolower(names(iphc_coast2))
iphc_coast3 <- iphc_coast2 %>%
  filter(eff == "Y") %>%
  filter(purpose == "Standard Grid") %>%
  mutate(startlonfix = ifelse(beginlon > 0, beginlon * -1, beginlon)) %>%
  filter(iphc.reg.area == "2B") %>%
  mutate(depth_m = 1.8288 * avgdepth..fm.) %>%
  mutate(depth_m_log = log(depth_m)) %>%
  dplyr::select(
    depth_m_log, year, beginlat, beginlon, station,
    iphc.reg.area, number.observed, hooksobserved, date
  ) %>%
  mutate(hooksobserved2 = as.numeric(hooksobserved)) %>%
  drop_na(hooksobserved2)

iphc_coast4 <- add_utm_columns(iphc_coast3,
  ll_names = c("beginlon", "beginlat"),
  utm_names = c("UTM.lon.m", "UTM.lat.m"),
  utm_crs = 32609
) %>%
  inner_join(iphc_coast3) %>%
  rename(latitude = beginlat, longitude = beginlon) %>%
  mutate(cpue = number.observed / hooksobserved2) %>%
  mutate(dmy = lubridate::dmy(date)) %>%
  mutate(julian = lubridate::yday(dmy)) %>%
  drop_na(julian) %>%
  mutate(station = as.integer(station)) %>%
  mutate(UTM.lat = UTM.lat.m, UTM.lon = UTM.lon.m) |>
  mutate(UTM.lat.m = UTM.lat.m * 1000, UTM.lon.m = UTM.lon.m * 1000) |>
  distinct(.keep_all = TRUE)

filter(iphc_coast4, station == 2099 & year == 2019) # check no duplications

# get rid of SOG points the expansion set in 2018
shelf <- st_read("data/raw", "Shelf_polygon_noSOG") %>%
  st_transform(crs = "EPSG:32609")

iphc_coast4sf <- st_as_sf(iphc_coast4,
  coords = c("UTM.lon.m", "UTM.lat.m"),
  crs = "EPSG:32609"
)

iphc_nosog <- st_intersection(iphc_coast4sf, st_geometry(shelf)) %>%
  st_drop_geometry() %>%
  dplyr::select(-dmy)

# stations with only one survey
surveyed1 <- iphc_nosog %>%
  group_by(station) %>%
  mutate(count = n()) %>%
  filter(count == 1)

iphc_coast_trimmed3 <- filter(iphc_nosog, !(station %in% surveyed1$station))

x <- ggplot(data = filter(iphc_coast_trimmed3, iphc.reg.area == "2B"),
            aes(UTM.lon, UTM.lat), size = 1.5, col = "blue") +
  geom_point()
x + geom_point(data = filter(surveyed1, iphc.reg.area == "2B"), aes(UTM.lon, UTM.lat, col = "red"))

saveRDS(iphc_coast_trimmed3, "data/generated/IPHC_coastdata_nosog.rds")

d <- readRDS("data/generated/IPHC_coastdata_nosog.rds")


# h <- readxl::read_excel("data/raw/iphc-2021-fiss-hadj.xlsx") |>
# dplyr::filter(`IPHC Reg Area` %in% "2B")
# saveRDS(h, file = "data/raw/iphc-2021-fiss-hadj.rds")

h <- readRDS("data/raw/iphc-2021-fiss-hadj.rds") |>
  filter(Year >= 1998, Effective == "Y", Purpose == "SG") |>
  select(year = Year, station = Station, bait = Bait, hookobserved = `Hooks Observed`, purpose = Purpose, hadj = h.adj, date = Date) |>
  mutate(hadj = as.numeric(hadj))

h$bait[h$bait == 0] <- 1
h <- h[h$hookobserved > 0, ]
prop_bait_hooks <- h$bait / h$hookobserved
range(prop_bait_hooks)
hook_adjust_factor <- -log(prop_bait_hooks) / (1 - prop_bait_hooks)
plot(hook_adjust_factor, h$hadj)
abline(0, 1)
range(hook_adjust_factor)
h$hook_adjust_factor <- hook_adjust_factor
h$date <- lubridate::as_date(h$date)

d <- left_join(d, select(h, -hookobserved), by = join_by(year, station, date)) |>
  filter(iphc.reg.area %in% "2B")

names(d) <- tolower(names(d))
names(d) <- gsub("\\.", "_", names(d))
d$X <- d$Y <- NULL
d <- sdmTMB::add_utm_columns(d, ll_names = c("longitude", "latitude"), utm_crs = 32609)

nrow(d)
# 8 dogfish caught
# all hooks were used but 1
# 100 hooks looked at
# so, it's an underestimate of dogfish
# need to inflate catch of dogfish by 'h'
# 8 * h / 100 is the CPUE
# (8 * h)/100
# log(100) is the offset term
# log((8 * h)/100) = b0 + b1*x ...
# log(8 * h) - log(100) = b0 + b1*x ...
# log(8 * h) = b0 + b1*x + log(100) ...
# log(8) + log(h) = b0 + b1*x + log(100) ...
# log(8) = b0 + b1*x + log(100) - log(h) ...
# log(8) = b0 + b1*x + log(100/h) ...

glimpse(d)
sort(unique(d$year))
nrow(d)

ggplot(d, aes(X, Y, size = cpue)) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  coord_fixed()

stopifnot(sum(is.na(d$depth_m)) == 0L)
stopifnot(sum(is.na(d$depth_m_log)) == 0L)
stopifnot(sum(is.na(d$number_observed)) == 0L)
stopifnot(sum(is.na(d$hooksobserved2)) == 0L)

d <- filter(d, year <= 2021) # hook adj. not ready for 2022
d <- filter(d, !is.na(purpose)) # a few in 2021!?
stopifnot(sum(is.na(d$hook_adjust_factor)) == 0L)


d[is.na(d$hook_adjust_factor), ]
range(d$number_observed)
range(d$depth_m_log)
# d$offset <- log(d$hooksobserved)

mesh <- make_mesh(d, c("X", "Y"), cutoff = 20)
plot(mesh)
mesh$mesh$n

d$offset <- log(d$hooksobserved2 / d$hook_adjust_factor)
d[is.na(d$offset), ]

fit_iphc_nb2 <- sdmTMB(
  number_observed ~ 0 + poly(depth_m_log, 2L),
  family = nbinom2(link = "log"),
  time_varying = ~1,
  data = d,
  mesh = mesh,
  time = "year",
  offset = "offset",
  spatiotemporal = "ar1",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
)
saveRDS(fit_iphc_nb2, file = "data/generated/iphc-nb2-sdmTMB.rds")
fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB.rds")

fit_iphc_nb2
sanity(fit_iphc_nb2)
fit_iphc_nb2$sd_report
plot_anisotropy(fit_iphc_nb2)

# grid of IPHC main fixed survey locations
s <- d %>%
  # outside only, downloaded from website, expansion set and SOG removed
  dplyr::filter(iphc_reg_area == "2B") %>%
  distinct(station, .keep_all = TRUE)

grid <- s %>%
  dplyr::select(longitude, latitude, depth_m_log) %>%
  distinct(.keep_all = TRUE)

g <- add_utm_columns(grid, ll_names = c("longitude", "latitude"), utm_crs = 32609)
plot(g$X, g$Y)
nrow(g)
nrow(distinct(g))

ggplot(g, aes(X, Y, colour = depth_m_log)) +
  geom_point() +
  coord_fixed() +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)

years <- sort(unique(d$year))
grid <- sdmTMB::replicate_df(g, "year", years)

p <- predict(fit_iphc_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)

saveRDS(ind, file = "data/generated/geostat-ind-iphc.rds")
ind <- readRDS("data/generated/geostat-ind-iphc.rds")

obs <- group_by(d, year) |>
  summarise(n_hooksobserved = mean(hooksobserved2))

ind |>
  left_join(obs) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = n_hooksobserved)) +
  geom_pointrange() +
  coord_cartesian(ylim = c(0, NA)) +
  geom_vline(xintercept = 2020, lty = 2) +
  geom_vline(xintercept = 2000, lty = 2) +
  scale_colour_viridis_c()

ind |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.4) +
  geom_line() +
  coord_cartesian(ylim = c(0, NA))

mean(ind$est[ind$year %in% c(1998:2002)]) / min(ind$est)
plot(obs$n_hooksobserved, ind$est)
