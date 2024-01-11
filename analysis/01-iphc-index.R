library(ggplot2)
library(dplyr)
library(sdmTMB)
library(tidyr)
library(sf)
#devtools::install_github("pbs-assess/gfiphc")
library(gfiphc)
library(gfdata)

# note 2020 fishing was completed in July and August, whereas it is usually May to August.
# also 2021 and 2022 have reduced WCVI sampling.

# catch <- read.csv("~/Downloads/Non-Pacific halibut data_raw.csv")
# stations <- read.csv("~/Downloads/Map select_standardgrid.csv")
# latlongs <- read.csv("~/Downloads/Set and Pacific halibut data_raw.csv") |>
#   dplyr::filter(`IPHC.Reg.Area` %in% "2B")
# saveRDS(catch, file = "data/raw/Non-Pacific halibut data_raw.rds")
# saveRDS(stations, file = "data/raw/Map select_standardgrid.rds")
# saveRDS(latlongs, file = "data/raw/Set and Pacific halibut data_raw.rds")

iphc_stations <- readRDS("data/raw/Map select_standardgrid.rds")
names(iphc_stations) <- tolower(names(iphc_stations))
iphc_stations <- iphc_stations |>
  mutate(station = as.character(station))

# number of hooks
# test <- get_iphc_hooks("north pacific spiny dogfish")
iphc_hksobs <- readRDS("data/raw/Non-Pacific halibut data_raw.rds") |>
  dplyr::select(Year, Station, HooksFished, HooksRetrieved, HooksObserved) |>
  mutate(Station = as.character(Station))
names(iphc_hksobs) <- tolower(names(iphc_hksobs))
unique(iphc_hksobs$year)

# load iphc data from gfiphc
#reran to update years
# x <- get_iphc_spp_name()
# x[grep("Dogfish", x$iphc_common_name), ]
# sp <- "north pacific spiny dogfish"
# cache_pbs_data_iphc(sp)
# sp_set_counts <- readRDS(paste0(gsub(" ", "-", sp), ".rds"))
# unique(sp_set_counts$set_counts$year)
# df_iphc <- filter(sp_set_counts$set_counts, standard == "Y" &  usable == "Y")
# df_iphc |>
#  group_by(year) %>%
#  summarise(total = sum(N_it20)) |>
#  ggplot(aes(year, total)) + geom_line()
# df_iphc |>
#   group_by(year) %>%
#   ggplot(aes(lon, lat)) + geom_point() + facet_wrap(~year)
#
# saveRDS(df_iphc, "data/raw/Non-Pacific halibut data_raw_gfdata.rds") #this has station but doesn't have IPHC area
iphc_coast <- readRDS("data/raw/Non-Pacific halibut data_raw_gfdata.rds")

#this has station and IPHC reg area and date (if including julian date)
iphc_latlongs <- readRDS("data/raw/Set and Pacific halibut data_raw.rds") %>%
  dplyr::select(IPHC.Reg.Area, Station, Date, Eff, Ineffcde, BeginLat, BeginLon, AvgDepth..fm., Stlkey) |>
  mutate(Station = as.character(Station)) |>
  mutate(date2 = format(as.Date(Date, format = "%d-%b-%Y"), "%Y")) |> # get year from date
  mutate(date2 = as.numeric(date2)) |>
  mutate(year = substr(Stlkey, 1, 4)) |>
  mutate(year = as.numeric(year))
names(iphc_latlongs) <- tolower(names(iphc_latlongs))

iphc_stations <- filter(iphc_stations,
  purpose == "Standard Grid", iphc.reg.area..group. == "2B" # 2B = BC
) |>
  select(station) |>
  distinct()

# iphc_latlongs <- filter(iphc_latlongs, iphc.reg.area == "2B") |>
#   select(station, date) |>
#   distinct()

# fixed - bring datasets together
iphc_stations2a <- filter(iphc_stations, iphc.reg.area..group. == "2B")
station <- iphc_stations2a$station
iphc_coast2 <- iphc_coast |>
  filter(station %in% station) |>
  inner_join(iphc_latlongs,
             by = c("station" = "station", "year" = "year"), relationship = "many-to-many")
x <- iphc_coast2 |> select(station, year, date)
x[duplicated(x), ] #so there are a couple stations that have been fished twice in the same year
#these pairs pose a problem for the next step that does not have a date associated with it.
#we can drop these points or assume that the hook values are the same
#i dropped them
# If you want to add them back in, you can bind_rows this to the iphc_coast2
year2019_stations <- 
iphc_coast |>
  filter(station %in% station) |>
  filter(year == 2019 & station %in% c(2107, 2099)) |>
  mutate(lat = round(lat, digits = 4), lon = round(lon, digits = 4)) |>
  inner_join(iphc_latlongs |>
    mutate(lat = round(beginlat, digits = 4), lon = round(beginlon, digits = 4)),
             by = c("station" = "station", "year" = "year", 'lat' = 'lat', 'lon' = 'lon'))

iphc_coast2 <- iphc_coast2 |>  #for julian date
  filter(station !=2107 & year !=2019) |>
  filter(station != 2099 & year != 2019) |>
  inner_join(iphc_hksobs) |> #for hook information
  distinct(.keep_all = TRUE) |>
  bind_rows(year2019_stations)



# check for duplicate years and stations
iphc_coast2[duplicated(iphc_coast2), ]

iphc_coast3 <- iphc_coast2 %>%
  filter(eff == "Y") %>%
  mutate(startlonfix = ifelse(beginlon > 0, beginlon * -1, beginlon)) %>%
  mutate(depth_m = 1.8288 * avgdepth..fm.) %>%
  mutate(depth_m_log = log(depth_m)) %>%
  dplyr::select(
    depth_m_log, year, beginlat, beginlon, station,
    iphc.reg.area, N_it20, N_it, E_it20, C_it20, date, hooksfished, hooksobserved
  ) |>
  drop_na(E_it20)

iphc_coast4 <- add_utm_columns(iphc_coast3,
  ll_names = c("beginlon", "beginlat"),
  utm_names = c("UTM.lon.m", "UTM.lat.m"),
  utm_crs = 32609
) %>%
  inner_join(iphc_coast3) %>%
  rename(latitude = beginlat, longitude = beginlon) %>%
  # mutate(cpue = number.observed / hooksobserved2) %>%
  mutate(dmy = lubridate::dmy(date)) %>%
  mutate(julian = lubridate::yday(dmy)) %>%
  drop_na(julian) %>%
  mutate(station = as.integer(station)) %>%
  mutate(UTM.lat = UTM.lat.m, UTM.lon = UTM.lon.m) |>
  mutate(UTM.lat.m = UTM.lat.m * 1000, UTM.lon.m = UTM.lon.m * 1000) |>
  distinct(year, station, N_it20, .keep_all = TRUE)

x <- filter(iphc_coast4, station == 2099 & year == 2019) #Two sets that day or is this a mistake?
saveRDS(iphc_coast4, "data/generated/IPHC_coastdata_nosog_gfdata.rds")

# Check of the data  ------------------------------------------------------
# get rid of SOG points the expansion set in 2018
shelf <- st_read("data/raw", "Shelf_polygon_noSOG") %>%
  st_transform(crs = 32609)

iphc_coast4sf <- st_as_sf(iphc_coast4,
  coords = c("UTM.lon.m", "UTM.lat.m"),
  crs = 32609
)

plot(st_geometry(shelf))
plot(iphc_coast4sf, add = T) #all the points are outside of the SOG

iphc_nosog <- st_intersection(iphc_coast4sf, st_geometry(shelf)) %>%
 st_drop_geometry() %>%
 dplyr::select(-dmy)

# Check of website and gfiphc data trends ----
d_website <- readRDS("data/generated/IPHC_coastdata_nosog.rds")
d <- readRDS("data/generated/IPHC_coastdata_nosog_gfdata.rds") |>
  mutate(depth_m = exp(depth_m_log)) |>
  mutate(hooksobserved = as.numeric(hooksobserved))
ggplot(data = d, aes(E_it20, hooksobserved, group = year, colour = year)) +
  geom_point()
test2 <- d |>
  group_by(year) |>
  summarize(count = sum(N_it20) / sum(E_it20)) |>
  mutate(data = "gfiphc")
test <- d_website |>
  group_by(year) |>
  summarize(count = sum(number.observed) / sum(hooksobserved2) * 100) |>
  mutate(data = "website")
both <- rbind(test2, test)
ggplot(both, aes(year, count, group = data, colour = data)) +
  geom_line(size = 2)


# Add Hook Competition ----
#from https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.iphc.int%2Fuploads%2F2023%2F12%2Fiphc-2023-fiss-hadj-20231031.xlsx&wdOrigin=BROWSELINK
# h <- readxl::read_excel("data/raw/iphc-2023-fiss-hadj-20231031.xlsx") |> #iphc-2021-fiss-hadj.xlsx") #old data set
# dplyr::filter(`IPHC Reg Area` %in% "2B")
# saveRDS(h, file = "data/raw/iphc-2023-fiss-hadj.rds")

d <- readRDS("data/generated/IPHC_coastdata_nosog_gfdata.rds") |>
  mutate(depth_m = exp(depth_m_log)) |>
  mutate(hooksobserved = as.numeric(hooksobserved), hooksfished = as.numeric(hooksfished), station = as.character(station))

h <- readRDS("data/raw/iphc-2023-fiss-hadj.rds") |>
  filter(Year >= 1998, Effective == "Y", Purpose == "SG") |>
  select(year = Year, station = Station, bait = Bait, hookobserved = `Hooks Observed`, purpose = Purpose, hadj = h.adj, date = Date) |>
  mutate(hadj = as.numeric(hadj), bait = as.numeric(bait), hookobserved = as.numeric(hookobserved),
         station = as.character(station))

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
d$date <- lubridate::dmy(d$date)

d <- left_join(d, select(h, -hookobserved), by = join_by(year, station, date)) |>
  filter(iphc.reg.area %in% "2B")
names(d) <- gsub("\\.", "_", names(d))
# d$X <- d$Y <- NULL
# d <- sdmTMB::add_utm_columns(d, ll_names = c("longitude", "latitude"), utm_crs = 32609)

nrow(d)
# say 8 dogfish were caught
# all hooks were used but 1
# 100 hooks looked at
# so, it's an underestimate of dogfish
# need to inflate catch of dogfish by 'h'
# 8 * h / 100 is the CPUE
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

ggplot(d, aes(UTM_lon, UTM_lat, size = C_it20)) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  coord_fixed()

stopifnot(sum(is.na(d$depth_m)) == 0L)
stopifnot(sum(is.na(d$depth_m_log)) == 0L)
stopifnot(sum(is.na(d$N_it20)) == 0L)
stopifnot(sum(is.na(d$E_it20)) == 0L)

#d <- filter(d, year <= 2021) # hook adj. not ready for 2022
d <- filter(d, !is.na(purpose)) # a few in 2021!?
stopifnot(sum(is.na(d$hook_adjust_factor)) == 0L)

# d$offset <- log(d$hooksobserved2)
stopifnot(sum(is.na(d$hooksobserved)) == 0L)

d$offset <- log(d$hooksfished / d$hook_adjust_factor) # hook comp
#d$offset <- log(d$hooksobserved / d$hook_adjust_factor) # hook comp
#d$offset <- log(d$hooksobserved) # no hook comp
stopifnot(sum(is.na(d$offset)) == 0L)

saveRDS(d, "data/generated/IPHC_coastdata_nosog_gfdata_hk.rds")

## Figures for report - data and hook adjustment ----
coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

gg <- ggplot(d, aes(longitude, latitude, fill = bait/hooksobserved, colour = bait/hooksobserved)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  scale_fill_viridis_c(option = "C", limits = c(0, 1)) +
  scale_colour_viridis_c(option = "C", limits = c(0, 1)) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", fill = "Proportion baited hooks", colour = "Proportion baited hooks")
ggsave("figs/iphc/baited_hooks.png", gg, height = 6, width = 5, dpi = 600)

gg <- ggplot(d, aes(longitude, latitude, fill = N_it20/exp(offset), colour = N_it20/exp(offset))) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  theme(panel.spacing = unit(0, "in"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_colour_viridis_c(trans = "log", breaks = c(0.006, 0.050, 0.37)) +
  scale_fill_viridis_c(trans = "log", breaks = c(0.006, 0.050, 0.37)) +
  labs(x = "Longitude", y = "Latitude", fill = "Adjusted CPUE", colour = "Adjusted CPUE")
ggsave("figs/iphc/adjusted_cpue.png", gg, height = 6, width = 5, dpi = 600)

gg <- d %>%
  mutate(cpue = N_it20/exp(offset)) %>%
  ggplot(aes(x = cpue, y = after_stat(count))) +
  geom_histogram(bins = 20, colour = 1, fill = "grey80") +
  facet_wrap(vars(year), ncol = 5) +
  theme(panel.spacing = unit(0, "in")) +
  labs(x = "Adjusted CPUE", y = "Frequency") +
  coord_cartesian(xlim = c(0, 0.5))
ggsave("figs/iphc/adjusted_cpue_hist.png", gg, height = 5, width = 6)

## Nominal index ----
do_boot <- function(x, nsim = 250) {
  boot_fn <- function(d, i) {
    d <- d[i, ]
    mean(d$N_it20/exp(d$offset))
  }
  boot_out <- lapply(unique(x$year), function(y) {
    samps <- filter(x, year == y)
    boot::boot(samps, boot_fn, R = nsim)
  })
  data.frame(year = unique(x$year),
             index = sapply(boot_out, getElement, "t0"),
             var = sapply(boot_out, function(i) var(i$t))) %>%
    mutate(sd = sqrt(var), cv = sd/index)
}
index_boot <- do_boot(d)

gg <- index_boot %>%
  ggplot(aes(year, index)) +
  geom_linerange(aes(ymin = index - 2 * sd, ymax = index + 2 * sd)) +
  geom_point() +
  #geom_line() +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Index of abundance")
ggsave("figs/iphc/iphc_index_nominal.png", gg, height = 4, width = 6)

## Fit sdm model ----
mesh <- make_mesh(d, c("UTM_lon", "UTM_lat"), cutoff = 15)
plot(mesh)
mesh$mesh$n

# Plot mesh ----
g <- local({
  mesh_m <- mesh$mesh
  mesh_m$loc <- 1e3 * mesh_m$loc

  ggplot() +
    inlabru::gg(mesh_m, edge.color = "grey60") +
    geom_sf(data = coast %>% sf::st_transform(crs = 32609), inherit.aes = FALSE) +
    #geom_point(data = mesh$loc_xy %>% as.data.frame() %>% `*`(1e3), aes(X, Y), fill = "red", shape = 21, size = 1) +
    labs(x = "Longitude", y = "Latitude")
})
ggsave("figs/iphc/iphc_mesh.png", g, width = 5, height = 6)

# Call sdm
d <- readRDS("data/generated/IPHC_coastdata_nosog_gfdata_hk.rds")
d$numobs <- ifelse(is.na(d$N_it20) == TRUE, d$N_it, d$N_it20)
d |> filter(N_it20) |> group_by(year) |> tally()

fit_iphc_nb2 <- sdmTMB(
  numobs ~ 0 + poly(depth_m_log, 2L), #should this be N_it or N_it20, previously was N_it20
  family = nbinom2(link = "log"),
  time_varying = ~1,
  data = d,
  mesh = mesh,
  time = "year",
  offset = "offset",
  spatiotemporal = "ar1",
  spatial = "on",
  silent = TRUE,
  anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
)
saveRDS(fit_iphc_nb2, file = "data/generated/iphc-nb2-sdmTMB_gfdata.rds")
#fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB.rds")
fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB_gfdata.rds")

fit_iphc_nb2$data |> filter(N_it20 == 0) |> group_by(year) |> tally()

# #with julian
# ggplot(d, aes(year, julian)) + geom_point()
# range(d$julian)
# d$julian_small <- d$julian/100
# fit_iphc_nb2_wjulian <- sdmTMB(
#   numobs ~ 0 + poly(depth_m_log, 2L) + julian_small,
#   family = nbinom2(link = "log"),
#   time_varying = ~1,
#   data = d,
#   mesh = mesh,
#   time = "year",
#   offset = "offset",
#   spatiotemporal = "ar1",
#   spatial = "on",
#   silent = TRUE,
#   anisotropy = TRUE,
#   control = sdmTMBcontrol(newton_loops = 1L)
# )
# saveRDS(fit_iphc_nb2_wjulian, file = "data/generated/iphc-nb2-sdmTMB_gfdata_wjulian.rds")
# fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB_gfdata_wjulian.rds")

# fit_rw <- update(fit_iphc_nb2, spatiotemporal = "rw", time_varying = NULL,
#   formula. = number_observed ~ 1 + poly(depth_m_log, 2L))

fit_iphc_nb2
fit_iphc_nb2$sd_report
sanity(fit_iphc_nb2)
fit_iphc_nb2$sd_report
plot_anisotropy(fit_iphc_nb2)
tidy(fit_iphc_nb2, conf.int = TRUE)
tidy(fit_iphc_nb2, effects = "ran_pars", conf.int = TRUE)

# fit_rw
# sanity(fit_rw)

# grid of IPHC main fixed survey locations
s <- d %>%
  # outside only, downloaded from website, expansion set and SOG removed
  dplyr::filter(iphc_reg_area == "2B") %>%
  distinct(station, .keep_all = TRUE)

grid <- s %>%
  dplyr::select(longitude, latitude, depth_m_log) %>%
  distinct(.keep_all = TRUE) |>
  mutate(julian = 200, julian_small = 2)

g <- add_utm_columns(grid, ll_names = c("longitude", "latitude"), utm_crs = 32609) |>
  rename(UTM_lon = X, UTM_lat = Y)
plot(g$UTM_lon, g$UTM_lat)
nrow(g)
nrow(distinct(g))

ggplot(g, aes(UTM_lon, UTM_lat, colour = depth_m_log)) +
  geom_point() +
  coord_fixed() +
  scale_colour_viridis_c(trans = "sqrt", direction = -1)

years <- sort(unique(d$year))
grid <- sdmTMB::replicate_df(g, "year", years)

# p_rw <- predict(fit_rw, newdata = grid, return_tmb_object = TRUE)
# ind_rw <- get_index(p_rw, bias_correct = TRUE)

p <- predict(fit_iphc_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)

saveRDS(ind, file = "data/generated/geostat-ind-iphc_gfdata.rds")
#saveRDS(ind, file = "data/generated/geostat-ind-iphc_gfdata_julian.rds")
#ind_web <- readRDS("data/generated/geostat-ind-iphc.rds")
ind <- readRDS("data/generated/geostat-ind-iphc_gfdata.rds")
#ind_julian <- readRDS("data/generated/geostat-ind-iphc_gfdata_julian.rds")
# ind_withouthk <- readRDS("data/generated/geostat-ind-iphc_withouthk.rds")

# hk <- ggplot(ind, aes(year, log(est)), colour = "black") +
#   geom_line()
# hk + geom_line(data = ind_withouthk, aes(year, log(est)), col = "red")

obs <- group_by(d, year) |>
  summarise(n_hooksobserved = mean(hooksobserved))

ind |>
  left_join(obs) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = n_hooksobserved)) +
  geom_pointrange() +
  geom_line() +
coord_cartesian(ylim = c(0, NA)) +
geom_vline(xintercept = 2020, lty = 2) +
geom_vline(xintercept = 2000, lty = 2) +
scale_colour_viridis_c()

# ggplot(ind_julian, aes(year, est, ymin = lwr, ymax = upr), colour = "blue") +
#   geom_pointrange() +
#   geom_line()

x <- ind |>
  left_join(obs) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr), colour = "blue") +
  geom_pointrange() +
  geom_line()
# ind_julian <- ind_julian |>
#   left_join(obs)
# x + geom_line(data = ind_julian, aes(year, est), col = "red") + coord_cartesian(ylim = c(0, 50)) +
# geom_pointrange(data = ind_julian, aes(ymin = lwr, ymax = upr), col = "red") + geom_point()


# ind_rw |>
#   left_join(obs) |>
#   ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = n_hooksobserved)) +
#   geom_pointrange() +
#   coord_cartesian(ylim = c(0, NA)) +
#   geom_vline(xintercept = 2020, lty = 2) +
#   geom_vline(xintercept = 2000, lty = 2) +
#   scale_colour_viridis_c()


ind |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.4) +
  geom_line() +
  coord_cartesian(ylim = c(0, NA))

# mean(ind$est[ind$year %in% c(1998:2002)]) / min(ind$est)
plot(obs$n_hooksobserved, ind$est)

group_by(d, year) |>
  summarise(m = mean(C_it20)) |>
  ggplot(aes(year, m)) +
  geom_line()

# grid_wcvi <- filter(grid, Y <= 5600)
# ggplot(grid_wcvi, aes(X, Y, colour = depth_m_log)) +
#   geom_point() +
#   coord_fixed() +
#   scale_colour_viridis_c(trans = "sqrt", direction = -1)
#
# p_wcvi <- predict(fit_iphc_nb2, newdata = grid_wcvi, return_tmb_object = TRUE)
# ind_wcvi <- get_index(p_wcvi, bias_correct = TRUE)
#
# saveRDS(ind_wcvi, file = "data/generated/geostat-ind-iphc-wcvi.rds")
# ind_wcvi <- readRDS("data/generated/geostat-ind-iphc-wcvi.rds")
#
#
# grid_north <- filter(grid, Y > 5600)
# ggplot(grid_north, aes(X, Y, colour = depth_m_log)) +
#   geom_point() +
#   coord_fixed() +
#   scale_colour_viridis_c(trans = "sqrt", direction = -1)
#
# p_north <- predict(fit_iphc_nb2, newdata = grid_north, return_tmb_object = TRUE)
# ind_north <- get_index(p_north, bias_correct = TRUE)
#
# saveRDS(ind_north, file = "data/generated/geostat-ind-iphc-north.rds")
# ind_north <- readRDS("data/generated/geostat-ind-iphc-north.rds")
#
# ind_north |>
#   mutate(type = "north") |>
#   bind_rows(mutate(ind_wcvi, type = "wcvi")) |>
#   ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = type, fill = type)) +
#   geom_ribbon(alpha = 0.3) +
#   coord_cartesian(ylim = c(0, NA))



## Plot figures in prediction grid ----
# Depth ----
gg <- grid %>% filter(year == 1998) %>%
  ggplot(aes(longitude, latitude, fill = exp(depth_m_log), colour = exp(depth_m_log))) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(shape = 21) +
  scale_fill_viridis_c(trans = "sqrt") +
  scale_colour_viridis_c(trans = "sqrt") +
  labs(x = "Longitude", y = "Latitude", colour = "Depth (m)", fill = "Depth (m)")
ggsave("figs/iphc/prediction_grid_depth.png", gg, height = 4, width = 4, dpi = 600)

# Omega ----
rb_fill <- scale_fill_gradient2(high = "red", low = "blue", mid = "grey90")
rb_col <- scale_colour_gradient2(high = "red", low = "blue", mid = "grey90")

gg <- ggplot(p$data, aes(longitude, latitude, fill = omega_s, colour = omega_s)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(shape = 21) +
  rb_fill + rb_col +
  labs(x = "Longitude", y = "Latitude", colour = "Spatial effect", fill = "Spatial effect")
ggsave("figs/iphc/prediction_grid_omega.png", gg, height = 4, width = 4, dpi = 600)

# Epsilon ----
gg <- ggplot(p$data, aes(longitude, latitude, fill = epsilon_st, colour = epsilon_st)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_tile(height = 0.25, width = 0.25) + #point(shape = 21) +
  rb_fill + rb_col +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", colour = "Spatiotemporal\neffect", fill = "Spatiotemporal\neffect")
ggsave("figs/iphc/prediction_grid_eps.png", gg, height = 6, width = 5, dpi = 600)

# log-density ----
gg <- ggplot(p$data, aes(longitude, latitude, fill = est, colour = est)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_tile(width = 0.25, height = 0.25) +
  scale_colour_viridis_c(option = "C") +
  scale_fill_viridis_c(option = "C") +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Longitude", y = "Latitude", colour = "log density", fill = "log density")
ggsave("figs/iphc/prediction_grid_density.png", gg, height = 6, width = 5, dpi = 600)

# Index ----
gg <- ggplot(ind, aes(year, est)) +
  geom_point() +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  labs(x = "Year", y = "IPHC Index") +
  expand_limits(y = 0)
ggsave("figs/iphc/iphc_index.png", gg, height = 3, width = 4)

# Marginal effect of depth ----
marginal_depth <- visreg::visreg(fit_iphc_nb2, xvar = "depth_m_log", breaks = seq(0, 500, 25),
                                 data = fit_iphc_nb2$data,
                                 xtrans = exp,
                                 plot = FALSE)

gg <- plot(marginal_depth, gg = TRUE,
           line.par = list(col = 1),
           points.par = list(alpha = 0.2)) +
  coord_cartesian(xlim = c(0, 500), expand = FALSE) +
  labs(x = "Depth (m)", y = "log(CPUE)")
ggsave("figs/iphc/depth_marginal.png", gg, height = 3, width = 4)
