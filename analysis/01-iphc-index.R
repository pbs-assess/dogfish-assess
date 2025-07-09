library(ggplot2)
library(dplyr)
library(sdmTMB)
library(tidyr)
library(sf)
#devtools::install_github("pbs-assess/gfiphc")
library(gfiphc)
library(gfdata)
library(here)

# Set French language option
FRENCH <- TRUE

# Set decimal option for French
if (FRENCH) options(OutDec = ",")

# Translation helper function
tr <- function(english, french) {
  if (FRENCH) french else english
}

# note 2020 fishing was completed in July and August, whereas it is usually May to August.
# also 2021 and 2022 have reduced WCVI sampling.

#note you can't combine gfiphc and iphc data in 2022 because there were BC observers onboard
#and the number of fish counted won't match the hooks observed.

#this is downloaded from the IPHC FISS website
#https://www.iphc.int/data/fiss-survey-raw-survey-data/
#the search parameters included
#1. all years,
#2, 2B,
#3. purpose code = "Standard Grid",

#6. Spiny Dogfish,
#2023 data
#catch <- read.csv("data/raw/Non-Pacific halibut data_2023.csv")
#2022 data
# catch <- read.csv("~/Downloads/Non-Pacific halibut data_raw.csv")
# stations <- read.csv("~/Downloads/Map select_standardgrid.csv")
#latlongs <- read.csv("data/raw/Set and Pacific halibut data_2023.csv") |>
#    dplyr::filter(`IPHC.Reg.Area` %in% "2B")
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
# iphc_hksobs <- read.csv("data/raw/Non-Pacific halibut data_2023.csv") |>
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

# saveRDS(df_iphc, "data/raw/Non-Pacific halibut data_raw_gfdata.rds") #this has station but doesn't have IPHC area
iphc_coast <- readRDS("data/raw/Non-Pacific halibut data_raw_gfdata.rds")

#this has station and IPHC reg area and date (if including julian date)
# iphc_latlongs <- read.csv("data/raw/Set and Pacific halibut data_2023.csv") |>
  # dplyr::filter(`IPHC.Reg.Area` %in% "2B") |>
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
station <- iphc_stations$station
iphc_coast2 <- iphc_coast |>
  filter(station %in% station) |>
  inner_join(iphc_latlongs,
             by = c("station" = "station", "year" = "year"), relationship = "many-to-many")
x <- iphc_coast2 |> select(station, year, date)
x[duplicated(x), ]
#there are a couple stations that have been fished twice in the same year
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

iphc_coast3 <- iphc_coast2 |>
  mutate(remove = ifelse(year == 2019 & station %in% c(2107, 2099), "remove", "keep")) |>
  filter(remove != "remove") |>
  inner_join(iphc_hksobs) |> #for hook information
  distinct(.keep_all = TRUE) |>
  bind_rows(year2019_stations)

# check for duplicate years and stations
test<- dplyr::select(iphc_coast3, year, station )
iphc_coast3[duplicated(iphc_coast3), ]
test[duplicated(test), ]

iphc_coast3 <- iphc_coast3 %>%
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

filter(iphc_coast4, station == 2099 & year == 2019) #Two sets that day catches are different
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

# Check website and gfiphc data trends ----
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
  geom_line(size = 2) + geom_point(size = 4)


# Add Hook Competition ----
#from https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.iphc.int%2Fuploads%2F2023%2F12%2Fiphc-2023-fiss-hadj-20231031.xlsx&wdOrigin=BROWSELINK
h <- readxl::read_excel("data/raw/iphc-2023-fiss-hadj-20231031.xlsx") |> #iphc-2021-fiss-hadj.xlsx") #old data set
dplyr::filter(`IPHC Reg Area` %in% "2B")
saveRDS(h, file = "data/raw/iphc-2023-fiss-hadj.rds")

d <- readRDS("data/generated/IPHC_coastdata_nosog_gfdata.rds") |>
  mutate(depth_m = exp(depth_m_log)) |>
  mutate(hooksobserved = as.numeric(hooksobserved),
         hooksfished = as.numeric(hooksfished),
         station = as.character(station)) |>
  drop_na(hooksobserved)

h <- readRDS("data/raw/iphc-2023-fiss-hadj.rds") |>
  filter(Year >= 1998, Effective == "Y", Purpose == "SG") |>
  select(year = Year, station = Station, bait = Bait, hookobserved = `Hooks Observed`, purpose = Purpose, hadj = h.adj, date = Date) |>
  mutate(hadj = as.numeric(hadj), bait = as.numeric(bait), hookobserved = as.numeric(hookobserved),
         station = as.character(station)) |>
  drop_na(hookobserved)

h$bait[h$bait == 0] <- 1
h <- h[h$hookobserved > 0, ]

h$prop_bait_hooks <- h$bait / h$hookobserved
range(h$prop_bait_hooks)
h$hook_adjust_factor <- -log(h$prop_bait_hooks) / (1 - h$prop_bait_hooks)
plot(h$hook_adjust_factor, h$hadj)
abline(0, 1)
range(h$hook_adjust_factor)
h$date <- lubridate::as_date(h$date)
d$date <- lubridate::dmy(d$date)

ggplot(h, aes(prop_bait_hooks, hook_adjust_factor)) + geom_point()

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

d <- filter(d, !is.na(purpose)) # a few in 2021!?
stopifnot(sum(is.na(d$hook_adjust_factor)) == 0L)

d$offset <- log(d$hooksobserved)
d$offset_hk <- log(d$hooksfished / d$hook_adjust_factor) # hook comp
stopifnot(sum(is.na(d$offset_hk)) == 0L)
stopifnot(sum(is.na(d$hooksobserved)) == 0L)

saveRDS(d, "data/generated/IPHC_coastdata_nosog_gfdata_hk.rds")


## Figures for report - data and hook adjustment ----
coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

# Create appropriate figure directories
if (FRENCH) {
  dir.create("figs-french", showWarnings = FALSE)
  dir.create("figs-french/iphc", showWarnings = FALSE)
  fig_dir <- "figs-french"
} else {
  dir.create("figs/iphc", showWarnings = FALSE, recursive = TRUE)
  fig_dir <- "figs"
}

# Helper function for figure paths
fig_path <- function(filename) {
  file.path(fig_dir, filename)
}

theme_set(gfplot::theme_pbs())
gg <- ggplot(d, aes(longitude, latitude, fill = bait/hooksobserved, colour = bait/hooksobserved)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(pch = 21, alpha = 1, size = 0.4) +
  facet_wrap(vars(year)) +
  scale_fill_viridis_c(option = "C", limits = c(0, 1)) +
  scale_colour_viridis_c(option = "C", limits = c(0, 1)) +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"), 
       fill = tr("Proportion baited hooks", "Proportion d'hameçons appâtés"), 
       colour = tr("Proportion baited hooks", "Proportion d'hameçons appâtés"))
ggsave(fig_path("iphc/baited_hooks.png"), gg, height = 9, width = 7, dpi = 200)

d$numobs <- ifelse(is.na(d$N_it) == TRUE, d$N_it20, d$N_it)

gg <- ggplot(d, aes(longitude, latitude, fill = numobs/exp(offset), colour = numobs/exp(offset))) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(pch = 21, alpha = 1, size = 0.4) +
  facet_wrap(vars(year)) +
  theme(panel.spacing = unit(0, "in"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_colour_viridis_c(trans = "log", breaks = c(0.006, 0.050, 0.37)) +
  scale_fill_viridis_c(trans = "log", breaks = c(0.006, 0.050, 0.37)) +
  labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"), 
       fill = "CPUE", colour = "CPUE")
ggsave(fig_path("iphc/cpue.png"), gg, height = 9, width = 7, dpi = 190)

gg <- d %>%
  mutate(cpue = numobs/exp(offset)) %>%
  ggplot(aes(x = cpue, y = after_stat(count))) +
  geom_histogram(binwidth = 0.25, colour = 1, fill = "grey80", linewidth = 0.25) +
  facet_wrap(vars(year), ncol = 5) +
  theme(#panel.spacing = unit(0, "in"),
        strip.background = element_blank()) +
  labs(x = "CPUE", y = tr("Frequency", "Fréquence")) +
  coord_cartesian(xlim = c(-0.125, 3), expand = FALSE)
ggsave(fig_path("iphc/cpue_hist.png"), gg, height = 5, width = 6)

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
    labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"))
})
ggsave(fig_path("iphc/iphc_mesh.png"), g, width = 5, height = 6)

# Call sdm
d <- readRDS("data/generated/IPHC_coastdata_nosog_gfdata_hk.rds")
d$numobs <- ifelse(is.na(d$N_it) == TRUE, d$N_it20, d$N_it)

fit_iphc_nb2 <- sdmTMB(
  numobs ~ 0 + poly(depth_m_log, 2L), #this is both N_it20 or N_it, offset accounts for hooks observed
  family = nbinom2(link = "log"),
  time_varying = ~1,
  data = d,
  mesh = mesh,
  time = "year",
  offset = "offset",
  #offset = "offset_hk",
  spatiotemporal = "ar1",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE
)
saveRDS(fit_iphc_nb2, file = "data/generated/iphc-nb2-sdmTMB_gfdata.rds")
fit_iphc_nb2 <- readRDS("data/generated/iphc-nb2-sdmTMB_gfdata.rds")

set.seed(123)
r <- residuals(fit_iphc_nb2, type = "mle-mvn")
png(fig_path("iphc/qq.png"), width = 5, height = 5, res = 200, units = "in")
if (FRENCH) {
  qqnorm(r, main = "", asp = 1, xlab = "Quantiles théoriques", ylab = "Quantiles échantillons")
} else {
  qqnorm(r, main = "", asp = 1, xlab = "Theoretical quantiles", ylab = "Sample quantiels")
}
abline(0, 1)
dev.off()

set.seed(1)
s <- simulate(fit_iphc_nb2, nsim = 500, type = "mle-mvn")
dharma_residuals(s, fit_iphc_nb2, test_uniformity = F)
# ggsave(figs/)

#check
fit_iphc_nb2$data |> filter(N_it20 == 0) |> group_by(year) |> tally()
fit_iphc_nb2$data |> filter(N_it20 != 0) |> group_by(year) |> tally() |> as.data.frame()

#with hook comp
# fit_iphc_nb2_whk <- update(fit_iphc_nb2,
#                           formula =  numobs ~ 0 + poly(depth_m_log, 2L),
#                           offset = "offset_hk")
# saveRDS(fit_iphc_nb2_whk, file = "data/generated/iphc-nb2-sdmTMB_gfdata_wjulian.rds")
# fit_iphc_nb2_whk <- readRDS("data/generated/iphc-nb2-sdmTMB_gfdata_wjulian.rds")

# #with julian
# ggplot(d, aes(year, julian)) + geom_point()
# range(d$julian)
# d$julian_small <- d$julian/100
# fit_iphc_nb2_wjulian <- update(fit_iphc_nb2,
#                               formula =  numobs ~ 0 + poly(depth_m_log, 2L) + julian_small)
# saveRDS(fit_iphc_nb2_wjulian, file = "data/generated/iphc-nb2-sdmTMB_gfdata_wjulian.rds")
# fit_iphc_nb2_wjulian <- readRDS("data/generated/iphc-nb2-sdmTMB_gfdata_wjulian.rds")

# fit_rw <- update(fit_iphc_nb2, spatiotemporal = "rw", time_varying = NULL,
#   formula. = number_observed ~ 1 + poly(depth_m_log, 2L))

fit_iphc_nb2
fit_iphc_nb2$sd_report
sanity(fit_iphc_nb2)
plot_anisotropy(fit_iphc_nb2)
ggsave(fig_path("iphc/aniso.png"), width = 4, height = 4)
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

# years <- seq(1998, 2022,1)
years <- unique(fit_iphc_nb2$data$year)
grid <- sdmTMB::replicate_df(g, "year", years)

# p_rw <- predict(fit_rw, newdata = grid, return_tmb_object = TRUE)
# ind_rw <- get_index(p_rw, bias_correct = TRUE)

p <- predict(fit_iphc_nb2, newdata = grid, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)

saveRDS(ind, file = "data/generated/geostat-ind-iphc_gfdata.rds")
#saveRDS(ind, file = "data/generated/geostat-ind-iphc_gfdata_julian.rds")
ind <- readRDS("data/generated/geostat-ind-iphc_gfdata.rds")
#ind_julian <- readRDS("data/generated/geostat-ind-iphc_gfdata_julian.rds")
#ind_withouthk <- readRDS("data/generated/geostat-ind-iphc_withouthk.rds")

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
  scale_fill_viridis_c(trans = "sqrt", option = "G", direction = -1) +
  scale_colour_viridis_c(trans = "sqrt", option = "G", direction = -1) +
  labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"), 
       colour = tr("Depth (m)", "Profondeur (m)"), fill = tr("Depth (m)", "Profondeur (m)"))
ggsave(fig_path("iphc/prediction_grid_depth.png"), gg, height = 4, width = 4, dpi = 200)

# Omega ----
rb_fill <- scale_fill_gradient2(high = "red", low = "blue", mid = "grey90")
rb_col <- scale_colour_gradient2(high = "red", low = "blue", mid = "grey90")

gg <- ggplot(p$data, aes(longitude, latitude, fill = omega_s, colour = omega_s)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(shape = 21) +
  rb_fill + rb_col +
  labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"), 
       colour = tr("Spatial effect", "Effet spatial"), fill = tr("Spatial effect", "Effet spatial"))
ggsave(fig_path("iphc/prediction_grid_omega.png"), gg, height = 4, width = 4, dpi = 200)

# Epsilon ----
gg <- ggplot(p$data, aes(longitude, latitude, fill = epsilon_st, colour = epsilon_st)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_point(size = 0.4) + #point(shape = 21) +
  rb_fill + rb_col +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"), 
       colour = tr("Spatiotemporal\neffect", "Effet\nspatio-temporel"), 
       fill = tr("Spatiotemporal\neffect", "Effet\nspatio-temporel"))
ggsave(fig_path("iphc/prediction_grid_eps.png"), gg, height = 9, width = 6, dpi = 200)

# log-density ----
gg <- ggplot(p$data, aes(longitude, latitude, fill = est, colour = est)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(year)) +
  geom_point(size = 0.4) +
  scale_colour_viridis_c(option = "C") +
  scale_fill_viridis_c(option = "C") +
  theme(panel.spacing = unit(0, "in"),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"), 
       colour = tr("log density", "log densité"), fill = tr("log density", "log densité"))
ggsave(fig_path("iphc/prediction_grid_density.png"), gg, height = 9, width = 6, dpi = 200)

# Index ----
# Compare geo-spatial index with nominal index (bootstrapped mean) ----
gg <- ggplot(ind, aes(year, est)) +
  geom_point() +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  labs(x = tr("Year", "Année"), y = tr("IPHC Index", "Indice IPHC")) +
  expand_limits(y = 0)
ggsave(fig_path("iphc/iphc_index.png"), gg, height = 3, width = 4)

do_boot <- function(x, nsim = 250) {
  boot_fn <- function(d, i) {
    d <- d[i, ]
    mean(d$numobs/exp(d$offset))
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
index_boot_Nit <- d %>%
  mutate(numobs = N_it) %>%
  filter(!is.na(numobs)) %>%
  do_boot()

index_boot_Nit20 <- d %>%
  mutate(numobs = N_it20) %>%
  filter(!is.na(numobs)) %>%
  do_boot()

index_compare <- rbind(
  ind %>% select(year, est, lwr, upr) %>% mutate(type = tr("Geospatial model", "Modèle géospatial")),
  index_boot_Nit %>%
    #mutate(lwr = exp(log(index) - 1.96 * sqrt(log(1 + sd^2))),
    #       upr = exp(log(index) + 1.96 * sqrt(log(1 + sd^2)))) %>%
    mutate(lwr = index - 1.96 * sd,
           upr = index + 1.96 * sd) %>%
    select(year, index, lwr, upr) %>%
    rename(est = index) %>%
    mutate(type = tr("Bootstrapped mean - all hooks", "Moyenne bootstrap - tous les hameçons")),
  index_boot_Nit20 %>%
    #mutate(lwr = exp(log(index) - 1.96 * sqrt(log(1 + sd^2))),
    #       upr = exp(log(index) + 1.96 * sqrt(log(1 + sd^2)))) %>%
    mutate(lwr = index - 1.96 * sd,
           upr = index + 1.96 * sd) %>%
    select(year, index, lwr, upr) %>%
    rename(est = index) %>%
    mutate(type = tr("Bootstrapped mean - 20 hooks", "Moyenne bootstrap - 20 hameçons"))
)

gg <- index_compare %>%
  ggplot(aes(year, est)) +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line(linetype = 2, linewidth = 0.1) +
  facet_wrap(vars(type), ncol = 2, scales = "free_y") +
  gfplot::theme_pbs() +
  expand_limits(y = 0) +
  labs(x = tr("Year", "Année"), y = tr("Index of abundance", "Indice d'abondance"))
ggsave(fig_path("iphc/iphc_index_compare.png"), gg, height = 5, width = 6)


# Marginal effect of depth ----
marginal_depth <- visreg::visreg(fit_iphc_nb2, xvar = "depth_m_log", breaks = seq(0, 500, 25),
                                 data = fit_iphc_nb2$data,
                                 xtrans = exp,
                                 plot = FALSE)

gg <- plot(marginal_depth, gg = TRUE,
           line.par = list(col = 1),
           points.par = list(alpha = 0.2)) +
  coord_cartesian(xlim = c(0, 500), expand = FALSE) +
  labs(x = tr("Depth (m)", "Profondeur (m)"), y = "log(CPUE)")
ggsave(fig_path("iphc/depth_marginal.png"), gg, height = 4, width = 5)


# Generate IPHC index on HBLL grid - is decline in HBLL a function of habitat?
hbll <- sdmTMB::replicate_df(gfplot::hbll_grid$grid, "year", unique(fit_iphc_nb2$data$year)) %>%
  mutate(depth_m_log = log(depth), area = 4) %>%
  bind_rows() %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_names = c("UTM_lon", "UTM_lat"), utm_crs = 32609)

g <- rbind(
  gfplot::hbll_grid$grid %>% rename(longitude = X, latitude = Y) %>%
    select(longitude, latitude) %>%
    mutate(name = tr("HBLL grid", "Grille HBLL")),
  d %>%
    select(longitude, latitude) %>%
    mutate(name = tr("IPHC sets", "Traits IPHC"))
) %>%
  ggplot(aes(longitude, latitude, colour = name)) +
  geom_sf(data = coast, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  geom_point(shape = 1) +
  labs(x = tr("Longitude", "Longitude"), y = tr("Latitude", "Latitude"), colour = NULL) +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c(1, 2))
ggsave(fig_path("iphc/iphc_hbll_grid.png"), g, width = 5, height = 5)

p <- predict(fit_iphc_nb2, newdata = hbll, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)
readr::write_csv(ind, file = "data/generated/iphc_index_on_hbll.csv")

g <- ggplot(ind, aes(year, est)) +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  geom_point() +
  expand_limits(y = 0) +
  labs(x = tr("Year", "Année"), y = tr("IPHC index on HBLL grid", "Indice IPHC sur grille HBLL"))
ggsave(fig_path("iphc/iphc_index_on_hbll.png"), g, height = 3, width = 4)

hbll_wcvi <- filter(hbll, Y <= 51)
p <- predict(fit_iphc_nb2, newdata = hbll_wcvi, return_tmb_object = TRUE)
ind <- get_index(p, bias_correct = TRUE)
readr::write_csv(ind, file = "data/generated/iphc_index_on_hbll_south51.csv")




## Generate IPHC index from data collected only in HBLL domain

# library(dplyr)
#
# hg <- gfplot::hbll_grid$grid
#
# hg <- hg %>%
#   sf::st_as_sf(coords = c("X", "Y"))
#
# hgc <- concaveman::concaveman(hg)
# plot(hgc)
#
# dsf <- sf::st_as_sf(d, coords = c("longitude", "latitude"))
#
# hgi <- sf::st_intersection(hgc, dsf)
# hgi <- sf::st_intersection(dsf, hgc)
#
# x <- sf::st_coordinates(hgi)
# plot(x)
#
# hg <- gfplot::hbll_grid$grid
#
# plot(d$longitude, d$latitude)
# points(hg$X, hg$Y, col = "red", pch = ".")

# %>%
#   group_by(region) %>%
#   summarize(geometry = sf::st_union(geometry)) %>%
#   sf::st_convex_hull()
# plot(hulls)

# Revert decimal option
if (FRENCH) options(OutDec = ".")
