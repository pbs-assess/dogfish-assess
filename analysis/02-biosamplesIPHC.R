library(dplyr)
library(gfiphc)
library(PBSdata)
library(ggplot2)
library(sf)

d <- read.csv("data/raw/IPHC_dogfish_lengths2021.csv") %>%
  filter(reg_area == "2B", sex %in% c("F", "M")
  )
glimpse(d)


#get the lat and longs of the stations
iphc_coast <- readRDS("data/raw/Non-Pacific halibut data_raw.rds")
glimpse(iphc_coast)
names(iphc_coast) <- tolower(names(iphc_coast))
iphc_latlongs <- readRDS("data/raw/Set and Pacific halibut data_raw.rds") %>%
  dplyr::select(IPHC.Reg.Area, Date, Eff, Ineffcde, BeginLat, BeginLon, AvgDepth..fm., Stlkey)
glimpse(iphc_latlongs)
names(iphc_latlongs) <- tolower(names(iphc_latlongs))

d2 <- d %>%
  left_join(dplyr::select(iphc_coast, year, station, stlkey)) %>%
  left_join(dplyr::select(iphc_latlongs, stlkey, beginlat, beginlon )) %>%
  drop_na(stlkey)
glimpse(d2)

unique(d$reg_area)
unique(d$sex)
sort(unique(d$year))
range(na.omit(d$length)) #units are cm

#Exploratory figures
d %>% group_by(year, sex) %>%
  summarize(count_sex = n()) %>%
  ggplot() +
  geom_line(aes(year, count_sex, group = sex, col = sex)) +
  geom_point(aes(year, count_sex, group = sex, col = sex))

d %>%
  drop_na(length) %>%
  ggplot() +
  geom_jitter(aes(as.factor(year), length, group = sex, col = sex), alpha = 0.05) +
  geom_boxplot(aes(as.factor(year), length, col = sex)) +
  facet_wrap(~sex)

# Annual length comp
d %>%
  drop_na(length) %>%
  ggplot() +
  geom_freqpoly(aes(length, group = sex, col = sex)) +
  facet_wrap(vars(year))
ggsave("figs/biosample-iphc.png", width = 6, height = 4, dpi = 150)

# Annual length comp using gfplot function
lengths_iphc <- d %>%
  drop_na(length) %>%
  mutate(survey_abbrev = "IPHC FISS", usability_code = 0,
         specimen_id = 1:n(),
         species_common_name = "dogfish", age = NA,
         sex = ifelse(sex == "F", 2, 1)) %>%
  gfplot::tidy_lengths_raw(survey = "IPHC FISS")
g <- gfplot::plot_lengths(lengths_iphc, show_year = "all")
ggsave("figs/lengths-iphc.png", g, width = 4, height = 6)

## Get coordinates of each IPHC station to coordinates,
## Plot length comp by latitude band (48-50 = WCVI, 50-52 = Central Coast, 52-56 = North Coast)
## To do: match coordinates to area (3CD, etc.)
iphc_coord <- rbind(
  gfiphc::sets_other_years %>% select(year, station, lat, long, usable) %>% rename(lon = long),
  gfiphc::setData2013 %>% select(year, station, lat, lon, usable),
  gfiphc::setData2020 %>% select(year, station, lat, lon, usable),
  gfiphc::setData2021 %>% select(year, station, lat, lon, usable)
)
d_coord <- d %>%
  mutate(station = as.character(station)) %>%
  left_join(iphc_coord) %>%
  mutate(band = cut(lat, breaks = c(48, 50, 52, 56)))

g <- d_coord %>%
  ggplot(aes(length, linetype = sex)) +
  geom_freqpoly(binwidth = 4) +
  facet_wrap(vars(band), scales = "free_y") +
  ggtitle("IPHC - by latitude") +
  labs(x = "Length (cm)", y = "Frequency", linetype = "Sex") +
  theme(legend.position = "bottom")
ggsave("figs/biosample-iphc-latitude.png", width = 6, height = 3, dpi = 150)

# commercial groundfish fishing areas shapefile ---------------------------
#BC
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)
bc_coast_proj <- sf::st_transform(bc_coast, crs = 32609)

data(major) #from PBSdata

gmas_PIDs <- data.frame(PID = c(1, seq(3,9, 1)), GMAs = c("5E", "5D", "5C", "5B", "5A",
                                                          "3D", "3C", "4B"))
gma <- major %>%
  left_join(gmas_PIDs) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(crs = 32609) %>%
  group_by(GMAs) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

ggplot() +
  geom_sf(data = gma, aes(fill = as.factor(GMAs))) +
  geom_sf(data = bc_coast_proj, aes(fill  = "grey50"))

d_sp <- d2 %>%
  st_as_sf(coords = c("beginlon", "beginlat"), crs = 4326) %>%
  st_transform(crs = 32609)

ggplot() +
  geom_sf(data = gma, aes(fill = as.factor(GMAs))) +
  geom_sf(data = bc_coast_proj, aes(fill  = "grey50")) +
  geom_sf(data = d_sp, col = "red")

d_mfa <- st_intersection(d_sp, gma) %>%
  st_drop_geometry()

ggplot() +
  geom_density(data = d_mfa, aes(length, group = sex, col = sex)) +
  facet_wrap(~GMAs) +
  theme_classic()

ggplot() +
  geom_density(data = d_mfa, aes(length, group = sex, col = sex), size = 2) +
  theme_classic()

males <- filter(d_mfa, sex == "M")
range(males$length)
plot(density(males$length))

ggplot() +
  geom_boxplot(data = d_mfa, aes(GMAs, length)) +
  theme_classic() +
  facet_wrap (~sex)

range(males$length)
