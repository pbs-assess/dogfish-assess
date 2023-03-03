library(dplyr)
library(gfiphc)

d <- read.csv("data/raw/IPHC_dogfish_lengths2021.csv") %>%
  filter(reg_area == "2B", sex %in% c("F", "M")
  )
glimpse(d)

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



