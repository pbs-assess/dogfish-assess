

# library -----------------------------------------------------------------
library(tidyverse)

# irec: load raw data -----------------------------------------------------------
d <- readxl::read_excel("data/raw/iREC/iREC estimates Jul 2012 to Dec 2023 29012024.xlsx") |>
  filter(ITEM == "Dogfish")

#see here for recreational management areas
#https://www.researchgate.net/figure/DFO-management-areas-of-the-Pacific-Region-Fisheries-and-Oceans-Canada-2004_fig1_242162660

#note: #estimate is in pieces, not kg and accounts for effort
#see raw data for README
#Units are pieces for catches and licence days for effort. Zero estimates are not provided; these result when no catch is reported for a combination of month, area, method, species, mark status, and disposition. See worksheet "how estimates are calculated" for more information.

# irec: summarise by year -------------------------------------------------------
names(d) <- tolower(names(d))
glimpse(d)

unique(d$logistical_area)
#"Barkley"            "Winter Harbour"     "Campbell River"     "Nanaimo"
#"Victoria"           "Tahsis/Nootka"      "Vancouver"          "Prince Rupert"
# "Central Coast"      "Haida Gwaii"        "Port Hardy"         "Port Renfrew"
# "Sunshine Coast"     "Tofino"             "Kyuquot"            "Lower Fraser River"
# "Port Alberni"


d |>
  group_by(year, month, area, disposition) |>
  summarise(sum = sum(estimate))

d |>
  group_by(year, month, area, disposition) |>
  summarise(sum = sum(estimate))

d |>
  group_by(year) |>
  summarise(sum = sum(estimate)) #27,000 dogfish were captured in 2023??

d |>
  group_by(year) |>
  summarise(sum_ton = (sum(estimate) * 5)/2204) #5 lbs

d |>
  group_by(year) |>
  filter(retainable == "Legal Size") |>
  summarise(sum = sum(estimate))


# irec: exploratory figures -----------------------------------------------------------------

unique(d$logistical_area)
#"Barkley"            "Winter Harbour"     "Campbell River"     "Nanaimo"
#"Victoria"           "Tahsis/Nootka"      "Vancouver"          "Prince Rupert"
# "Central Coast"      "Haida Gwaii"        "Port Hardy"         "Port Renfrew"
# "Sunshine Coast"     "Tofino"             "Kyuquot"            "Lower Fraser River"
# "Port Alberni"


d |>
  group_by(year) |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum))

d |>
  group_by(year, logistical_area  ) |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum)) +
  facet_wrap(vars(logistical_area), scales = "free")

d |>
  group_by(year, logistical_area  ) |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum)) +
  facet_wrap(vars(logistical_area))

d |>
  group_by(year, month) |>
  filter(logistical_area == "Victoria") |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum)) +
  facet_wrap(vars(month))

d |>
  group_by(year, month) |>
  filter(logistical_area == "Campbell River") |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum)) +
  facet_wrap(vars(month))

d |>
  group_by(year, month, logistical_area) |>
  filter(!(logistical_area %in% c("Campbell River", "Central Coast", "Lower Fraser River", "Nanaimo",
                                "Sunshine Coast", "Vancouver"))) |>
  filter(logistical_area != "Victoria") |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, log(sum), colour = logistical_area)) +
  geom_line(aes(year, log(sum), colour = logistical_area)) +
  facet_wrap(vars(month)) +
  theme_classic()

d |>
  group_by(year, month, logistical_area) |>
  filter(logistical_area %in% c("Campbell River", "Central Coast", "Lower Fraser River", "Nanaimo",
                                  "Sunshine Coast", "Vancouver")) |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, log(sum), colour = logistical_area)) +
  geom_line(aes(year, log(sum), colour = logistical_area)) +
  facet_wrap(vars(month), scales = "free") +
  theme_classic()

d |>
  group_by(year, month) |>
  filter(!(logistical_area %in% c("Campbell River", "Central Coast", "Lower Fraser River", "Nanaimo",
                                  "Sunshine Coast", "Vancouver"))) |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, log(sum))) +
  geom_line(aes(year, log(sum))) +
  facet_wrap(vars(month)) +
  theme_classic()

d |>
  group_by(year, month) |>
  filter(logistical_area %in% c("Haida Gwaii")) |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, log(sum))) +
  geom_line(aes(year, log(sum))) +
  facet_wrap(vars(month), scales = "free") +
  theme_classic()

d |>
  group_by(year, month) |>
  summarise(sum = sum(estimate)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum)) +
  facet_wrap(vars(month))







# salmon: load raw data ------------------------------------------------
#data from Jason Parsley - salmon data unit
#need to ask about effort, doesn't seem to be in here
#for now one boat is one unit of effort
d <- read.csv("C:/Salmon Bycatch 2022/FISHDATA-4285-LDavidson_Dogfish_PT1_1998-2011_dataraw.csv")
names(d) <- tolower(names(d))
str(d)
d2 <- d |>
  mutate(date = as.Date(date_captured)) |>
  #mutate(date = as.Date(date_captured, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date)) |>
  mutate(month = lubridate::month(date), year = lubridate::year(date)) |>
  mutate(julian = lubridate::yday(dmy)) |>
  mutate(effort = 1)
d2


# salmon: exploratory figures-------------------------------------------------

d2 |>
  group_by(year) |>
  summarise(sum_ton = sum((catch_qty)*5)/2204) #5 lbs for each dogfish, lbs to tones

d2 |>
  group_by(year) |>
  summarise(sum_ton = sum(catch_qty)) #the whole of the salmon fishery catches less than recreational??

d2 |>
  group_by(year, gear_type) |>
  summarize(count = n()) |>
  ggplot() +
  geom_point(aes(year, count, colour = gear_type)) +
  geom_line(aes(year, count, colour = gear_type))

d2 |>
  group_by(year) |>
  summarise(sum = sum(catch_qty)/sum(effort)) |>
  ggplot() +
  geom_point(aes(year, sum)) + geom_line(aes(year, sum))

d2 |>
  group_by(year, ) |>
  summarise(sum = sum(catch_qty)) |>
  ggplot() +
  geom_point(aes(year, sum)) + geom_line(aes(year, sum))

d2 |>
  group_by(year, month) |>
  summarise(sum = sum(catch_qty)/sum(effort)) |>
  ggplot() +
  geom_point(aes(year, sum, colour = month)) + geom_line(aes(year, sum, colour = month)) +
  facet_wrap(~month, scales bycatch= "free")

d2 |>
  group_by(year, month, gear_type) |>
  summarise(sum = sum(catch_qty)/sum(effort)) |>
  ggplot() +
  geom_point(aes(year, log(sum), colour = gear_type)) + geom_line(aes(year, log(sum), colour = gear_type)) +
  facet_wrap(~month, scales = "free")

