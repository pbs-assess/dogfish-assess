

# library -----------------------------------------------------------------
library(tidyverse)

#https://www.researchgate.net/figure/DFO-management-areas-of-the-Pacific-Region-Fisheries-and-Oceans-Canada-2004_fig1_242162660

# load raw data -----------------------------------------------------------
d <- readxl::read_excel("data/raw/iREC/iREC estimates Jul 2012 to Dec 2023 29012024.xlsx") |>
  filter(ITEM == "Dogfish")

#note: #estimate is in pieces, not kg and accounts for effort
#see raw data for README
#Units are pieces for catches and licence days for effort. Zero estimates are not provided; these result when no catch is reported for a combination of month, area, method, species, mark status, and disposition. See worksheet "how estimates are calculated" for more information.

# summarise by year -------------------------------------------------------
names(d) <- tolower(names(d))
glimpse(d)
d$estimate_tonnes <- d$estimate/1000

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
  summarise(sum_ton = (sum(estimate) * 5)/1000)

d |>
  group_by(year) |>
  filter(retainable == "Legal Size") |>
  summarise(sum = sum(estimate))


# exploratory figures -----------------------------------------------------------------

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




