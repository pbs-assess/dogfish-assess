#assumed avereage weight of a dogfish is 1 kg and use that to convert counts to weights
avgwt_kg <- 2

# library -----------------------------------------------------------------
library(tidyverse)
#remotes::install_github("pbs-assess/gfdata")
library(gfdata)

# irec: load raw data -----------------------------------------------------------
d <- readxl::read_excel("data/raw/iREC/iREC estimates Jul 2012 to Dec 2023 29012024.xlsx") |>
  filter(ITEM == "Dogfish")
names(d) <- tolower(names(d))

#how much was caught each year in tonnes?
#convert pieces to weights by average weight of a dogfish 5lbs or 2.2 kg
irec <- d |>
  group_by(year) |>
  summarise(catch_t = sum(estimate)*avgwt_kg/1000) #in pieces
irec

dups <- d |>
  distinct(area, year, estimate)
d |>
  group_by(year) |>
  summarise(catch_t = sum(estimate)) #in pieces

#see here for recreational management areas
#https://www.researchgate.net/figure/DFO-management-areas-of-the-Pacific-Region-Fisheries-and-Oceans-Canada-2004_fig1_242162660

#note: #estimate is in pieces, not kg and accounts for effort
#see raw data for README
#Units are pieces for catches and licence days for effort.
#Zero estimates are not provided; these result when no catch is reported for a combination of
#month, area, method, species, mark status, and disposition.
#See worksheet "how estimates are calculated" for more information.

# irec: summarise by year -------------------------------------------------------


unique(d$logistical_area)
#"Barkley"            "Winter Harbour"     "Campbell River"     "Nanaimo"
#"Victoria"           "Tahsis/Nootka"      "Vancouver"          "Prince Rupert"
# "Central Coast"      "Haida Gwaii"        "Port Hardy"         "Port Renfrew"
# "Sunshine Coast"     "Tofino"             "Kyuquot"            "Lower Fraser River"
# "Port Alberni"


d |>
  group_by(year) |>
  summarise(sum = sum(estimate)) #in pieces

d |>
  group_by(year) |>
  summarise(sum = sum(estimate)*2.2/1000) #in pieces


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

#how much was caught each year in tonnes?
d2 |>
  group_by(year) |>
  summarize(sum = sum(catch_qty)*avgwt_kg/1000)

salmon <- d2 |>
  group_by(year) |>
  summarize(catch_t = sum(catch_qty)*avgwt_kg/1000)

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
  facet_wrap(~month, scales = "free")

d2 |>
  group_by(year, month, gear_type) |>
  summarise(sum = sum(catch_qty)/sum(effort)) |>
  ggplot() +
  geom_point(aes(year, log(sum), colour = gear_type)) + geom_line(aes(year, log(sum), colour = gear_type)) +
  facet_wrap(~month, scales = "free")




# DFO surveys: load raw data ----------------------------------------------

dsurveys <- readRDS("data/raw/survey-sets_2023.rds")
#dsamps <- readRDS("output/data_survey_samples.rds")
unique(dsurveys$survey_abbrev)
survey <- c( "HBLL INS N" , "HBLL INS S")
d <- dplyr::filter(dsurveys, !survey_abbrev %in% survey)
d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))

#how much is caught each year in trawl in tonnes?
d |>
  filter(!survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "IPHC FISS")) |>
  group_by(year) |>
  summarize(sum_tl = sum(catch_weight)/1000) |> #kg to tons
  ggplot() +
  geom_point(aes(year, sum_tl)) +
  geom_line(aes(year, sum_tl))

tl <- d |>
  filter(!survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "IPHC FISS")) |>
  group_by(year) |>
  summarize(catch_t = sum(catch_weight)/1000)  #kg to tons

#how much is caught each year in hbll and iphc in tonnes?
d |>
  filter(survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "IPHC FISS")) |>
  group_by(year) |>
  summarize(sum_tl = sum(catch_count)*avgwt_kg/1000) |> #kg to tons
  ggplot() +
  geom_point(aes(year, sum_tl)) +
  geom_line(aes(year, sum_tl))

ll <- d |>
  filter(survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "IPHC FISS")) |>
  group_by(year) |>
  summarize(catch_t = sum(catch_count)*avgwt_kg/1000) #kg to tons


# Yearly catches (tonnes) -------------------------------------------------

tl
tl$type <- "tl"
ll
ll$type <- "ll"
salmon
salmon$type <- "salmon"
irec
irec$type <- "rec"

total <- bind_rows(tl, ll, salmon, irec) |>
  group_by(year) |>
  summarize(catch_tons = sum(catch_t))

ggplot(total, aes(year, catch_tons)) + geom_point() + geom_line()

total <- bind_rows(tl, ll, salmon, irec) |>
  group_by(year, type) |>
  summarize(catch_tons = sum(catch_t))

ggplot(total, aes(year, catch_tons, colour = type)) + geom_point() + geom_line()
