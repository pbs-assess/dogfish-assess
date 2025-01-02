#pull data on additional sources of mortality from recreational, salmon bycatch (1998-2011), and surveys


# library -----------------------------------------------------------------
library(tidyverse)
#remotes::install_github("pbs-assess/gfdata")
library(gfdata)

# irec: load raw data -----------------------------------------------------------
#see here for recreational management areas
#https://www.researchgate.net/figure/DFO-management-areas-of-the-Pacific-Region-Fisheries-and-Oceans-Canada-2004_fig1_242162660

d <- readxl::read_excel("data/raw/iREC estimates Jul 2012 to Jan 2024 29022024.xlsx") |>
  filter(ITEM == "Dogfish")
names(d) <- tolower(names(d))

# d_old <- readxl::read_excel("data/raw/iREC estimates Jul 2012 to Jan 2024 29022024.xlsx") |>
#   filter(ITEM == "Dogfish")
# names(d_revised) <- tolower(names(d_revised))
#
# # d_old<- d_old |>
#   group_by(year) |>
#   summarise(catch = sum(estimate))|>
#   mutate(method = "orig")
#
# d <- d |>
#   group_by(year) |>
#   summarise(catch = sum(estimate)) |>
#   mutate(method = "orig")
#
# both <- rbind(d, d_old)
# ggplot(both, aes(year, catch, group = method, colour = method)) +
#   geom_point() +
#   geom_line()

eff <- readxl::read_excel("data/raw/iREC estimates Jul 2012 to Dec 2023 29012024.xlsx")
#eff <- readxl::read_excel("data/raw/iREC estimates Jul 2012 to Jan 2024 29022024.xlsx")
names(eff) <- tolower(names(eff))
eff2 <- eff |>
        filter(item  %in% c("Fisher Days (adult)", "Fisher Days (juvenile)")) |>
  #filter(!area %in% sog) |>
  group_by(year, method) |>
  summarize(effort_fishingdays = sum(estimate))

catch <- d |>
  filter(item  == "Dogfish") |>
  group_by(year, method) |>
  summarize(catch_method = sum(estimate))

ggplot(catch, aes(year, catch_method, group = method, colour = method)) +
  geom_point() +
  geom_line()

ggplot(eff2, aes(year, effort_fishingdays, group = method, colour = method)) +
  geom_point() +
  geom_line()

final <- left_join(catch, eff2)
final <- final |>
  group_by(year, method)|>
  summarize(catch = sum(catch_method), effort = sum(effort_fishingdays))
final$cpue <- final$catch/final$effort

unique(d$logistical_area)
unique(d$area)

d_irec <- d |>
  mutate(pfma = strsplit(area, " ") %>% sapply(getElement, 2) %>% as.numeric()) %>% #NAs are from haida gwaii
  mutate(outside = is.na(pfma) | !pfma %in% c(12:20, 28:29)) %>%
  group_by(year, disposition, outside) |>
  summarise(catch_count = sum(estimate)) #in pieces
saveRDS(d_irec, "data/generated/catch_recreational.rds")

g <- d_irec %>%
  filter(outside == TRUE) %>%
  ggplot(aes(year, catch_count, colour = disposition)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "iRec catch (pieces)", colour = "Disposition")
ggsave("figs/irec-outside.png", g, width = 6, height = 3)


# salmon: load raw data ------------------------------------------------
#data from Jason Parsley - salmon data unit
#need to ask about effort, doesn't seem to be in here
d <- read.csv("data/raw/FISHDATA-4285-LDavidson_Dogfish_PT1_1998-2011_dataraw.csv")
names(d) <- tolower(names(d))

d2 <- d |>
  mutate(date = as.Date(date_captured)) |>
  mutate(dmy = lubridate::ymd(date)) |>
  mutate(month = lubridate::month(date), year = lubridate::year(date)) |>
  mutate(julian = lubridate::yday(dmy)) |>
  mutate(effort = 1)

# #how much was caught each year in tonnes?
# d2 |>
#   group_by(year) |>
#   summarize(sum = sum(catch_qty)*avgwt_kg/1000)

salmon <- d2 |>
  mutate(outside = !mgmt_area %in% c(12:20, 28:29)) %>%
  group_by(year, kept_rel, outside) |>
  summarize(catch_count = sum(catch_qty))
saveRDS(salmon, "data/generated/catch_salmonbycatch.rds")

g <- salmon %>%
  filter(outside == TRUE) %>%
  ggplot(aes(year, catch_count, colour = kept_rel)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Salmon catch (pieces)", colour = "Disposition")
ggsave("figs/salmon-outside.png", g, width = 6, height = 3)

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
  summarize(catch_ton = sum(catch_weight)/1000)  #kg to tons

saveRDS(tl, "data/generated/catch_trawlsurvey.rds")

#how much is caught each year in hbll and iphc in counts?
d |>
  filter(survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "IPHC FISS")) |>
  group_by(year, survey_abbrev) |>
  summarize(catch_count = sum(catch_count)/1000) |>
  ggplot(aes(year, catch_count, colour = survey_abbrev)) +
  geom_point() +
  geom_line()

ll <- d |>
  filter(survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S", "IPHC FISS")) |>
  group_by(year, survey_abbrev) |>
  summarize(catch_count = sum(catch_count))
saveRDS(ll, "data/generated/catch_longline.rds")




