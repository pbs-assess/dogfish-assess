# calculate a coastwide index using the Hecate Strait Multi Species trawl survey

# Hecate Strait multi species trawl extends back to 1984
# one index

# library -----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(sdmTMB)
library(cowplot)
theme_set(gfplot::theme_pbs())
# library(gfplot)
# library(gfdata)
library(sf)

# map objects -------------------------------------------------------------
CRS <- 32609
coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
  sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

# load data ---------------------------------------------------------------

# surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
surveysets <- readRDS("data/raw/survey-sets.rds")
d <- dplyr::filter(surveysets, survey_abbrev %in% c(
  "SYN WCVI", "SYN HS",
  "SYN WCHG", "HS MSA", "SYN QCS"
))
d <- sdmTMB::add_utm_columns(d, utm_crs = CRS)
sort(unique(d$year))
unique(d$survey_abbrev)

# summary plots -----------------------------------------------------------
d |>
  ggplot() +
  geom_point(aes(year, area_swept, colour = catch_weight), size = 2)

d |>
  ggplot() +
  geom_point(aes(Y, X, colour = log(catch_weight)), size = 2) +
  facet_wrap(~year)

d |>
  ggplot() +
  geom_jitter(aes(year, log(catch_weight), colour = log(catch_weight), size = catch_weight)) +
  facet_wrap(~survey_abbrev)

d |>
  ggplot() +
  geom_jitter(aes(year, depth_m, colour = catch_weight, size = catch_weight)) +
  facet_wrap(~survey_abbrev)

d |>
  ggplot() +
  geom_jitter(aes(year, area_swept, colour = catch_weight, size = catch_weight)) +
  facet_wrap(~survey_abbrev)

d |>
  ggplot() +
  geom_point(aes(year, doorspread_m, colour = catch_weight), size = 2) +
  facet_wrap(~survey_abbrev)

d |>
  ggplot() +
  geom_point(aes(year, tow_length_m, colour = catch_weight), size = 2) +
  facet_wrap(~survey_abbrev)

# data cleaning -----------------------------------------------------------
d <- d |>
  mutate(UTM.lon = X, UTM.lat = Y)
d <- d |> drop_na(depth_m)
d |>
  filter(is.na(area_swept) == TRUE) |>
  tally() # only 5 missing area swept
d <- d |> drop_na(area_swept)
d <- d |> mutate(survey_type = ifelse(survey_abbrev %in% c(
  "SYN WCVI", "SYN QCS",
  "SYN WCHG", "SYN HS"
), "trawl", "multi"))
d$offset <- log(d$area_swept)
d$offset_sm <- log(d$area_swept / 1000)
d <- d |> mutate(logbot_depth = log(depth_m))
meandepth <- mean(d$logbot_depth)
d <- d |>
  mutate(
    logbot_depthc = logbot_depth - meandepth,
    logbot_depth2c = logbot_depthc * logbot_depthc
  )
d |>
  ggplot() +
  geom_point(aes(year, area_swept, colour = catch_weight), size = 2)
d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))

d |>
  group_by(year, survey_abbrev) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch)) +
  facet_wrap(~survey_abbrev, scales = "free")

d |>
  group_by(year, survey_abbrev) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  facet_wrap(~survey_abbrev) +
  facet_wrap(~survey_abbrev, scales = "free")

d |>
  ggplot() +
  geom_jitter(aes(year, area_swept, size = log(catch_weight + 1), colour = log(catch_weight))) +
  facet_wrap(~survey_abbrev, scales = "free")

d <- d |>
  mutate(date2 = as.Date(time_deployed, format = "%Y-%m-%d H:M:S")) |>
  mutate(dmy = lubridate::ymd(date2)) |>
  mutate(julian = lubridate::yday(dmy))

d |>
  ggplot() +
  geom_point(aes(year, julian, colour = catch_weight), size = 2)

# create grid -------------------------------------------------------------

g <- gfplot::synoptic_grid
g <- g |> dplyr::select(-survey_domain_year)
g$survey_type <- "trawl"
g$survey_abbrev <- "SYN WCVI"

# add year to grid
sort(unique(d$year))
# g <- purrr::map_dfr(unique(d$year), ~ tibble(g, year = .x))
year <- seq(min(d$year), max(d$year))
missing_years <- setdiff(year, unique(d$year))
missing_years

g <- purrr::map_dfr(year, ~ tibble(g, year = .x))

g$logbot_depth <- log(g$depth)
g$logbot_depth2 <- log(g$depth) * log(g$depth)
g$logbot_depthc <- g$logbot_depth - meandepth
g$logbot_depth2c <- g$logbot_depthc * g$logbot_depthc
g <- g |> mutate(UTM.lon = X, UTM.lat = Y)
g$offset <- 0

plot(g$X, g$Y, pch = ".") # grid
points(d$X, d$Y, col = "red", pch = ".")

# create mesh -------------------------------------------------------------
mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 25)
plot(mesh$mesh)
mesh$mesh$n

# HS MS + trawl coast model -------------------------------------------------------------------
# grid
g$julian <- mean(d$julian)
g$julian_small <- g$julian / 100
g$offset_sm <- 0
g$survey_type <- "trawl"
plot(g$UTM.lon, g$UTM.lat, pch = ".")

d <- d |> mutate(logbot_depth = log(depth_m), logbot_depth2 = log(depth_m) * log(depth_m))
d$julian_small <- d$julian / 100

d |>
  group_by(year, survey_abbrev) |>
  summarize(catch = sum(catch_weight), effort = sum(area_swept)) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) +
  facet_wrap(~survey_abbrev, scales = "free")

m <- sdmTMB(
  # catch_weight ~ 1 + survey_type,
  catch_weight ~ 1 + survey_type + logbot_depthc + I(logbot_depthc^2),
  data = d,
  time = "year",
  mesh = mesh,
  spatiotemporal = "rw",
  silent = FALSE,
  offset = "offset_sm",
  spatial = TRUE,
  do_index = TRUE,
  anisotropy = TRUE,
  # priors = sdmTMBpriors(
  #   b = normal(location = c(NA, 0), scale = c(NA, 1))
  # ),
  extra_time = missing_years,
  predict_args = list(newdata = g),
  index_args = list(area = g$cell_area),
  family = delta_lognormal()
)
print(m)
sanity(m)
plot_anisotropy(m)
tidy(m, "ran_pars", conf.int = TRUE)
tidy(m, "ran_pars", conf.int = TRUE, model = 2)
tidy(m, "fixed", conf.int = TRUE)
tidy(m, "fixed", conf.int = TRUE, model = 2)
m$sd_report
saveRDS(m, "data/generated/m_HSMScoastdl.rds")
m <- readRDS("data/generated/m_HSMScoastdl.rds")

ind_dl <- get_index(m, bias_correct = TRUE)
saveRDS(ind_dl, "data/generated/m_HSMSdl-coast-index.rds")
ind_dl <- readRDS("data/generated/m_HSMSdl-coast-index.rds")

had_data <- select(d, year) |> distinct() |> mutate(surveyed = TRUE)
ind_dl_filtered <- left_join(ind_dl, had_data) |>
  mutate(surveyed = ifelse(!is.na(surveyed), TRUE, FALSE)) |>
  filter(surveyed) |>
  select(-surveyed)

end_msa <- filter(d, survey_abbrev %in% "HS MSA") |> pull(year) |> max()

ind_dl_filtered |>
  ggplot(aes(year, est)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  ylab("Relative biomass index") + xlab("Year") +
  coord_cartesian(expand = FALSE, ylim = c(0, max(ind_dl$upr) * 1.02),
    xlim = c(range(ind_dl$year) + c(-0.5, 0.5))) +
  geom_vline(xintercept = end_msa, col = "red", lty = 2)

saveRDS(ind_dl_filtered, "data/generated/hs-msa-coast-index.rds")
