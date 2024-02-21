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
mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 20)
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
  catch_weight ~ 1 + survey_type,
  data = d,
  time = "year",
  mesh = mesh,
  spatiotemporal = "rw",
  silent = FALSE,
  offset = "offset_sm",
  spatial = FALSE,
  do_index = TRUE,
  anisotropy = TRUE,
  priors = sdmTMBpriors(
    b = normal(location = c(NA, 0), scale = c(NA, 1))
  ),
  extra_time = missing_years,
  predict_args = list(newdata = g),
  index_args = list(area = g$cell_area),
  family = delta_lognormal()
)
print(m)
sanity(m)
plot_anistropy(m)
tidy(m, "ran_pars", conf.int = TRUE)
tidy(m, "fixed", conf.int = TRUE)
m$sd_report
saveRDS(m, "data/generated/m_HSMScoastdl.rds")
m <- readRDS("data/generated/m_HSMScoastdl.rds")

# SA stopped here ... ---------------------------------

m <- sdmTMB::sdmTMB(
  catch_weight ~ 1 + logbot_depthc + logbot_depth2c + survey_type,
  data = d,
  time = "year",
  mesh = mesh,
  spatiotemporal = "rw",
  silent = FALSE,
  offset = "offset_sm",
  spatial = FALSE,
  do_index = TRUE,
  share_range = FALSE,
  priors = sdmTMBpriors(
    # b = normal(location = c(NA, 0, 0), scale = c(NA, 1, 1))),
    b = normal(location = c(NA, 0, 0, 0), scale = c(NA, 1, 1, 1))
  ),
  control = sdmTMBcontrol(
    newton_loops = 1L
  ),
  extra_time = c(1985, 1986, 1988, 1990, 1992, 1994, 1997, 1999, 2001),
  predict_args = list(newdata = g),
  index_args = list(area = g$cell_area),
  family = delta_lognormal_mix()
)

sanity(m)
tidy(m, "ran_pars", conf.int = TRUE)
tidy(m, "fixed", conf.int = TRUE)
summary(m$sd_report)
saveRDS(m, "data/generated/m_HSMScoastdlmix.rds")


# load model files and calculate index ------------------------------------
m_jul <- readRDS("data/generated/m_HSMScoastdlmix_jul.rds")
ind_jul <- get_index(m_jul, bias_correct = TRUE)
ggplot(ind_jul, aes(year, est)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999")

m <- readRDS("data/generated/m_HSMScoastdlmix.rds")
ind_dlmix <- get_index(m, bias_correct = TRUE)
ggplot(ind_dlmix, aes(year, est)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999")

# diff distributions
m_dg <- update(m, family = delta_gamma())
ind_dg <- get_index(m_dg, bias_correct = TRUE)
ggplot(ind_dg, aes(year, est)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999")

# diff distributions
m_dl <- update(m, family = delta_lognormal())
ind_dl <- get_index(m_dl, bias_correct = TRUE)
ggplot(ind_dl, aes(year, est)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999")

# aic
AIC(m_dl)
AIC(m_dg)
AIC(m)
AIC(m_jul)

# ggplot of all indices
ind_dlmix$type <- "dlmix"
ind_jul$type <- "jul"
ind_dg$type <- "dg"
ind_dl$type <- "dl"
both <- rbind(ind_dlmix, ind_jul, ind_dl)
ggplot(both, aes(year, est, colour = type, fill = type)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = type), alpha = 0.4)
