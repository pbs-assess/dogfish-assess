# split trawl by sex

library(tidyverse)
library(gfdata)
library(sdmTMB)
library(gfplot)


file <- "data/generated/trawl_sets_splitbysex.rds"

if(!file.exists(file)) {

# load data ---------------------------------------------------------------
# survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
# survey_sets <- get_survey_sets(species = "north pacific spiny dogfish")
#
# saveRDS(survey_samples, "data/raw/survey-samples.rds")
# saveRDS(survey_sets, "data/raw/survey-sets.rds")

d <- readRDS("data/raw/survey-samples.rds") %>%
  filter(survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG"))
dsets <- readRDS("data/raw/survey-sets.rds") %>%
  filter(survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG"))

unique(d$sex) # 2 is female, 1 is male


# Exploratory plots of trawl sets with samples and without samples -------------------------------------------------------
dsamps <- d %>%
  mutate(sampsyn = "yes") %>%
  dplyr::select(sampsyn, fishing_event_id, year)

join <- dsamps %>%
  right_join(select(dsets, year, fishing_event_id, survey_abbrev, catch_weight, longitude, latitude),
             multiple = "all") %>%
  filter(catch_weight >0)

join %>% count(sampsyn)

join %>%
  group_by(survey_abbrev) %>%
  count(sampsyn)

join %>%
  group_by(survey_abbrev, sampsyn) %>%
  summarize(total_weight = sum(catch_weight))

join %>% filter(catch_weight > 10) %>% ggplot() +
geom_point(aes(longitude, latitude, col = sampsyn, size = catch_weight)) +
  facet_wrap(~year)


# Exploratory plots of sex and length -------------------------------------------------------
years <- data.frame(year = c(seq(2003, 2022, 1)), group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10))
d %>%
  left_join(years) %>%
  mutate(sex = ifelse(sex == 1, "M", ifelse(sex == 2, "F", NA))) %>%
  ggplot(aes(length, fill = as.factor(sex))) +
  geom_histogram(alpha = 0.5, binwidth = 1) +
  facet_grid(rows = vars(group), cols = vars(sex), scales = "free_y") +
  scale_x_continuous(breaks = c(25, 50, 75, 100, 125), labels = c(25, 50, 75, 100, 125)) +
  # scale_colour_manual(values = c("grey50", "grey50")) +
  scale_fill_manual(values = c("red", "blue", "grey50")) +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 15, colour = "grey20"),
    axis.title.y = element_text(size = 15, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )


# plot of length weight histogram by sex and survey and year.
# Many early surveys are missing weight but not length
ggplot(data = d, aes(weight, colour = sex, group = sex, fill = sex)) +
  geom_histogram() +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(year~survey_abbrev)

ggplot(data = d, aes(length, colour = sex, group = sex, fill = sex)) +
  geom_histogram() +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(year~survey_abbrev)

ggplot(
  data = d,
  aes(
    x = log10(length),
    y = log10(weight),
    col = interaction(sex, survey_abbrev)
  )
) +
  facet_grid(~sex) +
  viridis::scale_color_viridis(discrete = TRUE) +
  geom_point(
    size = .7,
    alpha = .8,
    position = "jitter"
  ) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    size = 2,
    alpha = .8
  ) +
  theme_minimal() +
  labs(title = "Raw data: Linear Relationship for Different Sexes/Trawls")




# Split the set catch (weight) by sex -----------------------------------------------------------

# To split the set catch (weight) by sex:
# 1. Use a log-log linear relationship to calculate length/weight relationship;
# 2. Calculate the weight of the sample catch that is m & f, per tow;
# 3. Apply the m/f ratio to the set catch weight.


# # 1. Use a log-log linear relationship to calculate length/weigh --------
# pull out outliers
dm <- d %>%
  filter(sex == 1) %>%
  select(sex, weight, length) %>%
  drop_na()
m <- lm(log(weight) ~ log(length), data = dm)
new_x <- data.frame(length = dm$length)

# predicting using model new data
dm$y_hat_new <- predict(m, new_x) # data must be in data.frame
dm <- dm %>% mutate(resid = (log(weight) - y_hat_new)^2)
pp <- ggplot(dm, aes(log(length), log(weight))) +
  geom_point()
pp + geom_point(data = filter(dm, resid > 0.39), aes(log(length), log(weight), col = "red"))
m_remove <- dm %>% filter(resid > 0.39)

# pull out outliers - female
df <- d %>%
  filter(sex == 2) %>%
  select(sex, weight, length) %>%
  drop_na()
f <- lm(log(weight) ~ log(length), data = df)
new_x <- data.frame(length = df$length)

# predicting using model new data
df$y_hat_new <- predict(f, new_x) # data must be in data.frame
df <- df %>% mutate(resid = (log(weight) - y_hat_new)^2)
pp <- ggplot(df, aes(log(length), log(weight))) +
  geom_point()
pp + geom_point(data = filter(df, resid > 0.39), aes(log(length), log(weight), col = "red"))

f_remove <- df %>% filter(resid > 0.39)
remove <- rbind(f_remove, m_remove)
d2 <- d %>% anti_join(remove)


#FYI - Count the number of species that are not sexed for each survey
unique(d$sex)
d2 %>%
  group_by(year) %>%
  filter(sex == 0) %>%
  tally()


# calculate length/weight relationship
f <- fit_length_weight(
  d2,
  sex = ("female"),
  min_samples = 50L,
  method = c("tmb"), # method = c("tmb", "rlm", "lm"),
  usability_codes = NULL,
  scale_weight = 1 / 1000 # double check this
)

m <- fit_length_weight(
  d2,
  sex = ("male"),
  min_samples = 50L,
  method = c("tmb"), # method = c("tmb", "rlm", "lm"),
  usability_codes = NULL,
  scale_weight = 1 / 1000
)


ggplot(d2, aes(length, weight)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~sex)

plot_length_weight(object_female = f, object_male = m)


trawl_f <- filter(d2, sex == 2)
trawl_f$weight_predicted <- exp(f$pars$log_a +
  f$pars$b * log(trawl_f$length)) * 1000

trawl_m <- filter(d2, sex == 1)
trawl_m$weight_predicted <- exp(m$pars$log_a +
  m$pars$b * log(trawl_m$length)) * 1000

predicted_weight_tw <- rbind(trawl_m, trawl_f)

weighttw <- predicted_weight_tw %>% select(
  year, fishing_event_id, survey_abbrev,
  length, weight, weight_predicted, sex
)


# 2. Calculate the weight of the sample catch that is m & f, per --------

weighttw2 <- weighttw %>% mutate(weight_complete = ifelse(is.na(weight) == TRUE,
  weight_predicted,
  weight
))

annratio <- weighttw2 %>%
  group_by(year, survey_abbrev) %>%
  mutate(totalweight = sum(weight_complete)) %>%
  group_by(year, survey_abbrev, sex) %>%
  summarize(
    ratioweight = sum(weight_complete),
    ratio = round((ratioweight / totalweight), 2)
  ) %>% distinct() %>%
  mutate(sex = ifelse(sex == 1, "M", "F"))%>%
  rename(mean_ratio = ratio)

ggplot(annratio, aes(year, mean_ratio)) + geom_path(aes(colour = sex)) +
  facet_wrap(~survey_abbrev) + gfplot::theme_pbs()



sexratio <- weighttw2 %>%
  group_by(year, fishing_event_id) %>%
  mutate(totalweight = sum(weight_complete)) %>%
  group_by(year, fishing_event_id, sex) %>%
  summarize(
    ratioweight = sum(weight_complete),
    ratio = round((ratioweight / totalweight), 2)
  ) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(sex = ifelse(sex == 1, "M", "F"))%>%
  dplyr::select(year, fishing_event_id, sex, ratio) %>%
  ungroup()



# 3. Apply the m/f ratio to the set catch weight. -----------------------

allsets <- dsets %>%
  dplyr::select(year, fishing_event_id) %>%
  distinct(year, fishing_event_id)

sex <- c("M", "F")
sexratio3 <- purrr::map_dfr(sex, ~ tibble(allsets, sex = .x))
sexratio4 <- sexratio3 %>% left_join(sexratio)

sexratio %>% group_by(sex) %>% summarise(ratio = mean(ratio, na.rm = TRUE))
sexratio4 %>% filter(sex == "M") %>% summarise(ratio = mean(ratio, na.rm = TRUE))
sexratio4 %>% filter(sex == "F") %>% summarise(ratio = mean(ratio, na.rm = TRUE))


sr1 <- sexratio4 %>% filter(sex == "M")
sr2 <- sexratio4 %>% filter(sex == "F")

ggplot(sr1, aes(ratio)) +
  geom_histogram() +
  geom_histogram(data = sr2, fill = "red", alpha = 0.5)


dsets_samps <- left_join(dsets, sexratio4, by = c(
  "year" = "year", "fishing_event_id" = "fishing_event_id"
))

dat <- left_join(dsets_samps, annratio) %>%
  mutate(ratio_filled = ifelse(is.na(ratio), mean_ratio, ratio),
         total_weight = catch_weight,
         catch_weight = total_weight * ratio_filled
  )

dat %>%
  filter(sex == "M") %>%
  ggplot(aes(longitude, latitude, col = catch_weight)) +
  geom_point() +
  facet_wrap(~year)
# it seems some years WCHG has no samples
# check catches involved... they are all pretty small, so will apply a 0.5 ratio to these nas
# dat %>% View() #


dat <- left_join(dsets_samps, annratio) %>%
  mutate(mean_ratio = ifelse(is.na(mean_ratio), 0.5, mean_ratio),
         ratio_filled = ifelse(is.na(ratio), mean_ratio, ratio),
         total_weight = catch_weight,
         catch_weight = total_weight * ratio_filled
  )

dat %>%
  filter(sex == "F") %>%
  ggplot(aes(longitude, latitude,
             size = catch_weight, alpha = catch_weight, colour = catch_weight)) +
  geom_point() +
  scale_colour_viridis_c(trans = sqrt, option = "B")+
  facet_wrap(~year)

dat %>%
  filter(sex == "M") %>%
  ggplot(aes(longitude, latitude,
             size = catch_weight, alpha = catch_weight, colour = catch_weight)) +
  geom_point() +
  scale_colour_viridis_c(trans = sqrt, option = "B")+
  facet_wrap(~year)

glimpse(dat)
saveRDS(dat, "data/generated/trawl_sets_splitbysex.rds")
}

# create index from males and females -------------------------------------
d <- readRDS("data/generated/trawl_sets_splitbysex.rds")

d <- sdmTMB::add_utm_columns(d, utm_crs = 32609)

# used old version of gfdata...
d$area_swept1 <- d$doorspread_m * d$tow_length_m
d$area_swept2 <- d$doorspread_m * (d$speed_mpm * d$duration_min)
d$area_swept <- ifelse(!is.na(d$area_swept1), d$area_swept1, d$area_swept2)


table(d$year[is.na(d$doorspread_m)])
table(d$year[is.na(d$tow_length_m)])
table(d$year[is.na(d$tow_length_m)])
d$survey_abbrev[is.na(d$doorspread_m)]

d <- dplyr::filter(d, !is.na(area_swept))

ggplot(d, aes(X, Y, size = density_kgpm2)) +
  geom_point(pch = 21, alpha = 0.3) +
  facet_wrap(vars(year)) +
  coord_fixed()

sum(is.na(d$depth_m))
table(d$year[is.na(d$depth_m)])
d <- dplyr::filter(d, !is.na(depth_m))
d$log_area_swept <- log(d$area_swept)


df <- filter(d, sex == "F")
dm <- filter(d, sex == "M")


## test a version where mean sex ratios not applied to the unsampled sets, instead they are excluded
# df <- filter(d, sex == "F") %>%
#   mutate(catch_weight = ifelse(is.na(ratio),
#                                ifelse(total_weight == 0, 0, NA),
#                                total_weight * ratio
#                                )
#          ) %>% filter(!is.na(catch_weight))
#
# dm <- filter(d, sex == "M") %>%
#   mutate(catch_weight = ifelse(is.na(ratio),
#                           ifelse(total_weight == 0, 0, NA),
#                           total_weight * ratio
#     )
#   ) %>% filter(!is.na(catch_weight))


mesh1 <- make_mesh(df, c("X", "Y"), cutoff = 15)
plot(mesh1)
mesh1$mesh$n

ffit <- sdmTMB(
  catch_weight ~ 1 + poly(log(depth_m), 2L),
  family = delta_gamma(),
  data = df,
  mesh = mesh,
  offset = "log_area_swept",
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
)

# saveRDS(ffit, file = "data/generated/synoptic-sdmTMB-female.rds")
# saveRDS(ffit, file = "data/generated/synoptic-sdmTMB-female-trim.rds")

mesh <- make_mesh(dm, c("X", "Y"), cutoff = 15)
mfit <- sdmTMB(
  catch_weight ~ 1 + poly(log(depth_m), 2L),
  family = delta_gamma(),
  data = dm,
  mesh = mesh,
  offset = "log_area_swept",
  time = "year",
  spatiotemporal = "rw",
  spatial = "on",
  silent = FALSE,
  anisotropy = TRUE,
  control = sdmTMBcontrol(newton_loops = 1L)
)

# saveRDS(mfit, file = "data/generated/synoptic-sdmTMB-male.rds")
# saveRDS(mfit, file = "data/generated/synoptic-sdmTMB-male-trim.rds")

# ffit <- readRDS("data/generated/synoptic-sdmTMB-female.rds")
# mfit <- readRDS("data/generated/synoptic-sdmTMB-male.rds")
# ffit <- readRDS("data/generated/synoptic-sdmTMB-female-trim.rds")
# mfit <- readRDS("data/generated/synoptic-sdmTMB-male-trim.rds")

sanity(ffit)
sanity(mfit)
plot_anisotropy(ffit)
plot_anisotropy(mfit)
ffit
mfit
ffit$sd_report
mfit$sd_report

g <- gfplot::synoptic_grid |> dplyr::select(-survey_domain_year)
g <- rename(g, depth_m = depth)
# g <- add_utm_columns(g, utm_crs = 32609)

yrs <- sort(unique(fit$data$year))
grid <- sdmTMB::replicate_df(g, time_name = "year", time_values = yrs)

fp <- predict(ffit, newdata = grid, return_tmb_object = TRUE)
ind_f <- get_index(fp, bias_correct = TRUE)

mp <- predict(mfit, newdata = grid, return_tmb_object = TRUE)
ind_m <- get_index(mp, bias_correct = TRUE)

saveRDS(ind_f, file = "data/generated/geostat-ind-female.rds")
saveRDS(ind_m, file = "data/generated/geostat-ind-male.rds")
# saveRDS(ind, file = "data/generated/geostat-ind-male-trim.rds")
# saveRDS(ind, file = "data/generated/geostat-ind-female-trim.rds")

ind_f <- readRDS("data/generated/geostat-ind-female.rds")
ind_m <- readRDS("data/generated/geostat-ind-male.rds")
ind_ft <- readRDS("data/generated/geostat-ind-female-trim.rds")
ind_mt <- readRDS("data/generated/geostat-ind-male-trim.rds")
ind_all <- readRDS("data/generated/geostat-ind-synoptic.rds")

bind_rows(
  mutate(ind_all, Index = "Total"),
  # mutate(ind_ft, Index = "Female trimmed"),
  # mutate(ind_mt, Index = "Male trimmed"),
  mutate(ind_f, Index = "Female"),
  mutate(ind_m, Index = "Male")
) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = Index)) +
  geom_pointrange(position=position_dodge(width=1)) +
  # scale_colour_manual(values = c("red", "darkred", "blue", "navyblue", "darkgrey")) +
  scale_colour_manual(values = c("red",  "blue", "darkgrey")) +
  coord_cartesian(ylim = c(0, NA)) +
  ylab("Relative biomass") +
  xlab("Year") +
  gfplot::theme_pbs()

ggsave("figs/sex-specific-indexes.png", width = 8, height = 5.0)
