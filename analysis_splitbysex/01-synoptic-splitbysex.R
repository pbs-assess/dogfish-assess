# split trawl by sex

library(gfdata)
library(sdmTMB)
library(gfplot)
library(dplyr)
library(ggplot2)


# load data ---------------------------------------------------------------
# data_survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
# data_surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
#
# sets_tl <- filter(data_surveysets, survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")) %>%
#   mutate(geartype = "trawl")
# samps_tl <- filter(data_survey_samples, survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI", "SYN WCHG")) %>%
#   mutate(geartype = "trawl") %>%
#   filter(!is.na(species_common_name) == TRUE)
# saveRDS(samps_tl, "data/raw/samples_trawl.rds")
# saveRDS(sets_tl, "data/raw/sets_trawl.rds")

d <- readRDS("data/raw/samples_trawl.rds")
dsets <- readRDS("data/raw/sets_trawl.rds")

unique(d$sex) # 2 is female, 1 is male


# Exploratory plots of trawl sets with samples and without samples -------------------------------------------------------
dsamps <- d %>%
  mutate(sampsyn = "yes") %>%
  dplyr::select(sampsyn, fishing_event_id, year)

join <- dsamps %>%
  right_join(select(dsets, year, fishing_event_id, survey_abbrev, catch_weight))

join %>% count(sampsyn)

join %>%
  group_by(survey_abbrev) %>%
  count(sampsyn)

join %>%
  group_by(survey_abbrev, sampsyn) %>%
  summarize(total_weight = sum(catch_weight))


# Exploratory plots of sex and length -------------------------------------------------------
d %>%
  left_join(dsets) %>%
  group_by(year, sex, survey_abbrev, fishing_event_id, latitude, longitude) %>%
  filter(sex == 1) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_jitter(
    aes(longitude, latitude, colour = log(count), size = log(count))
  ) +
  facet_wrap(~ sex + year)

d %>%
  left_join(dsets) %>%
  group_by(year, sex, survey_abbrev, fishing_event_id, latitude, longitude) %>%
  filter(sex == 2) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_jitter(
    aes(longitude, latitude, colour = log(count), size = log(count))
  ) +
  facet_wrap(~ sex + year)

years <- data.frame(year = c(seq(2003, 2021, 1)), group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10))
d %>%
  left_join(years) %>%
  mutate(sex = ifelse(sex == 1, "M", "F")) %>%
  ggplot(aes(length, fill = as.factor(sex))) +
  geom_histogram(alpha = 0.5, binwidth = 1) +
  facet_grid(rows = vars(group), cols = vars(sex), scales = "free_y") +
  scale_x_continuous(breaks = c(25, 50, 75, 100, 125), labels = c(25, 50, 75, 100, 125)) +
  scale_colour_manual(values = c("grey50", "grey50")) +
  scale_fill_manual(values = c("red", "grey50")) +
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
ggplot(data = d, aes(length, weight, colour = sex, group = sex)) +
  geom_bar(stat = "identity", width = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ survey_abbrev + year, nrow = 3)

ggplot(data = d, aes(length, colour = sex, group = sex, fill = sex)) +
  geom_histogram() +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ survey_abbrev + year, nrow = 3)

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

# calculate length/weight relationship
f <- fit_length_weight(
  d,
  sex = ("female"),
  min_samples = 50L,
  method = c("tmb"), # method = c("tmb", "rlm", "lm"),
  usability_codes = NULL,
  scale_weight = 1 / 1000 # double check this
)

m <- fit_length_weight(
  d,
  sex = ("male"),
  min_samples = 50L,
  method = c("tmb"), # method = c("tmb", "rlm", "lm"),
  usability_codes = NULL,
  scale_weight = 1 / 1000
)


ggplot(d, aes(length, weight)) +
  geom_jitter() +
  facet_wrap(~sex)
range(f$predictions)
fdata <- f$data
range(m$predictions)
y <- m$data
plot_length_weight(object_female = f, object_male = m)


trawl_f <- filter(d, sex == 2)
trawl_f$weight_predicted <- exp(f$pars$log_a +
  f$pars$b * log(trawl_f$length)) * 1000
range(trawl_f$weight_predicted)
plot(density(trawl_f$weight_predicted))

trawl_m <- filter(d, sex == 1)
trawl_m$weight_predicted <- exp(m$pars$log_a +
  m$pars$b * log(trawl_m$length)) * 1000
range(trawl_m$weight_predicted)
plot(density(trawl_m$weight_predicted))
ggplot(trawl_m, aes(length, weight_predicted)) +
  geom_jitter()

predicted_weight_tw <- rbind(trawl_m, trawl_f)
glimpse(predicted_weight_tw)

weighttw <- predicted_weight_tw %>% select(
  year, fishing_event_id,
  length, weight, weight_predicted, sex
)


# # 2. Calculate the weight of the sample catch that is m & f, per --------

weighttw2 <- weighttw %>% mutate(weight_complete = ifelse(is.na(weight) == TRUE,
  weight_predicted,
  weight
))

sexratio <- weighttw2 %>%
  group_by(year, fishing_event_id) %>%
  mutate(totalweight = sum(weight_complete)) %>%
  group_by(year, fishing_event_id, sex) %>%
  summarize(
    ratioweight = sum(weight_complete),
    ratio = round((ratioweight / totalweight), 2)
  ) %>%
  distinct(.keep_all = TRUE) %>%
  dplyr::select(year, fishing_event_id, sex, ratio)



# 3. Apply the m/f ratio to the set catch weight. -----------------------

dsets <- readRDS("data/raw/sets_trawl.rds")
sexratio

dsets



# come back to this
# count the number of species that are not sexed for each survey
num_samples <- trawl_samples %>% count(survey_abbrev, fishing_event_id, year)
trawl_samples %>%
  count(survey_abbrev, fishing_event_id, year) %>%
  distinct()
num_nosex <- trawl_samples %>%
  group_by(survey_abbrev, year) %>%
  filter(sex == 0) %>%
  tally()
no_sex_prop <- inner_join(num_samples, num_nosex, by = c("survey_abbrev" = "survey_abbrev", "year" = "year"))
names(no_sex_prop) <- c("survey_abbrev", "year", "total_samples", "num_nosex")
no_sex_prop$prop_nosex <- no_sex_prop$num_nosex / no_sex_prop$total_samples * 100
