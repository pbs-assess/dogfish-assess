library(gfplot)
library(dplyr)
library(ggplot2)

d <- readRDS("data/raw/survey-samples.rds")
d <- mutate(d, species_common_name = "north pacific spiny dogfish") # some missing!?, there are NAs at the bottom of the dataset for ALL columns
dc <- readRDS("data/raw/commercial-samples.rds")

# Summary of numbers and trips of outside surveys
d_sumry <- local({
  d_subset <- d %>%
    filter(major_stat_area_name != "4B: STRAIT OF GEORGIA",
           !is.na(length))

  d_fe <- d_subset %>%
    group_by(year, survey_abbrev) %>%
    summarise(n_fe = length(unique(fishing_event_id)), n_samp = sum(length > 0))

  d_fe
})

# Summary of number of trips of outside commercial fishing
dc_sumry <- dc %>%
  filter(major_stat_area_name != "4B: STRAIT OF GEORGIA",
         !sampling_desc == "UNKNOWN",
         gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL"),
         !is.na(length)) %>%
  mutate(month = lubridate::month(trip_start_date)) %>%
  group_by(year, month, gear_desc, sampling_desc) %>%
  summarise(n_fe = length(unique(fishing_event_id)), n_samp = sum(length > 0))

g <- ggplot(dc_sumry, aes(month, year)) +
  facet_grid(vars(gear_desc), vars(sampling_desc)) +
  geom_tile(height = 1, width = 1, aes(fill = n_fe)) +
  #geom_text(aes(label = n_fe)) +
  gfplot::theme_pbs() +
  scale_fill_viridis_c(trans = "log") +
  geom_hline(colour = "grey80", yintercept = 1970:2021 + 0.5) +
  geom_vline(colour = "grey80", xintercept = 1:12 + 0.5)

g <- ggplot(dc_sumry, aes(month, year)) +
  facet_grid(vars(gear_desc), vars(sampling_desc)) +
  geom_text(aes(label = n_fe)) +
  gfplot::theme_pbs()


# Length composition --------------------------------------------------

table(d$survey_abbrev)

lengths_syn <- tidy_lengths_raw(d,
  survey = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")
)
plot_lengths(lengths_syn, show_year = "all")
ggsave("figs/lengths-synoptic-outside.png", width = 7, height = 11)

lengths_dog <- tidy_lengths_raw(d,
  survey = c("DOG")
)
g <- plot_lengths(lengths_dog, show_year = "all")
ggsave("figs/lengths-dogfish-survey-inside.png", g, width = 4, height = 6)

lengths_hbll_ins <- tidy_lengths_raw(d,
  survey = c("HBLL INS N", "HBLL INS S")
)
g <- plot_lengths(lengths_hbll_ins, show_year = "all")
ggsave("figs/lengths-hbll-survey-inside.png", g, width = 6, height = 8)

table(dc$length_type)
table(dc$sampling_desc)
table(dc$gear_desc)

ins <- grep("4B", dc$major_stat_area_name)

lengths_comm <- dc[-ins, ] |>
  filter(sampling_desc %in% "UNSORTED") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., sample_type = "commercial"),
    .id = "survey_abbrev"
  )
g <- plot_lengths(lengths_comm, show_year = "all")
ggsave("figs/lengths-commercial-outside.png", g, width = 6, height = 8)

lengths_comm <- dc[ins, ] |>
  filter(sampling_desc %in% "UNSORTED") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., sample_type = "commercial"),
    .id = "survey_abbrev"
  )
g <- plot_lengths(lengths_comm, show_year = "all")
ggsave("figs/lengths-commercial-inside.png", g, width = 6, height = 10)

# Length weight -------------------------------------------------------

# all surveys combined right now:
mm <- fit_length_weight(d, sex = "male", df = 3, usability_codes = c(0, 1, 2, 6))
mf <- fit_length_weight(d, sex = "female", df = 3, usability_codes = c(0, 1, 2, 6))
plot_length_weight(object_female = mf, object_male = mm)
mm$pars
mf$pars
ggsave("figs/length-weight-survey.png", width = 5, height = 4)

# Maturity ------------------------------------------------------------

# gfbio threshold for mature dogfish
#mat_df <- readr::read_csv(
#  file = system.file("extdata", "maturity_assignment.csv", package = "gfplot"),
#  col_types = readr::cols(maturity_convention_code = readr::col_integer(),
#                          maturity_convention_desc = readr::col_character(),
#                          sex = readr::col_integer(),
#                          mature_at = readr::col_integer())
#)
#mat_df %>% filter(maturity_convention_code == 10)

# Summary of maturity codes
d %>%
  filter(sex == 2,
         major_stat_area_name != "4B: STRAIT OF GEORGIA") %>%
  group_by(maturity_code, maturity_desc) %>%
  summarise(n = n())

# outside stock - female mature if maturity_code >= 77
fit <- d |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6))
summary(fit$model)
plot_mat_ogive(fit)
ggsave("figs/maturity-outside-survey.png", width = 6, height = 3)

g <- fit$data %>%
  group_by(sex, age_or_length) %>%
  summarise(n = n(), p = mean(mature)) %>%
  ggplot(aes(age_or_length)) +
  geom_point(aes(y = p, shape = factor(sex))) +
  #geom_text(aes(y = p, label = n), alpha = 0.4, hjust = 0, nudge_y = 0.025) +
  scale_shape_manual(name = "Sex", values = c(16, 1)) +
  geom_line(data = fit$pred_data, aes(y = glmm_fe, linetype = factor(female)), show.legend = FALSE) +
  labs(x = "Length", y = "Proportion mature")
ggsave("figs/maturity-outside-survey-prop.png", g, width = 6, height = 3)

# Re-fit if females mature at 55
fit_55 <- fit$data %>%
  mutate(mature = ifelse(female == 1, maturity_code >= 55, mature)) %>%
  stats::glm(mature ~ age_or_length * female, data = ., family = binomial)

pred_data <- fit_55$data %>%
  select(age_or_length, female) %>%
  mutate(glmm_fe = predict(fit_55, newdata = ., type = "response"))

prop_55 <- rbind(
  g$data %>% filter(sex == 2) %>% ungroup() %>% select(age_or_length, n, p) %>% mutate(type = 77),
  fit_55$data %>%
    filter(sex == 2) %>%
    group_by(age_or_length) %>%
    summarise(n = n(), p = mean(mature)) %>%
    mutate(type = 55)
)

pred_55 <- rbind(
  fit$pred_data %>% filter(female == 1) %>% select(age_or_length, glmm_fe) %>% mutate(type = 77),
  pred_data %>% filter(female == 1) %>% select(age_or_length, glmm_fe) %>% mutate(type = 55)
)

g <- prop_55 %>%
  ggplot(aes(age_or_length)) +
  geom_point(aes(y = p, shape = factor(type))) +
  scale_shape_manual(name = "Mature at", values = c(16, 1)) +
  geom_line(data = pred_55, aes(y = glmm_fe, linetype = factor(type))) +
  labs(x = "Length", y = "Proportion mature", linetype = "Mature at")
ggsave("figs/maturity-outside-survey-prop-55.png", g, width = 6, height = 3)





# inside stock
fit <- d |>
  filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S", "DOG")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6))
summary(fit$model)
plot_mat_ogive(fit)
ggsave("figs/maturity-inside-survey.png", width = 6, height = 3)

# Available samples ---------------------------------------------------

yrs <- c(1966, 2022)
g1 <- d |> filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S", "DOG")) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Blues", year_range = yrs) +
  ggtitle("Inside stock HBLL and Dogfish longline survey samples")

g2 <- d |> filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Blues", year_range = yrs) +
  ggtitle("Outside stock synoptic trawl survey samples")

g3 <- dc |> filter(grepl("4B", major_stat_area_name)) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Reds", year_range = yrs) +
  ggtitle("Inside stock commercial samples")

g4 <- dc |> filter(grepl("5[ABCDE]+", major_stat_area_name)) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Reds", year_range = yrs) +
  ggtitle("Outside stock commercial samples")

cowplot::plot_grid(g1, g2, g3, g4, ncol = 1L)

ggsave("figs/biosample-available.png", width = 11, height = 10, dpi = 150)

# Synoptic trawl length composition by sex and area
# Some evidence that sex ratio changes by area (see 5E)
# but survey catches primarily small females
g <- d %>%
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) %>%
  filter(sex %in% c(1, 2)) %>%
  mutate(area = substr(major_stat_area_name, 1, 2),
         sex = ifelse(sex == 2, "Female", "Male")) %>%
  ggplot(aes(length, linetype = sex)) +
  geom_freqpoly(binwidth = 4) +
  facet_wrap(vars(area), scales = "free_y") +
  ggtitle("Synoptic trawl") +
  labs(x = "Length (cm)", y = "Frequency", linetype = "Sex") +
  theme(legend.position = "bottom")
ggsave("figs/biosample-synoptic-trawl-area.png", g, width = 6, height = 5, dpi = 150)

# Fishery length by sex and area
# Midwater trawl seems to behave similarly to synoptic trawl
g <- dc %>%
  filter(gear_desc == "MIDWATER TRAWL") %>%
  filter(sex %in% c(1, 2)) %>%
  mutate(area = substr(major_stat_area_name, 1, 2),
         sex = ifelse(sex == 2, "Female", "Male")) %>%
  ggplot(aes(length, linetype = sex)) +
  geom_freqpoly(binwidth = 4) +
  facet_wrap(vars(area), scales = "free_y") +
  ggtitle("MIDWATER TRAWL") +
  labs(x = "Length (cm)", y = "Frequency", linetype = "Sex") +
  theme(legend.position = "bottom")
ggsave("figs/biosample-midwater-trawl-area.png", g, width = 6, height = 3, dpi = 150)

# Bottom trawl catches the big females
g <- dc %>%
  filter(gear_desc == "BOTTOM TRAWL") %>%
  filter(sex %in% c(1, 2)) %>%
  mutate(area = substr(major_stat_area_name, 1, 2),
         sex = ifelse(sex == 2, "Female", "Male")) %>%
  ggplot(aes(length, linetype = sex)) +
  geom_freqpoly(binwidth = 4) +
  facet_wrap(vars(area), scales = "free_y") +
  ggtitle("BOTTOM TRAWL") +
  labs(x = "Length (cm)", y = "Frequency", linetype = "Sex") +
  theme(legend.position = "bottom")
ggsave("figs/biosample-bottom-trawl-area.png", g, width = 6, height = 5, dpi = 150)
