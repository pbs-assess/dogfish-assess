library(gfplot)
library(dplyr)
library(ggplot2)

d <- readRDS("data/raw/survey-samples.rds")
d <- mutate(d, species_common_name = "north pacific spiny dogfish") # some missing!?, there are NAs at the bottom of the dataset for ALL columns
dc <- readRDS("data/raw/commercial-samples.rds")

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
plot_lengths(lengths_dog, show_year = "all")
ggsave("figs/lengths-dogfish-survey-inside.png", width = 4, height = 6)

lengths_hbll_ins <- tidy_lengths_raw(d,
  survey = c("HBLL INS N", "HBLL INS S")
)
plot_lengths(lengths_hbll_ins, show_year = "all")
ggsave("figs/lengths-hbll-survey-inside.png", width = 6, height = 8)

table(dc$length_type)
table(dc$sampling_desc)
table(dc$gear_desc)

out <- grep("4B", dc$major_stat_area_name)

lengths_comm <- dc[out, ] |>
  filter(sampling_desc %in% "UNSORTED") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., sample_type = "commercial"),
    .id = "survey_abbrev"
  )
plot_lengths(lengths_comm, show_year = "all")
ggsave("figs/lengths-commercial-outside.png", width = 6, height = 8)

lengths_comm <- dc[-out, ] |>
  filter(sampling_desc %in% "UNSORTED") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., sample_type = "commercial"),
    .id = "survey_abbrev"
  )
plot_lengths(lengths_comm, show_year = "all")
ggsave("figs/lengths-commercial-inside.png", width = 6, height = 10)

# Length weight -------------------------------------------------------

# all surveys combined right now:
mm <- fit_length_weight(d, sex = "male", df = 3, usability_codes = c(0, 1, 2, 6))
mf <- fit_length_weight(d, sex = "female", df = 3, usability_codes = c(0, 1, 2, 6))
plot_length_weight(object_female = mf, object_male = mm)
mm$pars
mf$pars
ggsave("figs/length-weight-survey.png", width = 5, height = 4)

# Maturity ------------------------------------------------------------

# outside stock
fit <- d |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6))
summary(fit$model)
plot_mat_ogive(fit)
ggsave("figs/maturity-outside-survey.png", width = 6, height = 3)

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
