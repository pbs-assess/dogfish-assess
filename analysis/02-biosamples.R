library(gfplot)
library(dplyr)
library(ggplot2)

d <- readRDS("data/raw/survey-samples.rds")
d <- mutate(d, species_common_name = "north pacific spiny dogfish") # some missing!?
dc <- readRDS("data/raw/commercial-samples.rds")

# Length composition --------------------------------------------------

table(d$survey_abbrev)

lengths_syn <- tidy_lengths_raw(d,
  survey = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")
)
plot_lengths(lengths_syn, show_year = "all")

lengths_dog <- tidy_lengths_raw(d,
  survey = c("DOG")
)
plot_lengths(lengths_dog, show_year = "all")

lengths_hbll_ins <- tidy_lengths_raw(d,
  survey = c("HBLL INS N", "HBLL INS S")
)
plot_lengths(lengths_hbll_ins, show_year = "all")

table(dc$length_type)
table(dc$sampling_desc)
table(dc$gear_desc)

lengths_comm <- dc |>
  filter(sampling_desc %in% "UNSORTED") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., sample_type = "commercial"),
    .id = "survey_abbrev"
  )

plot_lengths(lengths_comm, show_year = "all")

# Length weight -------------------------------------------------------

# all surveys combined right now:
mm <- fit_length_weight(d, sex = "male", df = 3, usability_codes = c(0, 1, 2, 6))
mf <- fit_length_weight(d, sex = "female", df = 3, usability_codes = c(0, 1, 2, 6))
plot_length_weight(object_female = mf, object_male = mm)
mm$pars
mf$pars

# Maturity ------------------------------------------------------------

# outside stock
fit <- d |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6))
summary(fit$model)
plot_mat_ogive(fit)

# inside stock
fit <- d |>
  filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S", "DOG")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6))
summary(fit$model)
plot_mat_ogive(fit)

# Available samples ---------------------------------------------------

d |> filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S", "DOG")) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Blues") +
  ggtitle("Inside stock HBLL and Dogfish longline survey samples")

d |> filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Blues") +
  ggtitle("Outside stock synoptic trawl survey samples")

dc |> filter(grepl("4B", major_stat_area_name)) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Reds") +
  ggtitle("Inside stock commercial samples")

dc |> filter(grepl("5[ABCDE]+", major_stat_area_name)) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Reds") +
  ggtitle("Outside stock commercial samples")

