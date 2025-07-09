library(gfplot)
library(dplyr)
library(ggplot2)
theme_set(theme_pbs())

# Set French language option
FRENCH <- TRUE

# Translation helper function
tr <- function(english, french) {
  if (FRENCH) french else english
}

# Helper function for figure paths
fig_path <- function(filename) {
  if (FRENCH) {
    # Create French directory structure
    french_dir <- dirname(file.path("figs-french", filename))
    dir.create(french_dir, showWarnings = FALSE, recursive = TRUE)
    file.path("figs-french", filename)
  } else {
    file.path("figs", filename)
  }
}

# Set decimal separator for French
if (FRENCH) {
  old_dec <- options()$OutDec
  options(OutDec = ",")
}


d <- readRDS("data/raw/survey-samples.rds")
d <- mutate(d, species_common_name = "north pacific spiny dogfish") # some missing!?, there are NAs at the bottom of the dataset for ALL columns
dc <- readRDS("data/raw/commercial-samples.rds")

# Number of trips (sets?) of outside surveys ----
d_sumry <- d %>%
  filter(major_stat_area_name != "4B: STRAIT OF GEORGIA",
         !is.na(length)) %>%
  summarise(n_fe = length(unique(fishing_event_id)), n_samp = sum(length > 0), .by = c(year, survey_abbrev))


# Number of trips with bio sampling (opportunistic) of outside commercial fishing by year and month ----
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
  scale_fill_viridis_c(trans = "log") +
  geom_hline(colour = "grey80", yintercept = 1970:2021 + 0.5) +
  geom_vline(colour = "grey80", xintercept = 1:12 + 0.5)

g <- ggplot(dc_sumry, aes(month, year)) +
  facet_grid(vars(gear_desc), vars(sampling_desc)) +
  geom_text(aes(label = n_fe))

# Samples and specimens for summary:

# dc <- readRDS("data/raw/commercial-samples.rds")
# gg[[length(gg) + 1]] <- dc |> filter(grepl("5[ABCDE]+", major_stat_area_name)) |>
#   tidy_sample_avail() |>
#   plot_sample_avail(palette = "Reds", year_range = c(1966,1990)) +
#   viridis::scale_fill_viridis(option = "D", end = 0.82, na.value = "transparent") +
#   ggtitle("Outside stock commercial samples (1966-1990)")
#
# gg[[length(gg) + 1]] <- dc |> filter(grepl("5[ABCDE]+", major_stat_area_name)) |>
#   tidy_sample_avail() |>
#   plot_sample_avail(palette = "Reds", year_range = c(1991, yrs[2])) +
#   viridis::scale_fill_viridis(option = "D", end = 0.82, na.value = "transparent") +
#   ggtitle("Outside stock commercial samples (1991-2023)")


# dc <- readRDS(here::here("data/raw/commercial-samples.rds"))
dc_sumry <- dc %>%
  filter(
    major_stat_area_name != "4B: STRAIT OF GEORGIA",
    !sampling_desc == "UNKNOWN",
    #gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL"),
    !is.na(length)) %>%
  mutate(month = lubridate::month(trip_start_date)) %>%
  group_by(gear_desc, sampling_desc, year) %>%
  summarise(n_fe = length(unique(fishing_event_id)), n_samp = sum(length > 0))

# dplyr::rename(`Proportion positive count` = p_pos, `Proportion positive weight` = pw_pos) |>

g1 <- dc_sumry |>
  # Translate gear_desc and sampling_desc for display
  mutate(gear_desc = case_when(
    gear_desc == "BOTTOM TRAWL" ~ tr("BOTTOM TRAWL", "CHALUT DE FOND"),
    gear_desc == "MIDWATER TRAWL" ~ tr("MIDWATER TRAWL", "CHALUT PÉLAGIQUE"),
    gear_desc == "LONGLINE" ~ tr("LONGLINE", "PALANGRE"),
    TRUE ~ gear_desc
  )) |>
  mutate(sampling_desc = case_when(
    sampling_desc == "KEEPERS" ~ tr("KEEPERS", "CONSERVÉS"),
    sampling_desc == "DISCARDS" ~ tr("DISCARDS", "REJETS"),
    sampling_desc == "UNSORTED" ~ tr("UNSORTED", "NON TRIÉS"),
    TRUE ~ sampling_desc
  )) |>
  ggplot(aes(year, sampling_desc, fill = n_fe)) +
  geom_tile(colour = "grey50") +
  facet_wrap(~gear_desc, ncol = 1) +
  geom_text(aes(label = n_fe), size = 2.5) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  gfplot::theme_pbs() +
  coord_cartesian(xlim= c(1973, 2023))+
  labs(fill = "", y = "", x = "") +
  theme(legend.position = "none") +
  ggtitle(tr("Fishing events sampled", "Événements de pêche échantillonnés"))

g2 <- dc_sumry |>
  # Translate gear_desc and sampling_desc for display
  mutate(gear_desc = case_when(
    gear_desc == "BOTTOM TRAWL" ~ tr("BOTTOM TRAWL", "CHALUT DE FOND"),
    gear_desc == "MIDWATER TRAWL" ~ tr("MIDWATER TRAWL", "CHALUT PÉLAGIQUE"),
    gear_desc == "LONGLINE" ~ tr("LONGLINE", "PALANGRE"),
    TRUE ~ gear_desc
  )) |>
  mutate(sampling_desc = case_when(
    sampling_desc == "KEEPERS" ~ tr("KEEPERS", "CONSERVÉS"),
    sampling_desc == "DISCARDS" ~ tr("DISCARDS", "REJETS"),
    sampling_desc == "UNSORTED" ~ tr("UNSORTED", "NON TRIÉS"),
    TRUE ~ sampling_desc
  )) |>
  ggplot(aes(year, sampling_desc, fill = n_samp)) +
  geom_tile(colour = "grey50") +
  facet_wrap(~gear_desc, ncol = 1) +
  geom_text(aes(label = n_samp), size = 2.5) +
  coord_cartesian(xlim= c(1973, 2023))+
  scale_fill_distiller(palette = "Purples", direction = 1) +
  gfplot::theme_pbs() +
  labs(fill = "", y = "", x = "") +
  theme(legend.position = "none") +
  ggtitle(tr("Dogfish sampled", "Aiguillats échantillonnés"))

cowplot::plot_grid(g1, g2, ncol = 1)
ggsave(fig_path("commercial-sample-counts.png"), width = 13, height = 12, dpi = 200)

# Survey length composition --------------------------------------------------

table(d$survey_abbrev)

lengths_syn <- tidy_lengths_raw(d,
  survey = c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")
)
g <- plot_lengths(lengths_syn, show_year = "all", french = FRENCH)
ggsave(fig_path("lengths-synoptic-outside.png"), width = 6, height = 8)

lengths_dog <- tidy_lengths_raw(d,
  survey = c("DOG")
)
g <- plot_lengths(lengths_dog, show_year = "all", french = FRENCH)
ggsave(fig_path("lengths-dogfish-survey-inside.png"), g, width = 4, height = 6)

lengths_hbll_ins <- tidy_lengths_raw(d,
  survey = c("HBLL INS N", "HBLL INS S")
)
g <- plot_lengths(lengths_hbll_ins, show_year = "all", french = FRENCH)
ggsave(fig_path("lengths-hbll-survey-inside.png"), g, width = 4, height = 7)

table(dc$length_type)
table(dc$sampling_desc)
table(dc$gear_desc)

d2024 <- readRDS("data/raw/syn-wcvi-dogfish-samples-2024.rds")
gfplot::tidy_lengths_raw(d2024, survey = "SYN WCVI") |>
  right_join(data.frame(survey_abbrev = "SYN WCVI", year = seq(2004, 2024))) |>
  gfplot::plot_lengths(french = FRENCH) + ggtitle("")
ggsave(fig_path("lengths-syn-2024.png"), width = 3, height = 8)


# Outside commercial length ----
ins <- grep("4B", dc$major_stat_area_name)

lengths_comm <- dc[-ins, ] |>
  filter(sampling_desc %in% "UNSORTED") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., sample_type = "commercial"),
    .id = "survey_abbrev"
  )
g <- plot_lengths(lengths_comm, show_year = "all", french = FRENCH) +
  ggtitle(tr("Length frequencies - Unsorted", "Fréquences de longueur - Non triés"))
ggsave(fig_path("lengths-commercial-outside.png"), g, width = 5, height = 9)

lengths_comm_discard <- dc[-ins, ] |>
  filter(sampling_desc %in% "DISCARDS") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., spp_cat_code = c(1, 4), sample_type = "commercial"),
                 .id = "survey_abbrev"
  ) |>
  # Translate gear types for display
  mutate(survey_abbrev = case_when(
    survey_abbrev == "BOTTOM TRAWL" ~ tr("BOTTOM TRAWL", "CHALUT DE FOND"),
    survey_abbrev == "MIDWATER TRAWL" ~ tr("MIDWATER TRAWL", "CHALUT PÉLAGIQUE"),
    survey_abbrev == "LONGLINE" ~ tr("LONGLINE", "PALANGRE"),
    TRUE ~ survey_abbrev
  ))
g <- plot_lengths(lengths_comm_discard, show_year = "all", french = FRENCH) +
  ggtitle(tr("Length frequencies - Discards", "Fréquences de longueur - Rejets"))
ggsave(fig_path("lengths-commercial-outside-discard.png"), g, width = 4, height = 5)

lengths_comm_ret <- dc[-ins, ] |>
  filter(sampling_desc %in% "KEEPERS") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., spp_cat_code = 3, sample_type = "commercial"),
                 .id = "survey_abbrev"
  ) |>
  # Translate gear types for display
  mutate(survey_abbrev = case_when(
    survey_abbrev == "BOTTOM TRAWL" ~ tr("BOTTOM TRAWL", "CHALUT DE FOND"),
    survey_abbrev == "MIDWATER TRAWL" ~ tr("MIDWATER TRAWL", "CHALUT PÉLAGIQUE"),
    survey_abbrev == "LONGLINE" ~ tr("LONGLINE", "PALANGRE"),
    TRUE ~ survey_abbrev
  ))
g <- plot_lengths(lengths_comm_ret, show_year = "all", french = FRENCH) +
  ggtitle(tr("Length frequencies - Retained", "Fréquences de longueur - Conservés"))
ggsave(fig_path("lengths-commercial-outside-retained.png"), g, width = 5, height = 6)

# Inside commercial length ----
lengths_comm <- dc[ins, ] |>
  filter(sampling_desc %in% "UNSORTED") |>
  filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
  split(.$gear_desc) |>
  purrr::map_dfr(~ tidy_lengths_raw(., sample_type = "commercial"),
    .id = "survey_abbrev"
  ) |>
  # Translate gear types for display
  mutate(survey_abbrev = case_when(
    survey_abbrev == "BOTTOM TRAWL" ~ tr("BOTTOM TRAWL", "CHALUT DE FOND"),
    survey_abbrev == "MIDWATER TRAWL" ~ tr("MIDWATER TRAWL", "CHALUT PÉLAGIQUE"),
    survey_abbrev == "LONGLINE" ~ tr("LONGLINE", "PALANGRE"),
    TRUE ~ survey_abbrev
  ))
g <- plot_lengths(lengths_comm, show_year = "all", french = FRENCH)
ggsave(fig_path("lengths-commercial-inside.png"), g, width = 5, height = 5)

# Length weight -------------------------------------------------------

# all surveys combined right now:
mm <- fit_length_weight(d, sex = "male", df = 3, usability_codes = c(0, 1, 2, 6))
mf <- fit_length_weight(d, sex = "female", df = 3, usability_codes = c(0, 1, 2, 6))
g <- plot_length_weight(object_female = mf, object_male = mm, french = FRENCH) +
  ggtitle(tr("Length-weight relationship (all BC samples)", "Relation longueur-poids (tous les échantillons de C.-B.)"))
mm$pars
mf$pars
ggsave(fig_path("length-weight-survey.png"), g, width = 5, height = 4)


# Outside (all surveys except IPHC):
mm <- d %>%
  filter(major_stat_area_name != "4B: STRAIT OF GEORGIA") %>%
  fit_length_weight(sex = "male", df = 3, usability_codes = c(0, 1, 2, 6))
mf <- d %>%
  filter(major_stat_area_name != "4B: STRAIT OF GEORGIA") %>%
  fit_length_weight(sex = "female", df = 3, usability_codes = c(0, 1, 2, 6))
g <- plot_length_weight(object_female = mf, object_male = mm, pt_alpha = 1, french = FRENCH) +
  ggtitle(tr("Length-weight relationship", "Relation longueur-poids")) +
  facet_wrap(vars(sex)) +
  guides(linetype = "none", colour = "none")

mm$pars
mf$pars
ggsave(fig_path("length-weight-survey-outside.png"), g, width = 6, height = 4)



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

d %>%
  filter(major_stat_area_name != "4B: STRAIT OF GEORGIA") %>%
  tidy_maturity_months() %>%
  plot_maturity_months()


# outside stock - female mature if maturity_code >= 77
# For males, set maturity at 30 (see Jackie King's email to Quang and Sean, March 29, 2023)
fit <- d |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6),
                custom_maturity_at = c(30, 77))
summary(fit$model)
g <- plot_mat_ogive(fit, french = FRENCH)
ggsave(fig_path("maturity-outside-survey.png"), g, width = 6, height = 3)

g <- fit$data %>%
  summarise(n = n(), p = mean(mature), .by = c(sex, age_or_length)) %>%
  ggplot(aes(age_or_length)) +
  geom_point(aes(y = p, shape = factor(sex))) +
  #geom_text(aes(y = p, label = n), alpha = 0.4, hjust = 0, nudge_y = 0.025) +
  scale_shape_manual(name = tr("Sex", "Sexe"), values = c(16, 1), labels = c(tr("M", "M"), tr("F", "F"))) +
  geom_line(data = fit$pred_data, aes(y = glmm_fe, linetype = factor(female)), show.legend = FALSE) +
  labs(x = tr("Length", "Longueur"), y = tr("Proportion mature", "Proportion mature"))
ggsave(fig_path("maturity-outside-survey-prop.png"), g, width = 6, height = 3)

# Re-fit if females mature at 55
fit_55 <- d |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6),
                custom_maturity_at = c(30, 55))

# pred_data <- fit_55$data %>%
#   select(age_or_length, female) %>%
#   mutate(glmm_fe = predict(fit_55, newdata = ., type = "response"))

prop_55 <- rbind(
  fit$data %>%
    group_by(age_or_length, sex) %>%
    summarise(n = n(), p = mean(mature)) %>%
    mutate(type = ifelse(sex == 2, tr("Female (77)", "Femelle (77)"), tr("Male (30)", "Mâle (30)"))),
  fit_55$data %>%
    filter(sex == 2) %>%
    group_by(age_or_length) %>%
    summarise(n = n(), p = mean(mature)) %>%
    mutate(type = tr("Female (55)", "Femelle (55)"))
)

pred_55 <- rbind(
  fit$pred_data %>%
    #filter(female == 1) %>%
    select(age_or_length, glmm_fe, female) %>%
    mutate(type = ifelse(female == 1, tr("Female (77)", "Femelle (77)"), tr("Male (30)", "Mâle (30)"))),
  fit_55$pred_data %>%
    filter(female == 1) %>%
    select(age_or_length, glmm_fe, female) %>%
    mutate(type = tr("Female (55)", "Femelle (55)"))
)

g <- prop_55 %>%
  ggplot(aes(age_or_length)) +
  geom_point(aes(y = p, shape = factor(type))) +
  scale_shape_manual(values = c(16, 1, 4)) +
  geom_line(data = pred_55, aes(y = glmm_fe, linetype = factor(type))) +
  labs(x = tr("Length", "Longueur"), y = tr("Proportion", "Proportion"), linetype = tr("Sex", "Sexe"), shape = tr("Sex", "Sexe"))
ggsave(fig_path("maturity-outside-survey-compare.png"), g, width = 6, height = 3)

# Annual proportion of mature females (code 55)
ann_55 <- d %>%
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI"),
         sex == 2,
         maturity_code != 0) %>%
  group_by(year) %>%
  summarise(n = n(),
            p_55 = mean(maturity_code >= 55))

# Annual proportion of pregnant females (code 77) among mature animals
ann_77 <- d %>%
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI"),
         sex == 2,
         maturity_code != 0) %>%
  group_by(year) %>%
  summarise(n = n(),
            p_55 = sum(maturity_code >= 77)/sum(maturity_code >= 55))

#plot(p_55 ~ year, ann_55, typ = 'o', ylim = c(0, 1))
#lines(p_55 ~ year, ann_77, typ = 'o', col = 2)


# inside stock
fit <- d |>
  filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S", "DOG")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6))
summary(fit$model)
plot_mat_ogive(fit, french = FRENCH)
ggsave(fig_path("maturity-inside-survey.png"), width = 6, height = 3)

# Available samples ---------------------------------------------------

yrs <- c(1966, 2022)
g1 <- d |> filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S", "DOG")) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Blues", year_range = yrs, french = FRENCH) +
  viridis::scale_fill_viridis(option = "C", end = 0.82, na.value = "transparent") +
  ggtitle(tr("Inside stock HBLL and Dogfish longline survey samples", "Échantillons de relevé HBLL et palangre à aiguillat du stock interne"))

g2 <- d |> filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Blues", year_range = yrs, french = FRENCH) +
  viridis::scale_fill_viridis(option = "C", end = 0.82, na.value = "transparent") +
  ggtitle(tr("Outside stock synoptic trawl survey samples", "Échantillons de relevé au chalut synoptique du stock externe"))

g3 <- dc |> filter(grepl("4B", major_stat_area_name)) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Reds", year_range = yrs, french = FRENCH) +
  viridis::scale_fill_viridis(option = "D", end = 0.82, na.value = "transparent") +
  ggtitle(tr("Inside stock commercial samples", "Échantillons commerciaux du stock interne"))

g4 <- dc |> filter(grepl("5[ABCDE]+", major_stat_area_name)) |>
  tidy_sample_avail() |>
  plot_sample_avail(palette = "Reds", year_range = yrs, french = FRENCH) +
  viridis::scale_fill_viridis(option = "D", end = 0.82, na.value = "transparent") +
  ggtitle(tr("Outside stock commercial samples", "Échantillons commerciaux du stock externe"))

cowplot::plot_grid(g2, g4, ncol = 1L)

ggsave(fig_path("biosample-available.png"), width = 11, height = 10, dpi = 150)

# Synoptic trawl length composition by sex and area
# Some evidence that sex ratio changes by area (see 5E)
# but survey catches primarily small females
g <- d %>%
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) %>%
  filter(sex %in% c(1, 2)) %>%
  mutate(area = substr(major_stat_area_name, 1, 2),
         sex = ifelse(sex == 2, tr("Female", "Femelle"), tr("Male", "Mâle"))) %>%
  ggplot(aes(length, linetype = sex)) +
  geom_freqpoly(binwidth = 4) +
  facet_wrap(vars(area), scales = "free_y") +
  ggtitle(tr("Synoptic trawl", "Chalut synoptique")) +
  labs(x = tr("Length (cm)", "Longueur (cm)"), y = tr("Frequency", "Fréquence"), linetype = tr("Sex", "Sexe")) +
  theme(legend.position = "bottom")
ggsave(fig_path("biosample-synoptic-trawl-area.png"), g, width = 6, height = 5, dpi = 150)

# Fishery length by sex and area
# Midwater trawl seems to behave similarly to synoptic trawl
g <- dc %>%
  filter(gear_desc == "MIDWATER TRAWL") %>%
  filter(sex %in% c(1, 2)) %>%
  mutate(area = substr(major_stat_area_name, 1, 2),
         sex = ifelse(sex == 2, tr("Female", "Femelle"), tr("Male", "Mâle"))) %>%
  ggplot(aes(length, linetype = sex)) +
  geom_freqpoly(binwidth = 4) +
  facet_wrap(vars(area), scales = "free_y") +
  ggtitle(tr("MIDWATER TRAWL", "CHALUT PÉLAGIQUE")) +
  labs(x = tr("Length (cm)", "Longueur (cm)"), y = tr("Frequency", "Fréquence"), linetype = tr("Sex", "Sexe")) +
  theme(legend.position = "bottom")
ggsave(fig_path("biosample-midwater-trawl-area.png"), g, width = 6, height = 3, dpi = 150)

# Bottom trawl catches the big females
g <- dc %>%
  filter(gear_desc == "BOTTOM TRAWL") %>%
  filter(sex %in% c(1, 2)) %>%
  mutate(area = substr(major_stat_area_name, 1, 2),
         sex = ifelse(sex == 2, tr("Female", "Femelle"), tr("Male", "Mâle"))) %>%
  ggplot(aes(length, linetype = sex)) +
  geom_freqpoly(binwidth = 4) +
  facet_wrap(vars(area), scales = "free_y") +
  ggtitle(tr("BOTTOM TRAWL", "CHALUT DE FOND")) +
  labs(x = tr("Length (cm)", "Longueur (cm)"), y = tr("Frequency", "Fréquence"), linetype = tr("Sex", "Sexe")) +
  theme(legend.position = "bottom")
ggsave(fig_path("biosample-bottom-trawl-area.png"), g, width = 6, height = 5, dpi = 150)

# Reset decimal separator
if (FRENCH) {
  options(OutDec = old_dec)
}

