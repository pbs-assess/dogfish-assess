library(ggplot2)
library(dplyr)

s <- readRDS(here::here("data/raw/survey-sets.rds")) %>%
  filter(!grepl("HBLL INS", survey_abbrev),
    !grepl("DOG", survey_abbrev))

sumry <- s %>%
  group_by(survey_abbrev, year) %>%
  summarise(
    n_sets = length(unique(fishing_event_id)),
    n_pos = sum(catch_count > 0),
    w_pos = sum(catch_weight > 0),
    p_pos = mean(catch_count > 0) %>% round(2),
    pw_pos = mean(catch_weight > 0) %>% round(2)
  )

g1 <- sumry |>
  dplyr::rename(`Proportion positive count` = p_pos, `Proportion positive weight` = pw_pos) |>
  tidyr::pivot_longer(cols = c(`Proportion positive count`, `Proportion positive weight`)) |>
  ggplot(aes(year, survey_abbrev, fill = value)) +
  geom_tile(colour = "grey50") +
  facet_wrap(~name, ncol = 1) +
  geom_text(aes(label = value), size = 2.5) +
  # scale_fill_gradientn(colours = c("white", "grey20")) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  gfplot::theme_pbs() +
  labs(fill = "Proportion", y = "", x = "") +
  theme(legend.position = "none")
# ggsave("figs/survey-prop-positive.png", width = 10, height = 4)

g2 <- sumry |>
  dplyr::rename(`Number of sets` = n_sets) |>
  tidyr::pivot_longer(cols = c(`Number of sets`)) |>
  ggplot(aes(year, survey_abbrev, fill = value)) +
  geom_tile(colour = "grey50") +
  facet_wrap(~name, ncol = 1) +
  geom_text(aes(label = value), size = 2.5) +
  # scale_fill_gradientn(colours = c("white", "grey20")) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  gfplot::theme_pbs() +
  labs(fill = "Number of sets", y = "", x = "") +
  theme(legend.position = "none")
# ggsave("figs/survey-sets.png", width = 10, height = 2)


library(patchwork)

g2 / g1 + plot_layout(heights = c(1, 2))
ggsave("figs/survey-sets-summary.png", width = 10, height = 6)

# bio samples:

d <- readRDS(here::here("data/raw/survey-samples.rds")) %>%
  mutate(species_common_name = "north pacific spiny dogfish") %>%
  filter(major_stat_area_name != "4B: STRAIT OF GEORGIA")

gfplot::tidy_sample_avail(d) |> gfplot::plot_sample_avail()

diphc <- read.csv(here::here("data/raw/IPHC_dogfish_lengths2021.csv")) %>%
  filter(reg_area == "2B", sex %in% c("F", "M")) %>%
  group_by(year) %>%
  summarise(
    survey_abbrev = "IPHC",
    length = n(),
    weight = 0,
    maturity = 0
  )

sumry_len <- d %>%
  #mutate(survey_type = survey[match(survey_abbrev, names(survey))]) %>%
  group_by(survey_abbrev, year) %>%
  summarise(
    specimens = sum(!is.na(specimen_id)),
    length = sum(!is.na(length)),
    weight = sum(!is.na(weight)),
    maturity = sum(!is.na(maturity_code))
    #aged = sum(!is.na(age)),
    #age_specimen_collected = sum(!is.na(age_specimen_collected))
  ) %>%
  rbind(diphc)

g2 <- sumry |>
  dplyr::rename(`Number of sets` = n_sets) |>
  tidyr::pivot_longer(cols = c(`Number of sets`)) |>
  ggplot(aes(year, survey_abbrev, fill = value)) +
  geom_tile(colour = "grey50") +
  facet_wrap(~name, ncol = 1) +
  geom_text(aes(label = value), size = 2.5) +
  # scale_fill_gradientn(colours = c("white", "grey20")) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  gfplot::theme_pbs() +
  labs(fill = "Number of sets", y = "", x = "") +
  theme(legend.position = "none")
# ggsave("figs/survey-sets.png", width = 10, height = 2)

csasdown::csas_table(
  sumry_len,
  caption = "Biological samples by survey and year. HS MSA is the Hecate Strait Multispecies Assemblage Survey. Trawl surveys include the synoptic surveys in Hecate Strait (SYN HS), Queen Charlotte Sound (SYN QCS), and West Coast Vancouver Island (SYN WCVI).",
  col_names = c("Survey", "Year", "Number of specimens", "Lengths", "Weights", "Maturities"),
  align = c("l", "r", "r", "r", "r", "r")
)

d <- readRDS(here::here("data/raw/survey-samples.rds")) %>%
  mutate(species_common_name = "north pacific spiny dogfish") %>%
  filter(major_stat_area_name != "4B: STRAIT OF GEORGIA")

yrs <- c(1984, 2023)

s <- c("SYN WCHG", "HS MSA", "SYN HS", "SYN QCS", "SYN WCVI")
gg <- list()
for (i in seq_along(s)) {
  gg[[i]] <- d |> filter(survey_abbrev %in% s[i]) |>
    tidy_sample_avail() |>
    plot_sample_avail(year_range = yrs) +
    viridis::scale_fill_viridis(option = "C", end = 0.82, na.value = "transparent") +
    ggtitle(s[i])
}

########

diphc <- read.csv(here::here("data/raw/IPHC_dogfish_lengths2021.csv")) %>%
  filter(reg_area == "2B", sex %in% c("F", "M")) %>%
  group_by(year) %>%
  summarise(
    length = n(),
    weight = 0,
    maturity = 0,
    ageing_structure = 0,
    age = 0
  )
diphc2 <- tidyr::pivot_longer(diphc, cols = length:age) |>
  rename(n = value, type = name)

gg[[length(gg) + 1]] <- diphc2 |>
  plot_sample_avail(year_range = yrs) +
  viridis::scale_fill_viridis(option = "C", end = 0.82, na.value = "transparent") +
  ggtitle("IPHC")


cowplot::plot_grid(plotlist = gg, ncol = 1)

ggsave("figs/bio-sample-avail.png", width = 7.75, height = 10, dpi = 220)
