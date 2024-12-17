library(dplyr)
library(gfplot)
library(ggplot2)
library(readxl)
source("analysis/utils.R")
source("ss3/99-utils.R")
library(here)
dir.create("figs", showWarnings = FALSE)

# modern catches ------------------------------------------------------

if (Sys.info()[["user"]] == "seananderson") {
  d <- readRDS("data/raw/catch-2024-12-09.rds")
  # d_4b5abcde3cd <- tidy_catch_dogfish(d, areas = c("5[CDE]+", "5[AB]+", "4B"))

  # remove stat area 12 such that we have PFMA/GMU 3CD5ABCD rather than PSMFC
  # (Science older GMU) 3CD5ABCD
  d <- filter(d, dfo_stat_area_code != 12)
  d_4b5abcde3cd <- tidy_catch_dogfish(d, areas = c("5[ABCDE]+", "3[CD]+", "4B"))
  d_4b5abcde3cd$area[d_4b5abcde3cd$area == "3CD"] <- "5ABCDE3CD"
  d_4b5abcde3cd$area[d_4b5abcde3cd$area == "5ABCDE"] <- "5ABCDE3CD"
  d_4b5abcde3cd <- group_by(d_4b5abcde3cd, year, species_common_name, area, gear) |>
    summarise(
      landed_kg = sum(landed_kg),
      discarded_kg = sum(discarded_kg),
      discarded_pcs = sum(discarded_pcs)
    )
  saveRDS(d_4b5abcde3cd, file = "data/generated/catch-4B5ABCDE3CD-summarized.rds")
}

# outside: 5AB + 5CDE
# inside: 4B

# cached and force-pushed to GitHub:
d_4b5abcde3cd <- readRDS("data/generated/catch-4B5ABCDE3CD-summarized.rds")
# plot_catch(d_4b5abcde3cd)


filter(d_4b5abcde3cd, year >= 2010, year <= 2023) |>
  filter(area != "4B") |>
  mutate(
    discarded_kg =
      ifelse(
        gear != "Hook and line",
        discarded_kg,
        3.07 * discarded_pcs # assuming 3.07 kg avg. per discarded dogfish based on unsorted samples
      )
  ) |>
  group_by(year, gear) |>
  summarise(landed_t = sum(landed_kg/1000), discards_t = sum(discarded_kg/1000)) |>
  ungroup() |>
  group_by(year) |>
  arrange(gear) |>
  filter(!gear %in% c("Trap", "Unknown/trawl")) |>
  mutate(landed_t = round(landed_t, 1), discards_t = round(discards_t, 1)) |>
  readr::write_csv("~/Downloads/catch-oct18.csv")
  # knitr::kable(digits = 0)

dd <- filter(d_4b5abcde3cd, gear == "Hook and line", area != "4B") |>
  ungroup() |>
  select(year, discarded_pcs) |>
  mutate(discarded_pcs = discarded_pcs, weight_t = round(4.08233 * discarded_pcs / 1000, 3)) |>
  filter(year > 2004, year < 2024)

g1 <- ggplot(dd, aes(year, discarded_pcs/1000)) + geom_line() + scale_x_continuous() + theme_bw() + geom_point() + scale_x_continuous(breaks = 2005:2023)
g2 <- ggplot(dd, aes(year, weight_t)) + geom_line() + scale_x_continuous() + theme_bw() + geom_point() + scale_x_continuous(breaks = 2005:2023)
cowplot::plot_grid(g1, g2, nrow = 2)

readr::write_csv(dd, "~/Downloads/discards.csv")

ggplot(dd, aes(year, discarded_pcs/1000)) + geom_line()

d_4b5abcde3cd <- d_4b5abcde3cd |>
  mutate(
    discarded_kg =
      ifelse(
        !gear %in% c("Hook and line", "Trap"),
        discarded_kg,
        3.07 * discarded_pcs # assuming 3.07 kg avg. per discarded dogfish based on unsorted samples in longline - visualization and projections only; use numbers in model
      )) |>
  mutate(
    discarded_pcs =
      ifelse(
        gear %in% c("Hook and line", "Trap"),
        discarded_pcs,
        0 # zero out; not using
      ))

# Trust trawl >= 1996
# Trust longline >= 2007
# Take rest from last assessment
d_4b5abcde3cd_trawl <- d_4b5abcde3cd |>
  filter(year >= 1996 & gear %in% c("Bottom trawl", "Midwater trawl", "Unknown/trawl"))

# add on the outside midwater trawl from Maria for before 2006
gdu <- read_xlsx(here("data/raw/FD5046 dogfish trawl catch 1996-2024.xlsx"), sheet = 1L)
names(gdu) <- tolower(names(gdu))
gdu_sum <- group_by(gdu, gear_subtype, calendar_year) |>
  summarise(landed_kg = sum(landed_round_kg), discarded_kg = sum(total_released_round_kg)) |>
  filter(gear_subtype == "MIDWATER TRAWL", calendar_year < 2006) |>
  select(discarded_kg, year = calendar_year)

# check:
d_4b5abcde3cd_trawl[d_4b5abcde3cd_trawl$gear %in% "Midwater trawl" & d_4b5abcde3cd_trawl$year %in% 1996:2005 & d_4b5abcde3cd_trawl$area != "4B",]

d_4b5abcde3cd_trawl$discarded_kg[d_4b5abcde3cd_trawl$gear %in% "Midwater trawl" & d_4b5abcde3cd_trawl$year %in% 1996:2005 & d_4b5abcde3cd_trawl$area != "4B"] <- gdu_sum$discarded_kg

# check:
d_4b5abcde3cd_trawl[d_4b5abcde3cd_trawl$gear %in% "Midwater trawl" & d_4b5abcde3cd_trawl$year %in% 1996:2005 & d_4b5abcde3cd_trawl$area != "4B",]

# take 2006 and beforee from last assessment:
d_4b5abcde3cd_ll <- d_4b5abcde3cd |>
  filter(year >= 2007 & gear %in% c("Hook and line", "Trap"))

d_catch_modern <- bind_rows(d_4b5abcde3cd_trawl, d_4b5abcde3cd_ll)

# plot_catch(d_4b5abcde3cd_ll)
# plot_catch(d_4b5abcde3cd_trawl)

# historical catches --------------------------------------------------

landings_1935 <- readr::read_csv("data/raw/catches-all-gears-1935-1965.csv", comment = "#")
landings_1966_ll <- readr::read_csv("data/raw/catches-longline-1966-2008.csv", comment = "#")
landings_1966_trawl <- readr::read_csv("data/raw/catches-trawl-1966-2008.csv", comment = "#")
discards_1966_trawl <- readr::read_csv("data/raw/discards-trawl-1966-2008.csv", comment = "#")
discards_2001_ll <- readr::read_csv("data/raw/discards-longline-2001-2006.csv", comment = "#")

make_NAs_zero <- function(x) {
  for (i in seq_along(names(x))) {
    x[[i]][is.na(x[[i]])] <- 0
  }
  x
}
landings_1935 <- make_NAs_zero(landings_1935)
landings_1966_ll <- make_NAs_zero(landings_1966_ll)
landings_1966_trawl <- make_NAs_zero(landings_1966_trawl)
discards_1966_trawl <- make_NAs_zero(discards_1966_trawl)
discards_2001_ll <- make_NAs_zero(discards_2001_ll)

# Percent not assigned !?
d_1935 <- landings_1935 |>
  transmute(year = Year, `4B` = `4B`, `5ABCDE3CD` = Total)
d_1935 <- tidyr::pivot_longer(
  d_1935, -1, names_to = "area", values_to = "landed_kg") |>
  mutate(gear = "Trawl + hook and line", landed_kg = landed_kg * 1000)

# assume unknown area is outside: !? TODO
d_1966_ll <- landings_1966_ll |>
  transmute(year = Year, `4B` = `4B`, `5ABCDE3CD` = `Total outside` + `Unknown area`)
d_1966_ll <- tidyr::pivot_longer(
  d_1966_ll, -1, names_to = "area", values_to = "landed_kg") |>
  mutate(gear = "Hook and line", landed_kg = landed_kg * 1000) |>
  filter(year < 2007)

# assume unknown area is outside: !? TODO
d_1966_tr <- landings_1966_trawl |>
  transmute(year = Year, `4B` = `4B`, `5ABCDE3CD` = `Total` + `Unknown area`)
d_1966_tr <- tidyr::pivot_longer(
  d_1966_tr, -1, names_to = "area", values_to = "landed_kg") |>
  mutate(gear = "Trawl", landed_kg = landed_kg * 1000) |>
  filter(year < 1996)

disc_1966_tr <- discards_1966_trawl |>
  transmute(year = Year, `4B` = `4B`, `5ABCDE3CD` = `Total`)
disc_1966_tr <- tidyr::pivot_longer(
  disc_1966_tr, -1, names_to = "area", values_to = "discarded_kg") |>
  mutate(gear = "Trawl", discarded_kg = discarded_kg * 1000) |>
  filter(year < 1996)

# assume unknown area is outside: !? TODO
disc_2001_ll <- discards_2001_ll |>
  transmute(year = Year, `4B` = `4B`, `5ABCDE3CD` = `Total` + `Unknown area`)
disc_2001_ll <- tidyr::pivot_longer(
  disc_2001_ll, -1, names_to = "area", values_to = "discarded_kg") |>
  mutate(gear = "Hook and line", discarded_kg = discarded_kg * 1000)

disc <- bind_rows(disc_1966_tr, disc_2001_ll)

ggplot(disc, aes(x = year, y = discarded_kg, colour = gear, fill = gear)) +
  facet_wrap(~area, scales = "fixed") +
  geom_col()

old_landings <- bind_rows(list(
  d_1935,
  d_1966_ll,
  d_1966_tr
))

ggplot(old_landings, aes(x = year, y = landed_kg, colour = gear, fill = gear)) +
  facet_wrap(~area, scales = "fixed") +
  geom_col()

old_dat <- full_join(old_landings, disc)
old_dat <- make_NAs_zero(old_dat)

d <- bind_rows(d_catch_modern, old_dat)

catch_plot <- function(x, column) {
  gears <- sort(unique(x$gear))
  cols <- RColorBrewer::brewer.pal(n = length(gears), name = "Dark2")
  names(cols) <- gears
  ggplot(d, aes(x = year, y = {{ column }},  fill = gear)) +
    facet_wrap(~area, scales = "fixed") +
    geom_col(colour = "grey50", linewidth = 0.5) +
    coord_cartesian(expand = FALSE) +
    theme_pbs() +
    scale_fill_manual(values = cols) +
    expand_limits(y = 0)
    #ylim(0, 2.5e04)
}

gears <- sort(unique(d$gear))
cols <- RColorBrewer::brewer.pal(n = length(gears), name = "Dark2")
names(cols) <- gears

#g <- ggplot(old_dat, aes(x = year, y = 1e-6 * (landed_kg + discarded_kg), colour = gear, fill = gear)) +
#  facet_wrap(~area, scales = "fixed") +
#  geom_col() +
#  labs(x = "Year", y = "Catch (kt)")
#ggsave("figs/reconstructed-catch.png", g, width = 8, height = 8)

## Both Inside and Outside ----
land <- catch_plot(d, landed_kg/1e6) + labs(x = "Year", y = "Landings (kt)")
discard <- catch_plot(d, discarded_kg/1000) + labs(x = "Year", y = "Discards (kt)")
catch <- catch_plot(d, landed_kg/1000 + discarded_kg/1000) + labs(x = "Year", y = "Catch (t)")

g <- cowplot::plot_grid(plotlist = list(land, discard, catch), ncol = 1L)
ggsave("figs/reconstructed-catch-discards.png", g, width = 8, height = 6)

# Proportion discards
p_discard <- catch_plot(d, landed_kg/1000 + discarded_kg/1000) + labs(x = "Year", y = "Catch (t)")

g <- d %>% group_by(year, area) %>%
  summarise(p_discard = sum(discarded_kg)/sum(landed_kg + discarded_kg)) %>%
  ggplot(aes(x = year, y = p_discard)) +
  facet_wrap(~area, scales = "fixed") +
  geom_line() +
  theme_pbs() +
  labs(x = "Year", y = "Proportion discards")
ggsave("figs/proportion-discards.png", g, width = 5, height = 2.5)

g <- catch + facet_wrap(~area, ncol = 1) +
  ylab("Reconstructed catch (t)") +
  labs(fill = "Gear") + xlab("") +
  ggtitle("")
ggsave("figs/reconstructed-catch.png", g, width = 6.4, height = 5.5)

saveRDS(d, file = "data/generated/catch.rds")

## Outside only ----
g <- d %>%
  filter(year <= 2023) |>
  filter(area != "4B") %>%
  reshape2::melt(id.vars = c("year", "gear", "species_common_name", "area")) %>%
  mutate(variable = ifelse(variable == "landed_kg", "Landings (kt)", "Discards (kt)"),
         value = value/1e6) %>%
  mutate(variable = factor(variable, levels = c("Landings (kt)", "Discards (kt)"))) |>
  ggplot(aes(year, value, fill = gear)) +
  geom_col(colour = "grey50", linewidth = 0.5, width = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  facet_grid(vars(variable), vars(area), scales = "fixed", switch = "y") +
  theme_pbs() +
  scale_fill_manual(values = cols) +
  # expand_limits(y = 0) +
  labs(y = NULL, x = "Year", fill = "Gear") +
  theme(legend.position = "bottom",
        strip.placement = "outside") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
g
ggsave("figs/reconstructed-catch-discards-outside.png", g, width = 5, height = 5)

g1 <- d %>%
  filter(year <= 2023) |>
  filter(year >= 1980) |>
  filter(area != "4B") %>%
  ungroup() |>
  select(year, area, gear, landed_kg, discarded_kg) |>
  reshape2::melt(id.vars = c("year", "gear", "area")) %>%
  mutate(variable = ifelse(variable == "landed_kg", "Landings (kt)", "Discards (kt)"),
    value = value/1e6) %>%
  mutate(variable = factor(variable, levels = c("Landings (kt)", "Discards (kt)"))) |>
  mutate(area = paste0(area, " (zoomed to 1980-2023)")) |>
  ggplot(aes(year, value, fill = gear)) +
  geom_col(colour = "grey50", linewidth = 0.5, width = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  facet_grid(vars(variable), vars(area), scales = "fixed", switch = "y") +
  theme_pbs() +
  scale_fill_manual(values = cols) +
  coord_cartesian(xlim = c(1980, 2023)) +
  # expand_limits(y = 0) +
  labs(y = NULL, x = "Year", fill = "Gear") +
  theme(legend.position = "bottom",
    strip.placement = "outside") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
g1
ggsave_optipng("figs/reconstructed-catch-discards-outside-zoom.png", width = 5, height = 5)

gg <- patchwork::wrap_plots(list(g + theme(axis.title.x = element_blank()), g1 + guides(fill = "none") + theme(axis.title.x = element_blank())), widths = c(1.7, 1))
ggsave_optipng("figs/reconstructed-catch-discards-outside-sar.png", width = 8, height = 5)

d %>%
  filter(year <= 2023) |>
  filter(year >= 1978) |>
  filter(area != "4B") %>%
  reshape2::melt(id.vars = c("year", "gear", "species_common_name", "area")) %>%
  mutate(variable = ifelse(variable == "landed_kg", "Landings (kt)", "Discards (kt)"),
    value = value/1e6) %>%
  group_by(year, gear, area) |> summarise(value = sum(value)) |>
  mutate(area = paste0(area, " (zoomed in to 1980-2023)")) |>
  ggplot(aes(year, value, fill = gear)) +
  geom_col(colour = "grey50", linewidth = 0.5, width = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  # facet_grid(vars(variable), vars(area), scales = "fixed", switch = "y") +
  theme_pbs() +
  scale_fill_manual(values = cols) +
  coord_cartesian(xlim = c(1978, 2023), ylim = c(0, 6)) +
  # expand_limits(y = 0) +
  labs(y = NULL, x = "Year", fill = "Gear") +
  theme(legend.position = "bottom",
    strip.placement = "outside") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  # annotate(
  #   "rect", xmin = 1978, xmax = 2023,
  #   ymin = 4.5, ymax = 9.333, alpha = 0.15, fill = "black"
  # ) +
  ylab("Catch (kt) (discards + landings)")
source("ss3/99-utils.R")
ggsave_optipng("figs/reconstructed-catch-discards-outside-zoom-high-risk-band.png", width = 5, height = 4)

# table to share:
out <- d |>
  ungroup() |>
  filter(year <= 2023) |>
  filter(area != "4B") |>
  mutate(gear = ifelse(gear == "Unknown/trawl", "Bottom trawl", gear)) |>
  select(-species_common_name, -area) |>
  group_by(year, gear) |>
  summarise(landed_kg = sum(landed_kg), discarded_kg = sum(discarded_kg), discarded_pcs = sum(discarded_pcs)) |>
  ungroup() |>
  mutate(discarded_pcs = ifelse(gear %in% c("Hook and line", "Trap"), discarded_pcs, NA)) |>
  mutate(discarded_kg = ifelse(!gear %in% c("Hook and line", "Trap"), discarded_kg, NA)) |>
  arrange(gear, year) |>
  mutate(discarded_t_avg_dogfish = ifelse(gear %in% c("Hook and line", "Trap"), discarded_pcs * 3.07/1000, NA))

out |> readr::write_rds("data/generated/catch-dec2024.rds")

out |>
  filter(year >= 1996) |>
  mutate(landed_t = landed_kg / 1000, discarded_t = discarded_kg / 1000) |>
  select(-landed_kg, -discarded_kg) |>
  mutate(gear = ifelse(gear == "Hook and line", "Hook and line/longline", gear)) |>
  mutate(landed_t = round(landed_t, 2), discarded_t = round(discarded_t, 2), discarded_t_avg_dogfish = round(discarded_t_avg_dogfish, 2)) |>
  mutate(gear = factor(gear, levels = c("Bottom trawl", "Midwater trawl", "Hook and line/longline", "Trap"))) |>
  arrange(gear, year) |>
  select(year, gear, landed_t, discarded_t, discarded_count = discarded_pcs, discarded_t_avg_dogfish) |>
  readr::write_excel_csv("figs/dogfish-catch-weight.csv", na = "")

g <- d %>%
  filter(area != "4B") %>%
  group_by(year, area) %>%
  summarise(p_discard = sum(discarded_kg)/sum(landed_kg + discarded_kg)) %>%
  ggplot(aes(x = year, y = p_discard)) +
  facet_wrap(~area, scales = "fixed") +
  geom_line() +
  geom_point(shape = 1) +
  #geom_line(colour = "grey50", linewidth = 0.5) +
  #coord_cartesian(expand = FALSE) +
  theme_pbs() +
  labs(x = "Year", y = "Proportion discards") +
  geom_vline(xintercept = 1996, linetype = 2)
ggsave("figs/proportion-discards-outside.png", g, width = 4, height = 2.5)

g <- d %>%
  filter(area != "4B") %>%
  filter(gear != "Trap") %>%
  group_by(year, area, gear) %>%
  summarise(p_discard = sum(discarded_kg)/sum(landed_kg + discarded_kg)) %>%
  ggplot(aes(x = year, y = p_discard)) +
  facet_wrap(vars(gear), scales = "fixed") +
  geom_line() +
  #geom_point(shape = 1) +
  #geom_line(colour = "grey50", linewidth = 0.5) +
  #coord_cartesian(expand = FALSE) +
  theme_pbs() +
  labs(x = "Year", y = "Proportion discards") +
  geom_vline(xintercept = 1996, linetype = 2)
ggsave("figs/proportion-discards-outside-gear.png", g, width = 6, height = 4)


## Inside only ----
g <- d %>%
  filter(area == "4B") %>%
  reshape2::melt(id.vars = c("year", "gear", "species_common_name", "area")) %>%
  mutate(variable = ifelse(variable == "landed_kg", "Landings (kt)", "Discards (kt)"),
         value = value/1e6) %>%
  ggplot(aes(year, value, fill = gear)) +
  geom_col(colour = "grey50", linewidth = 0.5, width = 1) +
  #coord_cartesian(expand = FALSE) +
  facet_grid(vars(variable), vars(area), scales = "free_y", switch = "y") +
  theme_pbs() +
  scale_fill_manual(values = cols) +
  expand_limits(y = 0) +
  labs(y = NULL, x = "Year", fill = "Gear") +
  theme(legend.position = "bottom",
        strip.placement = "outside")
ggsave("figs/reconstructed-catch-discards-inside.png", g, width = 6, height = 6)

g <- d %>%
  filter(area == "4B") %>%
  group_by(year, area) %>%
  summarise(p_discard = sum(discarded_kg)/sum(landed_kg + discarded_kg)) %>%
  ggplot(aes(x = year, y = p_discard)) +
  facet_wrap(~area, scales = "fixed") +
  geom_line() +
  geom_point(shape = 1) +
  #geom_line(colour = "grey50", linewidth = 0.5) +
  #coord_cartesian(expand = FALSE) +
  theme_pbs() +
  labs(x = "Year", y = "Proportion discards") +
  geom_vline(xintercept = 1996, linetype = 2)
ggsave("figs/proportion-discards-inside.png", g, width = 4, height = 2.5)
