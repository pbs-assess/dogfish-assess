library(dplyr)
library(gfplot)
library(ggplot2)
source("analysis/utils.R")
dir.create("figs", showWarnings = FALSE)

# modern catches ------------------------------------------------------

if (Sys.info()[["user"]] == "seananderson") {
  d <- readRDS("data/raw/catch.rds")
  # d_4b5abcde3cd <- tidy_catch_dogfish(d, areas = c("5[CDE]+", "5[AB]+", "4B"))
  d_4b5abcde3cd <- tidy_catch_dogfish(d, areas = c("5[ABCDE]+", "3[CD]+", "4B"))
  d_4b5abcde3cd$area[d_4b5abcde3cd$area == "3CD"] <- "5ABCDE3CD"
  d_4b5abcde3cd$area[d_4b5abcde3cd$area == "5ABCDE"] <- "5ABCDE3CD"
  d_4b5abcde3cd <- group_by(d_4b5abcde3cd, year, species_common_name, area, gear) |>
    summarise(
      landed_kg = sum(landed_kg),
      discarded_kg = sum(discarded_kg)
    )
  saveRDS(d_4b5abcde3cd, file = "data/generated/catch-4B5ABCDE3CD-summarized.rds")
}

# outside: 5AB + 5CDE
# inside: 4B

# cached and force-pushed to GitHub:
d_4b5abcde3cd <- readRDS("data/generated/catch-4B5ABCDE3CD-summarized.rds")
# plot_catch(d_4b5abcde3cd)

# Trust trawl >= 1996
# Trust longline >= 2007

d_4b5abcde3cd_trawl <- d_4b5abcde3cd |>
  filter(year >= 1996 & gear %in% c("Bottom trawl", "Midwater trawl"))

d_4b5abcde3cd_ll <- d_4b5abcde3cd |>
  filter(year >= 2007 & gear %in% c("Hook and line", "Trap", "Unknown/trawl", "Discarded"))

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
  filter(area != "4B") %>%
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
ggsave("figs/reconstructed-catch-discards-outside.png", g, width = 6, height = 6)

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
