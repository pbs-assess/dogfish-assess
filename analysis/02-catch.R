library(dplyr)
library(gfplot)
library(ggplot2)
source("analysis/utils.R")
dir.create("figs", showWarnings = FALSE)

# modern catches ------------------------------------------------------

if (Sys.info()[["user"]] == "seananderson") {
  d <- readRDS("data/raw/catch.rds")
  table(d$major_stat_area_name)
  # d_4b5abcde3cd <- tidy_catch_dogfish(d, areas = c("5[CDE]+", "5[AB]+", "4B"))
  d_4b5abcde3cd <- tidy_catch_dogfish(d, areas = c("5[ABCDE3CD]+", "4B"))
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

ggplot(old_dat, aes(x = year, y = landed_kg + discarded_kg, colour = gear, fill = gear)) +
  facet_wrap(~area, scales = "fixed") +
  geom_col()

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
    ylim(0, 2.5e04)
}
ggsave("figs/reconstructed-catch.png", width = 8, height = 8)

land <- catch_plot(d, landed_kg/1000) + ggtitle("Landings")
discard <- catch_plot(d, discarded_kg/1000) + ggtitle("Discards")
catch <- catch_plot(d, landed_kg/1000 + discarded_kg/1000) + ggtitle("Catch")
cowplot::plot_grid(plotlist = list(land, discard, catch), ncol = 1L)

saveRDS(catch, file = "data/generated/catch.rds")
