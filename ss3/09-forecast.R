library(r4ss)
library(dplyr)
library(ggplot2)
library(future)

fit_ss3 <- function(
    model_dir = "model1",
    hessian = TRUE,
    ss_home = here::here("ss3"),
    max_phase,
    extra_args = "") {
  dir_cur <- getwd()
  dir_run <- file.path(ss_home, model_dir)
  setwd(dir_run)
  on.exit(setwd(dir_cur))

  if (.Platform$OS.type == "unix") {
    cmd <- "ss modelname ss"
  } else {
    cmd <- "ss.exe modelname ss"
  }

  if (!hessian) {
    cmd <- paste(cmd, "-nohess")
  }
  if (!missing(max_phase) && is.integer(max_phase)) {
    cmd <- paste(cmd, "-maxph", max_phase)
  }
  cmd <- paste(cmd, extra_args)
  message("File directory: ", dir_run)
  message("Command: ", cmd)

  system(cmd)
}

make_f_catch <- function(total, years = 20) {
  d <- SS_readdat("ss3/A0/data.ss", verbose = FALSE)
  temp <- filter(d$catch, year >= 2018) |> # last 5
    filter(fleet %in% c(1, 2, 3, 4, 5, 8)) |>
    # filter(fleet %in% c(2, 3)) |> # HACK!
    group_by(fleet) |>
    summarise(m = mean(catch)) |>
    mutate(fraction = m / sum(m)) |>
    # mutate(fraction = 0.5) |> # TODO HACK!!!!
    mutate(catch_or_F = total * fraction)
  temp <- purrr::map_dfr(2024:(2024 + years - 1), \(y) data.frame(temp, year = y))

  missing_fleets <- unique(c(d$catch$fleet))[!unique(d$catch$fleet) %in% unique(temp$fleet)]
  df <- expand.grid(fleet = missing_fleets, catch_or_F = 0, year = unique(temp$year))
  temp <- bind_rows(df, temp)

  transmute(temp, year = year, seas = 1, fleet = fleet, catch_or_F = catch_or_F) |>
    arrange(fleet, year)
}

# so 2/3 bottom trawl discards
# midwater trawl

run_projections <- function(model = "A0", catches = seq(0, 1200, by = 300), hessian = FALSE) {
  cat(model, "\n")
  # plan(multisession)
  # out <- furrr::future_map_dfr(catches, \(x) {
  out <- purrr::map_dfr(catches, \(x) {
    cat(x, "\n")
    fo <- paste0(model, "-forecast-", x)
    system(paste0("cp -r ss3/", model, "/ ss3/", fo))
    f <- SS_readforecast(paste0("ss3/", fo, "/forecast.ss"))
    f$ForeCatch <- make_f_catch(x)
    f$Nforecastyrs <- max(f$ForeCatch$year) - min(f$ForeCatch$year) + 1
    SS_writeforecast(f, paste0("ss3/", fo), overwrite = TRUE)
    fit_ss3(fo, hessian = hessian)
    d <- SS_output(paste0("ss3/", fo))
    derived_quants <- d$derived_quants
    row.names(derived_quants) <- NULL

    dat_F <- derived_quants[substring(derived_quants[["Label"]], 1, 2) == "F_", ] |>
      mutate(year = gsub("F_", "", Label)) |>
      mutate(year = as.numeric(year)) |>
      mutate(Label = gsub("_[0-9]+", "", Label))

    ref_F <- derived_quants |>
      filter(Label %in% c("annF_Btgt"))

    dat_B <- derived_quants[substring(derived_quants[["Label"]], 1, 6) == "Bratio", ] |>
      mutate(year = gsub("Bratio_", "", Label)) |>
      mutate(year = as.numeric(year)) |>
      mutate(Label = gsub("_[0-9]+", "", Label))

    ret <- bind_rows(dat_F, ref_F, dat_B) |>
      select(-`(Val-1.0)/Stddev`, -`CumNorm`) |>
      select(year, label = Label, est = Value, se = StdDev)
    ret$catch <- x
    ret$model <- model
    as_tibble(ret)
  })
  # plan(sequential)
  out
}

source("ss3/99-model-names.R")
reject <- c("B1_1990inc", "B3_2005step", "B4_1990inc_lowM", "A1", "A5_highdiscard", "A8_HBLLonly")
keep <- which(!mods %in% reject)
mods <- mods[keep]
model_name <- model_name[keep]

# out2 <- purrr::map(mods, run_projections, hessian = TRUE)

length(mods)
plan(multisession, workers = 7)
out2 <- furrr::future_map(mods, run_projections, hessian = TRUE)
plan(sequential)

saveRDS(out2, "data/generated/projections.rds")
out2 <- readRDS("data/generated/projections.rds")

x <- bind_rows(out2)

lu <- data.frame(model = mods, model_name = model_name)

temp <- x |>
  filter(year > 1900, label %in% "Bratio", !is.na(year), se < 2, est < 0.999) |>
  # filter(!grepl("B", model)) |>
  left_join(lu) |>
  mutate(model_name = forcats::fct_inorder(model_name)) |>
  mutate(catch = factor(catch)) |>
  mutate(catch = forcats::fct_rev(catch))

temp |>
  ggplot(aes(year, est,
    ymin = est - 2 * se, ymax = est + 2 * se,
    colour = catch, group = paste(model_name, catch), fill = catch
  )) +
  geom_ribbon(fill = "grey70", alpha = 0.7, colour = NA) +
  geom_line() +
  geom_line(data = filter(temp, catch == 0)) + # dark on top
  scale_x_continuous(breaks = seq(1960, 2090, 20)) +
  scale_colour_viridis_d(direction = -1, option = "D") +
  scale_fill_viridis_d(direction = -1, option = "D") +
  annotate(
    "rect",
    xmin = 2024, xmax = max(x$year, na.rm = TRUE),
    ymin = 0, ymax = 1e6,
    alpha = 0.1, fill = "grey55"
  ) +
  coord_cartesian(expand = FALSE, ylim = c(0, 0.42)) +
  geom_hline(yintercept = 0.4, lty = 3, colour = "grey40") +
  geom_hline(yintercept = 0.2, lty = 2, colour = "grey40") +
  facet_wrap(~model_name) +
  ylab(expression(S / S[0])) +
  xlab("") +
  labs(colour = "Catch")
ggsave("figs/ss3/refpts/proj-facet-model.png", width = 8, height = 6.5)

temp |>
  filter(!grepl("B", model)) |>
  mutate(catch = forcats::fct_rev(catch)) |>
  ggplot(aes(year, est,
    ymin = est - 2 * se, ymax = est + 2 * se,
    # colour = catch, group = paste(model_name, catch), fill = catch)) +
    colour = model_name, group = paste(model_name, catch), fill = catch
  )) +
  geom_ribbon(fill = "grey70", alpha = 0.7, colour = NA) +
  geom_line() +
  scale_x_continuous(breaks = seq(1960, 2090, 20)) +
  scale_colour_brewer(palette = "Paired") +
  # scale_colour_viridis_d(direction = -1, option = "C") +
  scale_colour_brewer(palette = "Paired") +
  annotate(
    "rect",
    xmin = 2024, xmax = max(x$year, na.rm = TRUE),
    ymin = 0, ymax = 1e6,
    alpha = 0.1, fill = "grey55"
  ) +
  coord_cartesian(expand = FALSE, ylim = c(0, 0.42)) +
  geom_hline(yintercept = 0.4, lty = 3, colour = "grey40") +
  geom_hline(yintercept = 0.2, lty = 2, colour = "grey40") +
  # facet_wrap(~model_name) +
  facet_wrap(~catch) +
  ylab(expression(S / S[0])) +
  xlab("") +
  labs(colour = "Model")
ggsave("figs/ss3/refpts/proj-facet-catch.png", width = 9, height = 4.5)


if (FALSE) {
  setwd("figs/ss3/refpts/")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    1, " -P ", 6, " /opt/homebrew/bin/optipng -strip all"
  ))
  setwd(here::here())
}

# theme(legend.position = "top", legend)

# x <- SS_output("ss3/A0-forecast-300/")
# SS_ForeCatch(x)
# SS_plots(x, forecastplot = T)
# SSplotTimeseries(x, subplot = 9)
#
# fit_ss3("A0-forecast-300", hessian = F)
#
# x <- SS_output("ss3/A0-forecast-300/")
# x$derived_quants
# dplyr::filter(x$derived_quants, Label == "Dead_Catch_Btgt")
# SSplotTimeseries(x, subplot = 3)
# SSplotTimeseries(x, subplot = 1)
# SSplotTimeseries(x, subplot = 9)
#
# filter(x$derived_quants, grepl("ForeCatch", Label))
#
# fo <- "A0-forecast-300"
# f <- SS_readforecast(paste0("ss3/", fo, "/forecast.ss"))
# f$ForeCatch <- make_f_catch(400, years = 100)
# make_f_catch(400, years = 100) |> readr::write_tsv("ss3/A0-forecast-300/forca.txt")
# f$Nforecastyrs <- max(f$ForeCatch$year) - min(f$ForeCatch$year) + 1
# SS_writeforecast(f, paste0("ss3/", fo), overwrite = TRUE)
#
# replist <- r4ss::SS_output("ss3/A0-forecast-300/")
# # Yield curve
# r4ss::SSplotYield(replist, subplots = 2)
#
# replist$derived_quants %>%
#   filter(grepl("ForeCatch", Label))
#
# replist$derived_quants %>%
#   filter(grepl("Btgt", Label))
#
# # Forecast catch by fleet
# replist$timeseries %>%
#   filter(Yr > 2023) %>%
#   select(Yr, starts_with("dead(B)"))
#
# # Useless
# replist$fatage %>%
#   filter(Era == "FORE")
#
# # Z at age vector
# Z_fore <- replist$Z_at_age %>% filter(Yr > 2023)
# plot(0:69, Z_fore[30, 3 + 1:70])
#
# N_fore <- replist$natage %>% filter(Yr > 2023) %>%
#   filter(`Beg/Mid` == "B") %>%
#   select(Yr, Sex, as.character(seq(0, 40, 10))) %>%
#   reshape2::melt(id.vars = c("Yr", "Sex")) %>%
#   ggplot(aes(Yr, value)) +
#   geom_line() +
#   facet_grid(vars(variable), vars(Sex), scales = "free_y")
#
# N_fore
