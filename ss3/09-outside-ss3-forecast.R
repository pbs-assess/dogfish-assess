options(max.print = 1e9)

library(r4ss)
library(dplyr)
library(ggplot2)
library(future)

source("ss3/99-utils.R")

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

  fn_exe <- if (Sys.info()[["user"]] == "seananderson") "ss" else "/home/anderson/src/ss3_opt"

  if (.Platform$OS.type == "unix") {
    cmd <- paste0(fn_exe, " modelname ss")
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

make_f_catch <- function(dir, total, years = 10) {
  ctl <- SS_readctl(paste0("ss3/", dir, "/control.ss_new"))
  file.copy(paste0("ss3/", dir, "/data_echo.ss_new"), paste0("ss3/", dir, "/data.ss_new"))
  i <- grepl("_Mult:", row.names(ctl$MG_parms))
  mult <- ctl$MG_parms[i,] |> select(catch_multiplier = INIT)
  mult$fleet <- as.integer(gsub("^Catch_Mult:_", "", row.names(mult)))
  suppressWarnings(
    dat <- SS_readdat(paste0("ss3/", dir, "/data.ss"), verbose = FALSE)
  )
  bratio_dat <- filter(dat$catch, year >= 2018) |> # last 5
    filter(fleet %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) |> # active fleets catching dogfish
    group_by(fleet) |>
    summarise(m = mean(catch))

  # keep surveys and iRec and salmon bycatch constant at average levels:
  temp1 <- filter(bratio_dat, fleet %in% c(6, 7, 8, 9, 10)) # surveys + salmon
  temp1$catch_or_F_before_mortality <- temp1$m
  total_constant_catch <- sum(temp1$m)
  total_remaining_quota <- total - total_constant_catch

  if (total_remaining_quota > 0) {
    temp2 <- filter(bratio_dat, fleet %in% c(1, 2, 3, 4, 5)) # quota fishing fleets
    temp2 <- mutate(temp2, fraction = m / sum(m))
    temp2 <- mutate(temp2, catch_or_F_before_mortality = total_remaining_quota * fraction)
    bratio_dat <- bind_rows(temp1, temp2)
    stopifnot(round(sum(bratio_dat$catch_or_F_before_mortality), 0) == round(total, 0))
  } else {
    bratio_dat <- temp1
  }

  bratio_dat <- left_join(bratio_dat, mult, by = join_by(fleet))

  # midwater is dead catch in data.ss; do separately
  temp_non_midwater <- filter(bratio_dat, fleet != 3)
  temp_non_midwater <- mutate(temp_non_midwater, catch_or_F = catch_or_F_before_mortality / catch_multiplier) |>
    select(-catch_or_F_before_mortality, -catch_multiplier)

  # midwater is dead catch in data.ss; do separately
  catch_multiplier_trawl <- filter(mult, fleet == 2) |> pull(catch_multiplier)
  temp_midwater <- filter(bratio_dat, fleet == 3)
  temp_midwater <- mutate(temp_midwater, catch_or_F = catch_or_F_before_mortality / catch_multiplier_trawl) |>
    select(-catch_or_F_before_mortality, -catch_multiplier)

  bratio_dat <- bind_rows(temp_non_midwater, temp_midwater)
  bratio_dat <- purrr::map_dfr(2024:(2024 + years - 1), \(y) data.frame(bratio_dat, year = y))

  missing_fleets <- unique(c(dat$catch$fleet))[!unique(dat$catch$fleet) %in% unique(bratio_dat$fleet)]
  df <- expand.grid(fleet = missing_fleets, catch_or_F = 0, year = unique(bratio_dat$year))
  bratio_dat <- bind_rows(df, bratio_dat)

  transmute(bratio_dat, year = year, seas = 1, fleet = fleet, catch_or_F = catch_or_F) |>
    arrange(fleet, year) |>
    mutate(catch_or_F = round(catch_or_F, 4L))
}

run_projection <- function(model = "A0", catch, hessian = FALSE, do_fit = TRUE, years = 10) {
    # cat("Catches:", catch, "t\n")
    # cat("Model:", model, "\n")
    fo <- paste0(model, "-forecast-", catch)
    to_folder <- paste0("ss3/", fo, "/")
    # if (file.exists(to_folder)) do_fit <- FALSE
    if (do_fit) {
      system(paste0("rm -rf ", to_folder))
      system(paste0("cp -r ss3/", model, "/ ", to_folder))
      system(paste0("cp ", to_folder, "control.ss_new ", to_folder, "control.ss")) # swap inits for fitted for speed
      f <- SS_readforecast(paste0("ss3/", fo, "/forecast.ss"))
      f$ForeCatch <- make_f_catch(total = catch, dir = fo, years = years)
      f$Nforecastyrs <- max(f$ForeCatch$year) - min(f$ForeCatch$year) + 1
      SS_writeforecast(f, paste0("ss3/", fo), overwrite = TRUE)
      fit_ss3(fo, hessian = hessian)
    }
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
    ret$catch <- catch
    ret$model <- model
    as_tibble(ret)
  # })
  # out
}

source("ss3/99-model-names.R")
reject <- c("B1_1990inc", "B3_2005step", "B4_1990inc_lowM", "A1", "A8_HBLLonly", "A15_100discard")
keep <- which(!mods %in% reject)
mods <- mods[keep]
model_name <- model_name[keep]

length(mods)
(tacs <- c(0, 100, 200, 300, seq(400, 1200, by = 200)))

torun <- expand.grid(model = mods, catch = tacs)
nrow(torun)

# don't accidentally overwrite!
if (FALSE) {
  plan(multicore, workers = 70)
  out <- furrr::future_pmap(torun, run_projection, hessian = TRUE)
  plan(sequential)
  saveRDS(out, "data/generated/projections.rds")
}

# rebuilding
# don't accidentally overwrite!
if (FALSE) {
  (tacs <- seq(0, 400, by = 100))
  torun <- expand.grid(model = mods, catch = tacs)
  nrow(torun)
  plan(multicore, workers = 30)
  out_rebuild <- furrr::future_pmap(torun, run_projection, hessian = F, years = 150L)
  plan(sequential)
  saveRDS(out_rebuild, "data/generated/projections-rebuilding.rds")
}

PLOT_TYPE <- "rebuilding" # SET HERE!!

if (PLOT_TYPE == "rebuilding") {
  out_rebuild <- readRDS("data/generated/projections-rebuilding.rds")
  x <- bind_rows(out_rebuild)
} else {
  out <- readRDS("data/generated/projections.rds")
  x <- bind_rows(out)
}

x <- filter(x, model %in% mods)
lu <- data.frame(model = mods, model_name = factor(model_name, levels = model_name))

mn <- model_name
bratio_dat <- x |>
  filter(year > 1900, label %in% "Bratio", !is.na(year), se < 2, est < 0.999) |>
  # filter(!grepl("B", model)) |>
  left_join(lu) |>
  mutate(model_name = factor(model_name, levels = mn)) |>
  mutate(catch = factor(catch)) |>
  mutate(catch = forcats::fct_rev(catch))

make_proj_by_model <- function(dat, type = c("B", "F"), ylab = "S / S<sub>0</sub>") {
  type <- match.arg(type)

  g <- dat |>
    # filter(catch %in% tacs[seq(1, 1e2, 3)]) |>
    ggplot(aes(year, est,
      ymin = est - 2 * se, ymax = est + 2 * se,
      colour = catch, group = paste(model_name, catch), fill = catch
    )) +
    geom_ribbon(fill = "grey70", alpha = 0.7, colour = NA) +
    geom_line() +
    geom_line(data = filter(dat, catch == 0)) + # dark on top
    scale_x_continuous(breaks = seq(1960, 2990, 20)) +
    scale_colour_viridis_d(direction = -1, option = "D") +
    scale_fill_viridis_d(direction = -1, option = "D") +
    annotate(
      "rect",
      xmin = 2024, xmax = max(x$year, na.rm = TRUE),
      ymin = 0, ymax = 1e6,
      alpha = 0.1, fill = "grey55"
    ) +
    facet_wrap(~model_name) +
    ylab(ylab) +
    xlab("") +
    labs(colour = "Catch (t)") +
    gfplot::theme_pbs() +
    theme(axis.title = ggtext::element_markdown())

  if (type == "B") {
    g <- g + coord_cartesian(expand = FALSE, ylim = c(0, 0.42)) +
      geom_hline(yintercept = 0.4, lty = 3, colour = "grey40") +
      geom_hline(yintercept = 0.2, lty = 2, colour = "grey40")
  }
  if (type == "F") {
    g <- g + coord_cartesian(expand = FALSE, ylim = c(0, 8)) +
      geom_hline(yintercept = 1, lty = 2, colour = "grey40")
  }
  g
}

if (PLOT_TYPE != "rebuilding") {
  bratio_dat |> filter(catch %in% tacs[seq(1, 1e2, 2)]) |>
    make_proj_by_model()
  ggsave_optipng("figs/ss3/refpts/proj-facet-model.png", width = 8.5, height = 6.5)
} else {
  line_dat <- bratio_dat |>
    filter(!grepl("^\\(B", model_name)) |>
    group_by(model_name, catch) |>
    group_split() |>
    purrr::map_dfr(\(x) {
      x <- filter(x, year >= 2024)
      if (x$est[length(x$est)] <= 0.2) {
        out <- NA
      } else {
        out <- min(x$year[x$est > 0.2])
      }
      data.frame(x, b0.2 = out)
    }) |>
    select(year, model_name, b0.2, catch) |> distinct()

  bratio_dat |>
    filter(!grepl("^\\(B", model_name)) |>
    make_proj_by_model() +
    coord_cartesian(expand = FALSE, ylim = c(0, 0.8)) +
    geom_vline(aes(xintercept = b0.2, colour = catch), data = line_dat, lty = 2) +
    scale_x_continuous(breaks = seq(2023, 2023+150, 50), labels = c(0, 50, 100, 150)) +
    annotate(
      "rect",
      xmin = 2024+50, xmax = 2024+100,
      ymin = 0, ymax = 1e6,
      alpha = 0.15, fill = "grey55"
    ) +
    scale_colour_viridis_d(option = "C", begin = 0, direction = -1, end = 0.9) +
    xlab("Years after 2024")
  ggsave_optipng("figs/ss3/refpts/rebuild-facet-model.png", width = 8.5, height = 6.5)
}

cols <- c("grey10", RColorBrewer::brewer.pal(12L, "Paired"))
names(cols) <- model_name[!grepl("^\\(B", model_name)]

make_proj_by_catch_level <- function(dat, ylab = "S / S<sub>0</sub>") {
  dat |>
  # filter(catch %in% tacs[seq(1, 1e2, 3)]) |>
    mutate(catch = forcats::fct_rev(catch)) |>
    ggplot(aes(year, est,
      ymin = est - 2 * se, ymax = est + 2 * se,
      # colour = catch, group = paste(model_name, catch), fill = catch)) +
      colour = model_name, group = paste(model_name, catch), fill = catch
    )) +
    geom_ribbon(fill = "grey70", alpha = 0.7, colour = NA) +
    geom_line() +
    geom_line(data = filter(dat, model == "A0")) + # dark on top
    scale_x_continuous(breaks = seq(1960, 2090, 20)) +
    scale_colour_manual(values = cols) +
    # scale_colour_brewer(palette = "Paired") +
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
    ylab(ylab) +
    xlab("") +
    labs(colour = "Model") +
    gfplot::theme_pbs() +
    theme(axis.title = ggtext::element_markdown())
}

if (PLOT_TYPE != "rebuilding") {
  bratio_dat |> filter(catch %in% tacs[seq(1, 12, 2)]) |>
    filter(!grepl("B", model)) |> make_proj_by_catch_level()
  ggsave("figs/ss3/refpts/proj-facet-catch.png", width = 9, height = 4.5)
} else {
  bratio_dat |>
    make_proj_by_catch_level() +
    coord_cartesian(expand = FALSE, ylim = c(0, 0.8)) +
    geom_vline(aes(xintercept = b0.2, colour = model_name), data = line_dat, lty = 1) +
    scale_x_continuous(breaks = seq(1950, 3000, 50))
  # facet_grid(model_name ~ catch) +
  # geom_line(colour = "black")
}

# F ----------------------------------------------------------------

multi_rep <- readRDS("data/generated/replist-ref-pts.rds")

fratio_dat <- x |>
  filter(year > 1900, label %in% "F", !is.na(year), se < 2, est < 1e6, est >= 0) |>
  # filter(!grepl("B", model)) |>
  left_join(lu) |>
  mutate(catch = factor(catch)) |>
  mutate(catch = forcats::fct_rev(catch))

out_F <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i) {
    multi_rep[[i]]$derived_quants %>%
      filter(Label %in% c("annF_Btgt")) %>%
      select(label = Label, est = Value, se = StdDev) |>
      mutate(scen = model_name[i])
  }) |>
  select(F_Btgt = est, F_Btgt_se = se, model_name = scen)
row.names(out_F) <- NULL

fratio_dat <- left_join(fratio_dat, out_F) |>
  mutate(model_name = factor(model_name, levels = lu$model_name))

fratio_dat |>
  filter(catch %in% tacs[seq(1, 1e2, 3)]) |>
  ggplot(aes(year, est / F_Btgt,
    ymin = (est - 2 * se) / F_Btgt, ymax = (est + 2 * se) / F_Btgt,
    colour = catch, group = paste(model_name, catch), fill = catch
  )) +
  geom_ribbon(fill = "grey70", alpha = 0.7, colour = NA) +
  geom_line() +
  geom_line(data = filter(fratio_dat, catch == 0)) + # dark on top
  scale_x_continuous(breaks = seq(1960, 2090, 20)) +
  scale_colour_viridis_d(direction = -1, option = "D", guide = guide_legend(reverse = F)) +
  scale_fill_viridis_d(direction = -1, option = "D", guide = guide_legend(reverse = F)) +
  coord_cartesian(expand = FALSE, ylim = c(0, 10)) +
  geom_hline(yintercept = 1, lty = 2, colour = "grey40") +
  facet_wrap(~model_name) +
  xlab("") +
  ylab("F / F<sub>0.4S0</sub>") +
  gfplot::theme_pbs() +
  theme(axis.title = ggtext::element_markdown()) +
  labs(colour = "Catch (t)\nassuming 30%\ndiscard mortality")
ggsave("figs/ss3/refpts/proj-F-facet-model.png", width = 8.5, height = 6.5)

fratio_dat |>
  filter(catch %in% tacs[seq(1, 1e2, 2)]) |>
  filter(!grepl("B", model)) |>
  mutate(catch = forcats::fct_rev(catch)) |>
  ggplot(aes(year, est / F_Btgt,
    ymin = (est - 2 * se) / F_Btgt, ymax = (est + 2 * se) / F_Btgt,
    colour = model_name, group = paste(model_name, catch), fill = catch
  )) +
  geom_ribbon(fill = "grey70", alpha = 0.7, colour = NA) +
  geom_line() +
  geom_line(data = filter(fratio_dat, model == "A0", catch %in% tacs[seq(1, 1e2, 2)])) + # dark on top
  scale_x_continuous(breaks = seq(1960, 2090, 20)) +
  scale_colour_manual(values = cols) +
  annotate(
    "rect",
    xmin = 2024, xmax = max(x$year, na.rm = TRUE),
    ymin = 0, ymax = 1e6,
    alpha = 0.1, fill = "grey55"
  ) +
  coord_cartesian(expand = FALSE, ylim = c(0, 15)) +
  geom_hline(yintercept = 1, lty = 2, colour = "grey40") +
  facet_wrap(~catch) +
  ylab("F / F<sub>0.4S0</sub>") +
  xlab("") +
  labs(colour = "Model") +
  gfplot::theme_pbs() +
  theme(axis.title = ggtext::element_markdown())
ggsave("figs/ss3/refpts/proj-F-facet-catch.png", width = 9, height = 4.5)

make_tigure_decision <- function(dat, fill_label = "P(F < F<sub>0.4S0</sub>)", xlab = "Catch (t)", type = c("F", "LRP", "USR")) {
  pal <- RColorBrewer::brewer.pal(8, "Greys")[7:2]
  type <- match.arg(type)
  dat <- dat |>
    filter(year %in% c(2024, 2025, 2026)) |>
    group_by(year, catch, model_name)

  if (type == "F") {
    dat <- dat |>
      summarise(
        frac_975 = (est - qnorm(0.95) * se) < F_Btgt,
        frac_75 = (est - qnorm(0.75) * se) < F_Btgt,
        frac_50 = (est < F_Btgt),
        frac_25 = (est + qnorm(0.75) * se) < F_Btgt,
        frac_025 = (est + qnorm(0.95) * se) < F_Btgt,
        .groups = "drop"
      ) |>
      group_by(year, catch, model_name) |>
      mutate(frac = sum(frac_975, frac_75, frac_50, frac_25, frac_025))
  }
  if (type %in% c("LRP", "USR")) {
    thresh <- if (type == "LRP") 0.2 else 0.4
    dat <- dat |>
      summarise(
        frac_975 = (est - qnorm(0.95) * se) < thresh,
        frac_75 = (est - qnorm(0.75) * se) < thresh,
        frac_50 = (est < thresh),
        frac_25 = (est + qnorm(0.75) * se) < thresh,
        frac_025 = (est + qnorm(0.95) * se) < thresh,
        .groups = "drop"
      ) |>
      group_by(year, catch, model_name) |>
      mutate(frac = sum(frac_975, frac_75, frac_50, frac_25, frac_025))
  }

  labs <- c("P \u2265 0.95", "0.75 \u2264 P < 0.95", "0.50 \u2264 P < 0.75", "0.25 \u2264 P < 0.50", "0.05 \u2264 P < 0.25", "P < 0.05")

  if (type == "F") labs <- rev(labs)
  dat |>
    ungroup() |>
    mutate(frac = as.factor(frac)) |>
    mutate(catch = as.numeric(as.character(catch))) |>
    mutate(model_name = forcats::fct_rev(model_name)) |>
    ggplot(aes(catch, model_name, fill = frac)) +
    geom_tile(colour = "grey50") +
    scale_fill_manual(
      values = pal, breaks = c(0:5),
      labels = labs, guide = guide_legend(reverse = TRUE)
    ) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = tacs[seq(1, 1e2, 2)]) +
    labs(fill = fill_label) +
    gfplot::theme_pbs() +
    theme(legend.title = ggtext::element_markdown(), legend.position = "bottom") +
    ylab("") +
    xlab(xlab) +
    theme(axis.ticks = element_blank(), legend.text = ggtext::element_markdown()) +
    facet_wrap(~year)
}

fratio_dat |>
  filter(!grepl("B", model), catch != "1500", catch != "1400") |>
  make_tigure_decision()
ggsave("figs/ss3/refpts/f-ref-pt-tigure.png", width = 10, height = 4)

bratio_dat |>
  filter(!grepl("B", model), catch != "1500", catch != "1400") |>
  make_tigure_decision(type = "LRP", fill_label = "P(B > 0.2B<sub>0</sub>)")
ggsave("figs/ss3/refpts/lrp-ref-pt-tigure.png", width = 10, height = 4)

bratio_dat |>
  filter(!grepl("B", model), catch != "1500", catch != "1400") |>
  make_tigure_decision(type = "USR", fill_label = "P(B > 0.4B<sub>0</sub>)")
ggsave("figs/ss3/refpts/usr-ref-pt-tigure.png", width = 10, height = 4)

if (FALSE) {
  setwd("figs/ss3/refpts/")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    1, " -P ", 6, " /opt/homebrew/bin/optipng -strip all"
  ))
  setwd(here::here())
}

# clean up

f <- list.files("ss3", pattern = "-forecast-", full.names = T)
f
# x <- sapply(f, unlink, recursive = T, force = T)
