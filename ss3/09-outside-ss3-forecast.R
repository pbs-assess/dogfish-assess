options(max.print = 1e9) # for r4ss print to forecast file

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

## updated version that uses 'dead catch' as quota levels... as it should:
make_f_dead_catch <- function(dir, total, years = 10) {
  ## figure out dead catch from last 5 years within the model by fleet
  ## keep those ratios applied in forecast
  rep <- r4ss::SS_output(
    dir,
    verbose = FALSE,
    printstats = FALSE,
    hidewarn = TRUE
  )
  dead_catch5 <- rep$catch |>
    filter(Yr %in% 2019:2023) |> ## last 5
    group_by(Fleet, Fleet_Name) |>
    summarise(avg_dead_bio = mean(dead_bio), .groups = "drop")

  out_non_surveys <- filter(dead_catch5, Fleet %in% c(1:5, 9, 10))
  out_surveys <- filter(dead_catch5, Fleet %in% c(6, 7, 8)) |>
    mutate(total_dead_catch = avg_dead_bio)

  total_survey_catch <- sum(out_surveys$total_dead_catch)

  if (total > total_survey_catch) {
    out_non_surveys <- out_non_surveys |>
      mutate(ratio = avg_dead_bio / sum(avg_dead_bio)) |>
      mutate(total_dead_catch = ratio * (total - total_survey_catch))
  } else {
    out_non_surveys <- mutate(out_non_surveys, total_dead_catch = 0, ratio = NA)
  }

  out <- bind_rows(out_non_surveys, out_surveys) |>
    arrange(Fleet) |>
    select(-ratio, -avg_dead_bio)
  out <- out |> mutate(seas = 1)
  out <- purrr::map_dfr(2024:(2024 + years - 1), \(y) data.frame(out, year = y))
  out <- transmute(out, year, seas, fleet = Fleet, catch_or_F = round(total_dead_catch, 5L)) |> arrange(year, seas, fleet)
  out
}

run_projection <- function(
    model = "A0", catch, hessian = FALSE,
    do_fit = TRUE, years = 10, tag =
      "forecast") {
  # cat("Catches:", catch, "t\n")
  # cat("Model:", model, "\n")
  fo <- paste0(model, "-", tag, "-", catch)
  to_folder <- paste0("ss3/", fo, "/")
  if (file.exists(paste0(to_folder, "Report.sso"))) do_fit <- FALSE
  if (do_fit) {
    system(paste0("rm -rf ", to_folder))
    system(paste0("cp -r ss3/", model, "/ ", to_folder))
    system(paste0("cp ", to_folder, "control.ss_new ", to_folder, "control.ss")) # swap inits for fitted for speed
    system(paste0("rm -rf ", to_folder, "retrospectives"))
    f <- r4ss::SS_readforecast(paste0("ss3/", fo, "/forecast.ss"), verbose = FALSE)
    f$ForeCatch <- make_f_dead_catch(total = catch, dir = file.path("ss3", fo), years = years)
    # clean out .sso files so we can check that it ran:
    system(paste0("rm ", to_folder, "*.sso"))
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
reject <- c("B1_1990inc", "B3_2005step", "B4_1990inc_lowM", "A1", "A8_HBLLonly", "A15_100discard", "A2_USgrowth", "A4_USgrowth_highmat")
keep <- which(!mods %in% reject)
mods <- mods[keep]
model_name <- model_name[keep]

length(mods)
# (tacs <- c(0, 100, 200, 300, seq(400, 1000, by = 200)))
# (tacs <- c(seq(0, 1000, by = 100)))
(tacs <- c(seq(0, 450, by = 25)))

torun <- expand.grid(model = mods, catch = tacs)
nrow(torun)

# .fo <- paste0(torun$model, "-forecast-", torun$catch)
# .to_folder <- paste0("ss3/", .fo, "/")

# torun <- filter(torun, )

# # Debugging catches in forecast:
# if (FALSE) {
# x <- run_projection(model = "A0", catch = 100, hessian = FALSE, years = 3)
#   replist <- r4ss::SS_output("ss3/A0-forecast-100")
#   # replist$timeseries %>% filter(Era == "FORE")
#   deadB <- replist$timeseries %>%
#     filter(Era == "FORE") %>%
#     select(starts_with("dead(B)"))
#   names(deadB) <- replist$FleetNames[1:10]
#   deadB <- reshape2::melt(deadB) |>
#     rename(fleet_name = variable, dead_catch_SS3 = value)
#
#   input_catches <- make_f_catch("A0-forecast-100", total = 100, years = 1, debug_mode = TRUE)
#   input_catches$fleet_name <- replist$FleetNames[1:10]
#   input_catches <- select(input_catches, -seas) |>
#     as_tibble() |>
#     mutate(implied_catch = catch_or_F * catch_multiplier) |>
#     select(-fleet) |>
#     select(-fraction) |>
#     select(-year) |>
#     select(fleet_name, everything())
#   input_catches
#
#   left_join(input_catches, deadB)
# }

# don't accidentally overwrite!
if (FALSE) {
  plan(multicore, workers = min(c(68, parallel::detectCores() / 2 - 0)))
  out <- furrr::future_pmap(torun, run_projection, hessian = TRUE, do_fit = TRUE)
  # run_projection(model = "A0", catch = 100, hessian = FALSE)
  plan(sequential)
  saveRDS(out, "data/generated/projections.rds")
}

# rebuilding
# don't accidentally overwrite!
# Warning: this will take a *very* long time run:
if (FALSE) {
  (tacs <- seq(0, 300, by = 100))
  torun <- expand.grid(model = mods, catch = tacs)
  torun <- torun |> filter(!grepl("B", model))
  nrow(torun)
  plan(multicore, workers = min(c(65, parallel::detectCores() / 2 - 0)))
  out_rebuild <- furrr::future_pmap(torun, run_projection, hessian = TRUE, years = 150L, tag = "projection")
  # out_rebuild <- furrr::future_pmap(filter(torun, model %in% "A0"), run_projection, hessian = F, years = 100L)
  # out_rebuild <- purrr::pmap(data.frame(model = "A0", catch = 0),
  #   run_projection, hessian = F, years = 150L)
  # out_rebuild[[1]] |> ggplot(aes(year, est)) + geom_line() + facet_wrap(~label, scales = "free_y") +
  #   # xlim(2000, 2200) +
  #   geom_vline(xintercept = 2023)
  plan(sequential)
  saveRDS(out_rebuild, "data/generated/projections-rebuilding.rds")
}

# grab 'dead' catch in 2023 for different discard assumptions ------------------
dc <- purrr::map_dfr(mods, get_dead_catch) |>
  filter(model %in% c("A0", "A14_lowdiscard", "A5_highdiscard"))
dc

# make plots -------------------------------------------------------------------

# PLOT_TYPE <- "forecast" # SET HERE!!
# PLOT_TYPE <- "rebuilding" # SET HERE!!

for (PLOT_TYPE in c("forecast", "rebuilding")) {
  if (PLOT_TYPE == "rebuilding") {
    out_rebuild <- readRDS("data/generated/projections-rebuilding.rds")
    x <- bind_rows(out_rebuild)
    tacs <- seq(0, 300, by = 100)
  } else {
    out <- readRDS("data/generated/projections.rds")
    x <- bind_rows(out) |> filter(year <= 2033)
    # tacs <- c(0, 100, 200, 300, seq(400, 1200, by = 200))
    tacs <- c(seq(0, 450, by = 25))
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
      geom_ribbon(alpha = 0.3, colour = NA) +
      geom_line() +
      geom_line(data = filter(dat, catch == 0)) + # dark on top
      scale_x_continuous(breaks = seq(1960, 2990, 20)) +
      scale_colour_viridis_d(direction = -1, option = "C", end = 0.9) +
      scale_fill_viridis_d(direction = -1, option = "C", end = 0.9) +
      annotate(
        "rect",
        xmin = 2024, xmax = max(x$year, na.rm = TRUE),
        ymin = 0, ymax = 1e6,
        alpha = 0.1, fill = "grey55"
      ) +
      facet_wrap(~model_name) +
      ylab(ylab) +
      xlab("") +
      labs(colour = "Dead catch (t)", fill = "Dead catch (t)") +
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
    bratio_dat |>
      filter(catch %in% tacs[seq(1, 1e2, 2)]) |>
      make_proj_by_model()
    # annotate(
    #   "rect",
    #   xmin = 2024+50, xmax = 2024+50+50,
    #   ymin = 0, ymax = 1e6,
    #   alpha = 0.1, fill = "grey55"
    # ) +
    # geom_vline(data = line_dat, mapping = aes(xintercept = year, colour = catch), na.rm = TRUE)
    ggsave_optipng("figs/ss3/refpts/proj-facet-model.png", width = 8.5, height = 6.5)

    if (FALSE) source("ss3/99-sopo-data.R")
  } else {
    line_dat <- bratio_dat |>
      mutate(lwr = est - 1.96 * se, upr = est + se * 1.96, lwr0.95 = est - qnorm(0.95) * se) |>
      group_by(model_name, catch) |>
      group_split() |>
      purrr::map_dfr(\(xx) {
        xx <- filter(xx, year >= 2024)
        if (max(xx$lwr) < 0.2) out_lwr <- NA
        if (max(xx$lwr) >= 0.2) {
          out_lwr <- xx$year[min(which(xx$lwr >= 0.2))]
        }
        if (max(xx$est) < 0.2) out_est <- NA
        if (max(xx$est) >= 0.2) {
          out_est <- xx$year[min(which(xx$est >= 0.2))]
        }
        if (max(xx$upr) < 0.2) out_upr <- NA
        if (max(xx$upr) >= 0.2) {
          out_upr <- xx$year[min(which(xx$upr >= 0.2))]
        }
        if (max(xx$lwr0.95) < 0.2) out_lwr0.95 <- NA
        if (max(xx$lwr0.95) >= 0.2) {
          out_lwr0.95 <- xx$year[min(which(xx$lwr0.95 >= 0.2))]
        }
        data.frame(
          b0.2_lwr = out_lwr, b0.2_est = out_est, b0.2_upr = out_upr, b0.2_lwr0.95 = out_lwr0.95,
          catch = xx$catch[1], model_name = xx$model_name[1]
        )
      }) |>
      filter(!grepl("^\\(B", model_name))

    bratio_dat |>
      # These crash!
      mutate(se = ifelse(catch == 400 & model_name == "(A7) SYN only", 0, se)) |>
      mutate(se = ifelse(catch == 400 & model_name == "(A13) Extra SD on IPHC" & se > 0.001, 0, se)) |>
      filter(!grepl("^\\(B", model_name)) |>
      make_proj_by_model() +
      coord_cartesian(expand = FALSE, ylim = c(0, 0.8)) +
      geom_vline(aes(xintercept = b0.2_lwr0.95, colour = catch), data = line_dat, lty = 2, na.rm = TRUE) +
      scale_x_continuous(breaks = seq(2023, 2023 + 150, 50), labels = c(0, 50, 100, 150)) +
      annotate(
        "rect",
        xmin = 2024 + 50, xmax = 2024 + 100,
        ymin = 0, ymax = 1e6,
        alpha = 0.15, fill = "grey55"
      ) +
      xlab("Years after 2024")
    ggsave_optipng("figs/ss3/refpts/rebuild-facet-model.png", width = 8.5, height = 5.0)

    line_dat |>
      # SS3 SE problems!? 0 or huge... these crash below 0:
      # filter(!(catch == 400 & model_name == "(A7) SYN only")) |>
      # filter(!(catch == 400 & model_name == "(A13) Extra SD on IPHC")) |>
      mutate(model_name = factor(model_name, levels = rev(levels(lu$model_name)))) |>
      mutate(b0.2_lwr = ifelse(is.na(b0.2_lwr), 9999, b0.2_lwr)) |>
      mutate(catch = forcats::fct_rev(catch)) |>
      ggplot(aes(model_name, b0.2_lwr, colour = catch)) +
      geom_point(pch = 21, position = position_dodge(width = 0.5), mapping = aes(y = b0.2_est), size = 1.1) +
      geom_point(pch = 8, position = position_dodge(width = 0.5), mapping = aes(y = b0.2_lwr0.95), size = 0.7) +
      geom_linerange(mapping = aes(ymin = b0.2_upr, ymax = b0.2_lwr, x = model_name), position = position_dodge(width = 0.5), alpha = 0.4) +
      scale_colour_viridis_d(option = "C", begin = 0, direction = 1, end = 0.9) +
      annotate(
        "rect",
        ymin = 2023, ymax = 2023 + 150,
        xmin = 0.5, xmax = length(unique(line_dat$model_name)) + 0.5,
        alpha = 0.1, fill = "grey55"
      ) +
      annotate(
        "rect",
        ymin = 2023 + 50, ymax = 2023 + 100,
        xmin = 0.5, xmax = length(unique(line_dat$model_name)) + 0.5,
        alpha = 0.1, fill = "grey55"
      ) +
      coord_flip(expand = FALSE, ylim = c(2023, 2023 + 150)) +
      gfplot::theme_pbs() +
      theme(panel.grid.major.y = element_line(colour = "grey90", linetype = 2)) +
      labs(colour = "Dead catch (t)", y = "Year forecasted S/S<sub>0</sub> > 0.2") +
      theme(axis.title = ggtext::element_markdown()) +
      theme(axis.title.y.left = element_blank()) +
      scale_y_continuous(breaks = seq(2023, 2023 + 150, 50), labels = c(0, 50, 100, 150)) +
      guides(colour = guide_legend(reverse = TRUE))
    ggsave_optipng("figs/ss3/refpts/rebuild-timeframe-dots.png", width = 5, height = 4)
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
        colour = model_name, group = paste(model_name, catch), fill = model_name
      )) +
      geom_ribbon(alpha = 0.3, colour = NA) +
      geom_line() +
      geom_line(data = filter(dat, model == "A0")) + # dark on top
      scale_x_continuous(breaks = seq(1960, 2090, 20)) +
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
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
      labs(colour = "Model", fill = "Model") +
      gfplot::theme_pbs() +
      theme(axis.title = ggtext::element_markdown())
  }

  if (PLOT_TYPE != "rebuilding") {
    bratio_dat |>
      filter(catch %in% tacs[seq(1, 12, 2)]) |>
      filter(!grepl("B", model)) |>
      make_proj_by_catch_level()
    ggsave_optipng("figs/ss3/refpts/proj-facet-catch.png", width = 9, height = 4.5)
  } else {
    bratio_dat |>
      # SEs are 0 or huge!??
      mutate(se = ifelse(catch == 400 & model_name == "(A7) SYN only", 0, se)) |>
      mutate(se = ifelse(catch == 400 & model_name == "(A13) Extra SD on IPHC" & se > 0.001, 0, se)) |>
      make_proj_by_catch_level() +
      coord_cartesian(expand = FALSE, ylim = c(0, 0.8)) +
      # geom_vline(aes(xintercept = b0.2, colour = model_name), data = line_dat, lty = 1) +
      # scale_x_continuous(breaks = seq(1950, 3000, 50))
      scale_x_continuous(breaks = seq(2023, 2023 + 150, 50), labels = c(0, 50, 100, 150))
    # facet_grid(model_name ~ catch) +
    # geom_line(colour = "black")
    ggsave_optipng("figs/ss3/refpts/rebuild-facet-catch.png", width = 9, height = 4.5)
  }

  if (PLOT_TYPE != "rebuilding") {
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
      filter(!grepl("B", model)) |>
      filter(catch %in% tacs[seq(1, 1e2, 2)]) |>
      ggplot(aes(year, est / F_Btgt,
        ymin = (est - 2 * se) / F_Btgt, ymax = (est + 2 * se) / F_Btgt,
        colour = catch, group = paste(model_name, catch), fill = catch
      )) +
      geom_ribbon(alpha = 0.3, colour = NA) +
      geom_line() +
      geom_line(data = filter(fratio_dat, catch == 0, !grepl("B", model))) + # dark on top
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
      labs(colour = "Dead catch (t)", fill = "Dead catch (t)") +
      annotate(
        "rect",
        xmin = 2024, xmax = max(x$year, na.rm = TRUE),
        ymin = 0, ymax = 1e6,
        alpha = 0.1, fill = "grey55"
      )
    ggsave_optipng("figs/ss3/refpts/proj-F-facet-model.png", width = 8.5, height = 5.6)

    fratio_dat |>
      filter(catch %in% tacs[seq(1, 1e2, 2)]) |>
      filter(!grepl("B", model)) |>
      mutate(catch = forcats::fct_rev(catch)) |>
      ggplot(aes(year, est / F_Btgt,
        ymin = (est - 2 * se) / F_Btgt, ymax = (est + 2 * se) / F_Btgt,
        colour = model_name, group = paste(model_name, catch), fill = model_name
      )) +
      geom_ribbon(alpha = 0.3, colour = NA) +
      geom_line() +
      geom_line(data = filter(fratio_dat, model == "A0", catch %in% tacs[seq(1, 1e2, 2)])) + # dark on top
      scale_x_continuous(breaks = seq(1960, 2090, 20)) +
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
      annotate(
        "rect",
        xmin = 2024, xmax = max(x$year, na.rm = TRUE),
        ymin = 0, ymax = 1e6,
        alpha = 0.1, fill = "grey55"
      ) +
      coord_cartesian(expand = FALSE, ylim = c(0, 10)) +
      geom_hline(yintercept = 1, lty = 2, colour = "grey40") +
      facet_wrap(~catch) +
      ylab("F / F<sub>0.4S0</sub>") +
      xlab("") +
      labs(colour = "Model", fill = "Model") +
      gfplot::theme_pbs() +
      theme(axis.title = ggtext::element_markdown())

    ggsave_optipng("figs/ss3/refpts/proj-F-facet-catch.png", width = 9, height = 4.5)

    tigure_pal <- RColorBrewer::brewer.pal(8, "Greys")[7:2]
    make_tigure_decision <- function(dat, fill_label = "P(F < F<sub>0.4S0</sub>)", xlab = "Dead catch (t)", type = c("F", "LRP", "USR"), years = c(2024, 2025, 2026, 2027, 2028), pal = tigure_pal) {
      type <- match.arg(type)
      dat <- dat |>
        filter(year %in% years) |>
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
        # scale_x_continuous(breaks = tacs[seq(1, 1e2, 2)]) +
        labs(fill = fill_label) +
        gfplot::theme_pbs() +
        theme(legend.title = ggtext::element_markdown(), legend.position = "bottom") +
        ylab("") +
        xlab(xlab) +
        # theme(axis.ticks = element_blank(), legend.text = ggtext::element_markdown()) +
        theme(legend.text = ggtext::element_markdown()) +
        facet_wrap(~year, nrow = 1)
    }

    # get current catch:
    # dc <- r4ss::SS_readdat("ss3/A0/data.ss_new")
    # catch2023 <- dc$catch |>
    #   filter(year == 2023) |>
    #   pull(catch) |>
    #   sum()
    # catch2023
    # catch2022 <- dc$catch |> filter(year == 2022) |> pull(catch) |> sum()
    # catch2021 <- dc$catch |> filter(year == 2021) |> pull(catch) |> sum()

    lu2 <- data.frame(model = c("A0", "A14_lowdiscard", "A5_highdiscard"), discard_name = factor(c("Base", "Low", "High"), levels = c("Low", "Base", "High")))
    dc2 <- left_join(dc, lu2, by = join_by(model))
    .pal <- RColorBrewer::brewer.pal(3, "RdBu")

    fratio_dat |>
      filter(!grepl("B", model), catch != "1500", catch != "1400", catch %in% seq(0, 450, 25)) |>
      make_tigure_decision(pal = tigure_pal) +
      geom_vline(data = dc2, mapping = aes(xintercept = dead_catch, colour = discard_name), lty = 2) +
      # geom_vline(xintercept = catch2023, lty = 2, col = "grey40") +
      scale_x_continuous(breaks = seq(0, 1200, 100)) +
      labs(colour = "2018-23 mean dead catch\\\nat discard mortality level") +
      xlab("Dead catch") +
      scale_colour_manual(values = c(.pal[3], "grey50", .pal[1]))
    # geom_vline(xintercept = catch2022, lty = 2, col = "grey20") +
    # geom_vline(xintercept = catch2021, lty = 2, col = "grey20")
    ggsave_optipng("figs/ss3/refpts/f-ref-pt-tigure.png", width = 10, height = 3.6)

    bratio_dat |>
      # filter(!grepl("B", model), catch != "1500", catch != "1400") |>
      filter(!grepl("B", model), catch != "1500", catch != "1400", catch %in% seq(0, 450, 25)) |>
      make_tigure_decision(type = "LRP", fill_label = "P(S > 0.2S<sub>0</sub>)", pal = rev(tigure_pal)) +
      geom_vline(data = dc2, mapping = aes(xintercept = dead_catch, colour = discard_name), lty = 2) +
      # geom_vline(xintercept = catch2023, lty = 2, col = "grey40") +
      scale_x_continuous(breaks = seq(0, 1200, 100)) +
      labs(colour = "2018-23 mean dead catch\\\nat discard mortality level") +
      xlab("Dead catch") +
      scale_colour_manual(values = c(.pal[3], "grey50", .pal[1])) +
      guides(fill = guide_legend(order = 1))
    ggsave_optipng("figs/ss3/refpts/lrp-ref-pt-tigure.png", width = 10, height = 3.4)

    bratio_dat |>
      # filter(!grepl("B", model), catch != "1500", catch != "1400") |>
      filter(!grepl("B", model), catch != "1500", catch != "1400", catch %in% seq(0, 450, 25)) |>
      make_tigure_decision(type = "USR", fill_label = "P(S > 0.4S<sub>0</sub>)", pal = rev(tigure_pal)) +
      # geom_vline(xintercept = catch2023, lty = 2, col = "grey40") +
      scale_x_continuous(breaks = seq(0, 1200, 100)) +
      geom_vline(data = dc2, mapping = aes(xintercept = dead_catch, colour = discard_name), lty = 2) +
      labs(colour = "2018-23 mean dead catch\\\nat discard mortality level") +
      xlab("Dead catch") +
      scale_colour_manual(values = c(.pal[3], "grey50", .pal[1])) +
      guides(fill = guide_legend(order = 1))
    ggsave_optipng("figs/ss3/refpts/usr-ref-pt-tigure.png", width = 10, height = 3.4)

    if (FALSE) {
      setwd("figs/ss3/refpts/")
      system(paste0(
        "find -X . -name '*.png' -print0 | xargs -0 -n ",
        1, " -P ", 6, " /opt/homebrew/bin/optipng -strip all"
      ))
      setwd(here::here())
    }

    # clean up
  }
}
f <- list.files("ss3", pattern = "-forecast-", full.names = T)
f
# x <- sapply(f, unlink, recursive = T, force = T)
