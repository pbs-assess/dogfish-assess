library(dplyr)

ss_home <- here::here("ss3")

#### Fit ss3 model version 3.30.22.1
# Note: use a custom compilation that fixes lognormal prior density function if necessary
fit_ss3 <- function(model_dir = "model1",
  hessian = FALSE,
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

mods <- c("A1", "A0",
  "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_highdiscard",
  "A6_IPHC+CPUE", "A7_SYNonly", "A8_HBLLonly", "A9_lowM", "A10_highM",
  "A11_low_zfrac", "A12_high_zfrac", "A13_extraSD", "A14_lowdiscard",
  "A15_100discard",
  "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM")

# Make sure starter and forecast files match...
tocopy <- seq_along(mods)[-2] # copying 2
for (i in tocopy) {
  to <- file.path(ss_home, mods[i], "starter.ss")
  cat("Copying starter.ss to:", to, "\n")
  file.copy(file.path(ss_home, "A0/starter.ss"), to, overwrite = TRUE)
  to <- file.path(ss_home, mods[i], "forecast.ss")
  cat("Copying forecast.ss to:", to, "\n")
  file.copy(file.path(ss_home, "A0/forecast.ss"), to, overwrite = TRUE)
  cat("\n")
}

# Copy ss.exe executable from A0 (Windows users, set up ss.exe executable in each model directory)
if (FALSE) {
  for (i in tocopy) {
    to <- file.path(ss_home, mods[i], "ss.exe")
    cat("Copying ss.exe to:", to, "\n")
    file.copy(file.path(ss_home, "A0/ss.exe"), to, overwrite = TRUE)
  }
}

# Fit a single model
if (FALSE) {
  fit_ss3(mods[3], hessian = F, ss_home = ss_home, extra_args = "-maxfn 500")
  r4ss::SS_output("ss3/A0") |> r4ss::SS_plots()
}

# Fit all of many models in parallel
snowfall::sfInit(parallel = TRUE, cpus = min(floor(parallel::detectCores() / 2)), length(mods))
snowfall::sfLapply(mods, fit_ss3, hessian = TRUE, ss_home = ss_home, extra_args = "-maxfn 250")
snowfall::sfStop()

# Some code to generate r4ss reports and look through the figures
if (FALSE) {

  # Load r4ss list
  covar <- TRUE
  replist <- r4ss::SS_output(
    file.path(ss_home, mods[3]),
    verbose = FALSE,
    printstats = FALSE,
    covar = covar,
    hidewarn = TRUE
  )
  replist$Length_Comp_Fit_Summary

  # # Save report list
  # #saveRDS(replist, file = file.path(ss_home, paste0("r4ss_", mods[1], ".rds")))
  #
  # # Selectivity at length (estimated)
  # SS3_sel(list(replist), "", type = "Lsel", bin_width = 5, do_mat = FALSE) +
  #   coord_cartesian(xlim = c(40, 115), ylim = c(0, 1.1))
  #
  # # Selectivity at age (converted from size based, also show maturity ogive)
  # SS3_sel(list(replist), "", bin_width = 5)
  # .ggsave("sel_age.png", g, height = 6, width = 8)
  #
  # # Selectivity at age (converted from size based, also show maturity ogive)
  # SS3_sel(list(replist), "", bin_width = 5, scale_max_1 = TRUE)

  # Generate HTML report
  if (covar) {
    r4ss::SS_plots(replist, verbose = FALSE)
  } else {
    # Yield curve doesn't work with dogfish SRR when Hessian is not calculated?
    r4ss::SS_plots(replist, verbose = FALSE, plot = c(1:21, 23:26))
  }

  # Report and plot various output in R
  replist$estimated_non_dev_parameters |>
    select(Value, Parm_StDev) |>
    mutate(CV = abs(Parm_StDev/Value) |> round(2)) |>
    View()

  r4ss::SSplotSelex(replist, fleets = 4)
  r4ss::SSplotTimeseries(replist, subplot = 12)
  # r4ss::SSplotRecdevs(replist)
  r4ss::SSplotCatch(replist)
  r4ss::SSplotIndices(replist)
  r4ss::SSplotComps(replist)
  replist$likelihoods_used
}