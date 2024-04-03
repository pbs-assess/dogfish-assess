
model_dir <- "m37_IPHC_g4"
ss_home <- here::here("ss3")
ss_home <- "C:/users/qhuynh/Desktop/dogfish"

#### Fit ss3 model version 3.30.22.1 with custom compilation that fixes lognormal prior density function
fit_ss3 <- function(model_dir = "model1",
                    hessian = FALSE,
                    ss_home = here::here("ss3"),
                    max_phase) {
  dir_cur <- getwd()
  dir_run <- file.path(ss_home, model_dir)
  setwd(dir_run)
  on.exit(setwd(dir_cur))

  if (.Platform$OS.type == "unix") {
    cmd <- "./ss modelname ss"
  } else {
    cmd <- "ss.exe modelname ss"
  }

  if (!hessian) {
    cmd <- paste(cmd, "-nohess")
  }
  if (!missing(max_phase) && is.integer(max_phase)) {
    cmd <- paste(cmd, "-maxph", max_phase)
  }
  message("File directory: ", dir_run)
  message("Command: ", cmd)

  system(cmd)
}

# fit_ss3(model_dir, hessian = FALSE, ss_home = ss_home, max_phase = 10L)


mods <- c("A1", "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_exHS", "A6_IPHC", "A7_SYN", "B1_2005")
snowfall::sfInit(parallel = TRUE, cpus = length(mods))
snowfall::sfLapply(mods, fit_ss3, hessian = TRUE, ss_home = ss_home)
snowfall::sfStop()

# Load r4ss list
model_dir <- "B1_2005"
covar <- TRUE
replist <- r4ss::SS_output(
  file.path(ss_home, model_dir),
  verbose = FALSE,
  printstats = FALSE,
  covar = covar,
  hidewarn = TRUE
)
replist$Length_Comp_Fit_Summary


# Save report list
#saveRDS(replist, file = file.path(ss_home, paste0("r4ss_", model_dir, ".rds")))

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

r4ss::SSplotSelex(replist, fleets = 1)
r4ss::SSplotTimeseries(replist, subplot = 12)
r4ss::SSplotRecdevs(replist)
r4ss::SSplotCatch(replist)
r4ss::SSplotIndices(replist)
r4ss::SSplotComps(replist)
replist$likelihoods_used


