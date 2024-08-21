library(adnuts)

# Run MCMC
ss_home <- here::here("ss3")
#ss_home <- "C:/users/quang/Desktop/dogfish"

# SS_dir <- c("A1", "B2_2010step")[1]

SS_dir <- c(
  # "A1",
  "A0",
  #"A2_USgrowth",
  #"A3_highmat",
  #"A4_USgrowth_highmat",
  #"A5_highdiscard",
  #"A6_IPHC+CPUE",
  #"A7_SYNonly",
  # "A8_HBLLonly",
  #"A9_lowM", "A10_highM",
  #"A11_low_zfrac",
  #"A12_high_zfrac",
  #"A13_extraSD",
  # "B1_1990inc",
  "B2_2010step"
  # "B3_2005step",
  # "B4_1990inc_lowM",
  #"B5_2010step_lowM"
)


fit_ss3 <- function(model_dir = "model1",
  hessian = FALSE,
  ss_home = here::here("ss3"),
  max_phase,
  extra_args = "") {
  dir_cur <- getwd()
  dir_run <- file.path(ss_home, model_dir)
  setwd(dir_run)
  on.exit(setwd(dir_cur))

  if (.Platform$OS.type == "unix") {
    if (Sys.info()[["user"]] == "seananderson")
      cmd <- "ss modelname ss"
    else
      cmd <- "/home/anderson/src/ss3_opt modelname ss"
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

# library(future)
# plan(multicore, workers = 10)

# furrr::future_walk(5:length(SS_dir), \(i) {
purrr::walk(TORUN, \(i) {
  print(i)
# for(i in 1:length(SS_dir)) {

  # Fit MPD
  #system("ss3.exe -nox -nohess modelname ss")

  # fit_ss3(SS_dir[i], hessian = FALSE, ss_home = ss_home, extra_args = "-hbf")
  fit_ss3(SS_dir[i], hessian = TRUE, ss_home = ss_home)

  if (Sys.info()[["user"]] == "seananderson") {
    system(paste0("cp /usr/local/bin/ss ss3/", SS_dir[i], "/"))
  } else {
    system(paste0("cp /home/anderson/src/ss3_opt ss3/", SS_dir[i], "/"))
  }

  fn_exe <- if (Sys.info()[["user"]] == "seananderson") "ss" else "ss3_opt"
  #
  # cmd <- paste0("cd ", paste0("ss3/", SS_dir[i]), " && ", fn_exe,
  #   " -hbf 1 -nox -iprint 200 -mcmc 15 -hess_step 5 ",
  #   "-binp ss.bar")
  # system(cmd)
  # xx <- readLines(file.path("ss3", SS_dir[i], "ss.log"))
  # nohess <- any(grepl("^Hessian does not appear to be positive definite.$", xx))

  # if (nohess) {
  #   fit_ss3(SS_dir[i], hessian = FALSE, ss_home = ss_home)
  # }

  # cmd <- paste0("cd ",  paste0("ss3/", SS_dir[i]), " && ",
  #   fn_exe, " -hbf 1 -nox -iprint 200 -mcmc 15")
  # system(cmd)

  par_base <- R2admb::read_pars(file.path(ss_home, SS_dir[i], "ss"))
  # npar <- par_base$npar
  # vcov <- par_base$vcov[1:npar, 1:npar]

  # CHAINS <- 1
  # adapt_delta <- 0.95
  # nuts_initial <- sample_nuts(
  #   model = fn_exe,
  #   path = file.path(ss_home, SS_dir[i]),
  #   iter = 200,
  #   seeds = seq_len(CHAINS),
  #   chains = CHAINS,
  #   cores = CHAINS,
  #   warmup = 50,
  #   verbose = TRUE,
  #   admb_args = "modelname ss",
  #   control = list(
  #     metric = "mle",
  #     adapt_delta = adapt_delta)
  # )


  # mass <- nuts_initial$covar_est

  # x <- sample_nuts(
  #   model = fn_exe,
  #   path = file.path(ss_home, SS_dir[i]),
  #   iter = 1000,
  #   seeds = seq_len(CHAINS),
  #   chains = CHAINS,
  #   cores = CHAINS,
  #   warmup = 200,
  #   verbose = TRUE,
  #   admb_args = "modelname ss",
  #   control = list(
  #     metric = "mle",
  #     adapt_delta = adapt_delta)
  # )

  CHAINS <- 10
  set.seed((i + 12) * 100)
  init <- lapply(1:CHAINS, function(chains) {
    lapply(names(par_base$coeflist), function(x) {
      val <- par_base$coeflist[[x]]
      if (x != "Fcast_recruitments") val <- val + rnorm(length(val), 0, 0.1)
      return(val)
    })
  })

  message("Sampling", SS_dir[i])
  tictoc::tic()
#
  # metric <- if (!nohess) "mle" else "unit"
  # metric <- "mle"
  x <- sample_nuts(model = fn_exe,
                   path = file.path(ss_home, SS_dir[i]),
                   iter = 800,
                   # iter = 100,
                   # init = init,

                   # control = list(metric = "mle", adapt_delta = 0.8),
                   warmup = 300,
                   # warmup = 20,
                   thin = 1,
                   chains = CHAINS,
                   cores = CHAINS,
                   verbose = TRUE,
                   # skip_unbounded = FALSE,
                   admb_args = "modelname ss",
                   seeds = seq_len(CHAINS))
#   #
#   # CHAINS <- 3
#   # x <- sample_rwm(
#   #   model = "ss3",
#   #   path = file.path(ss_home, SS_dir[i]),
#   #   iter = 1e6 + 10000,
#   #   # init = init,
#   #   # control = list(metric = vcov),
#   #   warmup = 10000,
#   #   skip_monitor = FALSE,
#   #   thin = 1000,
#   #   chains = CHAINS,
#   #   cores = CHAINS,
#   #   # skip_unbounded = FALSE,
#   #   admb_args = "modelname ss",
#   #   seeds = seq_len(CHAINS))
#
  tictoc::toc()
#   message("Done.\n\n")
  saveRDS(x, file = file.path(ss_home, paste0("adnuts_", SS_dir[i], ".rds")))
})

### Shinystan
#x <- readRDS(file.path(ss_home, paste0("adnuts_", SS_dir[i], ".rds")))
#adnuts::launch_shinyadmb(x)

### Run mceval - run separately from MCMC
# Configure starter.ss to return Report.sso for every 5th iteration to get 200 posterior samples
# Also need posterior.sso
# library(snowfall)
# sfInit(parallel = TRUE, cpus = length(SS_dir))
# sfExport("ss_home")
# sfLapply(SS_dir, function(x) {
#   setwd(file.path(ss_home, paste0(x, "_mceval")))
#   system("ss.exe -nox -mceval modelname ss", show.output.on.console = FALSE)
# })
# sfStop()
#
# mcmc_output <- lapply(
#
# )
