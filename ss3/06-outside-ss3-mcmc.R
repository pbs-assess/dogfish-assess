
library(adnuts)

# Run MCMC
#ss_home <- here::here("ss3")
ss_home <- "C:/users/quang/Desktop/dogfish"

SS_dir <- c("A1", "B2_2010step")


for(i in 1:length(SS_dir)) {

  # Fit MPD
  #system("ss3.exe -nox -nohess")
  par_base <- R2admb::read_pars(file.path(ss_home, SS_dir[i], "ss"))

  set.seed((i + 12) * 100)
  init <- lapply(1:2, function(chains) {
    lapply(names(par_base$coeflist), function(x) {
      val <- par_base$coeflist[[x]]
      if (x != "Fcast_recruitments") val <- val + rnorm(length(val), 0, 0.1)
      return(val)
    })
  })

  message("Sampling", SS_dir[i])
  tictoc::tic()
  x <- sample_nuts(model = "ss",
                   path = file.path(ss_home, SS_dir[i]),
                   iter = 3500,
                   init = init,
                   warmup = 1000,
                   thin = 5,
                   chains = 2,
                   cores = 2,
                   verbose = FALSE,
                   skip_unbounded = FALSE,
                   admb_args = "modelname ss",
                   seeds = c(4, 10))
  tictoc::toc()
  message("Done.\n\n")
  saveRDS(x, file = file.path(ss_home, paste0("adnuts_", SS_dir[i], ".rds")))
}

### Shinystan
#x <- readRDS(file.path(ss_home, paste0("adnuts_", SS_dir[i], ".rds")))
#adnuts::launch_shinyadmb(x)

### Run mceval - run separately from MCMC
# Configure starter.ss to return Report.sso for every 5th iteration to get 200 posterior samples
# Also need posterior.sso
#MSEtool::setup(length(SS_dir))
#sfLapply(SS_dir, function(x) {
#  setwd(paste0("C:/users/qhuynh/Desktop/SS3/", x, "_mceval"))
#  system("ss.exe -nox -mceval", show.output.on.console = FALSE)
#})
#sfStop()