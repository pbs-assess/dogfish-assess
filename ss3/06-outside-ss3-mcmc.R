
library(adnuts)

# Run MCMC
ss_home <- here::here("ss3")
#ss_home <- "C:/users/quang/Desktop/dogfish"

SS_dir <- c("A1", "B2_2010step")[1]

for(i in 1:length(SS_dir)) {

  # Fit MPD
  #system("ss3.exe -nox -nohess modelname ss")
  par_base <- R2admb::read_pars(file.path(ss_home, SS_dir[i], "ss"))

  set.seed((i + 12) * 100)
  init <- lapply(1:10, function(chains) {
    lapply(names(par_base$coeflist), function(x) {
      val <- par_base$coeflist[[x]]
      if (x != "Fcast_recruitments") val <- val + rnorm(length(val), 0, 0.1)
      return(val)
    })
  })

  message("Sampling", SS_dir[i])
  tictoc::tic()
  # system("cp /usr/local/bin/ss ss3/A1/")
  # fit_ss3(mods[1], hessian = TRUE, ss_home = ss_home, extra_args = '-hbf')
  # system("cp /usr/bin/ss3/build/ss3 ss3/A1")
  x <- sample_nuts(model = "ss3",
                   path = file.path(ss_home, SS_dir[i]),
                   iter = 350L,
                   init = init,
                   control = list(metric = 'mle', adapt_mass_dense = TRUE),
                   warmup = 175L,
                   thin = 1L,
                   chains = 10,
                   cores = 10,
                   verbose = TRUE,
                   skip_unbounded = FALSE,
                   admb_args = "modelname ss",
                   seeds = seq_len(10))
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
library(snowfall)
sfInit(parallel = TRUE, cpus = length(SS_dir))
sfExport("ss_home")
sfLapply(SS_dir, function(x) {
  setwd(file.path(ss_home, paste0(x, "_mceval")))
  system("ss.exe -nox -mceval modelname ss", show.output.on.console = FALSE)
})
sfStop()

mcmc_output <- lapply(

)
