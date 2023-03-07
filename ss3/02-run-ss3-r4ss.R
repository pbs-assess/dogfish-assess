

ssdir <- "model1"

fit_ss3 <- function(ssdir = "model1", hessian = FALSE) {
  dir_cur <- getwd()
  dir_run <- here::here(paste0("ss3/", ssdir))
  setwd(dir_run)
  on.exit(setwd(dir_cur))

  cmd <- "ss.exe -nox"
  if (!hessian) {
    cmd <- paste(cmd, "-nohess")
  }
  message("File directory: ", dir_run)
  message("Command: ", cmd)

  system(cmd)
}
fit_ss3(ssdir, hessian = FALSE)

# r4ss
replist <- r4ss::SS_output(here::here(paste0("ss3/", ssdir)),
                           #verbose = FALSE,
                           #printstats = FALSE,
                           hidewarn = TRUE)
r4ss::SS_plots(replist,
               verbose = FALSE)


