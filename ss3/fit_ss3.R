fit_ss3 <- function(
    model_dir = "model1",
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