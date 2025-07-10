# Set French language option
FRENCH <- TRUE

# Set decimal option for French
if (FRENCH) {
  old_dec <- options()$OutDec
  options(OutDec = ",")
}

# Translation helper function
tr <- function(english, french) {
  if (FRENCH) french else english
}

library(snowfall)
library(dplyr)
library(ggplot2)
theme_set(gfplot::theme_pbs())

ss_home <- here::here("ss3")
model_dir <- c("A0", "A9_lowM", "B2_2010step")

# Do retro
ypeel <- seq(0, -7, -1)
# for(i in model_dir) {
#   r4ss::retro(
#     file.path(ss_home, i),
#     years = ypeel,
#     exe = "ss",
#     extras = c("-nohess modelname ss")
#   )
# }

# sfInit(parallel = TRUE, cpus = length(model_dir))
# sfExport(list = c("ss_home", "ypeel", "model_dir"))
# sfLapply(model_dir, function(i) {

future::plan(future::multisession, workers = length(ypeel))
lapply(model_dir, function(i) {
  r4ss::retro(
    file.path(ss_home, i),
    years = ypeel,
    exe = "ss",
    extras = c("-nohess modelname ss"), skipfinished = FALSE
  )
})
# sfStop()

# Make figures
source("ss3/ss3_functions.R")

scale_colour_discrete <- function(...) {
  scale_colour_viridis_d(option = "D")
}

# Create appropriate figure directories
if (FRENCH) {
  dir.create("figs-french", showWarnings = FALSE)
  dir.create("figs-french/ss3/retro", showWarnings = FALSE, recursive = TRUE)
  fig_dir <- "figs-french"
} else {
  dir.create("figs/ss3/retro/", showWarnings = FALSE, recursive = TRUE)
  fig_dir <- "figs"
}

# Helper function for figure paths
fig_path <- function(filename) {
  file.path(fig_dir, filename)
}

for (i in 1:length(model_dir)) {
  ret <- r4ss::SSgetoutput(
    dirvec = file.path(ss_home, model_dir[i], "retrospectives", paste0("retro", ypeel))
  )
  g <- SS3_retro(ret, french = FRENCH)
  code <- substr(model_dir[i], 1, 2)
  ggsave(fig_path(paste0("ss3/retro/ret_", code, ".png")), height = 8, width = 5)
}

if (FALSE) {
  setwd("figs/ss3/retro/")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    1, " -P ", 6, " /opt/homebrew/bin/optipng -strip all"
  ))
  setwd(here::here())
}

# Reset decimal separator
if (FRENCH) {
  options(OutDec = old_dec)
}
