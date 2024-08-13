(cores <- parallel::detectCores())
files_per_core <- 5
setwd("figs")
system(paste0(
  "find -X . -name '*.png' -print0 | xargs -0 -n ",
  files_per_core, " -P ", cores, " /opt/homebrew/bin/optipng -strip all"
))
setwd("..")
