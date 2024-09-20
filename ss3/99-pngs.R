(cores <- parallel::detectCores())
files_per_core <- 5
setwd("figs")
system(paste0(
  "find -X . -name '*.png' -print0 | xargs -0 -n ",
  files_per_core, " -P ", cores, " /opt/homebrew/bin/optipng -strip all"
))
setwd(here::here())

if (FALSE) {
  system('rsync -av "figs/ss3/" "../dogfish-assess-resdoc/figs/ss3"')
  system('rsync -av "values/" "../dogfish-assess-resdoc/values"')
}

if (FALSE) {
  setwd("../dogfish-assess-resdoc/figs/")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " /opt/homebrew/bin/optipng -strip all"
  ))
  setwd(here::here())
}