(cores <- parallel::detectCores())
files_per_core <- 5
setwd("figs")
system(paste0(
  "find -X . -name '*.png' -print0 | xargs -0 -n ",
  files_per_core, " -P ", cores, " optipng -strip all"
))
setwd(here::here())

# if (FALSE) {
  # system('rsync -av "figs/ss3/" "../dogfish-assess-resdoc/figs/ss3"')
  system('rsync -av "figs/" "../dogfish-assess-resdoc/figs"')
  system('rsync -av "values/" "../dogfish-assess-resdoc/values"')
  system('rsync -av "tables/" "../dogfish-assess-resdoc/tables"')
# }

if (FALSE) {
  setwd("../dogfish-assess-resdoc/figs/")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " optipng -strip all"
  ))
  setwd(here::here())
}