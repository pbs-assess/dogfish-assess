mround <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), round(x, digits))
}
dir.create("values", showWarnings = FALSE)
write_tex <- function(x, macro, file = "values.tex", append = TRUE) {
  paste0("\\newcommand{\\", macro, "}{", x, "}") |>
    readr::write_lines(paste0("values/", file), append = append)
}
ggsave_optipng <- function(filename, width = NA, height = NA, ...) {
  ggsave(filename, width = width, height = height, ...)
  cmd <- paste0("optipng -strip all ", filename)
  if (system("which optipng") == 0L) system(cmd)
}

get_dead_catch <- function(dir = "A0", years = 2018:2023) {
  d <- r4ss::SS_readdat(paste0("ss3/", dir, "/data_echo.ss_new"), verbose = FALSE)
  catch <- d$catch |> filter(year %in% years)
  # fleets 5,6,8,9,10 are a *counts* not weight
  # turn into weight based on 3.07 kg / dogfish:
  catch <- catch |>
    mutate(catch = ifelse(fleet %in% c(5, 6, 7, 9, 10), catch * 3.07, catch))
  ctl <- r4ss::SS_readctl(paste0("ss3/", dir, "/control.ss_new"))
  i <- grepl("_Mult:", row.names(ctl$MG_parms))
  mult <- ctl$MG_parms[i,] |> select(catch_multiplier = INIT)
  mult$fleet <- as.integer(gsub("^Catch_Mult:_", "", row.names(mult)))
  ret <- left_join(catch, mult, by = join_by(fleet)) |>
    mutate(dead_catch = catch / catch_multiplier) |>
    group_by(year) |>
    summarise(total_dead_catch = sum(dead_catch))
  data.frame(model = dir, dead_catch = mean(ret$total_dead_catch))
}