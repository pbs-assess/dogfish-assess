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

