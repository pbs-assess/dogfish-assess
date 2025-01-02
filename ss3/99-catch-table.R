library(dplyr)
library(knitr)

d <- readr::read_csv(here::here("figs/dogfish-catch-weight.csv"))
d <- d |> mutate(gear = factor(gear,
  levels = c("Bottom trawl", "Midwater trawl", "Hook and line/longline", "Trap"))) |>
  arrange(gear, year) |> select(-discarded_t_avg_dogfish)

options(knitr.kable.NA = '')
d |>
  mutate(year = as.character(year)) |>
  knitr::kable(digits = 2, col.names = c("Year", "Gear", "Landings (t)", "Discards (t)", "Discards (count)"), caption = "Landings and release data for Dogfish in PFMA 3CD5ABCDE from 1996 to 2023. Longline and trap (combined) discards are entered into the model as counts. Years are calendar years.", format = "latex", booktabs = TRUE, linesep = "", longtable = TRUE, font_size = NULL, escape = FALSE, label = "tab:catch") |>
  kableExtra::kable_styling(latex_options = c("hold_position", "repeat_header"))
