library(dplyr)
library(ggplot2)

d <- readr::read_rds(here::here("data/generated/catch-dec2024.rds"))
glimpse(d)

# ggplot(d, aes(year, landed_kg))

bottom_trawl_gear <- c("Trawl + hook and line", "Trawl", "Bottom trawl", "Unknown/trawl")
d$gear[d$gear %in% bottom_trawl_gear] <- "Bottom trawl"
d$gear[d$gear %in% "Hook and line"] <- "Hook and line/longline"

table(d$gear)

d <- d |> 
  mutate(gear = factor(gear, levels = c("Bottom trawl", "Midwater trawl", "Hook and line/longline", "Trap"))) |> 
  arrange(gear, year) |>
  mutate(
    landed_t = landed_kg / 1000,
    discarded_t = discarded_kg / 1000
  )

d <- select(d, year, gear, landed_t, discarded_t, discarded_pcs, discarded_t_avg_dogfish)

options(knitr.kable.NA = '')
# sink("tables/catch.tex")
out <- d |> 
  mutate(year = as.character(year)) |> 
  knitr::kable(digits = 1, col.names = c("Year", "Gear", "Landed (t)", "Released (t)", "Released (count)", "Released (t)*"), caption = "Landings and release data for Dogfish in PFMA 3CD5ABCDE from 1996 to 2023. Longline and trap (combined) discards will be entered in the model as counts. The final discard weight column with an * is shown for illustrative purposes at this meeting with an average dogfish weight of 3.07 kg (6.77 lbs); it will not be used in model fitting. Years are calendar years.", format = "latex", linesep = "", longtable = TRUE, booktabs = TRUE, escape = TRUE)
# sink()

readr::write_lines(out, "tables/catch.tex")
