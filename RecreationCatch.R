

# library -----------------------------------------------------------------
library(tidyverse)

#https://www.researchgate.net/figure/DFO-management-areas-of-the-Pacific-Region-Fisheries-and-Oceans-Canada-2004_fig1_242162660

# load raw data -----------------------------------------------------------
d <- readxl::read_excel("data/raw/iREC/iREC estimates Jul 2012 to Dec 2023 29012024.xlsx") |>
  filter(ITEM == "Dogfish")

#note: #estimate is in pieces, not kg
#see raw data for README


# summarise by year -------------------------------------------------------
names(d) <- tolower(names(d))
glimpse(d)
d$estimate_tonnes <- d$estimate/1000

d |>
  group_by(year, month, area, disposition) |>
  summarise(sum = sum(estimate))

d |>
  group_by(year, month, area, disposition) |>
  summarise(sum = sum(estimate))


d |>
  group_by(year) |>
  summarise(sum = sum(estimate))
