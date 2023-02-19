library(dplyr)

iphclengths <- read.csv("C:/Dogfish_stitch/IPHC data from NOAA/IPHC_dogfish_lengths2021.csv") %>%
  filter(reg_area == "2B"
  )
glimpse(iphclengths)

