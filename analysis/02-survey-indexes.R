library(dplyr)
library(ggplot2)

ind_hbll_out <- readRDS("data/generated/geostat-ind-hbll-out.rds")
ind_syn_out <- readRDS("data/generated/geostat-ind-synoptic.rds")

bind_rows(
  mutate(ind_hbll_out, survey = "HBLL OUT"),
  mutate(ind_syn_out, survey = "Synoptic trawl")
) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  facet_wrap(~survey, scales = "free_y") +
  coord_cartesian(ylim = c(0, NA)) +
  ylab("Relative abundance or biomass") +
  xlab("Year") +
  gfplot::theme_pbs()
