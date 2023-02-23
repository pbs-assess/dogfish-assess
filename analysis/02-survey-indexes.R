library(dplyr)
library(ggplot2)

ind_hbll_out <- readRDS("data/generated/geostat-ind-hbll-out.rds")
ind_syn_out <- readRDS("data/generated/geostat-ind-synoptic.rds")
ind_iphc_out <- readRDS("data/generated/geostat-ind-iphc.rds")

bind_rows(
  mutate(ind_hbll_out, survey = "HBLL - abundance"),
  mutate(ind_syn_out, survey = "Synoptic trawl - biomass"),
  mutate(ind_iphc_out, survey = "IPHC - abundance")
) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  facet_wrap(~survey, scales = "free_y", ncol = 1L) +
  coord_cartesian(ylim = c(0, NA)) +
  ylab("Relative abundance or biomass") +
  xlab("Year") +
  gfplot::theme_pbs()

ggsave("figs/outside-indexes.png", width = 5.5, height = 8.0)
