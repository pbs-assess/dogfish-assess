library(dplyr)
library(ggplot2)

ind_hbll_out <- readRDS("data/generated/geostat-ind-hbll-out.rds")
# ind_syn_out <- readRDS("data/generated/geostat-ind-synoptic.rds")
ind_iphc_out <- readRDS("data/generated/geostat-ind-iphc.rds")
ind_syn_out_f <- readRDS("data/generated/geostat-ind-synoptic-female.rds")
ind_syn_out_m <- readRDS("data/generated/geostat-ind-synoptic-male.rds")

bind_rows(
  mutate(ind_hbll_out, survey = "HBLL - abundance",
         Index = "Total"),
  # mutate(ind_syn_out, survey = "Synoptic trawl - biomass"),
  mutate(ind_syn_out_f, survey = "Synoptic trawl - biomass",
                        Index = "Female"),
  mutate(ind_syn_out_m, survey = "Synoptic trawl - biomass",
                        Index = "Male"),
  mutate(ind_iphc_out, survey = "IPHC - abundance",
         Index = "Total")
) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = Index)) +
  geom_pointrange() +
  scale_colour_manual(values = c("red",  "blue", "black")) +
  facet_wrap(~survey, scales = "free_y", ncol = 1L) +
  coord_cartesian(ylim = c(0, NA)) +
  ylab("Relative abundance or biomass") +
  xlab("Year") +
  gfplot::theme_pbs()

ggsave("figs/outside-indexes.png", width = 5.5, height = 8.0)
