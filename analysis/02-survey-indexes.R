library(dplyr)
library(ggplot2)
source("ss3/99-utils.R")

theme_set(gfplot::theme_pbs())

ind_hbll_out <- readRDS("data/generated/geostat-ind-hbll-out.rds")
ind_syn_out <- readRDS("data/generated/geostat-ind-synoptic-lg.rds")
ind_iphc_out <- readRDS("data/generated/geostat-ind-iphc.rds")
ind_syn_out_f <- readRDS("data/generated/geostat-ind-synoptic-female.rds")
ind_syn_out_m <- readRDS("data/generated/geostat-ind-synoptic-male.rds")

bind_rows(
  mutate(ind_hbll_out, survey = "HBLL - abundance",
         Index = "Total"),
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

bind_rows(
  mutate(ind_hbll_out, survey = "HBLL - abundance",
         Index = "Total"),
  mutate(ind_syn_out, survey = "Synoptic trawl - biomass"),
  #mutate(ind_syn_out_f, survey = "Synoptic trawl - biomass",
  #                      Index = "Female"),
  #mutate(ind_syn_out_m, survey = "Synoptic trawl - biomass",
  #                      Index = "Male"),
  mutate(ind_iphc_out, survey = "IPHC - abundance",
         Index = "Total")
) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  #scale_colour_manual(values = c("red",  "blue", "black")) +
  facet_wrap(~survey, scales = "free_y", ncol = 1L) +
  coord_cartesian(ylim = c(0, NA)) +
  ylab("Relative abundance or biomass") +
  xlab("Year") +
  gfplot::theme_pbs()

ggsave("figs/outside-indexes2.png", width = 5.5, height = 8.0)

# Survey timing
s <- readRDS("data/raw/survey-sets_2023.rds")

sumry <- s %>%
  filter(!grepl("INS", survey_abbrev)) %>%
  summarise(n = n(), .by = c(survey_abbrev, year, month))

g <- ggplot(sumry, aes(year, month, fill = n)) +
  geom_tile(colour = "black") +
  geom_text(aes(label = n), size = 3) +
  facet_wrap(vars(survey_abbrev), ncol = 2, scales = "free_x") +
  scale_fill_gradient2() +
  gfplot::theme_pbs() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Month", fill = "Sets") +
  scale_y_continuous(breaks = 1:12)
ggsave("figs/survey_timing.png", g, height = 8, width = 10)



###



# IPHC - no hook competition
iphc <- readRDS("data/generated/geostat-ind-iphc_gfdata.rds") %>%
  mutate(fleet = "IPHC")

# HBLL - NB2 likelihood - no hook competition
hbll <- readRDS("data/generated/geostat-ind-hbll-out.rds") %>%
  filter(year != 2013) %>% # No survey in 2013
  mutate(fleet = "HBLL Outside") %>%
  select(-survey_abbrev)

# Synoptic Trawl
syn <- readRDS("data/generated/geostat-ind-synoptic-lg.rds") %>%
  mutate(fleet = "Synoptic")

hs_msa <- readRDS("data/generated/hs-msa-index.rds") %>%
  mutate(fleet = "HS MSA")

cpue <- readRDS("data/generated/geostat-spt-ind-cpue.rds") %>%
  mutate(fleet = "Bottom Trawl CPUE")

ind <- bind_rows(hbll, iphc, syn, hs_msa, cpue)


g <- ind %>%
  group_by(fleet) |>
  mutate(geo_mean = exp(mean(log(est)))) |>
  mutate(upr = upr / geo_mean) |>
  mutate(lwr = lwr / geo_mean) |>
  mutate(est = est / geo_mean) |>
  ggplot(aes(year, est)) +
  facet_wrap(vars(fleet), scales = "fixed", ncol = 2) +
  geom_point() +
  geom_line(linewidth = 0.25, linetype = 1, alpha = 0.3) +
  geom_linerange(aes(ymin = lwr, ymax = upr)) +
  gfplot::theme_pbs() +
  expand_limits(y = 0) +
  coord_cartesian(expand = FALSE, xlim = c(1980, 2025), ylim = c(0, 3.7)) +
  labs(x = "Year", y = "Index value")
g
ggsave("figs/indices-all.png", width = 6, height = 6, dpi = 200)

g + theme(axis.title.x = element_blank())
ggsave_optipng("figs/indices-all-sar.png", width = 5, height = 5, dpi = 300)
# COSEWIC ---------------------


ind$decade <- ind$year / 10


dd <- ind |> filter(!grepl("MSA", fleet))
fits <- dd |> split(dd$fleet) |>
  purrr::map_dfr(\(x) {
    x$decade <- x$year / 10
    m <- glm(est ~ decade, data = x, family = Gamma(link = "log"))
    est <- coef(m)[2]
    cc <- confint(m)
    lwr <- cc[2, 1]
    upr <- cc[2, 2]
    data.frame(lwr, est, upr, survey = unique(x$fleet))
  })
row.names(fits) <- NULL
fits

g1 <- ggplot(fits, aes(survey, y = 1-exp(est), ymin =1- exp(lwr), ymax = 1-exp(upr))) + geom_pointrange() +
  coord_flip() + xlab("") + ylab("Proportion decline per decade") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1), breaks = seq(0.1, 1, 0.2))

# times 5 to turn per decade into per 50 years, i.e. per generation
g2 <- ggplot(fits, aes(survey, y = 1-exp(est*5), ymin =1- exp(lwr*5), ymax = 1-exp(upr*5))) + geom_pointrange() +
  coord_flip() + xlab("") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1), breaks = seq(0.1, 1, 0.2)) +
  ylab("Proportion decline extrapolated\nto 50 years (one generation)") +
  geom_hline(yintercept = c(0.3, 0.5, 0.7), lty = 2, alpha = 0.3)+
  theme(axis.text.y = element_blank())

mults <- group_by(dd, fleet) |> summarise(decades = max(decade) - min(decade)) |>
  rename(survey = fleet)

# times by length of each survey for longest time series decline:

g3 <- left_join(fits, mults) |>
  mutate(
    lwr = 1 - exp(lwr * decades),
    upr = 1 - exp(upr * decades),
    est = 1 - exp(est * decades)
  ) |>
  mutate(yrs = 10 * round(decades, 1)) |>
  ggplot(aes(survey, y = est, ymin = lwr, ymax = upr)) +
  geom_pointrange() +
  coord_flip() + xlab("") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1), breaks = seq(0.1, 1, 0.2)) +
  ylab("Proportion decline\nover length of time series") +
  geom_hline(yintercept = c(0.3, 0.5, 0.7), lty = 2, alpha = 0.3) +
  geom_text(mapping = aes(x = survey, y = upr - 0.02, label = paste0(yrs, " years")), hjust = 1) +
  theme(axis.text.y = element_blank())
g3

cowplot::plot_grid(g1, g2, g3, align = "h", nrow = 1, rel_widths = c(1.41, 1, 1), labels = "auto", label_x = c(0.37, .11, .11), label_fontface = "plain",
  label_y = 0.99)
ggsave("figs/cosewic-decline-indexes.png", width = 9.5, height = 3.5)

if (FALSE) {
  system("optipng -strip all figs/cosewic-decline-indexes.png")
}

xx <- fits |> mutate(est = 1 - exp(est), lwr = 1 - exp(lwr), upr = 1 - exp(upr))

s_rate <- xx |>  filter(survey == "Synoptic") |> pull(est)
s_rate <- as.character(round(s_rate * 100, 0))

h_rate <- xx |>  filter(survey == "HBLL Outside") |> pull(est)
h_rate <- as.character(round(h_rate * 100, 0))

i_rate <- xx |>  filter(survey == "IPHC") |> pull(est)
i_rate <- as.character(round(i_rate * 100, 0))

c_rate <- xx |>  filter(survey == "Bottom Trawl CPUE") |> pull(est)
c_rate <- as.character(round(c_rate * 100, 0))

file.remove("values/survey-declines.tex")
write_tex(s_rate, "DeclineSyn", "survey-declines.tex")
write_tex(h_rate, "DeclineHBLL", "survey-declines.tex")
write_tex(c_rate, "DeclineCPUE", "survey-declines.tex")
write_tex(i_rate, "DeclineIPHC", "survey-declines.tex")


# Sablefish offshore trap index


# ind <- gfdata::get_survey_index("044")
# saveRDS(ind, "data/raw/gfdata-survey-indexes.rds")
ind <- readRDS("data/raw/gfdata-survey-indexes.rds")
ind |>
  filter(survey_abbrev == "SABLE RAND") |>
  mutate(geo_mean = exp(mean(log(biomass)))) |>
  mutate(upperci = upperci / geo_mean) |>
  mutate(lowerci = lowerci / geo_mean) |>
  mutate(biomass = biomass / geo_mean) |>
  ggplot(aes(year, biomass)) +
  geom_pointrange(alpha = 0.9, mapping = aes(ymin = lowerci, ymax = upperci), size = 0.4, pch = 21) +
  # geom_line() +
  gfplot::theme_pbs() +
  scale_y_continuous(limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))) +
  ylab("Relative biomass density") + xlab("Year") +
  geom_smooth(se = FALSE, formula = y ~ x, method = "gam",
    method.args = list(family = Gamma(link = "log")), colour = "blue")
ggsave_optipng("figs/sablefish-index.png", width = 5.5, height = 3.3)
