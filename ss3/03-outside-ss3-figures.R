
library(tidyverse)

source("ss3/ss3_functions.R")

# Compare SS models
ss_home <- here::here("ss3")

mods <- c("model4a_estH_dwLen", "model4b_estH_dwLen_lowM", "model4d_estH_dwLen_recdev", "model4e_estH_dwLen_exIPHC_recdev")
model_name <- c("(1) M = 0.07", "(2) M = 0.06", "(3) Est rec dev", "(4) rec dev + exIPHC")

multi_rep <- lapply(mods, function(x) {
  r4ss::SS_output(file.path(ss_home, x),
                  verbose = FALSE,
                  printstats = FALSE,
                  hidewarn = TRUE)
})
saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_03.21.2023.rds"))
multi_rep <- readRDS(file = file.path(ss_home, "multi_rep_03.21.2023.rds"))

### Tables
likelihoods <- lapply(1:length(mods), function(i) {
  x <- multi_rep[[i]][["likelihoods_used"]] %>%
    mutate(Component = rownames(.)) %>%
    select(Component, values)
  colnames(x)[2] <- model_name[i]
  return(x)
}) %>%
  Reduce(dplyr::left_join, .)
readr::write_excel_csv(likelihoods, file = "ss3/tables/likelihoods.csv")

# Parameters R0, steepness, reference points
pars_report <- Map(pars_fn, multi_rep, model_name) %>%
  Reduce(left_join, .)
readr::write_excel_csv(pars_report, file = "ss3/tables/pars_report.csv")

### r4ss Plot comparisons
multi_rep %>%
  r4ss::SSsummarize() %>%
  r4ss::SSplotComparisons(legendlabels = mods)

### Custom ggplots

# Plot index
g <- Map(SS3_index, multi_rep, model_name, figure = FALSE) %>%
  bind_rows() %>%
  ggplot(aes(Yr, Obs, ymin = exp(log(Obs) - 1.96 * SE), ymax = exp(log(Obs) + 1.96 * SE))) +
  geom_linerange() +
  geom_point(shape = 1) +
  geom_line(aes(y = Exp, colour = Fleet_name)) +
  expand_limits(y = 0) +
  facet_grid(vars(Fleet_name), vars(scen), scales = "free_y") +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Index") +
  guides(colour = "none") +
  theme(panel.spacing = unit(0, "in"))
ggsave("figs/ss3/index_fit.png", g, height = 4, width = 6)

# Plot SR
g <- SS3_SR(multi_rep, model_name) +
  labs(x = "Spawning output")
ggsave("figs/ss3/srr.png", g, height = 4, width = 6)

# Plot recruitment
g <- SS3_recruitment(multi_rep, model_name)
ggsave("figs/ss3/recruitment.png", g, height = 4, width = 6)

g <- SS3_recruitment(multi_rep, model_name, dev = TRUE)
ggsave("figs/ss3/recruit_dev.png", g, height = 4, width = 6)

# Plot SSB
g <- SS3_B(multi_rep, model_name) +
  guides(linetype = 'none') +
  labs(y = "Spawning output")
ggsave("figs/ss3/spawning_est.png", g, height = 4, width = 6)

g <- SS3_B(multi_rep, model_name, type = "SSB0") +
  guides(linetype = 'none') +
  labs(y = "Spawning depletion")
ggsave("figs/ss3/depletion_est.png", g, height = 4, width = 6)

g <- SS3_B(multi_rep, model_name, type = "SSBMSY") +
  guides(linetype = 'none') +
  geom_hline(yintercept = 0.4, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 3)
ggsave("figs/ss3/bbmsy_est.png", g, height = 4, width = 6)

g <- SS3_B(multi_rep, model_name, type = "B") +
  labs(linetype = 'Sex') +
  theme(legend.position = "bottom")
ggsave("figs/ss3/biomass_est.png", g, height = 5, width = 6)

# Maturity at age/length
g1 <- multi_rep[[1]]$endgrowth %>%
  filter(Sex == 1) %>%
  select(int_Age, `Len_Mat`) %>%
  ggplot(aes(int_Age, `Len_Mat`)) +
  #geom_point() +
  geom_line() +
  labs(x = "Age", y = "Maturity")

g2 <- multi_rep[[1]]$biology %>%
  ggplot(aes(Len_mean, Mat)) +
  #geom_point() +
  geom_line() +
  labs(x = "Length", y = "Maturity")
g <- cowplot::plot_grid(g2, g1)
ggsave("figs/ss3/maturity.png", g, height = 2, width = 6)

# Fecundity at age/length
g1 <- multi_rep[[1]]$endgrowth %>%
  filter(Sex == 1) %>%
  select(int_Age, `Mat*Fecund`, Len_Mat) %>%
  ggplot(aes(int_Age, `Mat*Fecund`/Len_Mat)) +
  #geom_point() +
  geom_line() +
  labs(x = "Age", y = "Fecundity")

g2 <- multi_rep[[1]]$biology%>%
  ggplot(aes(Len_mean, Fec)) +
  #geom_point() +
  geom_line() +
  labs(x = "Length", y = "Fecundity")
g <- cowplot::plot_grid(g2, g1)
ggsave("figs/ss3/fecundity.png", g, height = 2, width = 6)

# Spawning output
g1 <- multi_rep[[1]]$endgrowth %>%
  filter(Sex == 1) %>%
  select(int_Age, `Mat*Fecund`) %>%
  ggplot(aes(int_Age, `Mat*Fecund`)) +
  #geom_point() +
  geom_line() +
  labs(x = "Age", y = "Spawning output")

g2 <- multi_rep[[1]]$biology%>%
  ggplot(aes(Len_mean, `Mat*Fec`)) +
  #geom_point() +
  geom_line() +
  labs(x = "Length", y = "Spawning output")
g <- cowplot::plot_grid(g2, g1)
ggsave("figs/ss3/spawning_age.png", g, height = 2, width = 6)

# Selectivity
g <- SS3_sel(multi_rep, model_name)
ggsave("figs/ss3/sel_age.png", g, height = 6, width = 6)

g <- SS3_sel(multi_rep, model_name, type = "Lsel")
ggsave("figs/ss3/sel_len.png", g, height = 6, width = 6)

# Mean length
fleet_int <- c(1:4, 6)
g <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = fleet_int)) %>%
  bind_rows() %>%
  mutate(FleetName = factor(FleetName, levels = multi_rep[[1]]$FleetNames[fleet_int])) %>%
  ggplot(aes(Yr, Exp, linetype = Sex, colour = FleetName, shape = Sex)) +
  facet_grid(vars(FleetName), vars(scen), scales = "free_y") +
  geom_point(aes(y = Obs)) +
  geom_line() +
  #geom_line(linewidth = 1) +
  labs(x = "Year", y = "Mean length") +
  theme(panel.spacing = unit(0, "in"),
        legend.position = "bottom") +
  scale_shape_manual(values = c(16, 1)) +
  guides(colour = "none")
ggsave("figs/ss3/mean_length.png", g, height = 6, width = 6)


# Length comps
heights <- c(6, 6, 4, 5, NA, 8) + 1
xlim <- list(c(50, 125),
             c(25, 100),
             c(0, 125),
             c(40, 100),
             NA,
             c(30, 110))
for(ff in fleet_int) {
  len <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = ff, mean_length = FALSE)) %>%
    bind_rows() %>%
    mutate(Obs = ifelse(Sex == "Male", -1 * Obs, Obs),
           Exp = ifelse(Sex == "Male", -1 * Exp, Exp))

  g <- len %>%
    filter(scen == model_name[1]) %>%
    ggplot(aes(Bin, Obs, fill = Sex)) +
    geom_col(colour = "grey60", width = 4, alpha = 0.75) +
    geom_line(data = len, aes(y = Exp, linetype = Sex, colour = scen)) +
    facet_wrap(vars(Yr), ncol = 4) +
    scale_y_continuous(labels = abs) +
    theme(legend.position = "bottom",
          panel.spacing = unit(0, "in")) +
    scale_fill_manual(values = c("grey80", "white")) +
    xlim(xlim[[ff]]) +
    labs(x = "Length", y = "Proportion", colour = "Model") +
    guides(colour = guide_legend(nrow = 2)) +
    ggtitle(unique(len$FleetName))
  ggsave(paste0("figs/ss3/len_comp_fleet_", ff, ".png"), g, height = heights[ff], width = 6)
}

# Exploitation and apical F
g <- SS3_F(multi_rep, model_name)
ggsave("figs/ss3/harvest_rate.png", g, height = 4, width = 6)

g <- SS3_F(multi_rep, model_name, type = "fleet")
ggsave("figs/ss3/fleet_F.png", g, height = 4, width = 6)

g <- SS3_F(multi_rep, model_name, type = "FMSY") +
  labs(y = expression(U/U[MSY]))
ggsave("figs/ss3/harvest_rate_msy.png", g, height = 4, width = 6)

# Kobe
g <- SS3_Kobe(multi_rep, model_name) +
  labs(y = expression(U/U[MSY])) +
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 7.5))
ggsave("figs/ss3/kobe.png", g, height = 4, width = 6)

# Yield curve
g <- SS3_yieldcurve(multi_rep, model_name) +
  coord_cartesian(xlim = c(0, 0.03))
ggsave("figs/ss3/yieldcurve_F.png", g, height = 3, width = 5)

g <- SS3_yieldcurve(multi_rep, model_name, xvar = "SB0")
ggsave("figs/ss3/yieldcurve_depletion.png", g, height = 3, width = 6)


# Compare unfished vs. current length comp in a single OM
g <- lapply(c(1:4, 6), function(ff) {
  SS3_lencomp(multi_rep[[1]], fleet = ff, mean_length = FALSE, ghost = TRUE) %>%
    group_by(FleetName) %>%
    filter(Yr %in% range(Yr)) %>%
    mutate(Exp = ifelse(Sex == "Male", -1 * Exp, Exp)) %>%
    ggplot(aes(Bin, Exp, fill = Sex)) +
    geom_col(colour = "grey60", width = 4, alpha = 0.75) +
    #geom_line(data = len, aes(y = Exp, linetype = Sex, colour = scen)) +
    facet_grid(vars(FleetName), vars(Yr)) +
    scale_y_continuous(labels = abs) +
    theme(legend.position = "bottom",
          panel.spacing = unit(0, "in")) +
    scale_fill_manual(values = c("grey80", "white")) +
    #xlim(xlim[[ff]]) +
    labs(x = "Length", y = "Proportion", colour = "Model") +
    guides(colour = guide_legend(nrow = 2))
})

g2 <- ggpubr::ggarrange(plotlist = g, ncol = 1, common.legend = TRUE)
ggsave("figs/ss3/len_comp_1937.png", g2, height = 8, width = 4)

