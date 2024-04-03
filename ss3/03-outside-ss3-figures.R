
library(tidyverse)
library(r4ss)
source("ss3/ss3_functions.R")

# Compare SS models
#ss_home <- here::here("ss3")
ss_home <- "C:/users/qhuynh/Desktop/dogfish"

# Set A models with growth and maturity scenarios
mods <- c("A1", "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat")
model_name <- c("(A1) BCgrowth", "(A2) USgrowth", "(A3) BCgrowth, high mat", "(A4) USgrowth, high mat")

fig_dir <- "figs/ss3/set_a_mat"

# Set A models jackknifing indices
mods <- c("A1", "A6_IPHC+CPUE", "A7_SYNonly", "A8_HBLLonly")
model_name <- c("(A1) BCgrowth + all indices", "(A6) IPHC + CPUE only", "(A6) SYN only", "(A7) HBLL only")

fig_dir <- "figs/ss3/set_a_ind"


# Set B (M change)
mods <- c("A1", "B1_1990inc", "B2_2010step", "B3_2005step")
model_name <- c("(A1) Constant M", "(B1) Linear increase 1990", "(B2) Step increase 2010", "(B3) Step increase 2005")

fig_dir <- "figs/ss3/set_b"








# Read and save r4ss report lists ----
.ggsave <- function(filename, ...) {
  filename <- file.path(fig_dir, filename)
  ggsave(filename, ...)
}

multi_rep <- lapply(mods, function(x) {
  r4ss::SS_output(file.path(ss_home, x),
                  verbose = FALSE,
                  printstats = FALSE,
                  hidewarn = TRUE)
})

saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_mat.rds"))
multi_rep <- readRDS(file = file.path(ss_home, "multi_rep_mat.rds"))

saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_jackknife.rds"))
multi_rep <- readRDS(file = file.path(ss_home, "multi_rep_jackknife.rds"))

saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_Minc.rds"))
multi_rep <- readRDS(file = file.path(ss_home, "multi_rep_Minc.rds"))


# Check parameters and bounds
multi_rep[[1]]$estimated_non_dev_parameters %>% View()

# M change
Minc <- lapply(1:length(multi_rep), function(x) {
  multi_rep[[x]]$M_at_age %>%
    #filter(Yr %in% c(1937, 2023)) %>%
    select(Sex, Yr, `0`) %>%
    rename(M = `0`) %>%
    mutate(scen = model_name[x])
})

g <- bind_rows(Minc) %>%
  filter(Sex == 1) %>%
  ggplot(aes(Yr, M, colour = scen)) +
  geom_line() +
  #facet_wrap(vars(Sex)) +
  gfplot::theme_pbs() +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Natural mortality", colour = "Model")
.ggsave("M_year.png", g, height = 3, width = 6)

# Steepness
sapply(multi_rep, SS3_steep)

# Density-dependent M
lapply(multi_rep, function(x) {
  r4ss::SSplotTimeseries(x, subplot = 4)
  x$Natural_Mortality %>% View()
  x$M_age %>% View()
})

# Length at age distribution (add empirical samples)
age_samps <- readr::read_csv("data/generated/length-age.csv") %>%
  filter(Age <= 70) %>%
  filter(Age %in% seq(5, 70, 5)) %>%
  rename(Length = length) %>%
  mutate(Age = paste("Age", round(Age, 0)) %>% factor(levels = paste("Age", 0:70)))
LAK <- multi_rep[[1]]$ALK %>%
  reshape2::melt() %>%
  mutate(TrueAge = as.character(TrueAge) %>% as.numeric(),
         Age = paste("Age", TrueAge) %>% factor(levels = paste("Age", 0:70)),
         Length = as.character(Length) %>% as.numeric()) %>%
  filter(Matrix == "Seas: 1 Sub_Seas: 1 Morph: 1") %>%
  filter(TrueAge <= 70) %>%
  filter(TrueAge %in% seq(5, 70, 5))
mu <- multi_rep[[1]]$endgrowth %>%
  filter(Sex == 1) %>%
  select(int_Age, Len_Beg) %>%
  filter(int_Age %in% seq(5, 70, 5)) %>%
  mutate(Age = paste("Age", int_Age) %>% factor(levels = paste("Age", 0:70)))
g <- LAK %>%
  ggplot(aes(Length)) +
  #geom_vline(data = mu, aes(xintercept = Len_Beg), linetype = 2) +
  geom_vline(xintercept = 95, linetype = 3) +
  geom_histogram(data = age_samps, aes(y = after_stat(density)), binwidth = 5, colour = "grey80", linewidth = 0.1) +
  geom_density(data = age_samps, aes(y = after_stat(density)), linetype = 2) +
  geom_line(aes(y = value)) +
  facet_wrap(vars(Age)) +
  gfplot::theme_pbs() +
  theme(panel.spacing = unit(0, "in")) +
  labs(x = "Length", y = "Length-at-age probability") +
  ggtitle("Female")
ggsave("figs/ss3/prob-length-at-age.png", g, height = 6, width = 8)

### Tables
likelihoods <- lapply(1:length(mods), function(i) {
  x <- multi_rep[[i]][["likelihoods_used"]] %>%
    mutate(Component = rownames(.)) %>%
    select(Component, values)
  colnames(x)[2] <- model_name[i]
  return(x)
}) %>%
  Reduce(dplyr::left_join, .)
readr::write_excel_csv(likelihoods, file = "tables/ss3_likelihoods.csv")

# Parameters R0, steepness, reference points
pars_report <- Map(pars_fn, multi_rep, model_name) %>%
  Reduce(left_join, .)
readr::write_excel_csv(pars_report, file = "tables/ss3_pars.csv")

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
  geom_line(aes(y = Exp, colour = scen)) +
  expand_limits(y = 0) +
  facet_wrap(vars(Fleet_name), scales = "free_y") +
  #facet_grid(vars(Fleet_name), vars(scen), scales = "free_y") +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Index", colour = "Model") +
  guides(colour = guide_legend(ncol = 2)) +
  theme(panel.spacing = unit(0, "in"), legend.position = "bottom")
.ggsave("index_fit.png", g, height = 4, width = 6)

# Plot SR
g <- SS3_SR(multi_rep, model_name) +
  labs(x = "Spawning output (pup production)")
.ggsave("srr.png", g, height = 4, width = 6)

# Plot recruitment
g <- SS3_recruitment(multi_rep, model_name) +
  labs(y = "Recruitment (age 0)") +
  coord_cartesian(expand = FALSE)
.ggsave("recruitment.png", g, height = 4, width = 6)

#g <- SS3_recruitment(multi_rep, model_name, dev = TRUE)
#.ggsave("recruit_dev.png", g, height = 4, width = 6)

# Plot SSB
g <- SS3_B(multi_rep, model_name) +
  guides(linetype = 'none') +
  labs(y = "Female spawning output") +
  coord_cartesian(expand = FALSE)
.ggsave("spawning_est.png", g, height = 4, width = 6)

g <- SS3_B(multi_rep, model_name, type = "SSB0") +
  guides(linetype = 'none') +
  labs(y = "Spawning depletion") +
  coord_cartesian(expand = FALSE)
.ggsave("depletion_est.png", g, height = 4, width = 6)

g <- SS3_B(multi_rep, model_name, type = "SSBMSY") +
  guides(linetype = 'none') +
  geom_hline(yintercept = 0.4, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 3) +
  coord_cartesian(expand = FALSE)
.ggsave("bbmsy_est.png", g, height = 4, width = 6)

g <- SS3_B(multi_rep, model_name, type = "B") +
  labs(linetype = 'Sex') +
  theme(legend.position = "bottom") +
  coord_cartesian(expand = FALSE)
.ggsave("biomass_est.png", g, height = 5, width = 6)

# Population fecundity
g <- SS3_fecundity(multi_rep, model_name) +
  coord_cartesian(ylim = c(0, 10))
.ggsave("fecundity_hist.png", g, height = 4, width = 6)

g <- SS3_fecundity(multi_rep, model_name, type = "mat") +
  coord_cartesian(ylim = c(0, 0.1))
.ggsave("fecundity_hist2.png", g, height = 4, width = 6)




# Maturity at age/length
#gmodel <- c("BC growth", "US growth", "BC growth", "US growth")
#g1 <- lapply(1:2, function(x) {
#  multi_rep[[x]]$endgrowth %>%
#    filter(Sex == 1) %>%
#    select(int_Age, `Len_Mat`) %>%
#    mutate(model = gmodel[x])
#}) %>%
#  bind_rows() %>%
#  ggplot(aes(int_Age, `Len_Mat`, linetype = factor(model))) +
#  #geom_point() +
#  geom_line() +
#  coord_cartesian(ylim = c(0, 1)) +
#  labs(x = "Age", y = NULL, linetype = "Growth model")
#
#g2 <- lapply(1:2, function(x) {
#  multi_rep[[x]]$biology %>%
#    select(Len_mean, Mat) %>%
#    mutate(model = gmodel[x])
#}) %>%
#  bind_rows() %>%
#  ggplot(aes(Len_mean, Mat)) +
#  #geom_point() +
#  geom_line() +
#  labs(x = "Length", y = "Maturity")
#g <- cowplot::plot_grid(g2, g1, rel_widths = c(1, 1.25))
#.ggsave("maturity.png", g, height = 2, width = 6)



# Fecundity at age/length
gmodel <- c("BC growth", "US growth", "BC growth", "US growth")
g1 <- lapply(3:4, function(x) {
  multi_rep[[x]]$endgrowth %>%
    filter(Sex == 1) %>%
    select(int_Age, `Mat*Fecund`, Age_Mat) %>%
    mutate(model = gmodel[x])
}) %>%
  bind_rows() %>%
  mutate(Fec = `Mat*Fecund`/Age_Mat) %>%
  ggplot(aes(int_Age, Fec, linetype = factor(model))) +
  #geom_point() +
  geom_line() +
  expand_limits(y = 0) +
  labs(x = "Age", y = NULL, linetype = "Growth model")

g2 <- lapply(3:4, function(x) {
  multi_rep[[x]]$biology %>%
    select(Len_mean, Fec) %>%
    mutate(model = gmodel[x])
}) %>%
  bind_rows() %>%
  ggplot(aes(Len_mean, Fec)) +
  #geom_point() +
  geom_line() +
  labs(x = "Length", y = "Fecundity")
g <- cowplot::plot_grid(g2, g1, rel_widths = c(1, 1.25))
.ggsave("fecundity.png", g, height = 2, width = 6)

# Spawning output
#g1 <- lapply(3:4, function(x) {
#  multi_rep[[x]]$endgrowth %>%
#    filter(Sex == 1) %>%
#    select(int_Age, `Mat*Fecund`) %>%
#    mutate(model = gmodel[x])
#}) %>%
#  bind_rows() %>%
#  ggplot(aes(int_Age, `Mat*Fecund`, linetype = factor(model))) +
#  #geom_point() +
#  geom_line() +
#  labs(x = "Age", y = NULL, linetype = "Growth model")
#
#g2 <- lapply(3:4, function(x) {
#  multi_rep[[x]]$biology %>%
#    select(Len_mean, `Mat*Fec`) %>%
#    mutate(model = gmodel[x])
#}) %>%
#  bind_rows() %>%
#  ggplot(aes(Len_mean, `Mat*Fec`)) +
#  #geom_point() +
#  geom_line() +
#  labs(x = "Length", y = "Spawning output")
#
#g2 <- multi_rep[[1]]$biology%>%
#  ggplot(aes(Len_mean, `Mat*Fec`)) +
#  #geom_point() +
#  geom_line() +
#  labs(x = "Length", y = "Spawning output")
#g <- cowplot::plot_grid(g2, g1, rel_widths = c(1, 1.25))
#.ggsave("spawning_age.png", g, height = 2, width = 6)

# Selectivity at length (estimated)
g <- SS3_sel(multi_rep, model_name, type = "Lsel", bin_width = 5, do_mat = FALSE) +
  coord_cartesian(xlim = c(40, 115), ylim = c(0, 1.1))
.ggsave("sel_len.png", g, height = 6, width = 6)

# Selectivity at age (converted from size based with maturity ogive)
g <- SS3_sel(multi_rep, model_name, bin_width = 5)
.ggsave("sel_age.png", g, height = 6, width = 6)

# Mean length
fleet_int <- c(1:8)
g <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = fleet_int)) %>%
  bind_rows() %>%
  mutate(FleetName = factor(FleetName, levels = multi_rep[[1]]$FleetNames[fleet_int])) %>%
  ggplot(aes(Yr, Exp, linetype = Sex, colour = FleetName, shape = Sex)) +
  facet_grid(vars(FleetName), vars(scen), scales = "free_y") +
  geom_point(aes(y = Obs)) +
  geom_line() +
  labs(x = "Year", y = "Mean length") +
  theme(panel.spacing = unit(0, "in"),
        strip.background = element_blank(),
        legend.position = "bottom") +
  scale_shape_manual(values = c(16, 1)) +
  guides(colour = "none")
.ggsave("mean_length.png", g, height = 6, width = 6)

# Length comps
heights <- c(4, 5, 6, 4, NA, 6, NA, 8) + 1

xlim <- c(20, 130)

for(ff in fleet_int) {
  len <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = ff, mean_length = FALSE)) %>%
    bind_rows()

  if (nrow(len)) {
    len <- len %>%
      mutate(Obs = ifelse(Sex == "Male", -1 * Obs, Obs),
             Exp = ifelse(Sex == "Male", -1 * Exp, Exp),
             Bin = Bin + 5)

    len_N <- len %>%
      summarise(N = unique(Nsamp_in), .by = c(Yr, FleetName))

    g <- len %>%
      filter(scen == unique(len$scen)[1]) %>%
      ggplot(aes(Bin, Obs)) +
      geom_col(colour = "grey60", width = 5, alpha = 0.75, aes(fill = Sex)) +
      geom_line(data = len, aes(y = Exp, linetype = Sex, colour = scen)) +
      geom_label(data = len_N, x = Inf, y = -Inf, hjust = "inward", vjust = "inward", aes(label = N)) +
      facet_wrap(vars(Yr), ncol = 4) +
      scale_y_continuous(labels = abs) +
      theme(legend.position = "bottom",
            panel.spacing = unit(0, "in")) +
      scale_fill_manual(values = c("grey80", "white")) +
      coord_cartesian(xlim = c(30, 120)) +
      labs(x = "Length", y = "Proportion", colour = "Model") +
      guides(colour = guide_legend(nrow = 2), fill = "none", linetype = "none") +
      ggtitle(unique(len$FleetName))
    ggsave(paste0("figs/ss3/len_comp_fleet_", ff, ".png"), g, height = heights[ff], width = 6)
  }
}

# Length comp residual - heat map
g <- SS3_compresid(multi_rep[[1]], model_name[1], fleet = fleet_int) +
  ggtitle(model_name[1]) +
  theme(panel.spacing = unit(0, "in"))
.ggsave("len_comp_resid_A1.png", g, height = 7, width = 6)

# Length comp residual - histogram
g <- SS3_compresid(multi_rep[[1]], model_name[1], fleet = fleet_int, figure = "histogram") +
  ggtitle(model_name[1]) +
  theme(panel.spacing = unit(0, "in"))
.ggsave("len_comp_hist_A1.png", g, height = 4, width = 6)



# Numbers at age
g <- SS3_N(multi_rep[1], model_name[1], age = seq(0, 40, 10))
g2 <- g$data %>%
  mutate(variable = paste("Age", variable)) %>%
  #mutate(value = value/max(value), .by = c(variable, Sex, scen)) %>%
  ggplot(aes(Yr, value, colour = variable)) +
  geom_line() +
  guides(colour = "none") +
  gfplot::theme_pbs() +
  expand_limits(y = 0) +
  facet_grid(vars(variable), vars(Sex), scales = "free_y") +
  labs(x = "Year", y = "Estimated abundance") +
  ggtitle(model_name[1])
.ggsave("N_age_A1.png", g2, height = 6, width = 4)


#g <- SS3_N(multi_rep[1], model_name[1], age = seq(0, 40, 10))
#.ggsave("N_age_B1.png", g, height = 2, width = 6)

#g <- SS3_N(multi_rep, model_name, age = seq(0, 40, 10))
#.ggsave("N_age.png", g, height = 6, width = 6)

#g <- SS3_N(multi_rep, model_name, age = seq(0, 40, 10)) +
#  coord_trans(y = "log") +
#  scale_y_continuous(breaks = c(1, 1000, 5000, 10000))
#.ggsave("N_age_log.png", g, height = 6, width = 6)

#g <- SS3_N(multi_rep, model_name, age = seq(0, 60, 10), sex_ratio = TRUE) +
#  coord_cartesian(ylim = c(0, 0.55), expand = FALSE)
#.ggsave("sex_ratio_age.png", g, height = 4, width = 6)

# Compare with numbers at length
g <- SS3_N(multi_rep[1], model_name[1], type = "length", len = seq(50, 115, 15))
g2 <- g$data %>%
  mutate(variable = paste(variable, "cm") %>% factor(levels = paste(seq(50, 115, 15), "cm"))) %>%
  #mutate(value = value/max(value), .by = c(variable, Sex, scen)) %>%
  ggplot(aes(Yr, value, colour = variable)) +
  geom_line() +
  guides(colour = "none") +
  gfplot::theme_pbs() +
  expand_limits(y = 0) +
  facet_grid(vars(variable), vars(Sex), scales = "free_y") +
  labs(x = "Year", y = "Estimated abundance") +
  ggtitle(model_name[1])
.ggsave("N_len_A1.png", g2, height = 6, width = 6)

#g <- SS3_N(multi_rep, model_name, type = "length", len = seq(50, 115, 15)) +
#  coord_trans(y = "log") +
#  scale_y_continuous(breaks = c(1, 1000, 5000, 10000))
#.ggsave("N_len_log.png", g, height = 6, width = 6)

#g <- SS3_N(multi_rep, model_name, type = "length", len = seq(50, 115, 15), sex_ratio = TRUE) +
#  coord_cartesian(ylim = c(0, 1), expand = FALSE)
#.ggsave("sex_ratio_len.png", g, height = 4, width = 6)


# Exploitation and apical F
g <- SS3_F(multi_rep, model_name) +
  coord_cartesian(ylim = c(0, 0.25), expand = FALSE)
.ggsave("harvest_rate_total.png", g, height = 4, width = 6)

g <- SS3_F(multi_rep, model_name, type = "fleet") +
  labs(y = "Harvest rate") +
  coord_cartesian(ylim = c(0, 1), expand = FALSE)
.ggsave("harvest_rate_fleet.png", g, height = 4, width = 6)

g <- SS3_vuln(multi_rep, model_name)

#g <- SS3_F(multi_rep, model_name, type = "FMSY") +
#  labs(y = expression(U/U[MSY]))
#.ggsave("harvest_rate_msy.png", g, height = 4, width = 6)

# Kobe
#g <- SS3_Kobe(multi_rep, model_name) +
#  labs(y = expression(U/U[MSY])) +
#  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 7.5))
#.ggsave("kobe.png", g, height = 4, width = 6)

# Yield curve
g <- SS3_yieldcurve(multi_rep, model_name) +
  coord_cartesian(xlim = c(0, 0.03))
.ggsave("yieldcurve_F.png", g, height = 3, width = 5)

g <- SS3_yieldcurve(multi_rep, model_name, xvar = "SB0")
.ggsave("yieldcurve_depletion.png", g, height = 3, width = 6)

# Compare unfished vs. current length comp in a single model
for(i in 1:length(multi_rep)) {
  g <- lapply(c(1:4, 6, 8), function(ff) {
    dat <- SS3_lencomp(multi_rep[[i]], fleet = ff, mean_length = FALSE, ghost = TRUE) %>%
      group_by(FleetName) %>%
      filter(Yr %in% range(Yr)) %>%
      mutate(Exp = ifelse(Sex == "Male", -1 * Exp, Exp))

    ML <- dat %>%
      ungroup() %>%
      summarise(value = weighted.mean(Bin, Exp) %>% round(1), .by = c(Yr, Sex, Fleet, FleetName, scen))

    gout <- dat %>%
      ggplot(aes(Bin, Exp)) +
      geom_col(colour = "grey60", width = 5, alpha = 0.75, aes(fill = Sex)) +
      geom_label(data = ML %>% filter(Sex == "Female"),
                 aes(label = value),
                 x = -Inf, y = Inf, hjust = "inward", vjust = "inward") +
      geom_label(data = ML %>% filter(Sex == "Male"),
                 aes(label = value),
                 x = -Inf, y = -Inf, hjust = "inward", vjust = "inward") +
      facet_grid(vars(FleetName), vars(Yr)) +
      scale_y_continuous(labels = abs) +
      theme(legend.position = "bottom",
            panel.spacing = unit(0, "in")) +
      scale_fill_manual(values = c("grey80", "white")) +
      #xlim(xlim[[ff]]) +
      labs(x = "Length", y = "Proportion", colour = "Model") +
      guides(colour = guide_legend(nrow = 2))

    return(gout)
  })
  g2 <- ggpubr::ggarrange(plotlist = g, ncol = 1, common.legend = TRUE, legend = "bottom")
  ggsave(paste0("figs/ss3/len_comp_1937_model", i, ".png"), g2, height = 10, width = 6)
}


# Report mean age !
#for(i in 1:length(multi_rep)) {
#  g <- lapply(c(1:4, 6, 8), function(ff) {
#    SS3_agecomp(multi_rep[[i]], fleet = ff, y = c(1937, 2023))
#  })
#  g2 <- ggpubr::ggarrange(plotlist = g, ncol = 1, common.legend = TRUE, legend = "bottom")
#  ggsave(paste0("figs/ss3/age_comp_1937_model", i, ".png"), g2, height = 10, width = 6)
#}

# Sex ratio of size comps
#fleet_int = 1:8
#comps <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = fleet_int), mean_length = FALSE) %>%
#  bind_rows() %>%
#  mutate(FleetName = factor(FleetName, levels = multi_rep[[1]]$FleetNames[fleet_int]))
#ratio_exp <- comps %>%
#  summarise(N = sum(Exp), .by = c(Yr, FleetName, Sex, scen)) %>%
#  mutate(p_female = N/sum(N), .by = c(Yr, FleetName, scen)) %>%
#  filter(Sex == "Female")
#
#ratio_obs <- comps %>%
#  summarise(N = sum(Obs), .by = c(Yr, FleetName, Sex, scen)) %>%
#  mutate(p_female = N/sum(N), .by = c(Yr, FleetName, scen)) %>%
#  filter(Sex == "Female")
#
#g <- ggplot(ratio_exp, aes(Yr, p_female)) +
#  geom_point(data = ratio_obs %>% select(!scen)) +
#  geom_line(data = ratio_obs %>% select(!scen), linetype = 3, linewidth = 0.1) +
#  geom_line(aes(colour = scen)) +
#  facet_wrap(vars(FleetName)) +
#  coord_cartesian(xlim = c(1975, 2025), ylim = c(0, 1.05), expand = FALSE) +
#  gfplot::theme_pbs() +
#  theme(legend.position = "bottom", panel.spacing = unit(0, "in")) +
#  labs(x = "Year", y = "Proportion female", colour = "Model") +
#  guides(colour = guide_legend(ncol = 2))
#.ggsave("sex_ratio_comp.png", g, height = 4.5, width = 5)


