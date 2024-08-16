library(dplyr)
library(ggplot2)
library(future)
library(purrr)

ss_home <- here::here("ss3")
#ss_home <- "C:/users/quang/Desktop/dogfish"

source("ss3/ss3_functions.R")

scale_colour_discrete <- function(...) {
  scale_colour_viridis_d()
}

theme_set(gfplot::theme_pbs())

zdir <- "A0_zfracprof" # See also "A10_extraSD_zfracprof"
system("cp -r ss3/A0 ss3/A0_zfracprof")
s <- r4ss::SS_readstarter("ss3/A0_zfracprof/starter.ss")
s$ctlfile <- "control_modified.ss"
r4ss::SS_writestarter(s, file = "ss3/A0_zfracprof/starter.ss", overwrite = TRUE)

# Profile zfrac
zfrac <- c(1e-3, seq(0.1, 1, 0.1))

plan(multisession)
x <- r4ss::profile(
  file.path(ss_home, zdir),
  string = "SR_surv_zfrac",
  profilevec = zfrac,
  exe = "ss",
  verbose = FALSE,
  prior_check = FALSE,
  extras = "-nox -nohess modelname ss"
)
plan(sequential)

zfrac_prof <- r4ss::SSgetoutput(
  keyvec = 1:length(zfrac),
  dirvec = file.path(ss_home, zdir)
)
saveRDS(zfrac_prof, file = file.path(ss_home, "zfrac_prof.rds"))

# Plot state variables
g1 <- SS3_prof(zfrac_prof, zfrac, SpawnBio) +
  labs(x = "Year", y = "Spawning output", colour = expression(z[frac])) +
  guides(colour = guide_legend(nrow = 2))
g2 <- SS3_prof(zfrac_prof, zfrac, pred_recr) +
  labs(x = "Year", y = "Recruitment", colour = expression(z[frac]))
g3 <- SS3_prof(zfrac_prof, zfrac, dep) +
  labs(x = "Year", y = "Spawning depletion", colour = expression(z[frac])) +
  expand_limits(y = 0)

g4 <- local({
  g <- SS3_B(zfrac_prof, zfrac, type = "SSBMSY")
  g$data %>%
    ggplot(aes(Yr, y, colour = factor(scen))) +
    geom_line() +
    expand_limits(y = 0) +
    labs(x = "Year", y = expression(S/S[MSY]), colour = expression(z[frac]))
})

g5 <- local({
  g <- SS3_F(zfrac_prof, zfrac, type = "FMSY")
  g$data %>%
    ggplot(aes(Yr, F_FMSY, colour = factor(scen))) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_line() +
    coord_trans(y = "sqrt", ylim = c(0, 20)) +
    expand_limits(y = 0) +
    labs(x = "Year", y = expression(F/F[MSY]), colour = expression(z[frac]))
})
g <- ggpubr::ggarrange(plotlist = list(g1, g2, g3, g4, g5), common.legend = TRUE, legend = "bottom")
ggsave("figs/ss3/prof/prof_zfrac_SB.png", g, height = 6, width = 10)

g <- SS3_SR(zfrac_prof, paste("zfrac = ", zfrac)) +
  labs(x = "Spawning output") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g$facet$params$ncol <- 3
ggsave("figs/ss3/prof/prof_zfrac_SR.png", g, height = 6, width = 6)

g <- Map(SS3_index, zfrac_prof, zfrac, figure = FALSE) %>%
  bind_rows() %>%
  left_join(fleet_names, by = "Fleet_name") %>%
  ggplot(aes(Yr, Obs, ymin = exp(log(Obs) - 1.96 * SE), ymax = exp(log(Obs) + 1.96 * SE))) +
  geom_linerange() +
  geom_point() +
  geom_line(aes(y = Exp, colour = factor(scen))) +
  expand_limits(y = 0) +
  facet_wrap(vars(FName), scales = "free_y") +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Index", colour = expression(z[frac])) +
  guides(colour = guide_legend(ncol = 6)) +
  theme(panel.spacing = unit(0, "in"), legend.position = "bottom")
ggsave("figs/ss3/prof/prof_zfrac_index.png", g, height = 4.5, width = 6)

# Plot likelihood profile
g1 <- SS3_prof_like(zfrac_prof, zfrac, by_fleet = FALSE) +
  labs(x = expression(z[frac])) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_color_brewer(palette = "Set2")
g2 <- SS3_prof_like(zfrac_prof, zfrac, xval = "steep", by_fleet = FALSE) +
  labs(x = "Steepness") +
  coord_cartesian(xlim = c(0.2, 0.45)) +
  scale_color_brewer(palette = "Set2")
g <- ggpubr::ggarrange(g1, g2, common.legend = TRUE, legend = "bottom")
ggsave("figs/ss3/prof/like_zfrac.png", g, height = 3, width = 5)

g <- SS3_prof_like(zfrac_prof, zfrac, by_fleet = TRUE, component = c("Length_like", "Surv_like")) +
  labs(x = expression(z[frac])) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_colour_brewer(palette = "Set2")
ggsave("figs/ss3/prof/like_zfrac_fleet.png", g, height = 3, width = 6)

# Plot yield curve
g <- SS3_yieldcurve(zfrac_prof, paste0("zfrac = ", zfrac), xvar = "SB0") +
  theme(legend.position = "bottom")
g$facet$params$ncol <- 3
ggsave("figs/ss3/prof/zfrac_yield_curve.png", g, height = 6, width = 6)

g <- SS3_yieldcurve(zfrac_prof, paste0("zfrac = ", zfrac), xvar = "F") +
  theme(legend.position = "bottom") +
  labs(x = "Fishing mortality")
g$facet$params$ncol <- 3
ggsave("figs/ss3/prof/zfrac_yield_curve_F.png", g, height = 6, width = 6)

# Selectivity curves:
purrr::map2_dfr(zfrac_prof, zfrac, \(x, y) {
  mutate(.SS3_sel(x), zfrac_val = y)
}) |>
  left_join(fleet_names, by = c("FleetName" = "Fleet_name")) %>%
  mutate(FName = factor(FName, levels = fleet_names$FName)) |>
  mutate(Sex = ifelse(Sex == 1, "Female", "Male")) |>
  mutate(value = value/max(value), .by = c(FName, zfrac_val)) |>
  ggplot(aes(variable, value, colour = as.factor(zfrac_val), group = zfrac_val)) +
  geom_line() +
  facet_grid(FName~Sex) +
  gfplot::theme_pbs() + labs(x = "Length (cm)", y = "Selectivity") +
  labs(colour = expression(z[frac]))
ggsave("figs/ss3/prof/zfrac_selectivity.png", height = 7, width = 6)

#zfrac2 <- r4ss::SSsummarize(zfrac_prof)
#r4ss::SSplotComparisons(zfrac2, legendlabels = paste("zfrac =", seq(0.1, 0.9, 0.1)))
#r4ss::SSplotProfile(zfrac2, profile.string = "SR_surv_zfrac", profile.label = expression(z[frac]))

#readr::write_excel_csv(likelihoods, file = "ss3/tables/likelihoods.csv")

# Potential removal reference
# Always double check in the forecast file that Btarget is 0.4 B0, SPR target = 0.4, etc.
RR <- lapply(1:length(zfrac_prof), function(x) {
  zfrac_prof[[x]]$derived_quants %>%
    filter(Label %in% c("annF_Btgt", "annF_SPR", "annF_MSY")) %>%
    select(Label, Value) %>%
    mutate(zfrac = .env$zfrac[x])
}) %>%
  bind_rows() %>%
  mutate(type = ifelse(Label == "annF_Btgt", "F[0.4~S[0]]", ifelse(Label == "annF_SPR", "F[0.4~SPR]", "F[MSY]")))

g <- ggplot(RR, aes(zfrac, Value)) +
  geom_point() +
  geom_line() +
  gfplot::theme_pbs() +
  facet_wrap(vars(type), labeller = label_parsed) +
  labs(x = expression(z[frac]))
ggsave("figs/ss3/prof/zfrac_rr.png", height = 2, width = 6)

if (FALSE) { # issues!?
  # Profile R0
  logR0 <- seq(8.5, 12.5, 0.25)

  system("cp -r ss3/A0 ss3/A0_R0prof")
  s <- r4ss::SS_readstarter("ss3/A0_R0prof/starter.ss")
  s$ctlfile <- "control_modified.ss"
  r4ss::SS_writestarter(s, file = "ss3/A0_R0prof/starter.ss", overwrite = TRUE)
  plan(multisession)
  x <- r4ss::profile(
    file.path(ss_home, "A0_R0prof"),
    string = "SR_LN(R0)",
    profilevec = logR0,
    #overwrite = FALSE,
    exe = "ss",
    verbose = FALSE,
    prior_check = FALSE,
    extras = "-nox -nohess modelname ss"
  )
  plan(sequential)
  R0_prof <- r4ss::SSgetoutput(
    keyvec = 1:length(logR0),
    dirvec = file.path(ss_home, "A0_R0prof"),
    getcomp = FALSE,
    getcovar = FALSE
  )
  saveRDS(R0_prof, file = file.path(ss_home, "R0_prof.rds"))

  g1 <- SS3_prof(R0_prof, logR0, SpawnBio) +
    labs(x = "Year", y = "Spawning output", colour = expression(log(R[0])))
  g2 <- SS3_prof(R0_prof, logR0, pred_recr) +
    labs(x = "Year", y = "Recruitment", colour = expression(log(R[0])))
  g3 <- SS3_prof(R0_prof, logR0, dep) +
    labs(x = "Year", y = "Spawning depletion", colour = expression(log(R[0])))
  g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE)
  ggsave("figs/ss3/prof/prof_R0_SB.png", g, height = 6, width = 5)

  g <- SS3_SR(R0_prof, paste("log(R0) = ", logR0))
  ggsave("figs/ss3/prof/prof_R0_SR.png", g, height = 6, width = 6)

  g <- SS3_prof_like(R0_prof[-1], logR0[-1], by_fleet = FALSE) +
    labs(x = expression(log(R[0])))
  ggsave("figs/ss3/prof/like_R0.png", g, height = 3, width = 5)

  g <- SS3_prof_like(R0_prof[-1], logR0[-1], by_fleet = TRUE, component = c("Length_like", "Surv_like")) +
    labs(x = expression(log(R[0])))
  ggsave("figs/ss3/prof/like_R0_fleet.png", g, height = 3, width = 6)
}

# Profile M

M <- seq(0.03, 0.09, 0.01)

system("cp -r ss3/A0 ss3/A0_Mprof")
s <- r4ss::SS_readstarter("ss3/A0_Mprof/starter.ss")
s$ctlfile <- "control_modified.ss"
r4ss::SS_writestarter(s, file = "ss3/A0_Mprof/starter.ss", overwrite = TRUE)
plan(multisession)
x <- r4ss::profile(
  file.path(ss_home, "A0_Mprof"),
  string = "NatM_uniform_Fem_GP_1",
  profilevec = M,
  #overwrite = FALSE,
  exe = "ss",
  verbose = FALSE,
  prior_check = FALSE,
  extras = "-nox -nohess modelname ss"
)
plan(sequential)
M_prof <- r4ss::SSgetoutput(
  keyvec = 1:length(M),
  dirvec = file.path(ss_home, "A0_Mprof"),
  getcomp = FALSE,
  getcovar = FALSE
)
saveRDS(M_prof, file = file.path(ss_home, "M_prof.rds"))

g1 <- SS3_prof(M_prof, M, SpawnBio) +
  labs(x = "Year", y = "Spawning output", colour = "M")
g2 <- SS3_prof(M_prof, M, pred_recr) +
  labs(x = "Year", y = "Recruitment", colour = "M")
g3 <- SS3_prof(M_prof, M, dep) +
  labs(x = "Year", y = "Spawning depletion", colour = "M") +
  expand_limits(y = 0)
g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("figs/ss3/prof/prof_M_SB.png", g, height = 6, width = 5)

g <- SS3_SR(M_prof, paste("M = ", M))
ggsave("figs/ss3/prof/prof_M_SR.png", g, height = 6, width = 6)

g <- SS3_prof_like(M_prof, M, by_fleet = FALSE) +
  labs(x = "M") + scale_colour_brewer(palette = "Set2")
ggsave("figs/ss3/prof/like_M.png", g, height = 3, width = 5)

g <- SS3_prof_like(M_prof, M, by_fleet = TRUE, component = c("Length_like", "Surv_like")) +
  labs(x = "M")
ggsave("figs/ss3/prof/like_M_fleet.png", g, height = 3, width = 6)

g <- Map(SS3_index, M_prof, M, figure = FALSE) %>%
  bind_rows() %>%
  left_join(fleet_names, by = "Fleet_name") %>%
  ggplot(aes(Yr, Obs, ymin = exp(log(Obs) - 1.96 * SE), ymax = exp(log(Obs) + 1.96 * SE))) +
  geom_linerange() +
  geom_point() +
  geom_line(aes(y = Exp, colour = factor(scen))) +
  expand_limits(y = 0) +
  facet_wrap(vars(FName), scales = "free_y") +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Index", colour = "M") +
  guides(colour = guide_legend(ncol = 6)) +
  theme(panel.spacing = unit(0, "in"), legend.position = "bottom")
ggsave("figs/ss3/prof/prof_M_index.png", g, height = 4.5, width = 6)

# Profile over SR Beta param:

Beta_dir <- "A0_Beta_prof"
system("cp -r ss3/A0 ss3/A0_Beta_prof")
Beta <- seq(0.5, 2, 0.25)
Beta
s <- r4ss::SS_readstarter("ss3/A0_Beta_prof/starter.ss")
s$ctlfile <- "control_modified.ss"
r4ss::SS_writestarter(s, file = "ss3/A0_Beta_prof/starter.ss", overwrite = TRUE)

plan(multisession)
x <- r4ss::profile(
  file.path(ss_home, Beta_dir),
  string = "SR_surv_Beta",
  profilevec = Beta,
  exe = "ss",
  verbose = TRUE,
  prior_check = FALSE,
  skipfinished = FALSE,
  extras = "-nox -nohess modelname ss"
)
plan(sequential)

Beta_prof <- r4ss::SSgetoutput(
  keyvec = 1:length(Beta),
  dirvec = file.path(ss_home, Beta_dir)
)
saveRDS(Beta_prof, file = file.path(ss_home, "Beta_prof.rds"))

# Plot yield curve
g <- SS3_yieldcurve(Beta_prof, paste0("Beta = ", Beta), xvar = "SB0") +
  theme(legend.position = "bottom") + gfplot::theme_pbs()
g$facet$params$ncol <- 3
ggsave("figs/ss3/prof/Beta_yield_curve.png", g, height = 6, width = 6)

g <- SS3_yieldcurve(Beta_prof, paste0("Beta = ", Beta), xvar = "F") +
  theme(legend.position = "bottom") +
  labs(x = "Fishing mortality") + gfplot::theme_pbs()
g$facet$params$ncol <- 3
ggsave("figs/ss3/prof/Beta_yield_curve_F.png", g, height = 6, width = 6)

Sy <- function(zfrac, Beta, R0 = 2000, B0 = 1000, By = 1000) {
  S0 <- R0 / B0
  z0 <- -log(S0)
  zmin <- z0 * (1 - zfrac)
  Sy <- exp(-z0 + (z0 - zmin) * (1 - (By / B0)^Beta))
  Sy
}

R0 <- 5000
By <- seq(0, R0, length.out = 200)
plot(By, Sy(0.4, 1, By = By, R0 = R0))
