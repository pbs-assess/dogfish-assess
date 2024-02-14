

ss_home <- here::here("ss3")
ss_home <- "C:/users/qhuynh/Desktop/dogfish"

source("ss3/ss3_functions.R")


# Profile zfrac
zfrac <- c(1e-3, seq(0.1, 1, 0.1))

x <- r4ss::profile(
  file.path(ss_home, "A1_zfracprof"),
  string = "SR_surv_zfrac",
  profilevec = zfrac,
  #overwrite = TRUE,
  exe = "ss",
  verbose = FALSE,
  prior_check = FALSE,
  extras = "-nox -nohess modelname ss"
)

zfrac_prof <- r4ss::SSgetoutput(
  keyvec = 1:length(zfrac),
  dirvec = file.path(ss_home, "A1_zfracprof")
)

g1 <- SS3_prof(zfrac_prof, zfrac, SpawnBio) +
  labs(x = "Year", y = "Spawning output", colour = expression(z[frac]))
g2 <- SS3_prof(zfrac_prof, zfrac, pred_recr) +
  labs(x = "Year", y = "Recruitment", colour = expression(z[frac]))
g3 <- SS3_prof(zfrac_prof, zfrac, dep) +
  labs(x = "Year", y = "Spawning depletion", colour = expression(z[frac])) +
  expand_limits(y = 0)
g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("figs/ss3/prof/prof_zfrac_SB.png", g, height = 6, width = 5)

g <- SS3_SR(zfrac_prof, paste("zfrac = ", zfrac))
ggsave("figs/ss3/prof/prof_zfrac_SR.png", g, height = 6, width = 6)


g1 <- SS3_prof_like(zfrac_prof, zfrac, by_fleet = FALSE) +
  labs(x = expression(z[frac])) +
  coord_cartesian(xlim = c(0, 1))
g2 <- SS3_prof_like(zfrac_prof, zfrac, xval = "steep", by_fleet = FALSE) +
  labs(x = "Steepness") +
  coord_cartesian(xlim = c(0.2, 0.6))

g <- ggpubr::ggarrange(g1, g2, common.legend = TRUE, legend = "bottom")
ggsave("figs/ss3/prof/like_zfrac.png", g, height = 3, width = 5)

g <- SS3_prof_like(zfrac_prof, zfrac, by_fleet = TRUE, component = c("Length_like", "Surv_like")) +
  labs(x = expression(z[frac])) +
  coord_cartesian(xlim = c(0, 1))
ggsave("figs/ss3/prof/like_zfrac_fleet.png", g, height = 3, width = 6)





zfrac2 <- r4ss::SSsummarize(zfrac_prof)
r4ss::SSplotComparisons(zfrac2, legendlabels = paste("zfrac =", seq(0.1, 0.9, 0.1)))
r4ss::SSplotProfile(zfrac2, profile.string = "SR_surv_zfrac", profile.label = expression(z[frac]))

#readr::write_excel_csv(likelihoods, file = "ss3/tables/likelihoods.csv")








# Profile beta
SRbeta <- seq(0.5, 2, 0.25)

x <- r4ss::profile(
  file.path(ss_home, "m37_flatselsurv_dw_betaprof"),
  string = "SR_surv_Beta",
  profilevec = SRbeta,
  overwrite = FALSE,
  exe = "ss",
  verbose = FALSE,
  extras = "-nox -nohess"
)


beta_prof <- r4ss::SSgetoutput(
  keyvec = 1:length(SRbeta),
  dirvec = file.path(ss_home, "m37_flatselsurv_dw_betaprof")
)

g1 <- SS3_prof(beta_prof, SRbeta, SpawnBio) +
  labs(x = "Year", y = "Spawning output", colour = "SR beta")
g2 <- SS3_prof(beta_prof, SRbeta, pred_recr) +
  labs(x = "Year", y = "Recruitment", colour = "SR beta")
g3 <- SS3_prof(beta_prof, SRbeta, dep) +
  labs(x = "Year", y = "Spawning depletion", colour = "SR beta")
g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE)
ggsave("figs/ss3/prof/prof_SRbeta_SB.png", g, height = 6, width = 5)

g <- SS3_SR(beta_prof, paste("SR beta = ", SRbeta))
ggsave("figs/ss3/prof/prof_SRbeta_SR.png", g, height = 6, width = 6)

g <- SS3_prof_like(beta_prof, SRbeta, by_fleet = TRUE, component = c("Length_like", "Surv_like")) +
  labs(x = "SR beta")
ggsave("figs/ss3/prof/like_SRbeta_fleet.png", g, height = 3, width = 6)





# Profile R0
logR0 <- seq(8.5, 10.5, 0.25)

x <- r4ss::profile(
  file.path(ss_home, "m37_flatselsurv_dw_R0prof"),
  string = "SR_LN(R0)",
  profilevec = logR0,
  #overwrite = FALSE,
  exe = "ss",
  verbose = FALSE,
  extras = "-nox -nohess"
)


R0_prof <- r4ss::SSgetoutput(
  keyvec = 1:length(logR0),
  dirvec = file.path(ss_home, "m37_flatselsurv_dw_R0prof"),
  getcomp = FALSE,
  getcovar = FALSE
)

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
  labs(x = expression(B[2023]/B[0]))
ggsave("figs/ss3/prof/like_R0.png", g, height = 3, width = 5)

g <- SS3_prof_like(R0_prof[-1], logR0[-1], by_fleet = TRUE, component = c("Length_like", "Surv_like")) +
  labs(x = expression(log(R[0])))
ggsave("figs/ss3/prof/like_R0_fleet.png", g, height = 3, width = 6)



