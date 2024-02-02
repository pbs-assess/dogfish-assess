

ss_home <- here::here("ss3")
ss_home <- "C:/users/qhuynh/Desktop/dogfish"
model_dir <- c("m37_IPHC_g4", "m37_SYN_g4")


ypeel <- seq(0, -10, -2)
for(i in model_dir) {

  r4ss::retro(
    file.path(ss_home, i),
    years = ypeel,
    exe = "ss",
    extras = c("-nohess modelname ss")
  )

}


ret <- r4ss::SSgetoutput(
  dirvec = file.path(ss_home, model_dir[1], "retrospectives", paste0("retro", ypeel))
)


source("ss3/ss3_functions.R")

g1 <- SS3_prof(ret, abs(ypeel), SpawnBio) +
  labs(x = "Year", y = "Spawning output", colour = "Years peeled")
g2 <- SS3_prof(ret, abs(ypeel), pred_recr) +
  labs(x = "Year", y = "Recruitment", colour = "Years peeled")
g3 <- SS3_prof(ret, abs(ypeel), dep) +
  labs(x = "Year", y = "Spawning depletion", colour = "Years peeled") +
  expand_limits(y = 0)
g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("figs/ss3/ret_SB_IPHC.png", g, height = 8, width = 5)



ret <- r4ss::SSgetoutput(
  dirvec = file.path(ss_home, model_dir[2], "retrospectives", paste0("retro", ypeel))
)

g1 <- SS3_prof(ret, abs(ypeel), SpawnBio) +
  labs(x = "Year", y = "Spawning output", colour = "Years peeled")
g2 <- SS3_prof(ret, abs(ypeel), pred_recr) +
  labs(x = "Year", y = "Recruitment", colour = "Years peeled")
g3 <- SS3_prof(ret, abs(ypeel), dep) +
  labs(x = "Year", y = "Spawning depletion", colour = "Years peeled") +
  expand_limits(y = 0)
g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE, legend = "bottom")
ggsave("figs/ss3/ret_SB_SYN.png", g, height = 8, width = 5)
