

ss_home <- here::here("ss3")
ss_home <- "C:/users/qhuynh/Desktop/dogfish"
model_dir <- "m37_flatselsurv_dw"

source("ss3/ss3_functions.R")

ypeel <- seq(0, -10, -2)
r4ss::retro(
  file.path(ss_home, model_dir),
  years = ypeel,
  exe = "ss",
  extras = c("-nox -nohess")
)


ret <- r4ss::SSgetoutput(
  dirvec = file.path(ss_home, model_dir, "retrospectives", paste0("retro", ypeel))
)



g1 <- SS3_prof(ret, ypeel, SpawnBio) +
  labs(x = "Year", y = "Spawning output", colour = "Years peeled")
g2 <- SS3_prof(ret, ypeel, pred_recr) +
  labs(x = "Year", y = "Recruitment", colour = "Years peeled")
g3 <- SS3_prof(ret, ypeel, dep) +
  labs(x = "Year", y = "Spawning depletion", colour = "Years peeled")
g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE)
ggsave("figs/ss3/ret_SB.png", g, height = 6, width = 5)
