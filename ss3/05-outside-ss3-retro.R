

ss_home <- here::here("ss3")
#ss_home <- "C:/users/qhuynh/Desktop/dogfish"
model_dir <- c("A1", "B2", "C1")

# Do retro
ypeel <- seq(0, -7, -1)
for(i in model_dir) {

  r4ss::retro(
    file.path(ss_home, i),
    years = ypeel,
    exe = "ss",
    extras = c("-nohess modelname ss")
  )

}

# Make figures
source("ss3/ss3_functions.R")

ret <- r4ss::SSgetoutput(
  dirvec = file.path(ss_home, model_dir[1], "retrospectives", paste0("retro", ypeel))
)
g <- SS3_retro(ret)
ggsave("figs/ss3/ret_A1.png", g, height = 8, width = 5)


ret <- r4ss::SSgetoutput(
  dirvec = file.path(ss_home, model_dir[2], "retrospectives", paste0("retro", ypeel))
)
g <- SS3_retro(ret)
ggsave("figs/ss3/ret_B2.png", g, height = 8, width = 5)

ret <- r4ss::SSgetoutput(
  dirvec = file.path(ss_home, model_dir[3], "retrospectives", paste0("retro", ypeel))
)
g <- SS3_retro(ret)
ggsave("figs/ss3/ret_C1.png", g, height = 8, width = 5)

