

ss_home <- here::here("ss3")
ss_home <- "C:/users/quang/Documents/dogfish"
model_dir <- c("A1", "A9_lowM", "B2_2010step")

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

library(snowfall)
sfInit(parallel = TRUE, cpus = length(model_dir))
sfExport(list = c("ss_home", "ypeel"))
sfLapply(model_dir, function(i) {
  r4ss::retro(
    file.path(ss_home, i),
    years = ypeel,
    exe = "ss",
    extras = c("-nohess modelname ss")
  )
})
sfStop()


# Make figures
source("ss3/ss3_functions.R")

ret <- r4ss::SSgetoutput(
  dirvec = file.path(ss_home, model_dir[3], "retrospectives", paste0("retro", ypeel))
)
g <- SS3_retro(ret)
ggsave("figs/ss3/ret_A1.png", g, height = 8, width = 5)
ggsave("figs/ss3/ret_B2.png", g, height = 8, width = 5)

