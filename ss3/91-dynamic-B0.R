library(r4ss)
library(ggplot2)
library(dplyr)
source("ss3/fit_ss3.R")

system("cp -r ss3/A0 ss3/A0-dynamicB0")
d <- r4ss::SS_readdat("ss3/A0-dynamicB0/data_echo.ss_new")

d$catch <- d$catch |>
  mutate(catch = ifelse(year > 1950, 0, catch))

ggplot(filter(d$catch, year > 0), aes(year, catch)) +
  geom_line() +
  facet_wrap(~fleet, scales = "free_y")

r4ss::SS_writedat(d, "ss3/A0-dynamicB0/data.ss", overwrite = T)

s <- r4ss::SS_readstarter("ss3/A0-dynamicB0/starter.ss_new")

s$last_estimation_phase <- -99
s$init_values_src <- 1 # 1 = use ss.par

r4ss::SS_writestarter(s, "ss3/A0-dynamicB0/", overwrite = T)

fit_ss3(model_dir = "A0-dynamicB0")

dd <- SS_read_summary("ss3/A0-dynamicB0/ss_summary.sso")
dd <- dd$derived_quants
dd$Label <- row.names(dd)
dat_A0_dyn <- dd[substring(dd[["Label"]], 1, 6) == "Bratio", ] |>
  mutate(year = gsub("Bratio_", "", Label)) |>
  mutate(year = as.numeric(year)) |>
  mutate(Label = gsub("_[0-9]+", "", Label))

dd <- SS_read_summary("ss3/A0/ss_summary.sso")
dd <- dd$derived_quants
dd$Label <- row.names(dd)
dat_A0 <- dd[substring(dd[["Label"]], 1, 6) == "Bratio", ] |>
  mutate(year = gsub("Bratio_", "", Label)) |>
  mutate(year = as.numeric(year)) |>
  mutate(Label = gsub("_[0-9]+", "", Label))

dat <- bind_rows(
  mutate(dat_A0, type = "A0"),
  mutate(dat_A0_dyn, type = "A0 dynamic B0 post 1950")
)
row.names(dat) <- NULL

ggplot(dat, aes(year, Value, colour = type)) +
  geom_line()
