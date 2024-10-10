library(r4ss)
library(ggplot2)
library(dplyr)
source("ss3/fit_ss3.R")

system("cp -r ss3/A3_highmat ss3/A3-dynamicB0")

zero_out_catch <- function(yrs, return_B = TRUE) {
  d <- r4ss::SS_readdat("ss3/A3_highmat/data_echo.ss_new")
  d$catch <- d$catch |>
    mutate(catch = ifelse(year %in% yrs, 0, catch))
  r4ss::SS_writedat(d, "ss3/A3-dynamicB0/data.ss", overwrite = T)
  s <- r4ss::SS_readstarter("ss3/A3-dynamicB0/starter.ss_new")
  s$last_estimation_phase <- -99
  s$init_values_src <- 1 # 1 = use ss.par
  r4ss::SS_writestarter(s, "ss3/A3-dynamicB0/", overwrite = T)
  fit_ss3(model_dir = "A3-dynamicB0", hessian = FALSE, extra_args = "-maxI 0")
  if (return_B) {
    dd <- SS_read_summary("ss3/A3-dynamicB0/ss_summary.sso")
    dd <- dd$derived_quants
    dd$Label <- row.names(dd)
    dat_A0_dyn <- dd[substring(dd[["Label"]], 1, 6) == "Bratio", ] |>
      mutate(year = gsub("Bratio_", "", Label)) |>
      mutate(year = as.numeric(year)) |>
      mutate(Label = gsub("_[0-9]+", "", Label))
    return(dat_A0_dyn)
  } else {
    dd <- SS_output("ss3/A3-dynamicB0/")
    dd <- dd$natage
    age <- seq(0, 60, 10)
    out <- dd |> filter(Era == "TIME", `Beg/Mid` == "B") %>%
      select(Yr, Sex, as.character(age)) %>%
      reshape2::melt(id.vars = c("Yr", "Sex")) %>%
      mutate(Sex = ifelse(Sex == 1, "Female", "Male"))
    return(out)
  }
}

dat_A0_dyn1 <- zero_out_catch(1950:2030) |> mutate(type = "A0 dynamic post 1950")
# dat_A0_dyn2 <- zero_out_catch(c(1950:1974, 1986:2030))|> mutate(type = "A0 dynamic post add back 1975-85")

dd <- SS_read_summary("ss3/A3_highmat/ss_summary.sso")
dd <- dd$derived_quants
dd$Label <- row.names(dd)
dat_A0 <- dd[substring(dd[["Label"]], 1, 6) == "Bratio", ] |>
  mutate(year = gsub("Bratio_", "", Label)) |>
  mutate(year = as.numeric(year)) |>
  mutate(Label = gsub("_[0-9]+", "", Label))

dat <- bind_rows(
  mutate(dat_A0, type = "A0"),
  dat_A0_dyn1
)



ggplot(dat, aes(year, Value, colour = type)) +
  geom_line()

dat_A0_dyn1 <- zero_out_catch(1950:2030, return_B = F) |> mutate(type = "A0 dynamic post 1950")
# dat_A0_dyn2 <- zero_out_catch(c(1950:1974, 1986:2030), return_B = F) |> mutate(type = "A0 dynamic post add back 1975-85")

dat_A0_dyn1 |>
  ggplot(aes(Yr, variable, size = value, colour = value)) +
  geom_point(pch = 21) +
  geom_point(pch = 19, alpha = 0.07) +
  geom_abline(intercept = seq(-500, 0, 1), slope = 0.1, colour = "grey60", lty = 2) +
  theme(axis.title.x = element_blank()) +
  ylab("Age") +
  facet_wrap(~Sex) +
  scale_size_area(max_size = 12) +
  guides(colour = "none", size = "none") +
  coord_cartesian(expand = FALSE, xlim = range(dat_A0_dyn1$Yr), ylim = c(0.8, 6.2)) +
  scale_x_continuous(breaks = seq(1940, 2020, 10)) +
  ggtitle("No fishing after 1950")
ggsave("figs/bubble.png", width = 9, height = 3.5)

