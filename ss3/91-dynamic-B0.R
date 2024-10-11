library(r4ss)
library(ggplot2)
library(dplyr)
source("ss3/fit_ss3.R")
theme_set(gfplot::theme_pbs())

unlink("ss3/A0-dynamicB0", recursive = T, force = T)
unlink("ss3/A3_high-dynamicB0", recursive = T, force = T)

system("cp -r ss3/A0 ss3/A0-dynamicB0")
system("cp -r ss3/A3_highmat ss3/A3-dynamicB0")

zero_out_catch_A0 <- function(yrs, return_B = TRUE) {
  d <- r4ss::SS_readdat("ss3/A0/data_echo.ss_new")
  d$catch <- d$catch |>
    mutate(catch = ifelse(year %in% yrs, 0, catch))
  r4ss::SS_writedat(d, "ss3/A0-dynamicB0/data.ss", overwrite = T)
  s <- r4ss::SS_readstarter("ss3/A0-dynamicB0/starter.ss_new")
  s$last_estimation_phase <- -99
  s$init_values_src <- 1 # 1 = use ss.par
  r4ss::SS_writestarter(s, "ss3/A0-dynamicB0/", overwrite = T)
  fit_ss3(model_dir = "A0-dynamicB0", hessian = FALSE, extra_args = "-maxI 0")
  if (return_B) {
    dd <- SS_read_summary("ss3/A0-dynamicB0/ss_summary.sso")
    dd <- dd$derived_quants
    dd$Label <- row.names(dd)
    dat_A0_dyn <- dd[substring(dd[["Label"]], 1, 6) == "Bratio", ] |>
      mutate(year = gsub("Bratio_", "", Label)) |>
      mutate(year = as.numeric(year)) |>
      mutate(Label = gsub("_[0-9]+", "", Label))
    return(dat_A0_dyn)
  } else {
    dd <- SS_output("ss3/A0-dynamicB0/")
    dd <- dd$natage
    age <- seq(0, 60, 10)
    out <- dd |>
      filter(Era == "TIME", `Beg/Mid` == "B") %>%
      select(Yr, Sex, as.character(age)) %>%
      reshape2::melt(id.vars = c("Yr", "Sex")) %>%
      mutate(Sex = ifelse(Sex == 1, "Female", "Male"))
    return(out)
  }
}

dat_A0_dyn1 <- zero_out_catch_A0(1950:2030) |> mutate(type = "A0 no catch after 1950")
# dat_A0_dyn2 <- zero_out_catch_A0(c(1950:1974, 1986:2030)) |> mutate(type = "A0 no catch 1950-1974 or 1986-2023")
dd <- SS_read_summary("ss3/A0/ss_summary.sso")
dd <- dd$derived_quants
dd$Label <- row.names(dd)
dat_A0 <- dd[substring(dd[["Label"]], 1, 6) == "Bratio", ] |>
  mutate(year = gsub("Bratio_", "", Label)) |>
  mutate(year = as.numeric(year)) |>
  mutate(Label = gsub("_[0-9]+", "", Label))
dat_A0 <- bind_rows(
  mutate(dat_A0, type = "A0"),
  dat_A0_dyn1 # , dat_A0_dyn2
)


zero_out_catch_A3 <- function(yrs, return_B = TRUE) {
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
    out <- dd |>
      filter(Era == "TIME", `Beg/Mid` == "B") %>%
      select(Yr, Sex, as.character(age)) %>%
      reshape2::melt(id.vars = c("Yr", "Sex")) %>%
      mutate(Sex = ifelse(Sex == 1, "Female", "Male"))
    return(out)
  }
}

dat_A3_dyn1 <- zero_out_catch_A3(1950:2030) |> mutate(type = "A3 no catch after 1950")
# dat_A3_dyn2 <- zero_out_catch_A3(c(1950:1974, 1986:2030)) |> mutate(type = "A3 no catch 1950-1974 or 1986-2023")
dd <- SS_read_summary("ss3/A3_highmat/ss_summary.sso")
dd <- dd$derived_quants
dd$Label <- row.names(dd)
dat_A3 <- dd[substring(dd[["Label"]], 1, 6) == "Bratio", ] |>
  mutate(year = gsub("Bratio_", "", Label)) |>
  mutate(year = as.numeric(year)) |>
  mutate(Label = gsub("_[0-9]+", "", Label))
dat_A3 <- bind_rows(
  mutate(dat_A3, type = "A3"),
  dat_A3_dyn1 # , dat_A3_dyn2
)

make_ts_plot <- function(dat) {
  dat |>
    filter(year <= 2023) |>
    ggplot(aes(year, Value, colour = type)) +
    scale_colour_brewer(palette = "Set2") +
    annotate("rect", xmax = 1950, xmin = 1930, ymin = 0, ymax = 1, fill = "grey95") +
    geom_line() +
    theme(axis.title.x = element_blank()) +
    ylab("Depletion (S/S<sub>0</sub>)") +
    theme(axis.title.y = ggtext::element_markdown(), legend.position.inside = c(0.8, 0.8), legend.position = "inside") +
    coord_cartesian(xlim = c(min(dat$year), 2023), ylim = c(0, 1), expand = FALSE) +
    scale_x_continuous(breaks = seq(1940, 2030, 10)) +
    labs(colour = "Scenario")
}

# g0 <- make_ts_plot(dat_A3) + ggtitle("A3 (McFarlane and Beamish 1987 maturity ogive)")

g1 <- make_ts_plot(dat_A0) + ggtitle("A0 (base)")
g1
g2 <- make_ts_plot(dat_A3) + ggtitle(("A3 (McFarlane and Beamish 1987 maturity ogive)"))
patchwork::wrap_plots(g1, g2, nrow = 2)

source("ss3/99-utils.R")
ggsave_optipng("figs/dynamic-B0.png", width = 5, height = 5.5)

g1
ggsave_optipng("figs/dynamic-B0-A0.png", width = 5, height = 3.25)
g2
ggsave_optipng("figs/dynamic-B0-A3.png", width = 5, height = 3.25)

dat_A0_dyn_N <- zero_out_catch_A0(1950:2030, return_B = F) |> mutate(type = "A0 no catch after 1950")
dat_A0_N <- zero_out_catch_A0(0:1, return_B = F) |> mutate(type = "A0")
# dat_A0_dyn2 <- zero_out_catch(c(1950:1974, 1986:2030), return_B = F) |> mutate(type = "A0 dynamic post add back 1975-85")

dat <- bind_rows(dat_A0_dyn_N, dat_A0_N)

dat |>
  filter(Sex == "Female") |>
  ggplot(aes(Yr, variable, size = value, colour = value)) +
  geom_point(pch = 21) +
  geom_point(pch = 19, alpha = 0.07) +
  geom_abline(intercept = seq(-500, 0, 1), slope = 0.1, colour = "grey60", lty = 2) +
  theme(axis.title.x = element_blank()) +
  ylab("Age") +
  facet_wrap(~type) +
  scale_size_area(max_size = 12) +
  scale_colour_viridis_c() +
  guides(colour = "none", size = "none") +
  coord_cartesian(expand = FALSE, xlim = range(dat$Yr), ylim = c(0.8, 6.2)) +
  scale_x_continuous(breaks = seq(1940, 2020, 10))
ggsave_optipng("figs/dynamic-bubble-A0.png", width = 9, height = 3.5)


# A3 with lines??
dat_A3_dyn_N <- zero_out_catch_A3(1950:2030, return_B = F) |> mutate(type = "A3 no catch after 1950")
dat_A3_N <- zero_out_catch_A3(0:1, return_B = F) |> mutate(type = "A3")
dat_A3 <- bind_rows(dat_A3_dyn_N, dat_A3_N)

dat_A3 |>
  filter(Sex == "Female") |>
  ggplot(aes(Yr, variable, size = value, colour = value)) +
  geom_point(pch = 21) +
  geom_point(pch = 19, alpha = 0.07) +
  geom_abline(intercept = seq(-500, 0, 1), slope = 0.1, colour = "grey60", lty = 2) +
  theme(axis.title.x = element_blank()) +
  ylab("Age") +
  facet_wrap(~type) +
  scale_size_area(max_size = 12) +
  scale_colour_viridis_c() +
  guides(colour = "none", size = "none") +
  coord_cartesian(expand = FALSE, xlim = range(dat$Yr), ylim = c(0.8, 6.2)) +
  scale_x_continuous(breaks = seq(1940, 2020, 10))

ggsave_optipng("figs/dynamic-bubble-A3.png", width = 9, height = 3.5)
