library(tidyverse)
library(r4ss)
library(ggtext)

dir.create("figs/refpts/", showWarnings = F)
library(ggplot2)

source("ss3/99-utils.R")
file.remove("values/ref-pts.tex")

# options(ggplot2.continuous.colour = "viridis")
# scale_colour_discrete <- function(...) {
#   scale_colour_brewer(..., palette = "Set2")
# }
# scale_fill_discrete <- function(...) {
#   scale_fill_brewer(..., palette = "Set2")
# }
theme_set(gfplot::theme_pbs())

ss_home <- here::here("ss3")

source("ss3/99-model-names.R")

reject <- c("B1_1990inc", "B3_2005step", "B4_1990inc_lowM", "A1", "A8_HBLLonly", "A15_100discard")

keep <- which(!mods %in% reject)
mods <- mods[keep]
model_name <- model_name[keep]
library(future)
plan(multisession)
multi_rep <- furrr::future_map(mods, function(x) {
  cat(x, "\n")
  r4ss::SS_output(file.path(ss_home, x),
    verbose = FALSE,
    printstats = FALSE,
    hidewarn = TRUE
  )
})
plan(sequential)
saveRDS(multi_rep, file = "data/generated/replist-ref-pts.rds")

out_F <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i) {
    multi_rep[[i]]$derived_quants %>%
      # filter(Label %in% c("annF_Btgt", "annF_SPR", "annF_MSY")) %>%
      filter(Label %in% c("annF_Btgt", "annF_SPR")) %>%
      select(label = Label, est = Value, se = StdDev) |>
      mutate(scen = model_name[i])
  })
out_F |>
  mutate(scen = forcats::fct_inorder(scen)) |>
  filter(!grepl("^\\(B", scen)) |>
  mutate(scen = forcats::fct_rev(scen)) |>
  ggplot(aes(scen, est, ymin = est - 2 * se, ymax = est + 2 * se)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  facet_wrap(~label, scales = "free_x")

get_b_ratio <- function(replist) {
  derived_quants <- replist$derived_quants
  dd <- derived_quants[substring(derived_quants[["Label"]], 1, 6) == "Bratio", ] |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 2 * se, upr = est + 2 * se) |>
    mutate(year = gsub("Bratio_", "", Label)) |>
    mutate(year = as.numeric(year)) |>
    select(year, est, lwr, upr)
  row.names(dd) <- NULL
  dd
}

cols <- c("grey10", RColorBrewer::brewer.pal(12L, "Paired"))
names(cols) <- model_name[!grepl("^\\(B", model_name)]

cols_B <- c("grey60", RColorBrewer::brewer.pal(3, "Dark2")[1:2])
names(cols_B) <- c(model_name[1], model_name[grepl("^\\(B", model_name)])

out_depl <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i)
  get_b_ratio(multi_rep[[i]]) |>
    mutate(scen = model_name[i])) |>
  mutate(scen = forcats::fct_inorder(scen))

make_depl_plot <- function(dat, .col) {
  dat |>
    mutate(scen = forcats::fct_rev(scen)) |>
    ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = scen, value = scen, fill = scen)) +
    geom_ribbon(alpha = 0.3, colour = NA) +
    geom_line() +
    geom_hline(yintercept = 0.4, lty = 3, colour = "grey40") +
    geom_hline(yintercept = 0.2, lty = 2, colour = "grey40") +
    ylab("S / S<sub>0</sub>") +
    xlab("") +
    theme(axis.title = element_markdown()) +
    coord_cartesian(xlim = c(1936, 2028), ylim = c(0, 1), expand = FALSE) +
    annotate("rect", xmin = 2024, xmax = 2028, ymin = 0, ymax = 1e6,
      alpha = 0.1,fill = "grey30") +
    labs(colour = "Scenario", fill = "Scenario") +
    scale_colour_manual(values = .col, guide = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = .col, guide = guide_legend(reverse = TRUE))
}
out_depl |>
  filter(!grepl("^\\(B", scen)) |>
  make_depl_plot(.col = cols);
ggsave("figs/ss3/refpts/depl-ref-ts.png", width = 7, height = 4)
out_depl |> filter(grepl("^\\(B", scen) | grepl("A0", scen)) |>
  make_depl_plot(.col = cols_B) +
  ggtitle("**With M set at its historical value when calculating reference points**") +
  theme(title = element_markdown())
ggsave("figs/ss3/refpts/depl-ref-ts-B.png", width = 7, height = 4)

x <- out_depl |> filter(year == 2023) |>
  filter(!grepl("^\\(B", scen))
xbase <- filter(x, grepl("A0", scen))

write_tex(xbase$est |> mround(2), "BaseDepl", "ref-pts.tex")
ci <- paste0(mround(xbase$lwr, 2), "--", mround(xbase$upr, 2))
write_tex(ci, "BaseDeplCI", "ref-pts.tex")

mround(mean(x$est), 2) |>
  write_tex("EnsDeplMean", "ref-pts.tex")
paste0(mround(min(x$lwr), 2), "--", mround(max(x$upr), 2)) |>
  write_tex("EnsDeplCI", "ref-pts.tex")

paste0(mround(100 - mean(x$est) * 100, 0), "\\%") |>
  write_tex("InvEnsDeplMean", "ref-pts.tex")
paste0(paste0(mround(100 - max(x$upr) * 100, 0), "\\%"), "--", paste0(mround(100 - min(x$lwr) * 100, 0)), "\\%") |>
  write_tex("InvEnsDeplCI", "ref-pts.tex")

# ggrepel::geom_text_repel(data = filter(out, year == 2023), mapping = aes(x = year, y = est, label = scen), segment.size = 0.4, direction='x', nudge_x = 0.1, force=1)

get_F_ts_target <- function(replist) {
  derived_quants <- replist$derived_quants
  dd <- derived_quants[substring(derived_quants[["Label"]], 1, 2) == "F_", ] |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 2 * se, upr = est + 2 * se) |>
    mutate(year = gsub("F_", "", Label)) |>
    mutate(year = as.numeric(year)) |>
    select(year, est, lwr, upr)
  row.names(dd) <- NULL
  ref <- derived_quants |>
    filter(Label %in% c("annF_Btgt")) |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 2 * se, upr = est + 2 * se)
  dd$est_ref <- ref$est
  dd$lwr_ref <- ref$lwr
  dd$upr_ref <- ref$upr
  dd <- mutate(dd, est_ratio = est / est_ref, lwr_ratio = lwr / est_ref, upr_ratio = upr / est_ref)
  dd
}
out_Ftarg <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i)
  get_F_ts_target(multi_rep[[i]]) |>
    mutate(scen = model_name[i])) |>
  mutate(scen = forcats::fct_inorder(scen))

ftarg_plot <- function(dat) {
  dat |>
    mutate(scen = forcats::fct_rev(scen)) |>
    ggplot(aes(year, est_ratio, ymin = lwr_ratio, ymax = upr_ratio, colour = scen, fill = scen)) +
    geom_ribbon(alpha = 0.4, colour = NA) +
    geom_line() +
    geom_hline(yintercept = 1, lty = 2, colour = "grey40") +
    ylab("F/F<sub>0.4S0</sub>") +
    xlab("") +
    coord_cartesian(xlim = c(1936, 2023), ylim = c(0, 20), expand = FALSE) +
    labs(colour = "Scenario", fill = "Scenario") +
    scale_y_continuous(trans = "sqrt", breaks = c(0, 0.2, 0.5, 1, 2, 5, 10, 15, 20)) +
    scale_colour_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
    theme(
      axis.title.y = element_markdown()
    )
}

out_Ftarg |> filter(!grepl("^\\(B", scen)) |> ftarg_plot()
ggsave("figs/ss3/refpts/f-ref-ts.png", width = 7, height = 4)

out_Ftarg |> filter(grepl("A0", scen) | grepl("^\\(B", scen)) |> ftarg_plot() +
  ggtitle("**With M set at its historical value when calculating reference points**") +
  theme(
    title = element_markdown()
  ) +
  scale_colour_manual(values = cols_B, guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = cols_B, guide = guide_legend(reverse = TRUE))
ggsave("figs/ss3/refpts/f-ref-ts-B.png", width = 7, height = 4)


x <- out_Ftarg |> filter(year == 2023) |>
  filter(!grepl("^\\(B", scen))
xbase <- filter(x, grepl("A0", scen))

write_tex(xbase$est_ratio |> mround(1), "BaseFratio", "ref-pts.tex")
ci <- paste0(mround(xbase$lwr_ratio, 1), "--", mround(xbase$upr_ratio, 1))
write_tex(ci, "BaseFratioCI", "ref-pts.tex")

mround(mean(x$est_ratio), 1) |>
  write_tex("EnsFratioMean", "ref-pts.tex")
paste0(mround(min(x$lwr_ratio), 1), "--", mround(max(x$upr_ratio), 1)) |>
  write_tex("EnsFratioCI", "ref-pts.tex")

x <- filter(x, !grepl("Low prod", scen))
mround(mean(x$est_ratio), 1) |>
  write_tex("EnsFratioMeanNoLow", "ref-pts.tex")
paste0(mround(min(x$lwr_ratio), 1), "--", mround(max(x$upr_ratio), 1)) |>
  write_tex("EnsFratioCINoLow", "ref-pts.tex")

# Kobe plot -----------------------------------------------------------------

d1 <- out_Ftarg |>
  select(year, f = est_ratio, flwr = lwr_ratio, fupr = upr_ratio, scen)
d2 <- out_depl |>
  select(year, b = est, blwr = lwr, bupr = upr, scen)
d <- left_join(d1, d2)
d <- group_by(d, scen) |>
  mutate(bend = lead(b), fend = lead(f))
d |>
  filter(!grepl("^\\(B", scen)) |>
  filter(year <= 2023) |>
  ggplot(aes(b, f, colour = year)) +
  # geom_segment(aes(x = blwr, xend = bupr), alpha = 0.7) +
  # geom_segment(aes(y = flwr, yend = fupr), alpha = 0.7) +
  geom_segment(aes(xend = bend, yend = fend), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_point(size = 0.5) +
  facet_wrap(~scen) +
  coord_cartesian(ylim = c(0, 15), expand = FALSE, xlim = c(0, 1)) +
  scale_y_continuous(trans = "sqrt", breaks = c(0, 0.2, 1, 5, 10)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  scale_colour_viridis_c(option = "G", direction = -1) +
  xlab(expression(S / S[0])) +
  ylab("F/F<sub>0.4S0</sub>") +
  theme(
    axis.title.y = element_markdown(),
    legend.position = "right"
  ) +
  labs(colour = "Year") +
  geom_hline(yintercept = 1, lty = 2, colour = "grey40") +
  geom_vline(xintercept = 0.2, lty = 2, colour = "grey40") +
  geom_vline(xintercept = 0.4, lty = 3, colour = "grey40")

ggsave("figs/ss3/refpts/kobe.png", width = 9.5, height = 7)

# F40 -----------------------------------------------------------------------

get_F_ts_spr <- function(replist) {
  derived_quants <- replist$derived_quants
  dd <- derived_quants[substring(derived_quants[["Label"]], 1, 2) == "F_", ] |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 2 * se, upr = est + 2 * se) |>
    mutate(year = gsub("F_", "", Label)) |>
    mutate(year = as.numeric(year)) |>
    select(year, est, lwr, upr)
  row.names(dd) <- NULL
  ref <- derived_quants |>
    filter(Label %in% c("annF_SPR")) |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 2 * se, upr = est + 2 * se)
  dd$est_ref <- ref$est
  dd$lwr_ref <- ref$lwr
  dd$upr_ref <- ref$upr
  dd <- mutate(dd, est_ratio = est / est_ref, lwr_ratio = lwr / est_ref, upr_ratio = upr / est_ref)
  dd
}

# Catch at F40 --------------------------------------------------------------

out_f40 <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i)
  get_F_ts_spr(multi_rep[[i]]) |>
    mutate(scen = model_name[i])) |>
  mutate(scen = forcats::fct_inorder(scen))
out_f40 |>
  ggplot(aes(year, est_ratio, ymin = lwr_ratio, ymax = upr_ratio, colour = scen, fill = scen)) +
  geom_ribbon(alpha = 0.4, colour = NA) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2, colour = "grey40") +
  ylab("F at SPR 40") +
  xlab("Year") +
  coord_cartesian(xlim = c(1936, 2024), ylim = c(0, 4.5), expand = FALSE) +
  labs(colour = "Scenario", fill = "Scenario") +
  scale_y_continuous()

# Catch at Fmsy -------------------------------------------------------------

get_MSY <- function(replist) {
  dq <- replist$derived_quants
  dd <- dplyr::filter(dq, Label == "Dead_Catch_MSY") |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 2 * se, upr = est + 2 * se) |>
    select(est, lwr, upr)
  row.names(dd) <- NULL
  dd
}
out_msy <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i)
  get_MSY(multi_rep[[i]]) |>
    mutate(scen = model_name[i])) |>
  mutate(scen = forcats::fct_inorder(scen))
out_msy |>
  filter(!grepl("^\\(B", scen)) |>
  mutate(scen = forcats::fct_rev(scen)) |>
  ggplot(aes(scen, est, ymin = lwr, ymax = upr)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  xlab("") +
  ylab("MSY")

# Catch at target B ---------------------------------------------------------

get_catch_Btgt <- function(replist) {
  dq <- replist$derived_quants
  dd <- dplyr::filter(dq, Label == "Dead_Catch_Btgt") |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 2 * se, upr = est + 2 * se) |>
    select(est, lwr, upr)
  row.names(dd) <- NULL
  dd
}
out_catch <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i)
  get_catch_Btgt(multi_rep[[i]]) |>
    mutate(scen = model_name[i])) |>
  mutate(scen = forcats::fct_inorder(scen))
out_catch |>
  filter(!grepl("^\\(B", scen)) |>
  mutate(scen = forcats::fct_rev(scen)) |>
  ggplot(aes(scen, est, ymin = lwr, ymax = upr)) +
  geom_linerange() +
  geom_point(pch = 21, fill = "white", size = 2) +
  coord_flip() +
  xlab("") +
  ylab("Catch (t) at 0.4 S/S<sub>0</sub>") +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm")
  )
ggsave("figs/ss3/refpts/ref-catch.png", width = 4.5, height = 3.8)

# Get Catch at RR in 2024 ---------------------------------------------------

get_catch_RR2024 <- function(replist) {
  dq <- replist$derived_quants
  dd <- dplyr::filter(dq, Label == "ForeCatch_2024") |>
    mutate(est = Value, se = StdDev) |>
    mutate(lwr = est - 1.96 * se, upr = est + 1.96 * se) |>
    select(est, lwr, upr)
  row.names(dd) <- NULL
  dd
}
out_catch <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i)
    get_catch_RR2024(multi_rep[[i]]) |>
      mutate(scen = model_name[i])) |>
  mutate(scen = forcats::fct_inorder(scen))
out_catch |>
  filter(!grepl("^\\(B", scen)) |>
  mutate(scen = forcats::fct_rev(scen)) |>
  ggplot(aes(scen, est, ymin = lwr, ymax = upr)) +
  geom_linerange() +
  geom_point(pch = 21, fill = "white", size = 2) +
  coord_flip() +
  xlab("") +
  expand_limits(y = 0) +
  ylab("Yield (dead catch) (t) in 2024 at F<sub>0.4S0</sub>") +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm")
  )
ggsave("figs/ss3/refpts/2024-yield-catch.png", width = 4.5, height = 3.8)

x <- filter(out_catch, grepl("A0", scen))
write_tex(x |> pull(est) |> mround(0), "BaseNextYrYield", "ref-pts.tex")
ci <- paste0(mround(x$lwr, 0), "--", mround(x$upr, 0))
write_tex(ci, "BaseNextYrYieldCI", "ref-pts.tex")

xx <- filter(out_catch, !grepl("^\\(B", scen))
mround(mean(xx$est), 0) |>
  write_tex("EnsNextYrYield", "ref-pts.tex")
paste0(mround(min(xx$lwr), 0), "--", mround(max(xx$upr), 0))|>
  write_tex("EnsNextYrYieldCI", "ref-pts.tex")

if (FALSE) {
  setwd("figs/ss3/refpts/")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    1, " -P ", 6, " /opt/homebrew/bin/optipng -strip all"
  ))
  setwd(here::here())
}

plan(sequential)
