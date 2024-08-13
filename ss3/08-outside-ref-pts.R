library(tidyverse)
library(r4ss)

dir.create("figs/refpts/", showWarnings = F)
library(ggplot2)
# options(ggplot2.continuous.colour = "viridis")
# scale_colour_discrete <- function(...) {
#   scale_colour_brewer(..., palette = "Set2")
# }
# scale_fill_discrete <- function(...) {
#   scale_fill_brewer(..., palette = "Set2")
# }
theme_set(gfplot::theme_pbs())

ss_home <- here::here("ss3")

mods <- c(
  "A1", "A0",
  "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_highdiscard",
  "A6_IPHC+CPUE", "A7_SYNonly", "A8_HBLLonly", "A9_lowM", "A10_highM", "A11_low_zfrac", "A12_high_zfrac", "A13_extraSD",
  "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM"
)

model_name <- c(
  "(A1) Base with estimated zfrac",
  "(A0) Base",
  "(A2) US growth",
  "(A3) BC growth, high maturity",
  "(A4) US growth, high maturity",
  "(A5) 100% discard m.",
  "(A6) IPHC + CPUE",
  "(A7) SYN only",
  "(A8) HBLL only",
  # "(A9) M = 0.05",
  "(A9) Low M",
  # "(A10) M = 0.082",
  "(A10) High M",
  # "(A11) zfrac = 0.2, Beta = 0.6",
  "(A11) Low productivity",
  # "(A12) zfrac = 0.6, Beta = 2",
  "(A12) High productivity",
  "(A13) Extra SD on IPHC",
  "(B1) M = 0.074, inc. 1990",
  "(B2) M = 0.074, step 2010",
  "(B3) M = 0.074, step 2005",
  "(B4) M = 0.05, inc. 1990",
  "(B5) M = 0.05, inc. 2010"
)

# model_name <- c("(A0) M = 0.074", "(A9) M = 0.05", "(A10) M = 0.082",
#   "(B1) M = 0.074, inc. 1990", "(B2) M = 0.074, step 2010",
#   "(B3) M = 0.074, step 2005", "(B4) M = 0.05, inc. 1990", "(B5) M = 0.05, inc. 2010")
# model_name <- c("(A0) All indices", "(A6) IPHC + CPUE", "(A6) SYN", "(A7) HBLL")
# model_name <- c("(A0) BCgrowth", "(A2) USgrowth", "(A3) BCgrowth, high mat", "(A4) USgrowth, high mat", "(A5) 100% discard m.")
# zfrac = 0.2, Beta = 0.6", "(A3) zfrac = 0.6, Beta = 2")

reject <- c("B1_1990inc", "B3_2005step", "B4_1990inc_lowM", "A1", "A5_highdiscard", "A8_HBLLonly")

keep <- which(!mods %in% reject)
mods <- mods[keep]
model_name <- model_name[keep]
multi_rep <- lapply(mods, function(x) {
  cat(x, "\n")
  r4ss::SS_output(file.path(ss_home, x),
    verbose = FALSE,
    printstats = FALSE,
    hidewarn = TRUE
  )
})
saveRDS(multi_rep, file = "data/generated/replist-ref-pts.rds")

out <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i) {
    multi_rep[[i]]$derived_quants %>%
      # filter(Label %in% c("annF_Btgt", "annF_SPR", "annF_MSY")) %>%
      filter(Label %in% c("annF_Btgt", "annF_SPR")) %>%
      select(label = Label, est = Value, se = StdDev) |>
      mutate(scen = model_name[i])
  })
row.names(out) <- NULL
out <- out |>
  mutate(scen = forcats::fct_inorder(scen))

out |>
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
out_depl <- seq_along(multi_rep) |>
  purrr::map_dfr(\(i)
  get_b_ratio(multi_rep[[i]]) |>
    mutate(scen = model_name[i])) |>
  mutate(scen = forcats::fct_inorder(scen))

out_depl |>
  filter(!grepl("^\\(B", scen)) |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = scen, value = scen, fill = scen)) +
  geom_ribbon(alpha = 0.4, colour = NA) +
  geom_line() +
  geom_hline(yintercept = 0.4, lty = 3) +
  geom_hline(yintercept = 0.2, lty = 2) +
  ylab(expression(S / S[0])) +
  xlab("") +
  coord_cartesian(xlim = c(1936, 2024), ylim = c(0, 1), expand = FALSE) +
  labs(colour = "Scenario", fill = "Scenario") +
  scale_colour_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired")
ggsave("figs/refpts/depl-ref-ts.png", width = 7, height = 4)

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

library(ggtext)
out_Ftarg |>
  filter(!grepl("^\\(B", scen)) |>
  ggplot(aes(year, est_ratio, ymin = lwr_ratio, ymax = upr_ratio, colour = scen, fill = scen)) +
  geom_ribbon(alpha = 0.4, colour = NA) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  ylab("F/F<sub>0.4S0</sub>") +
  xlab("") +
  coord_cartesian(xlim = c(1936, 2024), ylim = c(0, 20), expand = FALSE) +
  labs(colour = "Scenario", fill = "Scenario") +
  scale_y_continuous(trans = "sqrt", breaks = c(0, 0.2, 0.5, 1, 2, 5, 10, 15, 20)) +
  scale_colour_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme(
    axis.title.y = element_markdown()
  )

ggsave("figs/ss3/refpts/f-ref-ts.png", width = 7, height = 4)

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
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(xintercept = 0.2, lty = 2) +
  geom_vline(xintercept = 0.4, lty = 3)

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
  geom_hline(yintercept = 1, lty = 2) +
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
  ylab("Catch (t) at F<sub>0.4S0</sub>") +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm")
  )
ggsave("figs/ss3/refpts/ref-catch.png", width = 4.5, height = 3.8)
