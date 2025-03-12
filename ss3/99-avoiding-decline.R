# begin avoiding decline: -----------------------------------
## look at avoiding decline year over year as per GMU and rebuilding criterion

bdi <- list()
for (i in 1:5) {
  bdi[[i]] <- bratio_dat |>
    filter(year %in% c(2023 + i, 2023 + i + 1)) |>
    group_by(label, catch, model, model_name) |>
    summarise(increase = est[year == 2023 + i + 1] / est[year == 2023 + i] >= 1) |>
    mutate(year_start = paste0(2023 + i, "-", 2023 + i + 1))
}
bratio_dat_increase <- bind_rows(bdi)

bratio_dat_increase |>
  ungroup() |>
  mutate(catch = as.numeric(as.character(catch))) |>
  mutate(model_name = forcats::fct_rev(model_name)) |>
  ggplot(aes(catch, model_name, fill = increase)) +
  geom_tile(colour = "grey50") +
  scale_fill_manual(
    values = c("TRUE" = "grey90", "FALSE" = "grey30"),
    # breaks = c(0:5),
    labels = c("TRUE" = "Increase or stable", "FALSE" = "Decline"),
    guide = guide_legend(reverse = TRUE)
  ) +
  coord_cartesian(expand = FALSE) +
  # scale_x_continuous(breaks = tacs[seq(1, 1e2, 2)]) +
  labs(fill = "Population\\\nchange") +
  gfplot::theme_pbs() +
  labs(colour = "2018-23 mean dead catch\\\nat discard mortality level") +
  theme(legend.title = ggtext::element_markdown(), legend.position = "bottom") +
  ylab("") +
  xlab("Dead catch (t)") +
  geom_vline(data = dc2, mapping = aes(xintercept = dead_catch, colour = discard_name), lty = 2) +
  # theme(axis.ticks = element_blank(), legend.text = ggtext::element_markdown()) +
  theme(legend.text = ggtext::element_markdown()) +
  scale_colour_manual(values = c(.pal[3], "grey50", .pal[1])) +
  facet_wrap(~year_start, nrow = 1)
ggsave_optipng("figs/ss3/refpts/avoid-decline.png", width = 10, height = 3.9)

# aggregate across 'A' models:
bratio_dat_increase |>
  filter(!grepl("\\(B", model_name)) |>
  group_by(label, catch, year_start) |>
  summarise(prop_increase = mean(increase)) |>
  ungroup() |>
  mutate(catch = as.numeric(as.character(catch))) |>
  mutate(prop_increase_label = sdmTMB:::mround(round(prop_increase, 1), 1)) |>
  ggplot(aes(catch, year_start, fill = prop_increase, label = prop_increase_label)) +
  geom_tile(colour = "grey50") +
  geom_text(size = 3, colour = "grey5") +
  scale_fill_distiller(palette = "Greys", limits = c(0, 1)
    # values = tigure_pal,
    # breaks = c(0:5),
    # labels = labs,
    # guide = guide_legend(reverse = TRUE)
  ) +
  coord_cartesian(expand = FALSE) +
  # scale_x_continuous(breaks = tacs[seq(1, 1e2, 2)]) +
  labs(fill = "Probability of\navoiding decline", "") +
  gfplot::theme_pbs() +
  labs(colour = "2018-23 mean dead catch\nat discard mortality level") +
  # theme(legend.title = ggtext::element_markdown(), legend.position = "bottom") +
  # theme(legend.position = "bottom") +
  ylab("") +
  xlab("Dead catch (t)") +
  geom_vline(data = dc2, mapping = aes(xintercept = dead_catch, colour = discard_name), lty = 2) +
  # theme(axis.ticks = element_blank(), legend.text = ggtext::element_markdown()) +
  # theme(legend.text = ggtext::element_markdown()) +
  scale_colour_manual(values = c(.pal[3], "grey50", .pal[1]))
# facet_wrap(~year_start, nrow = 1)
ggsave_optipng("figs/ss3/refpts/avoid-decline-aggregated.png", width = 7, height = 3.1)

# end avoiding decline ---------------------------------