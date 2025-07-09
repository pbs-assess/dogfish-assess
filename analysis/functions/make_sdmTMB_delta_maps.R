make_sdmTMB_delta_maps <- function(p, folder = here::here("figs/sub-folder-name-here"), french = FALSE) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  w <- 2.01 # tile width
  h <- 2.01 # tile height
  gw <- 9 # ggplot width etc.
  gh <- 8

  # Label translations
  label_encounter_title <- if (french) "Estimations de la probabilité de rencontre" else "Encounter probability estimates"
  label_encounter_prob <- if (french) "Probabilité" else "Probability"
  label_density_title <- if (french) "Estimations de la densité positive" else "Positive density estimates"
  label_density <- if (french) "Densité" else "Density"
  label_est_density_title <- if (french) "Densité estimée (probabilité de rencontre x densité positive)" else "Estimated density (encounter probability x positive density)"
  label_eps1_title <- if (french) "Effets aléatoires spatiotemporels (probabilité de rencontre)" else "Spatiotemporal random effects (encounter probability)"
  label_eps2_title <- if (french) "Effets aléatoires spatiotemporels (densité positive)" else "Spatiotemporal random effects (positive density)"
  label_deviation_logit <- if (french) "Déviation (logit)" else "Deviation in\nlogit space"
  label_deviation_log <- if (french) "Déviation (log)" else "Deviation in\nlog space"
  label_omega1_title <- if (french) "Effets aléatoires spatiaux (probabilité de rencontre)" else "Spatial random effects (encounter probability)"
  label_omega2_title <- if (french) "Effets aléatoires spatiaux (densité positive)" else "Spatial random effects (positive density)"
  label_depth1_title <- if (french) "Effet de la profondeur (probabilité de rencontre)" else "Depth effect (encounter probability)"
  label_depth2_title <- if (french) "Effet de la profondeur (densité positive)" else "Depth effect (positive density)"

  g <- ggplot(p, aes(X, Y, colour = plogis(est1), fill = plogis(est1))) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_viridis_c() +
    scale_fill_viridis_c() +
    coord_fixed() +
    ggtitle(label_encounter_title) +
    labs(colour = label_encounter_prob, fill = label_encounter_prob)
  ggsave(file.path(folder, "map-est1.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = exp(est2), fill = exp(est2))) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans = "sqrt") +
    coord_fixed() +
    ggtitle(label_density_title) +
    labs(colour = label_density, fill = label_density)
  ggsave(file.path(folder, "map-est2.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = plogis(est1) * exp(est2), fill = plogis(est1) * exp(est2))) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans = "sqrt") +
    coord_fixed() +
    ggtitle(label_est_density_title) +
    labs(colour = label_density, fill = label_density)
  ggsave(file.path(folder, "map-est.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = epsilon_st1, fill = epsilon_st1)) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle(label_eps1_title) +
    labs(colour = label_deviation_logit, fill = label_deviation_logit)
  ggsave(file.path(folder, "map-eps1.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = epsilon_st2, fill = epsilon_st2)) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle(label_eps2_title) +
    labs(colour = label_deviation_log, fill = label_deviation_log)
  ggsave(file.path(folder, "map-eps2.png"), width = gw, height = gh)

  g1 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = omega_s1, fill = omega_s1)) +
    geom_tile(width = w, height = h) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle(label_omega1_title) +
    labs(colour = label_deviation_logit, fill = label_deviation_logit)

  g2 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = omega_s2, fill = omega_s2)) +
    geom_tile(width = w, height = h) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle(label_omega2_title) +
    labs(colour = label_deviation_log, fill = label_deviation_log)
  cowplot::plot_grid(g1, g2, ncol = 2, align = "v")
  ggsave(file.path(folder, "map-omega.png"), width = 9, height = 6)

  g1 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = plogis(est_non_rf1), fill = plogis(est_non_rf1))) +
    geom_tile(width = w, height = h) +
    scale_colour_viridis_c() +
    scale_fill_viridis_c() +
    coord_fixed() +
    ggtitle(label_depth1_title) +
    labs(colour = label_encounter_prob, fill = label_encounter_prob)

  g2 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = exp(est_non_rf2), fill = exp(est_non_rf2))) +
    geom_tile(width = w, height = h) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans = "sqrt") +
    coord_fixed() +
    ggtitle(label_depth2_title) +
    labs(colour = label_density, fill = label_density)
  cowplot::plot_grid(g1, g2, ncol = 2, align = "v")
  ggsave(file.path(folder, "map-depth.png"), width = 9, height = 6)
}
