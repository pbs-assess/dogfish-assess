make_sdmTMB_delta_maps <- function(p, folder = here::here("figs/sub-folder-name-here")) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  w <- 2.01 # tile width
  h <- 2.01 # tile height
  gw <- 9 # ggplot width etc.
  gh <- 8
  g <- ggplot(p, aes(X, Y, colour = plogis(est1), fill = plogis(est1))) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_viridis_c() +
    scale_fill_viridis_c() +
    coord_fixed() +
    ggtitle("Encounter probability estimates") +
    labs(colour = "Probability", fill = "Probability")
  ggsave(file.path(folder, "map-est1.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = exp(est2), fill = exp(est2))) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans = "sqrt") +
    coord_fixed() +
    ggtitle("Positive density estimates") +
    labs(colour = "Density", fill = "Density")
  ggsave(file.path(folder, "map-est2.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = plogis(est1) * exp(est2), fill = plogis(est1) * exp(est2))) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans = "sqrt") +
    coord_fixed() +
    ggtitle("Estimated density (encounter probability x positive density)") +
    labs(colour = "Density", fill = "Density")
  ggsave(file.path(folder, "map-est.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = epsilon_st1, fill = epsilon_st1)) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle("Spatiotemporal random effects (encounter probability)") +
    labs(colour = "Deviation in\nlogit space", fill = "Deviation in\nlogit space")
  ggsave(file.path(folder, "map-eps1.png"), width = gw, height = gh)

  g <- ggplot(p, aes(X, Y, colour = epsilon_st2, fill = epsilon_st2)) +
    geom_tile(width = w, height = h) +
    facet_wrap(~year) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle("Spatiotemporal random effects (positive density)") +
    labs(colour = "Deviation in\nlog space", fill = "Deviation in\nlog space")
  ggsave(file.path(folder, "map-eps2.png"), width = gw, height = gh)

  g1 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = omega_s1, fill = omega_s1)) +
    geom_tile(width = w, height = h) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle("Spatial random effects (encounter probability)") +
    labs(colour = "Deviation in\nlogit space", fill = "Deviation in\nlogit space")

  g2 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = omega_s2, fill = omega_s2)) +
    geom_tile(width = w, height = h) +
    scale_colour_gradient2() +
    scale_fill_gradient2() +
    coord_fixed() +
    ggtitle("Spatial random effects (positive density)") +
    labs(colour = "Deviation in\nlog space", fill = "Deviation in\nlog space")
  cowplot::plot_grid(g1, g2, ncol = 2, align = "v")
  ggsave(file.path(folder, "map-omega.png"), width = 9, height = 6)

  g1 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = plogis(est_non_rf1), fill = plogis(est_non_rf1))) +
    geom_tile(width = w, height = h) +
    scale_colour_viridis_c() +
    scale_fill_viridis_c() +
    coord_fixed() +
    ggtitle("Depth effect (encounter probability)") +
    labs(colour = "Probability", fill = "Probability")

  g2 <- ggplot(filter(p, year == max(p$year)), aes(X, Y, colour = exp(est_non_rf2), fill = exp(est_non_rf2))) +
    geom_tile(width = w, height = h) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans = "sqrt") +
    coord_fixed() +
    ggtitle("Depth effect (positive density)") +
    labs(colour = "Density", fill = "Density")
  cowplot::plot_grid(g1, g2, ncol = 2, align = "v")
  ggsave(file.path(folder, "map-depth.png"), width = 9, height = 6)
}
