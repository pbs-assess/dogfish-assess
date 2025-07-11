library(ggplot2)
options(ggplot2.continuous.colour = "viridis")
scale_colour_discrete <- function(...) {
  scale_colour_brewer(..., palette = "Set2")
}

# Set French language option
FRENCH <- TRUE

# Translation helper function
tr <- function(english, french) {
  if (FRENCH) french else english
}

# Helper function for figure paths
fig_path <- function(filename) {
  if (FRENCH) {
    # Create French directory structure
    french_dir <- dirname(file.path("figs-french", filename))
    dir.create(french_dir, showWarnings = FALSE, recursive = TRUE)
    file.path("figs-french", filename)
  } else {
    file.path("figs", filename)
  }
}

.ggsave <- function(filename, ...) {
  filename <- if (FRENCH) {
    # Create French directory structure
    french_dir <- dirname(file.path("figs-french", filename))
    dir.create(french_dir, showWarnings = FALSE, recursive = TRUE)
    .this_fig_dir <- gsub("figs", "figs-french", this_fig_dir)
    file.path(.this_fig_dir, filename)
  } else {
    file.path(this_fig_dir, filename)
  }
  ggplot2::ggsave(filename, ...)
}
theme_set(gfplot::theme_pbs())
library(tidyverse)
library(r4ss)
source("ss3/ss3_functions.R")

source("ss3/99-utils.R")
file.remove("values/models.tex")

# Set decimal separator for French
if (FRENCH) {
  old_dec <- options()$OutDec
  options(OutDec = ",")
}

# Create French fleet names if needed
if (FRENCH) {
  stopifnot(identical(fleet_names$FName, c("Bottom Trawl\nLandings", "Bottom Trawl\nDiscards", "Midwater Trawl",
    "Longline\nLandings", "Hook-Line\nDiscards", "Bottom Trawl CPUE",
    "HBLL Outside", "HS MSA", "IPHC", "Synoptic", "iRec", "Salmon_Bycatch"
  )))
  fleet_names$FName <- c(
    "Chalut de fond\nDébarquements",
    "Chalut de fond\nRejets",
    "Chalut pélagique",
    "Palangre\nDébarquements",
    "Ligne et hameçon\nRejets",
    "CPUE chalut de fond",
    "HBLL extérieur",
    "HS MSA",
    "IPHC",
    "Synoptique",
    "iRec",
    "Prise accessoire saumon"
  )
}

# Compare SS models
ss_home <- here::here("ss3")
# ss_home <- "C:/users/quang/Desktop/dogfish"

dir.create("figs/ss3/set_b", showWarnings = FALSE)
dir.create("figs/ss3/set_a_mat", showWarnings = FALSE)
dir.create("figs/ss3/set_a_zfrac", showWarnings = FALSE)
dir.create("figs/ss3/set_a_ind", showWarnings = FALSE)
dir.create("figs/ss3/catch-update", showWarnings = FALSE)

# Create French directories if needed
if (FRENCH) {
  dir.create("figs-french/ss3/set_b", showWarnings = FALSE, recursive = TRUE)
  dir.create("figs-french/ss3/set_a_mat", showWarnings = FALSE, recursive = TRUE)
  dir.create("figs-french/ss3/set_a_zfrac", showWarnings = FALSE, recursive = TRUE)
  dir.create("figs-french/ss3/set_a_ind", showWarnings = FALSE, recursive = TRUE)
  dir.create("figs-french/ss3/catch-update", showWarnings = FALSE, recursive = TRUE)
}

# Specify which set of plots to generate here
# set_to_plot <- c("growth", "index", "M")[3]

# # RPR update comparison catch:
# mods <- c("A0", "A0-RPR")
# model_name <- c("(A0) Update", "(A0) RPR")
# fig_dir <- "figs/ss3/catch-update"
# multi_rep <- lapply(mods, function(x) {
#   r4ss::SS_output(file.path(ss_home, x),
#     verbose = FALSE,
#     printstats = FALSE,
#     hidewarn = TRUE)
# })
#
# # Plot SR
# g <- SS3_B(multi_rep, model_name) +
#   guides(linetype = 'none') +
#   labs(y = "Spawning output") +
#   coord_cartesian(expand = FALSE) +
#   guides(colour = guide_legend(ncol = 2))
# # .ggsave("spawning_est.png", g, height = 5, width = 6)
# g
# ggsave("figs/catch-rpr-update.png", width = 5, height = 4)

##########################


for (set_to_plot in c("growth", "index", "M", "zfrac")) {

  set_to_plot <- match.arg(set_to_plot, choices = c("growth", "index", "M", "zfrac"))

  if (set_to_plot == "zfrac") {
    mods <- c("A0", "A11_low_zfrac", "A12_high_zfrac")
    model_name <- c(
      tr("(A0) zfrac = 0.4, Beta = 1.0\n(base)", "(A0) zfrac = 0,4, Beta = 1,0\n(base)"),
      tr("(A11) zfrac = 0.2, Beta = 0.6\n(low productivity)", "(A11) zfrac = 0,2, Beta = 0,6\n(productivité faible)"),
      tr("(A3) zfrac = 0.6, Beta = 2\n(high productivity)", "(A3) zfrac = 0,6, Beta = 2\n(productivité élevée)")
    )
    this_fig_dir <- "figs/ss3/set_a_zfrac"
    multi_rep <- lapply(mods, function(x) {
      r4ss::SS_output(file.path(ss_home, x),
        verbose = FALSE,
        printstats = FALSE,
        hidewarn = TRUE)
    })
    saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_zfrac.rds"))
  }
  if (set_to_plot == "growth") {

    # Set A models with growth and maturity scenarios
    mods <- c("A0", "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A14_lowdiscard", "A5_highdiscard", "A15_100discard")

    model_name <- c(
      tr("(A0) BC growth\n(base)", "(A0) Croissance C.-B.\n(base)"),
      tr("(A2) US growth", "(A2) Croissance É.-U."),
      tr("(A3) BC growth,\nhigh maturity", "(A3) Croissance C.-B.,\nmaturité élevée"),
      tr("(A4) USgrowth,\nhigh maturity", "(A4) Croissance É.-U.,\nmaturité élevée"),
      tr("(A14) Low discard\nmortality", "(A14) Faible mortalité\nde rejet"),
      tr("(A5) High discard\nmortality", "(A5) Forte mortalité\nde rejet"),
      tr("(A15) 100% \ndiscard mortality", "(A15) Mortalité de rejet\n100%")
    )

    this_fig_dir <- "figs/ss3/set_a_mat"

    multi_rep <- lapply(mods, function(x) {
      r4ss::SS_output(file.path(ss_home, x),
        verbose = FALSE,
        printstats = FALSE,
        hidewarn = TRUE)
    })
    saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_mat.rds"))

    # Save the model parameters in model 1 (A0) for Res. Doc.
    # This table is then annotated by hand in ss3_par_annotate_dogfish.csv
    multi_rep[[1]]$parameters %>%
      mutate(`Estimated?` = ifelse(Phase > 0, "Yes", "Fixed")) %>%
      select(Label, `Estimated?`, Value, Parm_StDev) %>%
      readr::write_csv(file = "tables/ss3_par_A0.csv")

    g <- lapply(1:2, function(i) {
      replist <- multi_rep[[i]]
      label <- ifelse(i == 1, tr("BC growth", "Croissance C.-B."), tr("US growth", "Croissance É.-U."))
      replist$endgrowth %>%
        select(Sex, int_Age, Len_Beg) %>%
        mutate(Sex = ifelse(Sex == 1, tr("Female", "Femelle"), tr("Male", "Mâle"))) %>%
        mutate(Model = label)
    }) %>%
      bind_rows() %>%
      ggplot(aes(int_Age, Len_Beg, linetype = Model)) +
      geom_line() +
      facet_wrap(vars(Sex)) +
      #gfplot::theme_pbs() +
      labs(x = tr("Age", "Âge"), y = tr("Length", "Longueur")) +
      expand_limits(y = 0)
    if (FRENCH) {
      ggsave("figs-french/ss3/growth-compare.png", g, width = 6, height = 2.5)
    } else {
      ggsave("figs/ss3/growth-compare.png", g, width = 6, height = 2.5)
    }

    # Length at age distribution (add empirical samples)
    age_samps <- readr::read_csv("data/generated/length-age.csv") %>%
      filter(Age <= 70) %>%
      filter(Age %in% seq(5, 70, 5)) %>%
      rename(Length = length) %>%
      mutate(Age = paste("Age", round(Age, 0)) %>% factor(levels = paste("Age", 0:70)))
    LAK <- multi_rep[[1]]$ALK %>%
      reshape2::melt() %>%
      mutate(TrueAge = as.character(TrueAge) %>% as.numeric(),
        Age = paste("Age", TrueAge) %>% factor(levels = paste("Age", 0:70)),
        Length = as.character(Length) %>% as.numeric()) %>%
      filter(Matrix == "Seas: 1 Sub_Seas: 1 Morph: 1") %>%
      filter(TrueAge <= 70) %>%
      filter(TrueAge %in% seq(5, 70, 5))
    mu <- multi_rep[[1]]$endgrowth %>%
      filter(Sex == 1) %>%
      select(int_Age, Len_Beg) %>%
      filter(int_Age %in% seq(5, 70, 5)) %>%
      mutate(Age = paste("Age", int_Age) %>% factor(levels = paste("Age", 0:70)))
    g <- LAK %>%
      ggplot(aes(Length)) +
      #geom_vline(data = mu, aes(xintercept = Len_Beg), linetype = 2) +
      geom_vline(xintercept = 95, linetype = 3) +
      geom_histogram(data = age_samps, aes(y = after_stat(density)), binwidth = 5, colour = "grey80", linewidth = 0.1) +
      geom_density(data = age_samps, aes(y = after_stat(density)), linetype = 2) +
      geom_line(aes(y = value)) +
      facet_wrap(vars(Age)) +
      gfplot::theme_pbs() +
      theme(panel.spacing = unit(0, "in")) +
      labs(x = tr("Length", "Longueur"), y = tr("Length-at-age probability", "Probabilité longueur à l'âge")) +
      ggtitle(tr("Female", "Femelle"))
    if (FRENCH) {
      ggsave("figs-french/ss3/prob-length-at-age.png", g, height = 6, width = 8)
    } else {
      ggsave("figs/ss3/prob-length-at-age.png", g, height = 6, width = 8)
    }

    # Fecundity at age/length
    gmodel <- c(tr("BC growth", "Croissance C.-B."), tr("US growth", "Croissance É.-U."), tr("BC growth", "Croissance C.-B."), tr("US growth", "Croissance É.-U."))
    g1 <- lapply(3:4, function(x) {
      multi_rep[[x]]$endgrowth %>%
        filter(Sex == 1) %>%
        select(int_Age, `Mat*Fecund`, Age_Mat) %>%
        mutate(model = gmodel[x])
    }) %>%
      bind_rows() %>%
      mutate(Fec = `Mat*Fecund`/Age_Mat) %>%
      ggplot(aes(int_Age, Fec, linetype = factor(model))) +
      #geom_point() +
      geom_line() +
      expand_limits(y = 0) +
      labs(x = tr("Age", "Âge"), y = NULL, linetype = tr("Growth model", "Modèle de croissance"))

    g2 <- lapply(3:4, function(x) {
      multi_rep[[x]]$biology %>%
        select(Len_mean, Fec) %>%
        mutate(model = gmodel[x])
    }) %>%
      bind_rows() %>%
      ggplot(aes(Len_mean, Fec)) +
      #geom_point() +
      geom_line() +
      labs(x = tr("Length", "Longueur"), y = tr("Fecundity", "Fécondité"))
    g <- cowplot::plot_grid(g2, g1, rel_widths = c(1, 1.25))
    .ggsave("fecundity.png", g, height = 2, width = 6)

    # Maturity at age/length
    #gmodel <- c("BC growth", "US growth", "BC growth", "US growth")
    #g1 <- lapply(1:2, function(x) {
    #  multi_rep[[x]]$endgrowth %>%
    #    filter(Sex == 1) %>%
    #    select(int_Age, `Len_Mat`) %>%
    #    mutate(model = gmodel[x])
    #}) %>%
    #  bind_rows() %>%
    #  ggplot(aes(int_Age, `Len_Mat`, linetype = factor(model))) +
    #  #geom_point() +
    #  geom_line() +
    #  coord_cartesian(ylim = c(0, 1)) +
    #  labs(x = "Age", y = NULL, linetype = "Growth model")
    #
    #g2 <- lapply(1:2, function(x) {
    #  multi_rep[[x]]$biology %>%
    #    select(Len_mean, Mat) %>%
    #    mutate(model = gmodel[x])
    #}) %>%
    #  bind_rows() %>%
    #  ggplot(aes(Len_mean, Mat)) +
    #  #geom_point() +
    #  geom_line() +
    #  labs(x = "Length", y = "Maturity")
    #g <- cowplot::plot_grid(g2, g1, rel_widths = c(1, 1.25))
    #.ggsave("maturity.png", g, height = 2, width = 6)

  } else if (set_to_plot == "index") {

    # Set A models jackknifing indices
    mods <- c("A0", "A6_IPHC+CPUE", "A7_SYNonly", "A13_extraSD") #, "A8_HBLLonly")
    model_name <- c(
      tr("(A0) All indices (base)", "(A0) Tous les indices (base)"),
      tr("(A6) IPHC + CPUE", "(A6) IPHC + CPUE"),
      tr("(A7) SYN", "(A7) SYN"),
      tr("(A13) Extra SD on IPHC", "(A13) ÉT supplémentaire sur IPHC")
    )

    this_fig_dir <- "figs/ss3/set_a_ind"

    multi_rep <- lapply(mods, function(x) {
      r4ss::SS_output(file.path(ss_home, x),
        verbose = FALSE,
        printstats = FALSE,
        hidewarn = TRUE)
    })
    saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_index_jackknife.rds"))

  } else if (set_to_plot == "M") {

    # Compare low M and M change models (combination of A and B models)
    mods <- c("A0", "A9_lowM", "A10_highM", "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM")
    model_name <- c(
      tr("(A0) M = 0.065 (base)", "(A0) M = 0,065 (base)"),
      tr("(A9) M = 0.057 (low M)", "(A9) M = 0,057 (M faible)"),
      tr("(A10) M = 0.074 (high M)", "(A10) M = 0,074 (M élevé)"),
      tr("(B1) M = 0.065, inc. 1990", "(B1) M = 0,065, aug. 1990"),
      tr("(B2) M = 0.065, step 2010", "(B2) M = 0,065, étape 2010"),
      tr("(B3) M = 0.065, step 2005", "(B3) M = 0,065, étape 2005"),
      tr("(B4) M = 0.057, inc. 1990", "(B4) M = 0,057, aug. 1990"),
      tr("(B5) M = 0.057, inc. 2010", "(B5) M = 0,057, aug. 2010")
    )

    this_fig_dir <- "figs/ss3/set_b"

    multi_rep <- lapply(mods, function(x) {
      r4ss::SS_output(file.path(ss_home, x),
        verbose = FALSE,
        printstats = FALSE,
        hidewarn = TRUE)
    })
    saveRDS(multi_rep, file = file.path(ss_home, "multi_rep_M.rds"))

    # M change
    Minc <- lapply(1:length(multi_rep), function(x) {
      multi_rep[[x]]$M_at_age %>%
        #filter(Yr %in% c(1937, 2023)) %>%
        select(Sex, Yr, `0`) %>%
        rename(M = `0`) %>%
        mutate(scen = model_name[x]) |>
        mutate(scen = factor(scen, levels = model_name))
    })

    rr <- Minc |>
      bind_rows() |>
      filter(!grepl("^\\(A", scen)) |>
      group_by(scen) |>
      summarise(maxM = max(M)) |> pull(maxM) |> range()

    write_tex(mround(min(rr), 2), "EnsBMlwr", file = "models.tex")
    write_tex(mround(max(rr), 2), "EnsBMupr", file = "models.tex")

    xx <- Minc[grep("2010", model_name)] |>
      bind_rows() |>
      group_by(scen) |>
      summarise(maxM = max(M))

    xx |>
      filter(grepl("^\\(B2", scen)) |>
      pull(maxM) |> mround(2) |>
      write_tex("BtwoMmax", file = "models.tex")

    g <- bind_rows(Minc) %>%
      filter(Sex == 1) %>%
      filter(Yr <= 2023) |>
      ggplot(aes(Yr, M, colour = scen)) +
      geom_line() +
      #facet_wrap(vars(Sex)) +
      # gfplot::theme_pbs() +
      expand_limits(y = 0) +
      coord_cartesian(expand = FALSE) +
      labs(x = tr("Year", "Année"), y = tr("Natural mortality", "Mortalité naturelle"), colour = tr("Model", "Modèle"))
    .ggsave("M_year.png", g, height = 3, width = 6)

  }

  # Read and save r4ss report lists ----

  # Check parameters and bounds
  if (FALSE) {
    multi_rep[[1]]$estimated_non_dev_parameters %>% View()
  }

  # Steepness
  sapply(multi_rep, SS3_steep)


  ### Tables
  likelihoods <- lapply(1:length(mods), function(i) {
    x <- multi_rep[[i]][["likelihoods_by_fleet"]] %>%
      filter(!is.na(ALL))
    return(x)
  }) %>%
    structure(names = mods)

  likelihoods <- lapply(1:length(mods), function(i) {
    x <- multi_rep[[i]][["likelihoods_used"]] %>%
      mutate(Component = rownames(.)) %>%
      select(Component, values)
    colnames(x)[2] <- model_name[i]
    return(x)
  }) %>%
    Reduce(dplyr::left_join, .)
  #readr::write_excel_csv(likelihoods, file = "tables/ss3_likelihoods.csv")
  #
  ## Parameters R0, steepness, reference points
  #pars_report <- Map(pars_fn, multi_rep, model_name) %>%
  #  Reduce(left_join, .)
  #readr::write_excel_csv(pars_report, file = "tables/ss3_pars.csv")

  ### r4ss Plot comparisons
  if (FALSE) {
    multi_rep %>%
      r4ss::SSsummarize() %>%
      r4ss::SSplotComparisons(legendlabels = mods)
  }

  ### Custom ggplots

  # Plot index
  g <- Map(SS3_index, multi_rep, model_name, MoreArgs = list(figure = FALSE, french = FRENCH)) %>%
    bind_rows() %>%
    mutate(scen = forcats::fct_inorder(scen)) |>
    mutate(scen = factor(scen, levels = model_name)) |>
    left_join(fleet_names, by = "Fleet_name") %>%
    ggplot(aes(Yr, Obs, ymin = exp(log(Obs) - 1.96 * SE), ymax = exp(log(Obs) + 1.96 * SE))) +
    geom_linerange() +
    geom_point() +
    geom_line(aes(y = Exp, colour = scen)) +
    scale_y_continuous(lim = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(vars(FName), scales = "free_y") +
    gfplot::theme_pbs() +
    labs(x = tr("Year", "Année"), y = tr("Index", "Indice"), colour = tr("Model", "Modèle")) +
    guides(colour = guide_legend(ncol = 2)) +
    theme(panel.spacing = unit(0, "in"), legend.position = "bottom")
  .ggsave("index_fit.png", g, height = 5, width = 6)

  # Plot index with one fit for SAR:
  Map(SS3_index, multi_rep, model_name, MoreArgs = list(figure = FALSE, french = FRENCH)) %>%
    bind_rows() %>%
    mutate(scen = forcats::fct_inorder(scen)) |>
    mutate(scen = factor(scen, levels = model_name)) |>
    left_join(fleet_names, by = "Fleet_name") %>%
    filter(grepl("A0", scen)) |>
    group_by(scen, FName) |>
    mutate(geomean = exp(mean(log(Obs)))) |>
    mutate(Obs = Obs / geomean, Exp = Exp / geomean) |>
    mutate(FName = factor(FName, levels = c("HS MSA", "Bottom Trawl CPUE", "IPHC", "Synoptic", "HBLL Outside"))) |>
    ggplot(aes(Yr, Obs, ymin = exp(log(Obs) - 1.96 * SE), ymax = exp(log(Obs) + 1.96 * SE))) +
    geom_linerange(colour = "grey30") +
    geom_point(pch = 21, colour = "grey30") +
    geom_line(aes(y = Exp), lty = 1, colour = "#3182BD", lwd = 0.7, alpha = 0.8) +
    scale_y_continuous(lim = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(vars(FName), scales = "fixed") +
    gfplot::theme_pbs() +
    labs(x = tr("Year", "Année"), y = tr("Index value", "Valeur d'indice"), colour = tr("Model", "Modèle")) +
    guides(colour = guide_legend(ncol = 2)) +
    theme(panel.spacing = unit(0, "in"), legend.position = "bottom", axis.title.x = element_blank())
  if (FRENCH) {
    ggsave_optipng("figs-french/index_fit_SAR.png", height = 4, width = 6)
  } else {
    ggsave_optipng("figs/index_fit_SAR.png", height = 4, width = 6)
  }

  # Plot SR
  g <- SS3_SR(multi_rep, model_name, french = FRENCH) +
    labs(x = tr("Spawning output (pup production)", "Production reproductrice (production de petits)"))
  .ggsave("srr.png", g, height = 4, width = if (set_to_plot == "zfrac") 8 else 6)

  # Plot recruitment
  g <- SS3_recruitment(multi_rep, model_name, french = FRENCH) +
    labs(y = tr("Recruitment", "Recrutement")) +
    coord_cartesian(expand = FALSE) +
    guides(colour = guide_legend(ncol = 2))
  .ggsave("recruitment.png", g, height = 5, width = 6)

  #g <- SS3_recruitment(multi_rep, model_name, dev = TRUE)
  #.ggsave("recruit_dev.png", g, height = 4, width = 6)

  # Plot SSB
  g <- SS3_B(multi_rep, model_name, french = FRENCH) +
    guides(linetype = 'none') +
    labs(y = tr("Spawning output", "Production reproductrice")) +
    coord_cartesian(expand = FALSE) +
    guides(colour = guide_legend(ncol = 2))
  .ggsave("spawning_est.png", g, height = 5, width = 6)

  g <- SS3_B(multi_rep, model_name, type = "SSB0", french = FRENCH) +
    guides(linetype = 'none') +
    labs(y = tr("Spawning depletion", "Épuisement reproducteur")) +
    coord_cartesian(expand = FALSE, ylim = c(0, 1.1)) +
    guides(colour = guide_legend(ncol = 2))
  .ggsave("depletion_est.png", g, height = 5, width = 6)

  g <- SS3_B(multi_rep, model_name, type = "SSBMSY", french = FRENCH) +
    guides(linetype = 'none') +
    geom_hline(yintercept = 0.4, linetype = 2) +
    geom_hline(yintercept = 0.8, linetype = 3) +
    coord_cartesian(expand = FALSE) +
    guides(color = guide_legend(nrow = 3))
  .ggsave("bbmsy_est.png", g, height = 4, width = 6)

  g <- SS3_B(multi_rep, model_name, type = "B", french = FRENCH) +
    #labs(linetype = 'Sex') +
    theme(legend.position = "bottom") +
    facet_wrap(vars(Area)) +
    coord_cartesian(expand = FALSE) +
    guides(linetype = "none", colour = guide_legend(ncol = 2))
  .ggsave("biomass_est.png", g, height = 4, width = 6)

  # Population fecundity
  #g <- SS3_fecundity(multi_rep, model_name) +
  #  coord_cartesian(ylim = c(0, 10))
  #.ggsave("fecundity_hist.png", g, height = 4, width = 6)

  #g <- SS3_fecundity(multi_rep, model_name, type = "mat") +
  #  coord_cartesian(ylim = c(0, 0.1))
  #.ggsave("fecundity_hist2.png", g, height = 4, width = 6)

  # Selectivity at length (estimated)
  g <- SS3_sel(multi_rep, model_name, type = "Lsel", bin_width = 5, do_mat = FALSE, french = FRENCH) +
    coord_cartesian(xlim = c(40, 115), ylim = c(0, 1.1))
  .ggsave("sel_len.png", g, height = 6, width = 8)

  # Selectivity at age (converted from size based, also show maturity ogive)
  g <- SS3_sel(multi_rep, model_name, bin_width = 5, french = FRENCH)
  .ggsave("sel_age.png", g, height = 6, width = 8)

  # Selectivity at age (converted from size based, also show maturity ogive)
  g <- SS3_sel(multi_rep, model_name, bin_width = 5, scale_max_1 = TRUE, french = FRENCH)
  .ggsave("sel_age_max1.png", g, height = 6, width = 8)


  # Mean length
  fleet_int <- c(1:8)
  g <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = fleet_int)) %>%
    bind_rows() %>%
    left_join(fleet_names, by = c("FleetName" = "Fleet_name")) %>%
    mutate(FName = factor(FName, levels = fleet_names$FName)) %>%
    mutate(scen = factor(scen, levels = model_name)) |>
    ggplot(aes(Yr, Exp, linetype = Sex, shape = Sex)) +
    facet_grid(vars(FName), vars(scen), scales = "free_y") +
    geom_point(aes(y = Obs, colour = FName)) +
    geom_line() +
    labs(x = tr("Year", "Année"), y = tr("Mean length", "Longueur moyenne")) +
    # theme_bw() +
    theme(panel.spacing = unit(0, "in"),
      strip.background = element_blank(),
      legend.position = "bottom") +
    scale_shape_manual(values = c(16, 1)) +
    guides(colour = "none") +
    scale_x_continuous(breaks = seq(1900, 2020, 20))
  .ggsave("mean_length.png", g, height = 6.5, width = 9)

  # Length comps
  heights <- c(4, 5, 6, 4, NA, 6, NA, 8) + 1

  xlim <- c(20, 130)

  for(ff in fleet_int) {
    len <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = ff, mean_length = FALSE)) %>%
      bind_rows()

    if (nrow(len)) {
      len <- len %>%
        mutate(Obs = ifelse(Sex == "Male", -1 * Obs, Obs),
          Exp = ifelse(Sex == "Male", -1 * Exp, Exp),
          Bin = Bin + 5) %>%
        left_join(fleet_names, by = c("FleetName" = "Fleet_name")) %>%
        mutate(FName = factor(FName, levels = fleet_names$FName))

      len_N <- len %>%
        summarise(N = unique(Nsamp_in), .by = c(Yr, FleetName))

      g <- len %>%
        filter(scen == unique(len$scen)[1]) %>%
        ggplot(aes(Bin, Obs)) +
        geom_col(colour = "grey60", width = 5, alpha = 0.75, aes(fill = Sex)) +
        geom_line(data = len, aes(y = Exp, linetype = Sex, colour = scen)) +
        geom_label(data = len_N, x = Inf, y = -Inf, hjust = "inward", vjust = "inward", aes(label = N)) +
        facet_wrap(vars(Yr), ncol = 4) +
        scale_y_continuous(labels = abs) +
        theme(legend.position = "bottom",
          panel.spacing = unit(0, "in")) +
        scale_fill_manual(values = c("grey80", "white")) +
        coord_cartesian(xlim = c(30, 120)) +
        labs(x = tr("Length", "Longueur"), y = tr("Proportion", "Proportion"), colour = tr("Model", "Modèle")) +
        guides(colour = guide_legend(nrow = 3), fill = "none", linetype = "none") +
        ggtitle(unique(len$FName))
      # theme_bw()
      .ggsave(paste0("len_comp_fleet_", ff, ".png"), g, height = heights[ff], width = 6)
    }
  }

  # Base model only
  if (set_to_plot == "growth") {
    # Length comp residual - heat map

    #g <- SS3_compresid(multi_rep[[1]], model_name[1], fleet = fleet_int) +
    #  ggtitle(model_name[1]) +
    #  theme(panel.spacing = unit(0, "in"))
    #.ggsave("len_comp_resid_A1.png", g, height = 7, width = 6)

    # Length comp residual - histogram
    g <- SS3_compresid(multi_rep[[1]], model_name[1], fleet = fleet_int, figure = "histogram", FRENCH = FRENCH) +
      ggtitle(model_name[1]) +
      theme(panel.spacing = unit(0, "in"))
    .ggsave("len_comp_hist_A1.png", g, height = 4, width = 6)

    # Numbers at age
    g <- SS3_N(multi_rep[1], model_name[1], age = seq(0, 50, 10), french = FRENCH)
    g2 <- g$data %>%
      mutate(variable = paste("Age", variable)) %>%
      #mutate(value = value/max(value), .by = c(variable, Sex, scen)) %>%
      ggplot(aes(Yr, value)) +
      geom_line() +
      gfplot::theme_pbs() +
      expand_limits(y = 0) +
      facet_grid(vars(variable), vars(Sex), scales = "free_y") +
      labs(x = tr("Year", "Année"), y = tr("Estimated abundance (1000s)", "Abondance estimée (1000s)")) +
      ggtitle(model_name[1])
    .ggsave("N_age_A1.png", g2, height = 6, width = 5)

    # for SAR:
    g2 <- g$data %>%
      mutate(variable = paste("Age", variable)) %>%
      #mutate(value = value/max(value), .by = c(variable, Sex, scen)) %>%
      ggplot(aes(Yr, value)) +
      geom_line(colour = "grey30") +
      gfplot::theme_pbs() +
      scale_y_continuous(lim = c(-0.2, NA), expand = expansion(mult = c(0, 0.05))) +
      facet_grid(vars(variable), vars(Sex), scales = "free_y") +
      labs(x = tr("Year", "Année"), y = tr("Estimated abundance (1000s)", "Abondance estimée (1000s)")) +
      theme(axis.title.x = element_blank())
    if (FRENCH) {
      ggsave_optipng("figs-french/N_age_A1_SAR.png", g2, height = 5, width = 4.5)
    } else {
      ggsave_optipng("figs/N_age_A1_SAR.png", g2, height = 5, width = 4.5)
    }

    # Compare with numbers at length
    g <- SS3_N(multi_rep[1], model_name[1], type = "length", len = seq(50, 115, 15), french = FRENCH)
    g2 <- g$data %>%
      mutate(variable = paste(variable, "cm") %>% factor(levels = paste(seq(50, 115, 15), "cm"))) %>%
      ggplot(aes(Yr, value)) +
      geom_line() +
      gfplot::theme_pbs() +
      expand_limits(y = 0) +
      facet_grid(vars(variable), vars(Sex), scales = "free_y") +
      labs(x = tr("Year", "Année"), y = tr("Estimated abundance (1000s)", "Abondance estimée (1000s)")) +
      ggtitle(model_name[1])
    .ggsave("N_len_A1.png", g2, height = , width = 5)

    # Exploitation and apical F
    g <- SS3_apicalF(multi_rep[[1]], french = FRENCH)
    g$facet$params$ncol <- 3
    .ggsave("apicalF_fleet_A1.png", g, height = 6, width = 8)

    g <- SS3_apicalF(multi_rep[[1]], FALSE, french = FRENCH)
    .ggsave("apicalF_sex_A1.png", g, height = 2, width = 4)

    # Plot annual selectivity
    g <- SS3_selannual(multi_rep[[1]], french = FRENCH)
    .ggsave("sel_annual_A1.png", g, height = 4, width = 6)

  }

  # Exploitation and apical F
  #g <- SS3_F(multi_rep, model_name, instantaneous = TRUE) +
  #  coord_cartesian(ylim = c(0, 0.25), expand = FALSE)
  #.ggsave("harvest_rate_total.png", g, height = 4, width = 6)
  #
  #g <- SS3_F(multi_rep, model_name, type = "fleet") +
  #  labs(y = "Harvest rate") +
  #  coord_cartesian(ylim = c(0, 1), expand = FALSE)
  #.ggsave("harvest_rate_fleet.png", g, height = 4, width = 6)
  #
  #g <- SS3_vuln(multi_rep, model_name)


  #g <- SS3_F(multi_rep, model_name, type = "FMSY") +
  #  labs(y = expression(U/U[MSY]))
  #.ggsave("harvest_rate_msy.png", g, height = 4, width = 6)

  # Kobe
  #g <- SS3_Kobe(multi_rep, model_name) +
  #  labs(y = expression(U/U[MSY])) +
  #  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 7.5))
  #.ggsave("kobe.png", g, height = 4, width = 6)

  # Yield curve
  # Not used here, only show in zfrac profile, because most models hit lower boundary)
  #g <- SS3_yieldcurve(multi_rep, model_name) +
  #  coord_cartesian(xlim = c(0, 0.03))
  #.ggsave("yieldcurve_F.png", g, height = 3, width = 5)

  #g <- SS3_yieldcurve(multi_rep, model_name, xvar = "SB0")
  #.ggsave("yieldcurve_depletion.png", g, height = 3, width = 6)

  # Compare unfished vs. current length comp in a single model
  # A1 only
  if (set_to_plot == "growth") {

    for(i in 1) {
      #for(i in 1:length(multi_rep)) {
      g <- lapply(c(1:4, 6, 8), function(ff) {
        dat <- SS3_lencomp(multi_rep[[i]], fleet = ff, mean_length = FALSE, ghost = TRUE) %>%
          group_by(FleetName) %>%
          filter(Yr %in% range(Yr)) %>%
          mutate(Exp = ifelse(Sex == "Male", -1 * Exp, Exp)) %>%
          left_join(fleet_names, by = c("FleetName" = "Fleet_name")) %>%
          mutate(FName = factor(FName, levels = fleet_names$FName))

        ML <- dat %>%
          ungroup() %>%
          summarise(value = weighted.mean(Bin, Exp) %>% round(1), .by = c(Yr, Sex, Fleet, FleetName, scen)) %>%
          left_join(fleet_names, by = c("FleetName" = "Fleet_name")) %>%
          mutate(FName = factor(FName, levels = fleet_names$FName))

        gout <- dat %>%
          ggplot(aes(Bin, Exp)) +
          geom_col(colour = "grey60", width = 5, alpha = 0.75, aes(fill = Sex)) +
          geom_label(data = ML %>% filter(Sex == "Female"),
            aes(label = value),
            x = -Inf, y = Inf, hjust = "inward", vjust = "inward") +
          geom_label(data = ML %>% filter(Sex == "Male"),
            aes(label = value),
            x = -Inf, y = -Inf, hjust = "inward", vjust = "inward") +
          facet_grid(vars(FName), vars(Yr)) +
          gfplot::theme_pbs() +
          scale_y_continuous(labels = abs) +
          theme(legend.position = "bottom",
            panel.spacing = unit(0, "in")) +
          scale_fill_manual(values = c("grey80", "white")) +
          #xlim(xlim[[ff]]) +
          labs(x = NULL, y = NULL, colour = tr("Model", "Modèle")) +
          #labs(x = tr("Length", "Longueur"), y = tr("Proportion", "Proportion"), colour = tr("Model", "Modèle")) +
          guides(colour = guide_legend(nrow = 2))

        return(gout)
      })

      library(grid)
      library(gridExtra)
      legend <- ggpubr::get_legend(g)
      g2 <- grid.arrange(
        arrangeGrob(
          grobs = lapply(g, function(x) x + theme(legend.position = "none")),
          ncol = 1,
          bottom = textGrob(tr("Length", "Longueur")),
          left = textGrob(tr("Proportion", "Proportion"), rot = 90)
        ),
        legend,
        heights = c(10, 1)
      )
      #g2 <- ggpubr::ggarrange(plotlist = g, ncol = 1, labels = list(common.legend = TRUE, legend = "bottom")
      .ggsave(paste0("len_comp_1937_model", i, ".png"), g2, height = 10, width = 6)
    }
  }


  # Report mean age
  #for(i in 1:length(multi_rep)) {
  #  g <- lapply(c(1:4, 6, 8), function(ff) {
  #    SS3_agecomp(multi_rep[[i]], fleet = ff, y = c(1937, 2023))
  #  })
  #  g2 <- ggpubr::ggarrange(plotlist = g, ncol = 1, common.legend = TRUE, legend = "bottom")
  #  ggsave(paste0("figs/ss3/age_comp_1937_model", i, ".png"), g2, height = 10, width = 6)
  #}

  # Sex ratio of size comps
  #fleet_int = 1:8
  #comps <- Map(SS3_lencomp, multi_rep, model_name, MoreArgs = list(fleet = fleet_int), mean_length = FALSE) %>%
  #  bind_rows() %>%
  #  mutate(FleetName = factor(FleetName, levels = multi_rep[[1]]$FleetNames[fleet_int]))
  #ratio_exp <- comps %>%
  #  summarise(N = sum(Exp), .by = c(Yr, FleetName, Sex, scen)) %>%
  #  mutate(p_female = N/sum(N), .by = c(Yr, FleetName, scen)) %>%
  #  filter(Sex == "Female")
  #
  #ratio_obs <- comps %>%
  #  summarise(N = sum(Obs), .by = c(Yr, FleetName, Sex, scen)) %>%
  #  mutate(p_female = N/sum(N), .by = c(Yr, FleetName, scen)) %>%
  #  filter(Sex == "Female")
  #
  #g <- ggplot(ratio_exp, aes(Yr, p_female)) +
  #  geom_point(data = ratio_obs %>% select(!scen)) +
  #  geom_line(data = ratio_obs %>% select(!scen), linetype = 3, linewidth = 0.1) +
  #  geom_line(aes(colour = scen)) +
  #  facet_wrap(vars(FleetName)) +
  #  coord_cartesian(xlim = c(1975, 2025), ylim = c(0, 1.05), expand = FALSE) +
  #  gfplot::theme_pbs() +
  #  theme(legend.position = "bottom", panel.spacing = unit(0, "in")) +
  #  labs(x = "Year", y = "Proportion female", colour = "Model") +
  #  guides(colour = guide_legend(ncol = 2))
  #.ggsave("sex_ratio_comp.png", g, height = 4.5, width = 5)


}

# Reset decimal separator
if (FRENCH) {
  options(OutDec = old_dec)
}
