dir.create("figs/mcmc", showWarnings = FALSE)

FRENCH <- TRUE

# Translation helper function
tr <- function(english, french) {
  if (FRENCH) french else english
}

library(rosettafish)
library(GGally)
library(dplyr)

if (FRENCH) {
  old_dec <- options()$OutDec
  options(OutDec = ",")
}

# Create appropriate figure directories
if (FRENCH) {
  dir.create("figs-french", showWarnings = FALSE)
  dir.create("figs-french/mcmc", showWarnings = FALSE)
  fig_dir <- "figs-french"
} else {
  dir.create("figs/mcmc", showWarnings = FALSE)
  fig_dir <- "figs"
}

.ggsave <- function(filename, ...) {
  if (FRENCH) {
    filename <- file.path(fig_dir, filename)
  } else {
    filename <- file.path(fig_dir, filename)
  }
  ggplot2::ggsave(filename, ...)
}


# Check posterior
ss_home <- here::here("ss3")
#ss_home <- "C:/users/quang/Desktop/dogfish"

SS_dir <- c("A1", "B2_2010step")

WARMUP <- 300

for (i in 1:length(SS_dir)) {
  samps <- readRDS(file.path(ss_home, paste0("adnuts_", SS_dir[i], ".rds")))
  #adnuts::launch_shinyadmb(samps)

  replist <- r4ss::SS_output(file.path(ss_home, SS_dir[i]), "_mceval")

  rename_fn <- function(x) {
    strsplit(x, "_") %>%
      sapply(function(i) {
        if (grepl("Sz", i[1]) || grepl("Size", i[1])) {

          par_name <- paste0(i[1:3], collapse = "_")
          fname <- paste0(i[4:length(i)], collapse = "_")
          paste0(par_name, "_\n", fname)
        } else if (grepl("NatM", i[1])) {

          par_name <- paste0(i[1:4], collapse = "_")
          fname <- paste0(i[5:length(i)], collapse = "_")
          paste0(par_name, "_\n", fname)

        } else {
          paste0(i, collapse = "_")
        }
      })
  }

  ignore <- paste0("ForeRecr_", 2029:2123)
  par_key <- data.frame(
    Par = rownames(samps$monitor),
    ss_par = replist$parameters %>%
      filter(Phase > 0, !Label %in% ignore) %>%
      pull(Label) %>% c("Log-posterior")
  ) %>%
    mutate(ss_name = rename_fn(ss_par))

  samps_est <- samps$samples %>% reshape2::melt() %>%
    rename(It = Var1, Chain = Var2, Par = Var3) %>%
    left_join(par_key, by = "Par") %>%
    mutate(ss_name = factor(ss_name, levels = par_key$ss_name)) %>%
    mutate(Chain = factor(Chain))

  # Important population parameters + posterior
  par_plot <- c("NatM_uniform_Fem_GP_\n1_BLK1mult_2010", "SR_LN(R0)", "SR_surv_zfrac", "Log-posterior")

  # Plot prior
  prior_dens <- lapply(1:nrow(par_key), function(i) {
    par_info <- replist$parameters %>%
      filter(Label == par_key$ss_par[i])

    if (nrow(par_info) && !is.na(par_info$Phase) && par_info$Phase > 0) {
      if (par_info$Pr_type == "No_prior") { # Uniform
        x <- seq(par_info$Min, par_info$Max, length.out = 2)
        pr <- rep(1/(par_info$Max - par_info$Min), length(x))
      } else if (par_info$Pr_type == "Full_Beta") {

        # Bespoke code!
        if (par_info$Prior == 0.5 && par_info$Pr_SD == 0.287717 && par_info$Min == 0 && par_info$Max == 1) {
          x <- seq(0, 1, 0.01)
          pr <- dbeta(x, 1.01, 1.01)
        } else {
          stop("Double check Beta prior calculation")
        }
      } else if (par_info$Pr_type == "Normal") {
        x <- seq(-3, 3, length.out = 20) * par_info$Pr_SD + par_info$Prior
        pr <- dnorm(x, par_info$Prior, par_info$Pr_SD)
      }
    }

    if (exists("x", inherits = FALSE)) {
      data.frame(ss_par = par_key$ss_par[i], x = x, value = pr)
    } else {
      data.frame()
    }
  }) %>%
    bind_rows()

  g <- prior_dens %>%
    left_join(par_key, by = "ss_par") %>%
    mutate(ss_name = factor(ss_name, levels = par_key$ss_name)) %>%
    mutate(value = value/max(value), .by = ss_name) %>%
    #filter(ss_name %in% par_plot) %>%
    ggplot(aes(x, value)) +
    geom_histogram(data = samps_est %>% filter(ss_name != "Log-posterior", !grepl("Recr", ss_name)),
                   aes(value, after_stat(ndensity)),
                   inherit.aes = FALSE,
                   colour = "black", fill = "grey80", linewidth = 0.1) +
    geom_line() +
    gfplot::theme_pbs() +
    facet_wrap(vars(ss_name), scales = "free_x", ncol =  5) +
    labs(x = "Value", y = "Relative density") +
    coord_cartesian(expand = FALSE, ylim = c(0, 1.05)) +
    theme(panel.spacing = unit(0, "in"),
          strip.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  .ggsave(paste0("mcmc/prior_dens_", SS_dir[i], ".png"), g, height = 10, width = 7)

  # Trace plots
  g_worm <- samps_est %>%
    filter(ss_name %in% par_plot) %>%
    filter(It > WARMUP) %>%
    ggplot(aes(It, value, colour = Chain)) +
    geom_line(linewidth = 0.25) +
    gfplot::theme_pbs() +
    facet_wrap(vars(ss_name), scales = "free_y") +
    theme(legend.position = "bottom") +
    labs(x = tr("MCMC iteration", "Itération MCCM"),
         y = tr("Value", "Valeur"),
         colour = tr("Chain", "Chaîne"))
  .ggsave(paste0("mcmc/posterior_wormplot_", SS_dir[i], ".png"), g_worm, height = ifelse(i == 1, 2.5, 4.5), width = 6)

  g_post <- samps_est %>%
    filter(ss_name %in% par_plot) %>%
    filter(It > WARMUP) %>%
    ggplot(aes(value)) +
    gfplot::theme_pbs() +
    geom_histogram(aes(y = after_stat(ndensity)),
                   bins = 25,
                   #binwidth = 0.1,
                   fill = "grey60", colour = "black",
                   linewidth = 0.1) +
    #geom_line(data = prior_dens, aes(y = dens/max(dens))) +
    facet_wrap(vars(ss_name), scales = "free") +
    labs(x = tr("Value", "Valeur"), y = tr("Density", "Densité"))
  .ggsave(paste0("mcmc/posterior_density_", SS_dir[i], ".png"), g_post, height = ifelse(i == 1, 2.5, 4.5), width = 6)

  # Selectivity
  samps_sel <- samps_est %>%
    filter(grepl("sel", Par)) %>%
    filter(It > WARMUP)

  g_worm <- samps_sel %>%
    ggplot(aes(It, value, colour = Chain)) +
    geom_line(linewidth = 0.25) +
    gfplot::theme_pbs() +
    #facet_grid(vars(Fleet), vars(Sel_Par), scales = "free") +
    facet_wrap(vars(ss_name),
               ncol = 5,
               scales = "free_y") +
    theme(panel.spacing = unit(0, "in"),
          strip.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    labs(x = tr("MCMC iteration", "Itération MCCM"),
         y = tr("Value", "Valeur"),
         colour = tr("Chain", "Chaîne"))
  .ggsave(paste0("mcmc/posterior_wormplot_sel_", SS_dir[i], ".png"), g_worm, height = 8, width = 8)

  g_post <- samps_sel %>%
    ggplot(aes(value)) +
    gfplot::theme_pbs() +
    geom_histogram(aes(y = after_stat(ndensity)),
                   bins = 25,
                   fill = "grey60", colour = "black",
                   linewidth = 0.1) +
    theme(panel.spacing = unit(0, "in"),
          strip.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    facet_wrap(vars(ss_name),
               ncol = 4, scales = "free") +
    labs(x = tr("Value", "Valeur"), y = tr("Density", "Densité"))
  .ggsave(paste0("mcmc/posterior_density_sel_", SS_dir[i], ".png"), g_post, height = 10, width = 6)

  # Pair plots
  #library(GGally)
  #g <- samps_est %>%
  #  filter(ss_name %in% par_plot) %>%
  #  filter(It > 10) %>%
  #  mutate(It2 = paste0(Chain, "-", It)) %>%
  #  reshape2::dcast(list("It2", "ss_name"), value.var = "value") %>%
  #  mutate(Chain = strsplit(It2, "-") %>% sapply(getElement, 1) %>% factor()) %>%
  #  #mutate(It = strsplit(It2, "-") %>% sapply(getElement, 2) %>% as.numeric()) %>%
  #  #arrange(Chain, It) %>%
  #  select(-It2) %>%
  #  ggpairs(lower = list(continuous = wrap("points", alpha = 0.2)), labeller = "label_parsed")
  #.ggsave(paste0("figs/mcmc/ggpairs_", SS_dir[i], ".png"), g, height = 5, width = 6)
  #
  #
  #g <- samps_est %>%
  #  filter(grepl("Scale", ss_name) | grepl("Male_Descend", ss_name) | grepl("R0", ss_name) | grepl("NatM", ss_name)) %>%
  #  filter(It > 10) %>%
  #  mutate(It2 = paste0(Chain, "-", It)) %>%
  #  reshape2::dcast(list("It2", "ss_name"), value.var = "value") %>%
  #  mutate(Chain = strsplit(It2, "-") %>% sapply(getElement, 1) %>% factor()) %>%
  #  #mutate(It = strsplit(It2, "-") %>% sapply(getElement, 2) %>% as.numeric()) %>%
  #  #arrange(Chain, It) %>%
  #  select(-It2) %>%
  #  ggpairs(lower = list(continuous = wrap("points", alpha = 0.2)), labeller = "label_parsed")
  #.ggsave(paste0("figs/mcmc/ggpairs_domesel_", SS_dir[i], ".png"), g, height = 8, width = 10)


  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1, #r = abs(cor(x, y)),
                        #r = 0.25,
                        ...)
  {
    par(usr = c(0, 1, 0, 1))
    cor_xy <- cor(x, y)
    txt <- format(c(cor_xy, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    #if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor)
  }

  png(paste0("figs/mcmc/pairs_", SS_dir[i], ".png"), width = 6, height = 6, units = "in", res = 300)
  samps_est %>%
    filter(ss_name %in% par_plot) %>%
    filter(!is.na(ss_name)) %>%
    filter(It > WARMUP) %>%
    mutate(It2 = paste0(Chain, "-", It)) %>%
    reshape2::dcast(list("It2", "ss_name"), value.var = "value") %>%
    select(-It2) %>%
    pairs(upper.panel = panel.cor, gap = 0, row1attop = FALSE)
  dev.off()

  png(paste0("figs/mcmc/pairs_sel_", SS_dir[i], ".png"), width = 6, height = 6, units = "in", res = 300)
  samps_est %>%
    filter(!ss_name %in% par_plot) %>%
    filter(!is.na(ss_name)) %>%
    filter(It > WARMUP) %>%
    mutate(It2 = paste0(Chain, "-", It)) %>%
    reshape2::dcast(list("It2", "ss_name"), value.var = "value") %>%
    select(-It2) %>%
    pairs(upper.panel = panel.cor, gap = 0, row1attop = FALSE)
  dev.off()
}

# Report S and S/S0 time series
df <- lapply(SS_dir, function(i) {
  mcmc_df <- r4ss:::SSgetMCMC(dir = file.path(ss_home, paste0(i, "_mceval")))

  S <- select(mcmc_df, Iter, starts_with("SSB")) %>%
    reshape2::melt(id.vars = "Iter") %>%
    filter(!variable %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY")) %>%
    mutate(variable = as.character(variable)) %>%
    mutate(Year = strsplit(variable, "_") %>% sapply(getElement, 2) %>% as.numeric()) %>%
    rename(SSB = value) %>%
    select(!variable)

  S0 <- select(mcmc_df, Iter, starts_with("SSB_Virgin")) %>%
    reshape2::melt(id.vars = "Iter") %>%
    rename(SSB0 = value) %>%
    select(!variable)

  df <- left_join(S, S0, by = "Iter") %>%
    mutate(dep = SSB/SSB0, model = strsplit(i, "_")[[1]][1])
}) %>%
  bind_rows() %>%
  reshape2::melt(id.vars = c("Iter", "Year", "model")) %>%
  summarise(median = quantile(value, 0.5),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975),
            .by = c(Year, model, variable)) %>%
  filter(variable != "SSB0") %>%
  mutate(label = if (!FRENCH) ifelse(variable == "SSB", "Spawning~~output", "S/S[0]") else ifelse(variable == "SSB", "Production~~de~~recrues", "S/S[0]"))

g <- ggplot(df, aes(Year)) +
  geom_line(aes(y = median)) +
  geom_line(aes(y = lwr), linetype = 2) +
  geom_line(aes(y = upr), linetype = 2) +
  gfplot::theme_pbs() +
  expand_limits(y = 0) +
  coord_cartesian(expand = FALSE) +
  facet_grid(vars(label), vars(model),
             labeller = label_parsed,
             scales = "free_y",
             switch = "y") +
  theme(strip.placement = "outside") +
  labs(x = tr("Year", "Année"), y = NULL)
.ggsave("mcmc/posterior_ts.png", g, height = 4, width = 6)


# Posterior correlations
ss_cor <- lapply(2:length(par_key$ss_name) - 1, function(i) {
  lapply(seq(i+1, length(par_key$ss_name)), function(j) {

    samp <- samps_est %>%
      filter(ss_name %in% par_key$ss_name[c(i, j)]) %>%
      mutate(ItC = paste0(It, "-", Chain)) %>%
      reshape2::dcast(list("ItC", "ss_name"), value.var = "value")

    cor(
      samp[par_key$ss_name[i]],
      samp[par_key$ss_name[j]]
    ) %>%
      reshape2::melt()
  }) %>% bind_rows()
}) %>%
  bind_rows()

hist(ss_cor$value, breaks = seq(-1, 1, 0.05))

ss_cor %>%
  filter(abs(value) > 0.5)

if (FALSE) {
  setwd("figs/mcmc")
  system(paste0(
    "find -X . -name '*.png' -print0 | xargs -0 -n ",
    1, " -P ", 6, " /opt/homebrew/bin/optipng -strip all"
  ))
  setwd(here::here())
}

# Reset decimal separator
if (FRENCH) {
  options(OutDec = old_dec)
}
