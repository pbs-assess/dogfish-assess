

FRENCH <- FALSE
library(rosettafish)
library(GGally)

if (FRENCH) options(OutDec = ",")

.ggsave <- function(filename, ...) {
  if (FRENCH) filename <- file.path("fr", filename)
  ggplot2::ggsave(filename, ...)
}


# Check posterior
#ss_home <- here::here("ss3")
ss_home <- "C:/users/qhuynh/Desktop/dogfish"

SS_dir <- "B2_estM"
i <- 1
samps <- readRDS(file.path(ss_home, paste0("adnuts_", SS_dir[i], ".rds")))
replist <- r4ss::SS_output(file.path(ss_home, SS_dir[i]))

label_fn <- function(x) {
  xx <- x

  xx[grepl("NatM", x) & !grepl("BLK1", x)] <- "M[1937]"
  xx[grepl("NatM", x) & grepl("BLK1", x)] <- "M[2005]"
  xx[grepl("R0", x)] <- "log(R[0])"
  xx[grepl("zfrac", x)] <- "z[frac]"

  xx[grepl("Size_DblN_peak", x)] <- "p[1]^f"
  xx[grepl("Size_DblN_ascend_se", x)] <- "p[3]^f"
  xx[grepl("Size_DblN_descend_se", x)] <- "p[4]^f"
  xx[grepl("SzSel_Male_Peak", x)] <- "p[1]^m"
  xx[grepl("SzSel_Male_Ascend", x)] <- "p[2]^m"
  xx[grepl("SzSel_Male_Descend", x)] <- "p[3]^m"
  xx[grepl("SzSel_Male_Scale", x)] <- "p[5]^m"

  fleet <- ifelse(
    grepl("Size_DblN", x) | grepl("SzSel_Male", x),
    strsplit(x, "(\\(|\\))") %>% sapply(function(x) x[length(x)]),
    ""
  ) %>% as.numeric()

  xx <- ifelse(
    !is.na(fleet),
    paste0(xx, ifelse(fleet <= 5, "~\"Fleet\"~", "~\"Survey\"~"), fleet),
    xx
  )
  return(xx)
}


par_key <- data.frame(
  Par = dimnames(samps$samples)[[3]][!grepl("Fcast_recruitments", dimnames(samps$samples)[[3]])],
  Par_SS = c(rownames(replist$estimated_non_dev_parameters), "Log-posterior")
) %>%
  mutate(Par_label = label_fn(Par_SS))

if ("M[1937]" %in% par_key$Par_label) {
  if ("M[2005]" %in% par_key$Par_label) {
    samps$samples[, , 2] <- samps$samples[, , 1] * exp(samps$samples[, , 2])
  }
} else if ("M[2005]" %in% par_key$Par_label) {
  samps$samples[, , 2] <- 0.074 * exp(samps$samples[, , 2])
}

samps_est <- samps$samples %>% reshape2::melt() %>%
  rename(It = Var1, Chain = Var2, Par = Var3) %>%
  left_join(par_key) %>%
  mutate(Par_label = factor(Par_label, levels = par_key$Par_label)) %>%
  mutate(Chain = factor(Chain))


# Important population parameters + posterior
par_plot <- par_key$Par_label[!grepl("selparm", par_key$Par)]

g_worm <- samps_est %>%
  filter(Par_label %in% par_plot) %>%
  filter(It > 10) %>%
  ggplot(aes(It, value, colour = Chain)) +
  geom_line(linewidth = 0.25) +
  gfplot::theme_pbs() +
  facet_wrap(vars(Par_label),
             scales = "free_y",
             labeller = label_parsed) +
  theme(legend.position = "bottom") +
  labs(x = ifelse(FRENCH, "Itération MCCM", "MCMC iteration"),
       y = en2fr("Value", FRENCH),
       colour = ifelse(FRENCH, "Chaîne", "Chain"))
.ggsave(paste0("figs/mcmc/posterior_wormplot_", SS_dir[i], ".png"), g_worm, height = 4.5, width = 6)

g_post <- samps_est %>%
  filter(Par_label %in% par_plot) %>%
  filter(It > 10) %>%
  ggplot(aes(value)) +
  gfplot::theme_pbs() +
  geom_histogram(aes(y = after_stat(ndensity)),
                 bins = 25,
                 #binwidth = 0.1,
                 fill = "grey60", colour = "black",
                 linewidth = 0.1) +
  #geom_line(data = prior_dens, aes(y = dens/max(dens))) +
  facet_wrap(vars(Par_label), scales = "free", labeller = label_parsed) +
  labs(x = en2fr("Value", FRENCH), y = en2fr("Density", FRENCH))
.ggsave(paste0("figs/mcmc/posterior_density_", SS_dir[i], ".png"), g_post, height = 4.5, width = 6)

# Selectivity
samps_sel <- samps_est %>%
  filter(!Par_label %in% par_plot, !is.na(Par_label)) %>%
  filter(It > 10)

g_worm <- samps_sel %>%
  ggplot(aes(It, value, colour = Chain)) +
  geom_line(linewidth = 0.25) +
  gfplot::theme_pbs() +
  #facet_grid(vars(Fleet), vars(Sel_Par), scales = "free") +
  facet_wrap(vars(Par_label),
             ncol = 5,
             scales = "free_y",
             labeller = label_parsed) +
  theme(panel.spacing = unit(0, "in")) +
  labs(x = ifelse(FRENCH, "Itération MCCM", "MCMC iteration"),
       y = en2fr("Value", FRENCH),
       colour = ifelse(FRENCH, "Chaîne", "Chain"))
.ggsave(paste0("figs/mcmc/posterior_wormplot_sel_", SS_dir[i], ".png"), g_worm, height = 8, width = 6)

g_post <- samps_sel %>%
  ggplot(aes(value)) +
  gfplot::theme_pbs() +
  geom_histogram(aes(y = after_stat(ndensity)),
                 bins = 25,
                 #binwidth = 0.1,
                 fill = "grey60", colour = "black",
                 linewidth = 0.1) +
  #geom_line(data = prior_dens, aes(y = dens/max(dens))) +
  theme(panel.spacing = unit(0, "in")) +
  facet_wrap(vars(Par_label), ncol = 5, scales = "free", labeller = label_parsed) +
  labs(x = en2fr("Value", FRENCH), y = en2fr("Density", FRENCH))
.ggsave(paste0("figs/mcmc/posterior_density_sel_", SS_dir[i], ".png"), g_post, height = 8, width = 6)

# Pair plots
library(GGally)
g <- samps_est %>%
  filter(Par_label %in% par_plot) %>%
  filter(It > 10) %>%
  mutate(It2 = paste0(Chain, "-", It)) %>%
  reshape2::dcast(list("It2", "Par_label"), value.var = "value") %>%
  mutate(Chain = strsplit(It2, "-") %>% sapply(getElement, 1) %>% factor()) %>%
  #mutate(It = strsplit(It2, "-") %>% sapply(getElement, 2) %>% as.numeric()) %>%
  #arrange(Chain, It) %>%
  select(-It2) %>%
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.2)), labeller = "label_parsed")
.ggsave(paste0("figs/mcmc/ggpairs_", SS_dir[i], ".png"), g, height = 5, width = 6)


g <- samps_est %>%
  filter(grepl("Scale", Par_SS) | grepl("Male_Descend", Par_SS) | grepl("R0", Par_SS) | grepl("NatM", Par_SS)) %>%
  filter(It > 10) %>%
  mutate(It2 = paste0(Chain, "-", It)) %>%
  reshape2::dcast(list("It2", "Par_label"), value.var = "value") %>%
  mutate(Chain = strsplit(It2, "-") %>% sapply(getElement, 1) %>% factor()) %>%
  #mutate(It = strsplit(It2, "-") %>% sapply(getElement, 2) %>% as.numeric()) %>%
  #arrange(Chain, It) %>%
  select(-It2) %>%
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.2)), labeller = "label_parsed")
.ggsave(paste0("figs/mcmc/ggpairs_domesel_", SS_dir[i], ".png"), g, height = 8, width = 10)


samps_est %>%
  summarise(stdev = sd(value) %>% round(3), .by = Par_label) %>%
  View()

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

samps_est %>%
  filter(Par_label %in% par_plot) %>%
  filter(!is.na(Par_label)) %>%
  filter(It > 10) %>%
  mutate(It2 = paste0(Chain, "-", It)) %>%
  reshape2::dcast(list("It2", "Par_label"), value.var = "value") %>%
  select(-It2) %>%
  pairs(upper.panel = panel.cor, gap = 0, row1attop = FALSE)
