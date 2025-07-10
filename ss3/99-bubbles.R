# Set French language option
FRENCH <- TRUE

# Set decimal option for French
if (FRENCH) {
  old_dec <- options()$OutDec
  options(OutDec = ",")
}

# Translation helper function
tr <- function(english, french) {
  if (FRENCH) french else english
}

# Create appropriate figure directories
if (FRENCH) {
  dir.create("figs-french", showWarnings = FALSE)
  fig_dir <- "figs-french"
} else {
  dir.create("figs", showWarnings = FALSE, recursive = TRUE)
  fig_dir <- "figs"
}

# Helper function for figure paths
fig_path <- function(filename) {
  file.path(fig_dir, filename)
}

library(ggplot2)
library(dplyr)

# d <- readRDS("~/Downloads/SS3_N_A0.rds")

dd <- r4ss::SS_output("ss3/A0/")
dd <- dd$natage
age <- seq(0, 60, 10)
out <- dd |>
  filter(Era == "TIME", `Beg/Mid` == "B") %>%
  select(Yr, Sex, as.character(age)) %>%
  reshape2::melt(id.vars = c("Yr", "Sex")) %>%
  mutate(Sex = ifelse(Sex == 1, tr("Female", "Femelle"), tr("Male", "Mâle")))

out |>
  ggplot(aes(Yr, variable, size = value, colour = value)) +
  geom_point(pch = 21) +
  geom_point(pch = 19, alpha = 0.07) +
  geom_abline(intercept = seq(-500, 0, 1), slope = 0.1, colour = "grey60", lty = 2) +
  theme(axis.title.x = element_blank()) +
  ylab(tr("Age", "Âge")) +
  facet_wrap(~Sex) +
  scale_size_area(max_size = 12) +
  guides(colour = "none", size = "none") +
  coord_cartesian(expand = FALSE, xlim = range(out$Yr), ylim = c(0.8, 6.2)) +
  scale_x_continuous(breaks = seq(1940, 2020, 10)) +
  scale_colour_viridis_c() +
  gfplot::theme_pbs()
source("ss3/99-utils.R")
ggsave_optipng(fig_path("bubble.png"), width = 9, height = 3.5)

# Reset decimal separator
if (FRENCH) {
  options(OutDec = old_dec)
}
