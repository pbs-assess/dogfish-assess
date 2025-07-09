
library(tidyverse)

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

# Set decimal separator for French
if (FRENCH) {
  old_dec <- options()$OutDec
  options(OutDec = ",")
}

# Calculate maximum M given fecundity schedule
# Max M is dependent on growth as well since fecundity is a function of size, not age
ss_home <- here::here("ss3")
multi_rep <- readRDS(file.path(ss_home, "multi_rep_mat.rds"))

# Double check with 03-outside-ss3-figures.R
model_name <- c(
  tr("(A0) BC growth\n(base)", "(A0) Croissance C.-B.\n(base)"),
  tr("(A2) US growth", "(A2) Croissance É.-U."),
  tr("(A3) BC growth,\nhigh maturity", "(A3) Croissance C.-B.,\nmaturité élevée"),
  tr("(A4) USgrowth,\nhigh maturity", "(A4) Croissance É.-U.,\nmaturité élevée"),
  tr("(A14) Low discard\nmortality", "(A14) Faible mortalité\nde rejet"),
  tr("(A5) High discard\nmortality", "(A5) Forte mortalité\nde rejet"),
  tr("(A15) 100% \ndiscard mortality", "(A15) Mortalité de rejet\n100%")
)

Fec_age <- lapply(multi_rep, function(replist) {
  replist$endgrowth %>% filter(Sex == 1) %>% pull(`Mat*Fecund`)
})

M <- sapply(multi_rep, function(replist) {
  replist$endgrowth %>% pull(M) %>% unique()
})

A <- sapply(multi_rep, function(replist) {
  replist$endgrowth %>% pull(int_Age) %>% max()
})

# Survival
surv <- seq(0.01, 1, 0.01)

###### Leslie matrix. Given the age-0 survival (surv0), calculate the realized rate of population increase, conditional on M
calc_r <- function(surv0, Fec_age, M, A) {

  x <- matrix(0, A+1, A+1)

  for (a in 1:A) x[a+1, a] <- exp(-M)
  x[A+1, A+1] <- exp(-M) # Plus group

  x[1, ] <- surv0 * Fec_age[0:A + 1]

  lambda <- eigen(x)$values
  r <- lambda[!Im(lambda)] %>% Re() %>% max() %>% log()

  list(r = r, mat = x)
}

Fec_age_half <- lapply(Fec_age, "*", 0.5) # Model females (assuming sex ratio of pups = 0.5)

# Example with model A0
leslie <- lapply(surv, calc_r, Fec_age = Fec_age_half[[1]], M = M[1], A = A[1])
r <- sapply(leslie, getElement, "r")
plot(surv, r, xlab = "Density-dependent survival", ylab = "Rate of population increase")
abline(h = 0, lty = 2)
surv[r > 0] # Age-0 must exceed 0.30 for a viable population

# The unfished pup survival is the value such that r = 0 when Z = M.
# Otherwise, the population will increase indefinitely in the absence of fishing
approx(r, surv, 0) # 0.301 doesn't match 0.32 (value in SS3)

# Verify with SS3
source("ss3/ss3_functions.R")
SS3_steep(multi_rep[[1]])["S0"]

# The upper bound of r_intrinsic, i.e., the maximum rate of population increase, assuming that unfished pup survival cannot exceed adult survival
approx(surv, r, exp(-M[1])) # r_intrinsic = 0.0276


### Let's calculate unfished spawners per recruit to verify the calculations
calc_phi <- function(Z, A, Fec_age) {
  x <- numeric(A+1)
  x[] <- exp(-Z * (0:A))
  x[A+1] <- x[A+1]/(1 - exp(-Z))

  list(surv = x, phi0 = sum(x * Fec_age))
}
phi <- calc_phi(M[1], A[1], Fec_age_half[[1]])
1/phi$phi0
SS3_steep(multi_rep[[1]])["S0"]

# Search for upper bound on M
s0_fn <- function(M, fec, A) {
  surv <- numeric(A+1)
  surv[] <- exp(-M * (1:(A+1) - 1))
  surv[A+1] <- surv[A+1]/(1 - exp(-M))

  spawners_per_recruit <- sum(surv * fec)
  s0 <- 1/spawners_per_recruit
  return(s0)
}

Mvec <- seq(0.03, 0.15, 0.005)

# Do this for models 1-4
ind <- 1:4
s0 <- lapply(ind, function(i) {
  sapply(Mvec, s0_fn, fec = Fec_age_half[[i]], A = A[i])
})

# Interpolate
maxM <- sapply(s0, function(x) approx(x, Mvec, 1))

# M bound figure
growth <- c(
  tr("BC growth", "Croissance C.-B."),
  tr("US growth", "Croissance É.-U."),
  tr("BC growth", "Croissance C.-B."),
  tr("US growth", "Croissance É.-U.")
)
mat <- c("Taylor & Gallucci", "Taylor & Gallucci", "McFarlane & Beamish", "McFarlane & Beamish")

g <- lapply(1:length(s0), function(i) {
  data.frame(M = Mvec, value = s0[[i]], variable = model_name[i], growth = growth[i], mat = mat[i])
}) %>%
  bind_rows() %>%
  #ggplot(aes(M, value, colour = variable)) +
  ggplot(aes(M, value, colour = growth, linetype = mat)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
  #labs(x = "Natural mortality", y = "Unfished replacement line", colour = "Model") +
  labs(x = tr("Natural mortality", "Mortalité naturelle"),
       y = tr("Unfished replacement line", "Ligne de remplacement non pêchée"),
       colour = tr("Growth", "Croissance"),
       linetype = tr("Maturity", "Maturité")) +
  coord_cartesian(xlim = c(0.04, 0.12), ylim = c(0, 1.25), expand = FALSE)
ggsave(fig_path("ss3/M_bound_fec.png"), g, height = 3, width = 5)

# Reset decimal separator
if (FRENCH) {
  options(OutDec = old_dec)
}

