library(ggplot2)
library(dplyr)
theme_set(gfplot::theme_pbs())

EXTRA_PLOTS <- FALSE

# Stuff to adjust:
start <- 1920
end <- 2022

lw_a <- exp(-13.28) # (Anderson et al. 2021)
lw_b <- 3.20 # (Anderson et al. 2021)

k <- 0.03 # (Galluci et al, 2009, Tribuzio et al 2010 estimates k of 0.03 for females)
t0 <- -6.4 # females
linf <- 132

# maturity
age50 <- 35
sd50 <- 10

# commercial selectivity
a_hat <- 10 # logistic 50%
gamma_hat <- 5 # logistic SD

steepness <- 0.283 # US West Coast assessment
sigmaR <- 0.2 # US West Coast assessment

M <- 0.065 # (Galluci et al, 2009)

# fishing mortality time series
# F_total <- exp(as.numeric(arima.sim(n = N_t, list(ar = 0.9), sd = sqrt(0.2))))
N_t <- length(seq(start, end))
F_total <- rep(0.06, N_t)
fishing_stopped_n_yrs_ago <- 50
F_total[seq(N_t - fishing_stopped_n_yrs_ago, N_t)] <- 0.001

# ---------------------------------------------------------------------------

yrs <- seq(start, end)
N_t <- length(seq(start, end))
N_a <- 150
age <- seq(1, N_a)

l_a <- linf * (1 - exp(-k * (age - t0))) # G17
if (EXTRA_PLOTS) plot(age, l_a)

w_a <- lw_a * l_a^lw_b # G18
if (EXTRA_PLOTS) plot(l_a, w_a)

mat_a <- plogis(age, age50, sd50) # FIXME 'scale'
if (EXTRA_PLOTS) plot(age, mat_a)

f_a <- w_a * mat_a # FIXME change for dogfish?
if (EXTRA_PLOTS) plot(age, f_a)

if (EXTRA_PLOTS) plot(F_total)

stopifnot(identical(N_t, length(F_total)))

F_ta <- matrix(nrow = N_t, ncol = N_a)
for (a in 1:N_a) {
  F_ta[, a] <- F_total
}

v_a <- 1 / (1 + exp(-(age - a_hat) / gamma_hat))
if (EXTRA_PLOTS) plot(v_a)

Z_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    Z_ta[t, a] <- M + F_ta[t, a] * v_a[a]
  }
}

recdevs <- rnorm(N_t, 0, sigmaR)
if (EXTRA_PLOTS) plot(recdevs, type = "o")

init_omegas <- rnorm(N_a, 0, sigmaR)

N_s <- 1 # 1 sex

# NOTE: phi_E is phib in iSCAM code and some docs!!!
# phi_E is spawning biomass per recruit
# T.3.10: phi_E is the sum over ages of la * fa
# i.e. survivorship in unfished state to age a * fecundity at age a
# T3.8
# phi_b += lw * fa; iscam code
survivorship <- numeric(length = N_a)
survivorship[1] <- 1
for (a in 2:N_a) survivorship[a] <- survivorship[a - 1] * exp(-M)
# survivorship[N_a] <- survivorship[N_a] / 1 - exp(-M) # plus group # FIXME??
if (EXTRA_PLOTS) plot(1:N_a, survivorship, type = "o")

phi_E <- sum(survivorship * f_a)
phi_E

get_SR_params <- function(steepness, R0, phi_E) {
  kappa <- (4 * steepness) / (1 - steepness) # compensation ratio; e.g. Forrest et al. 2020 Eq. D.8
  s0 <- kappa / phi_E # s0: maximum juvenile survival rate (eq. 11)
  Beta <- (kappa - 1) / (R0 * phi_E) # Beta: density effect on recruitment (eq. 12)
  list(s0 = s0, Beta = Beta, kappa = kappa)
}

R0 <- 1000 # Adjust?

# numbers at age and SSB:
N_ta <- matrix(nrow = N_t, ncol = N_a, data = 0)
SSB_ta <- matrix(nrow = N_t, ncol = N_a, data = 0)
R_init <- R0

# initialize numbers at age and SSB in first time step
for (s in 1:N_s) { # T5.4 iscam docs
  ii <- 0
  for (t in 1) {
    for (a in 2:N_a) {
      ii <- ii + 1
      N_ta[t, a] <- R_init * exp(init_omegas[ii] - 0.5 * sigmaR * sigmaR) *
        exp(-M[s])^(a - 1) / N_s
    }
  }
}

# initialize SSB + B in first time step
SSB_ta <- matrix(nrow = N_t, ncol = N_a)
B_ta <- matrix(nrow = N_t, ncol = N_a)
for (a in 2:N_a) {
  SSB_ta[1, a] <- N_ta[1, a] * f_a[a]
  B_ta[1, a] <- N_ta[1, a] * w_a[a]
}

SSB_t <- numeric(N_t)
R_t <- numeric(N_t)
B_t <- numeric(N_t)

p <- get_SR_params(steepness, R0, phi_E)
Beta <- p$Beta
s0 <- p$s0

C_ta <- matrix(nrow = N_t, ncol = N_a)
V_ta <- matrix(nrow = N_t, ncol = N_a)

# initial year:
SSB_t[1] <- sum(SSB_ta[1, ], na.rm = TRUE)
B_t[1] <- sum(B_ta[1, ], na.rm = TRUE)

# loop through dynamics:
for (t in 1:N_t) {
  for (a in 1:N_a) {
    if (t > 1) { # t = 1 already done above
      if (a == 1) { # t = 1 already done above
        # N_ta[t, a] <- R_bar * exp(omegas[t]) / N_s # FIXME!!
        R_t[t] <- s0 * SSB_t[t - 1] / (1 + Beta * SSB_t[t - 1]) # B.H. T5.13  FIXME SSB[t]?
        R_t[t] <- R_t[t] * exp(recdevs[t] - 0.5 * sigmaR * sigmaR)
        N_ta[t, a] <- R_t[t]
      } else {
        N_ta[t, a] <- N_ta[t - 1, a - 1] * exp(-Z_ta[t - 1, a - 1])
        if (a == N_a) { # plus group
          N_ta[t, a] <- N_ta[t, a] +
            N_ta[t - 1, a - 1] * exp(-Z_ta[t - 1, a])
        }
      }
    }
    SSB_ta[t, a] <- N_ta[t, a] * f_a[a]
    B_ta[t, a] <- N_ta[t, a] * w_a[a]
    C_ta[t, a] <- (N_ta[t, a] * w_a[a] * F_ta[t, a] *
      v_a[a] * (1 - exp(-Z_ta[t, a]))) / Z_ta[t, a]
    lambda <- 0 # FIXME: I forget
    V_ta[t, a] <- N_ta[t, a] *
      exp(-lambda * Z_ta[t, a]) * v_a[a] * w_a[a]
  }
  SSB_t[t] <- sum(SSB_ta[t, ], na.rm = TRUE)
  B_t[t] <- sum(B_ta[t, ], na.rm = TRUE)
}

C_t <- apply(C_ta, 1, sum)
if (EXTRA_PLOTS) plot(C_t, type = "o")

V_t <- apply(V_ta, 1, sum)
if (EXTRA_PLOTS) plot(V_t, type = "o")

if (EXTRA_PLOTS) plot(SSB_t)
if (EXTRA_PLOTS) plot(B_t)

dat_ts <- bind_rows(
  data.frame(type = "Vulnerable biomass", value = V_t, years = yrs),
  data.frame(type = "Catch", value = C_t, years = yrs),
  data.frame(type = "SSB", value = SSB_t, years = yrs),
  data.frame(type = "Biomass", value = B_t, years = yrs),
  data.frame(type = "F", value = F_total, years = yrs),
  data.frame(type = "Recruitment (numbers)", value = c(NA, R_t[-1]), years = yrs)
)

dat_age <- bind_rows(
  data.frame(type = "Fecundity", value = f_a, age = age),
  data.frame(type = "Maturity", value = mat_a, age = age),
  data.frame(type = "Length", value = l_a, age = age),
  data.frame(type = "Vulnerability", value = v_a, age = age),
  data.frame(type = "Survivorship", value = survivorship, age = age)
)

g_age <- ggplot(dat_age, aes(age, value)) +
  geom_line() +
  facet_wrap(~type, scale = "free_y") +
  ylim(0, NA) +
  ylab("") +
  xlab("Age")

p <- get_SR_params(steepness, R0 = R0, phi_E = phi_E)
# p <- get_SR_params(0.2, R0 = R0, phi_E = phi_E)
SSB_plot <- seq(0, max(SSB_t), length.out = 100)
recruits_plot <- p$s0 * SSB_plot / (1 + p$Beta * SSB_plot)
g_sr <- ggplot(data.frame(SSB = SSB_plot, R = recruits_plot), aes(SSB, R)) +
  geom_line() +
  geom_point(data = data.frame(SSB_t = SSB_t[-1], R_t = R_t[-1], year = yrs[-1]), aes(SSB_t, R_t, colour = year)) +
  scale_colour_viridis_c()
g_sr

g_ts <- ggplot(dat_ts, aes(years, value)) +
  geom_line() +
  facet_wrap(~type, scale = "free_y", ncol = 3) +
  ylim(0, NA) +
  ylab("") +
  xlab("Year") +
  geom_vline(xintercept = end - fishing_stopped_n_yrs_ago)

g <- cowplot::plot_grid(g_sr, g_age, g_ts, ncol = 1L, rel_heights = c(1, 2, 2))
print(g)
