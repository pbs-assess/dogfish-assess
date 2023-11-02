start <- 1920
end <- 2022
yrs <- seq(start, end)

N_t <- length(seq(start, end))
N_a <- 120

M <- 0.094 # (Galluci et al, 2009)

lw_a <- exp(-13.28) # (Anderson et al. 2021)
lw_b <- 3.20 # (Anderson et al. 2021)

k <- 0.03 # (Galluci et al, 2009, Tribuzio et al 2010 estimates k of 0.03 for females)

t0 <- -6.4 # females
linf <- 132

age50 <- 35
sd50 <- 10

age <- seq(1, N_a)

l_a <- linf * (1 - exp(-k * (age - t0))) # G17
plot(age, l_a)

w_a <- lw_a * l_a^lw_b # G18
plot(l_a, w_a)

mat_a <- plogis(age, age50, sd50) # FIXME 'scale'
plot(age, mat_a)

f_a <- w_a * mat_a # FIXME change for dogfish?
plot(age, f_a)

F_total <- exp(as.numeric(arima.sim(n = N_t, list(ar = 0.9), sd = sqrt(0.2))))

F_total <- rep(0.15, N_t)
F_total[seq(N_t - 30, N_t)] <- 0.01

plot(F_total)

stopifnot(identical(N_t, length(F_total)))

F_ta <- matrix(nrow = N_t, ncol = N_a)
for (a in 1:N_a) {
  F_ta[, a] <- F_total
}

# female selectivity
a_hat <- 35
gamma_hat <- 10

v_a <- 1 / (1 + exp(-(age - a_hat) / gamma_hat))
plot(v_a)

Z_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    Z_ta[t, a] <- M + F_ta[t, a] * v_a[a]
  }
}

R_init <- 100 # table 6 rbar init
R_bar <- 100 # table 6 rbar init FIXME!!!!
N_ta <- matrix(nrow = N_t, ncol = N_a)

sigmaR <- 0.6
recdevs <- rnorm(N_t, 0, sigmaR)
plot(recdevs, type = "o")

N_ta[, 1] <- R_init * exp(recdevs)
for (t in 1) {
  for (a in 2:N_a) {
    N_ta[t, a] <- R_bar * exp(0) * exp(-M)^(a - 1)
    # should be exp(recdevs[t - a]) but just using mean recdevs
  }
}

for (t in 2:N_t) {
  for (a in 2:N_a) {
    N_ta[t, a] <- N_ta[t - 1, a - 1] * exp(-Z_ta[t - 1, a - 1])
  }
}

C_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    C_ta[t, a] <- (N_ta[t, a] * w_a[a] * F_ta[t, a] *
      v_a[a] * (1 - exp(-Z_ta[t, a]))) / Z_ta[t, a]
  }
}

C_t <- apply(C_ta, 1, sum)
plot(C_t, type = "o")

V_ta <- matrix(nrow = N_t, ncol = N_a)

lambda <- 0
for (t in 1:N_t) {
  for (a in 1:N_a) {
    V_ta[t, a] <- N_ta[t, a] *
      exp(-lambda * Z_ta[t, a]) * v_a[a] * w_a[a]
  }
}

V_t <- apply(V_ta, 1, sum)
plot(V_t, type = "o")

SSB_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    SSB_ta[t, a] <- N_ta[t, a] * f_a[a]
  }
}
SSB_t <- apply(SSB_ta, 1, sum)

B_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    B_ta[t, a] <- N_ta[t, a] * w_a[a]
  }
}
B_t <- apply(B_ta, 1, sum)

SSB_t <- apply(SSB_ta, 1, sum)

# cols <- RColorBrewer::brewer.pal(4, "Dark2")
#
# plot(1:N_t, SSB_t,
#   type = "l", ylab = "T", xlab = "Year",
#   ylim = c(0, max(B_t)), col = cols[1]
# )
# lines(1:N_t, V_t, col = cols[2], lty = 1)
# lines(1:N_t, C_t, col = cols[3], lty = 2)
# lines(1:N_t, B_t, col = cols[4], lty = 1)
# legend("topright",
#   legend = c("SSB", "VB", "Catch", "B"),
#   lty = c(1, 1, 1, 1), col = cols
# )

# ---------------------------------------------------------------------

library(ggplot2)
library(dplyr)
theme_set(gfplot::theme_pbs())

dat_ts <- bind_rows(
  data.frame(type = "Vulnerable biomass", value = V_t, years = yrs),
  data.frame(type = "Catch", value = C_t, years = yrs),
  data.frame(type = "SSB", value = SSB_t, years = yrs),
  data.frame(type = "Biomass", value = B_t, years = yrs),
  data.frame(type = "F", value = F_total, years = yrs),
  data.frame(type = "Recruitment (numbers)", value = R_init * exp(recdevs), years = yrs)
)

dat_age <- bind_rows(
  data.frame(type = "Fecundity", value = f_a, age = age),
  data.frame(type = "Maturity", value = mat_a, age = age),
  data.frame(type = "Length", value = l_a, age = age),
  data.frame(type = "Vulnerability", value = v_a, age = age)
)

g_age <- ggplot(dat_age, aes(age, value)) +
  geom_line() +
  facet_wrap(~type, scale = "free_y") +
  ylim(0, NA) + ylab("") + xlab("Age")

g_ts <- ggplot(dat_ts, aes(years, value)) +
  geom_line() +
  facet_wrap(~type, scale = "free_y", ncol = 3) +
  ylim(0, NA) + ylab("") + xlab("Year")

g <- cowplot::plot_grid(g_age, g_ts, ncol = 2L, rel_widths = c(1, 1.5))
print(g)

