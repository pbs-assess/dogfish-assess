## save output for SOPO report
bratio_dat |>
  filter(year <= 2023) |>
  filter(catch == 0) |>
  filter(label == "Bratio") |>
  ggplot(aes(year, y = est, ymin = est - 2 * se, ymax = est + 2 * se, colour = model_name, fill = model_name)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line()
combined_variances <- function(means, variances) {
  ## law of total variances
  .var <- function(x) mean((x - mean(x))^2)
  term1 <- mean(variances)
  term2 <- mean(.var(means))
  term1 + term2
}
bb <- bratio_dat |>
  filter(year <= 2023) |>
  filter(catch == 0) |>
  filter(!grepl("\\(B", model_name)) |>
  filter(label == "Bratio") |>
  group_by(year) |>
  summarise(mu = mean(est), se = sqrt(combined_variances(means = est, variances = se^2))) |>
  mutate(lwr = mu - 2 * se, upr = mu + 2 * se, logsd = se / mu)
# ggplot(bb, aes(year, y = mu, ymin = mu - 2 * se, ymax = mu + 2 * se)) +
#   geom_ribbon() +
#   geom_line(colour = "white")
# x <- rnorm(1e6, 0.8, 0.1)
# sd(log(x))
# sd(x) / mean(x)

dog_mvn <- bratio_dat |>
  filter(year <= 2023) |>
  filter(catch == 0) |>
  filter(!grepl("\\(B", model_name)) |>
  filter(label == "Bratio") |>
  filter(year == 2023) |>
  group_by(year, model_name) |>
  group_split() |>
  purrr::map_dfr(\(x) {
    data.frame(year = x$year, model = x$model_name, depletion = rnorm(3e3, mean = x$est, sd = x$se))
  })
dog_mvn <- dog_mvn |> mutate(
  year = year,
  b = depletion,
  model = model,
  lrp = 0.2, usr = 0.4,
  b_lrp = depletion / 0.2,
  b_usr = depletion / 0.4, .keep = "none",
)
# ggplot(dog_mvn, aes(factor(year), b, group = factor(year))) +
# geom_violin()
saveRDS(dog_mvn, file = "data/generated/dogfish-bc-mcmc-2023.rds")

bb_save <- transmute(
  bb,
  species = "pacific spiny dogfish",
  region = "BC",
  year = year,
  log_blrp = log(mu / 0.2),
  sd_log_blrp = logsd,
  q0.05_blrp = lwr / 0.2,
  q0.95_blrp = upr / 0.2,
  log_busr = log(mu / 0.4),
  sd_log_busr = logsd,
  q0.05_busr = lwr / 0.4,
  q0.95_busr = upr / 0.4,
  log_bbmsy = NA_real_,
  sd_log_bbmsy = NA_real_,
  q0.05_bmsy = NA_real_,
  q0.95_bmsy = NA_real_
)
saveRDS(bb_save, "data/generated/dogfish-bc.rds")
## end save data for SOPO report