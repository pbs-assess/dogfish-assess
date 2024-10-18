## Possible solution for different reporting units for discards
## requires a decision on size of catch on which to base mean weights
## to include all data use 1
## if a more representative sample is desired try 30 or 100?
## the larger the sample size the smaller the resulting mean weight

set_sample_size <- 1

d <- data$catch

## to focus on recent subset of catch data
# d <- d$catch |>
#   filter(year > 2005 & year < 2024) |>
#   select(year, best_date, gear, fishery_sector, landed_kg, landed_pcs, discarded_kg, discarded_pcs)

d[d$discarded_kg == 0 & d$discarded_pcs > 0, ]$discarded_kg <- NA
d[d$discarded_pcs == 0 & d$discarded_kg > 0, ]$discarded_pcs <- NA

# ## if we wanted to look at mean_weight by year
# d <- d |> mutate(is_kg_na = ifelse(is.na(discarded_kg), "T", "F"),
#                    is_pcs_na = ifelse(is.na(discarded_pcs), "T", "F"))
#
# d_sum <- d |> group_by(year, gear, is_kg_na, is_pcs_na) |> summarise(
#   discarded_kg = sum(discarded_kg, na.rm = TRUE),
#   discarded_pcs = sum(discarded_pcs, na.rm = TRUE),
#   mean_weight = ifelse(is_kg_na == "F" & is_pcs_na == "F",
#                        discarded_kg/discarded_pcs, NA),
#   n = n()
# ) |> distinct()
#
# d_sum |> filter(gear == "HOOK AND LINE") |> view()
#
# d_sum |>
#   filter(!is.na(mean_weight)) |>
#   select(year, gear, mean_weight, n) |>
#   distinct() |>
#   View()

d2 <- d |>
  group_by(fishery_sector, gear, is.na(discarded_kg), is.na(discarded_pcs)) |>
  mutate(
    mean_weight = mean(ifelse(
      !is.na(discarded_kg) & !is.na(discarded_pcs) & discarded_pcs > set_sample_size,
      discarded_kg/discarded_pcs, NA), na.rm = TRUE),
    n = ifelse(
      !is.na(discarded_kg) & !is.na(discarded_pcs) & discarded_pcs > set_sample_size,
      discarded_pcs, NA),
    mean_pcs_per_trip = mean(n, na.rm = TRUE),
    n_w_both = ifelse(!is.na(discarded_kg) & !is.na(discarded_pcs), sum(!is.na(n)), NA)
   ) |> ungroup() |>
  group_by(fishery_sector, gear) |>
  mutate(
     mean_weight = mean(mean_weight, na.rm = TRUE),
     mean_pcs_per_trip = mean(mean_pcs_per_trip, na.rm = TRUE),
     n_w_both = mean(n_w_both, na.rm = TRUE),
     n_w_kg = mean(ifelse(!is.na(discarded_kg) & discarded_kg > 0,
                     sum(!is.na(discarded_kg)), NA), na.rm = TRUE),
     n_w_pcs = mean(ifelse(!is.na(discarded_pcs) & discarded_pcs > 0,
                      sum(!is.na(discarded_pcs)), NA), na.rm = TRUE),
     n_trips = n()
   ) |> ungroup() |>
  mutate(
    discarded_kg_obs = discarded_kg,
    discarded_pcs_obs = discarded_pcs,
    discarded_kg = ifelse(is.na(discarded_kg_obs),
                          discarded_pcs_obs*mean_weight,
                          discarded_kg_obs),
    discarded_pcs = round(ifelse(is.na(discarded_pcs_obs),
                          discarded_kg_obs/mean_weight,
                          discarded_pcs_obs))
  ) |>
  relocate(
    gear, year,
    discarded_kg_obs, discarded_kg,
    discarded_pcs_obs, discarded_pcs
  )

d2 |> select(gear, fishery_sector, mean_weight, mean_pcs_per_trip,
             n_w_both, n_w_kg, n_w_pcs, n_trips) |>
  distinct() |> View()

