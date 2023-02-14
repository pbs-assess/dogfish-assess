library(gfdata)

spp <- "044"
ssid <- c(1, 3, 4, 16, 2, 14, 22, 36, 39, 40, 76, 92, 93)

survey_sets <- get_survey_sets(
  species = spp,
  ssid = ssid,
  join_sample_ids = TRUE,
  verbose = TRUE,
  sleep = 0
)

survey_samples <- get_survey_samples(
  species = spp,
  ssid = ssid
)

commercial_samples <- get_commercial_samples(
  spp,
  unsorted_only = FALSE,
  return_all_lengths = FALSE
)

catch <- get_catch(spp)

# spatial privacy issues:
# cpue_spatial <- get_cpue_spatial(spp)
# cpue_spatial_ll <- get_cpue_spatial_ll(spp)
# catch_spatial <- get_catch_spatial(spp)

# design-based indices:
survey_index <- get_survey_index(spp)

# age_precision <- get_age_precision(spp)

save_dat <- function(obj, filename) {
  attr(obj, "version") <- utils::packageVersion("gfdata")
  attr(obj, "date") <- Sys.time()
  saveRDS(obj, file = paste0("data/raw/", filename, ".rds"))
}

save_dat(survey_sets, "survey-sets")
save_dat(survey_samples, "survey-samples")
save_dat(commercial_samples, "commercial-samples")
save_dat(catch, "catch")
save_dat(survey_index, "design-based-indices")
