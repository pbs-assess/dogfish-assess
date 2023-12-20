#load Canadian survey data


# library -----------------------------------------------------------------

#remotes::install_github("pbs-assess/gfdata")
library(gfdata)

# load Can survey data ----------------------------------------------------
data_survey_samples <- get_survey_samples(species = "north pacific spiny dogfish")
data_survey_samples

data_surveysets <- get_survey_sets(species = "north pacific spiny dogfish")
data_surveysets

saveRDS(data_surveysets, "data/raw/survey-sets_2023.rds")
#saveRDS(data_survey_samples, "output/data_survey_samples.rds")

