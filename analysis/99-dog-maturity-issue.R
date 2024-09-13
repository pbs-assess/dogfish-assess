library(gfplot)
library(tidyr)
library(ggplot2)
library(dplyr)
theme_set(theme_pbs())


d <- readRDS("data/raw/survey-samples.rds")
d <- mutate(d, species_common_name = "north pacific spiny dogfish") # some missing!?, there are NAs at the bottom of the dataset for ALL columns

d |> filter(sex %in% c(1,2)) |>
  drop_na(maturity_code) |>
  filter(maturity_code != 0) |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  ggplot(aes(as.factor(maturity_code), length, colour = as.factor(maturity_code))) +
  geom_jitter() +
  facet_wrap(~sex)

#bin the data
d2 <- d |> mutate(matbin = ifelse(sex == 1 & maturity_code <30, "immature", ifelse(sex == 1 & maturity_code >= 30, "mature", ifelse(sex ==2 & maturity_code < 77, "immature", ifelse(sex ==2 & maturity_code >=77, "mature", NA)))))

d2 |> filter(sex %in% c(1,2)) |>
  drop_na(maturity_code) |>
  filter(maturity_code != 0) |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  ggplot(aes((matbin), length, colour = (matbin))) +
  geom_jitter() +
  facet_wrap(~sex) #lots of large females are not code 77 and therefore immature

d2 |> filter(sex %in% c(1,2)) |>
  drop_na(maturity_code) |>
  filter(maturity_code != 0)|>
  filter(sex == 2) |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  ggplot(aes((matbin), length, colour = (matbin))) +
  geom_jitter() +
  facet_wrap(~year)

d2 |> filter(sex %in% c(1,2)) |>
  drop_na(maturity_code) |>
  filter(maturity_code != 0)|>
  filter(sex == 2) |>
  ggplot(aes((matbin), length, colour = (matbin))) +
  geom_jitter() +
  facet_wrap(~month) #month 10 is the DOG inside survey

#run a model with outside DFO data
fit <- d |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6),
                custom_maturity_at = c(30, 77))
summary(fit$model)
plot_mat_ogive(fit) #f50 is 97.6

#test if we define females > 90 cm at mature what happens to the F50
#rm potentially misclassified large females
#test <- d |> mutate(maturity_code = ifelse(sex == 2 & length > 90, 77, maturity_code))
test <- d |> mutate(matbin = ifelse(sex == 1 & maturity_code <30, "immature", ifelse(sex == 1 & maturity_code >= 30, "mature", ifelse(sex ==2 & maturity_code < 77, "immature", ifelse(sex ==2 & maturity_code >=77, "mature", NA)))))
test2 <- test |> filter(sex == 2 & length > 90 & matbin == "immature")
test  <- filter(test, !test$specimen_id %in% test2$specimen_id)

test |>
  filter(sex %in% c(1,2)) |>
  drop_na(maturity_code) |>
  filter(maturity_code != 0) |>
  ggplot(aes((matbin), length, colour = (matbin))) +
  geom_jitter() +
  facet_wrap(~sex)

fit2 <- test |>
  filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) |>
  fit_mat_ogive(type = "length", usability_codes = c(0, 1, 2, 6),
                custom_maturity_at = c(30, 77))
summary(fit2$model)

#Gertseva, Taylor have a 50% maturity for females at 88 cm, and Tribuzio estimated 89.9
plot_mat_ogive(fit2) #f50 is 88.2 #mat curve when female legnth >90 is mature
plot_mat_ogive(fit) #f50 is 97.6
