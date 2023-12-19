library(dplyr)
library(ggplot2)

fleet_index <- c("Bottom trawl" = 1,
                 "Midwater trawl" = 2,
                 "Longline" = 3,
                 "Hook and line" = 3,
                 "Iphc" = 4,
                 "IPHC" = 4,
                 "HBLL" = 5,
                 "SYN" = 6,
                 "Syn" = 6,
                 "Trawl_3565" = 7,
                 "LL_3565" = 8,
                 "Trawl" = 9) # Trawl_6695

# trawl_1935 is the proportion of 1935-1965 catch assigned to bottom trawl (remaining goes to hook and line)
ss3_catch <- function(csv = TRUE, trawl_1935 = 0.5) {
  catch <- readRDS("data/generated/catch.rds") %>%
    filter(area != "4B") %>%
    mutate(catch = 1e-3 * (landed_kg + discarded_kg)) %>%
    reshape2::acast(list("year", "gear"), value.var = "catch")


  catch <- readRDS("data/generated/catch.rds") %>%
    filter(area != "4B",
           gear %in% c("Bottom trawl", "Midwater trawl", "Hook and line", "Trawl", "Trawl + hook and line")) %>%
    group_by(year, gear) %>%
    summarise(landing = 1e-3 * sum(landed_kg),
              discard = 1e-3 * sum(discarded_kg)) %>%
    reshape2::melt(id.vars = c("year", "gear"))

  g <- ggplot(catch, aes(year, value, shape = variable, linetype = variable)) +
    facet_grid(vars(gear), vars(variable)) +
    geom_point() +
    geom_line()

  ### Combine discard and landings for now
  catch_combine <- catch %>%
    group_by(year, gear) %>%
    summarise(value = sum(value))

  split_1935 <- function(catch, trawl_1935 = 0.5) {

    catch_ex1935 <- filter(catch_combine, gear != "Trawl + hook and line")
    catch_1935 <- filter(catch_combine, gear == "Trawl + hook and line") %>%
      mutate(`Trawl_3565` = value * trawl_1935,
             `LL_3565` = value * (1 - trawl_1935)) %>%
      select(year, Trawl_3565, LL_3565) %>%
      reshape2::melt(id.vars = "year") %>%
      rename(gear = variable)
    rbind(catch_1935, catch_ex1935)
  }

  out <- catch_combine %>%
    split_1935(trawl_1935 = trawl_1935) %>%
    mutate(fleet = fleet_index[match(gear, names(fleet_index))],
           se = 0.01,
           season = 1) %>%
    filter(value > 0,
           year < 2023) %>%
    select(year, season, fleet, value, se) %>%
    arrange(fleet, year)

  if (csv) write.csv(out, file = "data/ss3/ss3-catch.csv", row.names = FALSE)
  invisible(out)
}
ss3_catch()


ss3_index <- function(csv = TRUE) {
  # IPHC
  iphc <- readRDS("data/generated/geostat-ind-iphc.rds") %>%
    mutate(fleet = fleet_index["IPHC"])

  # HBLL
  hbll <- readRDS("data/generated/geostat-ind-hbll-out.rds") %>%
    mutate(fleet = fleet_index["HBLL"]) %>%
    select(-survey_abbrev)

  # Synoptic Trawl
  syn <- readRDS("data/generated/geostat-ind-synoptic.rds") %>%
    mutate(fleet = fleet_index["SYN"]) %>%
    select(-survey_abbrev)

  ind <- rbind(hbll, iphc, syn) %>%
    mutate(month = 1) %>%
    select(year, month, fleet, est, se) %>%
    arrange(fleet, year)

  if (csv) write.csv(ind, file = "data/ss3/ss3-index.csv", row.names = FALSE)

  invisible(ind)
}
ss3_index()

ss3_length <- function(csv = TRUE, bin_size = 4) {

  dtrawl <- readRDS("data/raw/survey-samples.rds")
  diphc <- read.csv("data/raw/IPHC_dogfish_lengths2021.csv") %>%
    filter(reg_area == "2B", sex %in% c("F", "M")
    )
  dc <- readRDS("data/raw/commercial-samples.rds")

  # Will naively sum all trawl lengths together
  lengths_survey <- dtrawl %>%
    filter(survey_abbrev %in% c("SYN WCHG", "SYN HS", "SYN QCS", "SYN WCVI")) %>%
    mutate(survey_abbrev = "SYN") %>%
    select(year, length, sex, survey_abbrev, specimen_id, age, usability_code)

  lengths_iphc <- diphc %>%
    mutate(sex = ifelse(sex == "F", 2, 1),
           survey_abbrev = "IPHC") %>%
    select(year, length, sex, survey_abbrev) %>%
    mutate(specimen_id = 1:n(),
           age = NA,
           usability_code = 1)

  lengths_comm <- dc |>
    filter(!grepl("4B", major_stat_area_name)) |>
    filter(sampling_desc %in% "UNSORTED") |>
    filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
    rename(survey_abbrev = gear_desc) %>%
    select(year, length, sex, survey_abbrev, specimen_id, age, usability_code)

  # Plot by sampling desc
  g <- dc %>%
    filter(!grepl("4B", major_stat_area_name)) |>
    #filter(sampling_desc %in% "UNSORTED") |>
    filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
    filter(sex %in% c(1, 2)) %>%
    ggplot(aes(length, #..ndensity..,
               group = sex, colour = factor(sex))) +
    facet_grid(vars(sampling_desc), vars(gear_desc), scales = "free_y") +
    geom_freqpoly()

  g <- dc %>%
    filter(!grepl("4B", major_stat_area_name)) |>
    #filter(sampling_desc %in% "UNSORTED") |>
    filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
    filter(sex %in% c(1, 2)) %>%
    group_by(year, gear_desc, sampling_desc, sex) %>%
    summarise(n = n()) %>%
    ggplot(aes(year, n,
               group = sex, colour = factor(sex))) +
    facet_grid(vars(sampling_desc), vars(gear_desc), scales = "free_y") +
    geom_line() +
    geom_point()



  lengths_all <- rbind(lengths_survey, lengths_iphc, lengths_comm) %>%
    mutate(species_common_name = unique(dc$species_common_name)) %>%
    gfplot::tidy_lengths_raw(sample = "survey",
                             survey = c("SYN", "IPHC", "BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL"),
                             bin_size = bin_size)

  #g <- gfplot::plot_lengths(lengths_all, show_year = "all", bin_size = bin_size)

  bin_all <- seq(min(lengths_all$length_bin),
                 max(lengths_all$length_bin),
                 by = bin_size)

  length_format <- lapply(unique(lengths_all$survey_abbrev), function(ff) {

    dat <- lengths_all %>%
      filter(survey_abbrev == ff) %>%
      mutate(n = total * proportion)

    dat_full <- expand.grid(length_bin = bin_all,
                            year = unique(dat$year),
                            sex = c("F", "M"))

    left_join(dat_full, dat, by = c("year", "length_bin", "sex")) %>%
      group_by(year) %>%
      mutate(total = na.omit(total) %>% unique(),
             n = ifelse(is.na(n), 0, n),
             month = 1,
             sex2 = 3, # 0 for independent sex ratio, 3 constrains sex ratio
             partition = 0, # 0 = combined, 1 = discard, 2 = retain
             fleet = fleet_index[stringr::str_to_sentence(ff) %>% grep(names(fleet_index))]) %>%
      reshape2::dcast(year + month + fleet + sex2 + partition + total ~ sex + length_bin,
                      value.var = "n")

  }) %>%
    bind_rows() %>%
    arrange(fleet, year)

  if (csv) write.csv(length_format, file = "data/ss3/ss3-length.csv", row.names = FALSE)
  invisible(list(length_format, bin_all))
}
len <- ss3_length()

# Length bins
len[[2]]

# Number of length bins
length(len[[2]])

# Width of length bins
len[[2]] %>% diff()



# Natural mortality predictors
# max age of 73 and 70 for F/M
ss3_m <- function(max_obs_age, linf = NA, k = NA) {
  prior <- list(M_Hoenig = c(exp(1.48 - log(max_obs_age)), 0.06), # See Hamel (2015)
                M_Then = c(exp(1.717 - 1.01 * log(max_obs_age)), 0.08), # Table 3 of Then et al 2015 (use log-log transformation, see Hamel 2015)
                M_Then2 = c(4.899 * max_obs_age^-0.916, MSEtool::sdconv(1, 0.11)), # Non-linear least squares
                M_Then_growth = c(4.118 * k^0.73 / linf^0.33))
  prior
}

ss3_m(73, 97.4, 0.05) # Female M = 0.073
ss3_m(70, 83.7, 0.08) # Male   M = 0.077

#
ss3_maturity_slope <- function(l95 = 115.1, l50 = 97.6) {
  x <- l95 - l50
  log(1/0.95 - 1)/x
}
ss3_maturity_slope()
