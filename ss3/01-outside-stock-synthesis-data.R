library(dplyr)
library(ggplot2)

fleet_index <- c(
  "Bottom Trawl Landings" = 1,   # Catch series since 1935, targets the biggest fish - Use KEEPERS lengths (1977-1980, 1987, 2000)
  "Bottom Trawl Discards" = 2,   # Catch series since 1966, catches smaller fish - Use DISCARDS lengths (1990s to 2019, sparse)
  "Midwater Trawl" = 3,         # Use UNSORTED lengths (data quantity, 1980s-1990s) - not a strong difference in unsorted comps over time when
                               # discards went from 0 to 90 percent
  "Longline Landings" = 4,      # Use RETAINED lengths
  "Longline Discards" = 5,      # Map selectivity to BottomTrawlDiscards
  "IPHC" = 6,
  "HBLL" = 7,
  "SYN" = 8,
  "iRec" = 9,
  "Salmon Bycatch" = 10,
  "HS MSA" = 11,
  "Bottom Trawl CPUE" = 12
)
fleet_factor <- paste(fleet_index, "-", names(fleet_index))


ss3_catch <- function(csv = TRUE, midwater_discard_rate = 0.37) {

  catch <- readRDS("data/generated/catch.rds") %>%
    filter(area != "4B", year <= 2023) %>%
    group_by(year, gear) %>%
    summarise(landing = 1e-3 * sum(landed_kg),
              discard = 1e-3 * sum(discarded_kg)) %>%
    ungroup()

  bottom_trawl_gear <- c("Trawl + hook and line", "Trawl", "Bottom trawl", "Unknown/trawl")
  f1 <- catch %>%
    filter(gear %in% bottom_trawl_gear) %>%
    select(year, landing, discard) %>%
    summarise(value = sum(landing), landing = sum(landing), discard = sum(discard), .by = year) %>%
    mutate(fleet = 1)

  f2 <- catch %>%
    filter(gear %in% bottom_trawl_gear) %>%
    select(year, landing, discard) %>%
    summarise(value = sum(discard), landing = sum(landing), discard = sum(discard), .by = year) %>%
    mutate(fleet = 2)

  # Due to initial fleet set-up, discards + landings were combined for midwater trawl
  # Incorporate alternative discard mortality values into the total catch series
  # For all other fleets, incorporate the discard mortality in the SS3 control file

  # disc_mort <- 0.19 # low estimate Dean	Courtney review
  # disc_mort <- 0.37 # base (actually 0.27 in review table)
  # disc_mort <- 0.56 # high estimate Dean Courtney review
  # disc_100 <- 1
  disc_mort <- midwater_discard_rate

  f3 <- catch %>%
    filter(gear == "Midwater trawl") %>%
    select(year, landing, discard) %>%
    summarise(value = sum(landing + disc_mort * discard), landing = sum(landing), discard = sum(discard), .by = year) %>%
    mutate(fleet = 3)

  f4 <- catch %>%
    filter(gear == "Hook and line") %>%
    select(year, landing, discard) %>%
    summarise(value = sum(landing), landing = sum(landing), discard = sum(discard), .by = year) %>%
    mutate(fleet = 4)

  f5 <- catch %>%
    filter(gear == "Hook and line") %>%
    select(year, landing, discard) %>%
    summarise(value = sum(discard), landing = sum(landing), discard = sum(discard), .by = year) %>%
    mutate(fleet = 5)

  # IPHC
  catch_ll <- readRDS("data/generated/catch_longline.rds")
  f6 <- catch_ll %>%
    filter(survey_abbrev == "IPHC FISS") %>%
    mutate(value = 1e-3 * catch_count, landing = 0, discard = value, fleet = 6) %>%
    select(year, value, landing, discard, fleet)

  # HBLL
  f7 <- catch_ll %>%
    filter(survey_abbrev != "IPHC FISS") %>%
    mutate(value = 1e-3 * catch_count, landing = 0, discard = value, fleet = 7) %>%
    select(year, value, landing, discard, fleet)

  # Trawl
  catch_trawl_survey <- readRDS("data/generated/catch_trawlsurvey.rds")
  f8 <- catch_trawl_survey %>%
    mutate(value = catch_ton, landing = 0, discard = value, fleet = 8) %>%
    select(year, value, landing, discard, fleet)

  f9 <- readRDS("data/generated/catch_recreational.rds") %>%
    filter(outside) %>%
    ungroup() %>%
    reshape2::dcast(list("year", "disposition"), value.var = "catch_count") %>%
    mutate(landing = 1e-3 * Kept, discard = 1e-3 * Released, value = landing + discard, fleet = 9) %>%
    select(year, value, landing, discard, fleet)
  #range(f9$discard/f9$value) # greater than 85 percent discard rate

  f10 <- readRDS("data/generated/catch_salmonbycatch.rds") %>%
    filter(outside) %>%
    ungroup() %>%
    reshape2::dcast(list("year", "kept_rel"), value.var = "catch_count") %>%
    mutate(landing = 1e-3 * KEPT, discard = 1e-3 * RELEASED, value = landing + discard, fleet = 10) %>%
    select(year, value, landing, discard, fleet)
  #f10$discard/f10$value # greater than 85 percent discard rate

  out <- rbind(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) %>%
    mutate(value = round(value, 3)) %>%
    mutate(se = 0.01,
           season = 1) %>%
    filter(value > 0) %>%
    select(year, season, fleet, value, se) %>%
    arrange(fleet, year)

  if (midwater_discard_rate == 0.37) {
    # Same y-axis for all fleets
    g <- out %>%
      mutate(fleet2 = paste(fleet, "-", names(fleet_index)[fleet]) %>% factor(fleet_factor)) %>%
      ggplot(aes(year, value, fill = fleet2)) +
      facet_wrap(vars(fleet2)) +
      geom_col(width = 1) +
      gfplot::theme_pbs() +
      guides(fill = "none") +
      coord_cartesian(expand = FALSE) +
      labs(x = "Year", y = "Catch (t)")
    ggsave("figs/ss3/catch_fleet.png", g, height = 4, width = 8)

    # Unique y-axis
    g <- out %>%
      mutate(fleet2 = paste(fleet, "-", names(fleet_index)[fleet]) %>% factor(fleet_factor)) %>%
      ggplot(aes(year, value, fill = fleet2)) +
      facet_wrap(vars(fleet2), scales = "free_y") +
      geom_col(width = 1) +
      gfplot::theme_pbs() +
      guides(fill = "none") +
      coord_cartesian(expand = FALSE) +
      labs(x = "Year", y = "Catch (t)")
    ggsave("figs/ss3/catch_fleet2.png", g, height = 4, width = 8)

    # Panel by landing/discard
    g <- rbind(f1, f3, f4, f9, f10) %>%
      mutate(fleet_name = names(fleet_index)[fleet]) %>%
      mutate(fleet_name = sub(" Landings", "", fleet_name)) %>%
      mutate(fleet_name = factor(fleet_name, levels = c("Bottom Trawl", "Midwater Trawl", "Longline", "iRec", "Salmon Bycatch"))) %>%
      select(year, landing, discard, fleet_name) %>%
      reshape2::melt(id.vars = c("year", "fleet_name")) %>%
      mutate(variable = ifelse(variable == "landing", "Landings", "Discards")) %>%
      ggplot(aes(year, value, fill = variable)) +
      geom_col(width = 1, linewidth = 0.05, colour = "grey40") +
      facet_wrap(vars(fleet_name), scales = "free_y") +
      gfplot::theme_pbs() +
      #coord_trans(y = "sqrt", expand = FALSE) +
      coord_cartesian(expand = FALSE, xlim = c(1935, 2024.5)) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = c("grey80", "black")) +
      labs(x = "Year", y = "Catch", fill = NULL)
    ggsave("figs/ss3/catch_fleet_discard_landings.png", g, height = 4, width = 8)
  }

  if (csv) {
    write.csv(out, file = paste0("data/ss3/ss3-catch-", midwater_discard_rate, ".csv"), row.names = FALSE)
  }
  invisible(out)
}
ss3_catch(midwater_discard_rate = 0.19)
ss3_catch(midwater_discard_rate = 0.37)
ss3_catch(midwater_discard_rate = 0.56)
ss3_catch(midwater_discard_rate = 1)

ss3_index <- function(csv = TRUE) {
  # IPHC - no hook competition
  iphc <- readRDS("data/generated/geostat-ind-iphc_gfdata.rds") %>%
    mutate(fleet = fleet_index["IPHC"])

  # HBLL - NB2 likelihood - no hook competition
  hbll <- readRDS("data/generated/geostat-ind-hbll-out.rds") %>%
    filter(year != 2013) %>% # No survey in 2013
    mutate(fleet = fleet_index["HBLL"]) %>%
    select(-survey_abbrev)

  # Synoptic Trawl
  syn <- readRDS("data/generated/geostat-ind-synoptic-lg.rds") %>%
    select(-type) |>
    mutate(fleet = fleet_index["SYN"])

  hs_msa <- readRDS("data/generated/hs-msa-index.rds") %>%
    mutate(fleet = fleet_index["HS MSA"])

  cpue <- readRDS("data/generated/geostat-spt-ind-cpue.rds") %>%
    mutate(fleet = fleet_index["Bottom Trawl CPUE"])

  ind <- rbind(hbll, iphc, syn, hs_msa, cpue)

  g <- ind %>%
    mutate(fleet2 = names(fleet_index)[fleet]) %>%
    ggplot(aes(year, est)) +
    facet_wrap(vars(fleet2), scales = "free_y", ncol = 2) +
    geom_point() +
    geom_line(linewidth = 0.25, linetype = 3) +
    geom_linerange(aes(ymin = lwr, ymax = upr)) +
    gfplot::theme_pbs() +
    expand_limits(y = 0) +
    coord_cartesian(expand = FALSE, xlim = c(1980, 2025)) +
    labs(x = "Year", y = "Index")
  ggsave("figs/ss3/index.png", g, height = 5, width = 6)

  out <- ind %>%
    mutate(month = 1) %>%
    select(year, month, fleet, est, se) %>%
    arrange(fleet, year) %>%
    round(3)

  if (csv) write.csv(out, file = "data/ss3/ss3-index.csv", row.names = FALSE)

  invisible(ind)
}
ss3_index()

ss3_length <- function(csv = TRUE, bin_size = 5, bin_range = c(35, 115)) {

  dtrawl <- readRDS("data/raw/survey-samples.rds")
  diphc <- read.csv("data/raw/IPHC_dogfish_lengths2021.csv") %>%
    filter(reg_area == "2B", sex %in% c("F", "M")
    ) %>%
    mutate(length_pcl = length, # Convert from pre-caudal length to length extended
           length = length_pcl * 1.20)
  dc <- readRDS("data/raw/commercial-samples.rds")

  # Will naively sum all trawl lengths together for trawl
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

  # Separate by fleet and sampling_desc
  lengths_comm <- dc |>
    filter(!grepl("4B", major_stat_area_name)) |>
    filter(gear_desc %in% c("BOTTOM TRAWL", "LONGLINE", "MIDWATER TRAWL")) %>%
    rename(survey_abbrev = gear_desc) %>%
    select(year, length, sex, survey_abbrev, specimen_id, age, usability_code, sampling_desc)

  f1 <- lengths_comm %>%
    filter(survey_abbrev == "BOTTOM TRAWL", sampling_desc == "KEEPERS") %>%
    mutate(survey_abbrev = "Bottom Trawl Landings") %>%
    select(-sampling_desc)

  f2 <- lengths_comm %>%
    filter(survey_abbrev == "BOTTOM TRAWL", sampling_desc == "DISCARDS") %>%
    mutate(survey_abbrev = "Bottom Trawl Discards") %>%
    select(-sampling_desc)

  f3 <- lengths_comm %>%
    filter(survey_abbrev == "MIDWATER TRAWL", sampling_desc == "UNSORTED") %>%
    mutate(survey_abbrev = "Midwater Trawl") %>%
    select(-sampling_desc)

  f4 <- lengths_comm %>%
    filter(survey_abbrev == "LONGLINE", sampling_desc == "KEEPERS") %>%
    mutate(survey_abbrev = "Hook & Line Landings") %>%
    select(-sampling_desc)


  length_all <- rbind(lengths_survey, lengths_iphc, f1, f2, f3, f4) %>%
    filter(!is.na(length), sex %in% 1:2, usability_code %in% c(0, 1, 2, 6)) %>%
    mutate(length = pmax(length, min(bin_range)) %>% pmin(max(bin_range)))

  bin_all <- seq(min(bin_range), max(bin_range), bin_size)

  comp <- length_all %>%
    mutate(bin = bin_all[findInterval(length, bin_all)]) %>%
    summarise(n = n(), .by = c(year, bin, sex, survey_abbrev)) %>%
    mutate(sex = ifelse(sex == 1, "M", "F"))

  length_format <- lapply(unique(comp$survey_abbrev), function(ff) {

    dat <- comp %>%
      filter(survey_abbrev == ff)

    dat_full <- expand.grid(bin = bin_all,
                            year = unique(dat$year),
                            sex = c("F", "M"))

    left_join(dat_full, dat, by = c("year", "bin", "sex")) %>%
      group_by(year) %>%
      mutate(n = ifelse(is.na(n), 0, n),
             N = sum(n),
             month = 1,
             sex2 = 3, # 3 constrains sex ratio
             partition = 0, # 0 = combined, 1 = discard, 2 = retain
             fleet = fleet_index[ff]) %>%
      reshape2::dcast(year + month + fleet + sex2 + partition + N ~ sex + bin,
                      value.var = "n")

  }) %>%
    bind_rows() %>%
    arrange(fleet, year)

  if (csv) write.csv(length_format, file = "data/ss3/ss3-length.csv", row.names = FALSE)
  invisible(list(length_format, bin_all, comp))
}
len <- ss3_length()

# Length bins
len[[2]]

# Number of length bins
length(len[[2]])

# Width of length bins
len[[2]] %>% diff()

# Size selectivity priors
length_format <- readr::read_csv("data/ss3/ss3-length.csv") %>%
  select(-month, -sex2, -partition, -N) %>%
  reshape2::melt(id.vars = c("year", "fleet")) %>%
  mutate(sex = as.character(variable) %>% strsplit("_") %>% sapply(getElement, 1),
         len = as.character(variable) %>% strsplit("_") %>% sapply(getElement, 2) %>% as.numeric()) %>%
  summarise(value = sum(value), .by = c(fleet, sex, len)) %>%
  mutate(p = value/sum(value), .by = c(fleet, sex))

L5_f <- function(len, p) len[sum(cumsum(p) <= 0.05)]
Ldome_f <- function(len, p) len[which.max(cumsum(p) >= 0.95)]

sel_prior <- length_format %>%
  summarise(L5 = L5_f(len, p),
            LFS = len[which.max(p)], .by = c(sex, fleet),
            Ldome = Ldome_f(len, p)) %>%
  arrange(fleet, sex) %>%
  mutate(asc_stdev = 0.5 * (LFS - L5),
         asc_mu = log(asc_stdev^2),
         asc_sd = 0.3,
         LFS_cv = 0.3,
         LFS_sd = LFS_cv * LFS,
         dsc_stdev = 0.5 * (Ldome - LFS),
         dsc_mu = log(dsc_stdev^2),
         dsc_sd = 0.3)

# Natural mortality predictors
ss3_m <- function(max_obs_age, linf = NA, k = NA) {
  prior <- list(M_Hoenig = c(exp(1.48 - log(max_obs_age)), 0.06), # See Hamel (2015)
                M_Then_loglog = c(exp(1.717 - 1.01 * log(max_obs_age)), 0.08), # Table 3 of Then et al 2015 (use log-log transformation, see Hamel 2015)
                M_Then_nls = c(4.899 * max_obs_age^-0.916, MSEtool::sdconv(1, 0.11)), # Non-linear least squares
                M_Then_growth = c(4.118 * k^0.73 / linf^0.33),
                Hamel = c(5.4/max_obs_age, 0.31))
  prior
}

# Growth from M4 - Max age from outside waters
ss3_m(54, 91.2, 0.07) # Female M = 0.10
ss3_m(53, 85.8, 0.09) # Male   M = 0.10

# Growth from M3 - Max age from inside waters
ss3_m(73, 93.2, 0.05) # Female M = 0.074
ss3_m(70, 84.4, 0.09) # Male   M = 0.077

# Max age of approximately 95 years from Macfarlane and King (200)
ss3_m(95, 0, 0) #  M = 0.056

#
ss3_maturity_slope <- function(l95 = 115.1, l50 = 97.6) {
  x <- l95 - l50
  log(1/0.95 - 1)/x
}
ss3_maturity_slope()



# We use growth parameters estimated from DFO + NWFSC samples
vb <- function(Linf, K, t0, a) Linf * (1 - exp(-K * (a - t0)))

# Decreasing variability in size at age
# Use A2 = 40 in SS3. SD is constant above A2
# Ketchen 1972 Figure 3. Embryos after 2 years gestation are around 20-30 cm
vb(97.7, 0.06, -5.73, 0) # Female age 0 = 28.42
vb(84.4, 0.09, -4.35, 0) # Male age 0 = 27.34

vb(97.7, 0.06, -5.73, 40) # Female age 40 = 91.42
vb(84.4, 0.09, -4.35, 40) # Male age 40 = 82.84


# Compare fecundity estimates from the literature
fec <- data.frame(len = seq(50, 120, 1)) %>%
  mutate(`Ketchen 1972` = -9.96 + 0.176 * len,
         `Taylor and Gallucci 2009 (1940s)` = -15.5 + 0.214 * len,
         `Taylor and Gallucci 2009 (2000s)` = -14.7 + 0.214 * len) %>%
  reshape2::melt(id.vars = "len") %>%
  mutate(value = pmax(value, 0)) %>%
  ggplot(aes(len, value, linetype = variable)) +
  geom_line() +
  #geom_point() +
  theme(legend.position = "bottom") +
  guides(linetype = guide_legend(ncol = 2)) +
  labs(x = "Length (cm)", y = "Fecundity", linetype = NULL)
ggsave("figs/fec-lit.png", fec, width = 5, height = 4)

#### Compare maturity at age from the literature ----
# McFarlane and Beamish 1987, Figure 6
# Taylor and Gallucci 2009, Figure 5, Table 1
# qnorm(a + b * 35) = 0.5; qnorm(a + b * 25) = 0.05
b <- qnorm(0.05)/-10
a <- -35 * b

mat <- data.frame(age = seq(10, 70)) %>%
  mutate(`McFarlane and Beamish 1987` = ifelse(age >= 24, pnorm(a + b * age), 0),
         `Taylor and Gallucci 2009 (1940s)` = ifelse(age >= 18, 1/(1 + exp(-log(19) * (age - 43.3)/16.2)), 0),
         `Taylor and Gallucci 2009 (2000s)` = ifelse(age >= 18, 1/(1 + exp(-log(19) * (age - 31.5)/24.3)), 0)) %>%
  reshape2::melt(id.vars = "age") %>%
  mutate(value = pmax(value, 0))
g <- ggplot(mat, aes(age, value, colour = variable, shape = variable)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 2)) +
  labs(x = "Age", y = "Maturity", colour = NULL, shape = NULL)
ggsave("figs/mat-lit.png", g, width = 5, height = 4)

# Add synoptic maturity at age (see 99-outside-ss3-extra-figures.R)
mat_syn <- readr::read_csv("data/ss3/synoptic-maturity-age.csv") %>%
  rename(age = int_Age, variable = model, value = Len_Mat) %>%
  select(age, variable, value)
g <- rbind(
   mat_syn,
   mat
 ) %>%
  ggplot(aes(age, value, colour = variable, shape = variable)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 2)) +
  labs(x = "Age", y = "Maturity", colour = NULL, shape = NULL)
ggsave("figs/mat-lit-with-synoptic.png", g, width = 6, height = 4.5)


mat_age <- data.frame(age = 0:70) %>%
  mutate(mat_high = ifelse(age >= 24, pnorm(a + b * age), 0),
         mat_adjust_high = round(mat_high * 0.5, 3),
         mat_low = ifelse(age >= 18, 1/(1 + exp(-log(19) * (age - 31.5)/24.3)), 0),
         mat_adjust_low = round(mat_low * 0.5, 3))
write.csv(mat_age, file = "data/ss3/mat_age.csv")

## priors on zfrac:
# estBetaParams <- function(mu, var) {
#   alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
#   beta <- alpha * (1 / mu - 1)
#   return(params = list(alpha = alpha, beta = beta))
# }
# p <- estBetaParams(0.5, 0.287717^2)
# x <- seq(0, 1, length.out = 200)
# plot(x, dbeta(x, p$alpha, p$beta), main = "mean: 0.4, sd = 0.2")