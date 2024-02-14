



area_to_PFMA <- function(Area) ifelse(Area == 1, "North - 5BCDE", "South - 5A3CD")

.SS3_yieldcurve <- function(replist, scenario = "OM 1") {

  equil_yield <- replist$equil_yield

  data.frame(F = equil_yield$F_report,
             Yield = equil_yield$Tot_Catch,
             Sim = 1,
             SB = equil_yield$SSB,
             SB0 = equil_yield$Depletion,
             scen = scenario)  %>%
    filter(Yield > 0)
}


SS3_yieldcurve <- function(x, scenario = paste("Scenario", 1:length(x)),
                           xvar = c("F", "SB", "SB0"), french = FALSE,
                           figure = TRUE) {
  xvar <- match.arg(xvar)
  yc <- map2_dfr(x, scenario, .SS3_yieldcurve)

  if(!figure) return(yc)

  require(rosettafish)

  if(xvar == "F") {
    g <- ggplot(yc, aes(F, Yield, group = Sim)) +
      xlab(en2fr("Harvest rate", french))
  } else if(xvar == "SB") {
    g <- ggplot(yc, aes(SB, Yield, group = Sim)) +
      xlab(en2fr("Spawning output", french))
  } else {

    BRP <- yc %>% group_by(scen) %>%
      summarise(LRP = 0.4 * SB0[which.max(Yield)],
                USR = 0.8 * SB0[which.max(Yield)]) %>%
      reshape2::melt()

    g <- ggplot(yc, aes(SB0, Yield, group = Sim)) +
      xlab(en2fr("Spawning depletion", french)) +
      geom_vline(data = BRP, aes(xintercept = value, linetype = variable)) +
      scale_linetype_manual("Biological\nreference point", values = c("LRP" = 2, "USR" = 3))
  }

  g <- g +
    geom_line() +
    facet_wrap(~scen, ncol = 2) +
    #gfplot::theme_pbs() +
    ylab(en2fr("Yield", french)) +
    theme(panel.spacing = unit(0, "in"))
  g

}

# Two-area figures
.SS3_B <- function(replist, scenario = "OM 1", type = c("SSB", "SSBMSY", "SSB0", "B")) {
  type <- match.arg(type)

  out <- replist$timeseries %>%
    filter(Era == "TIME") %>%
    mutate(Area = area_to_PFMA(Area)) %>%
    select(Yr, Area, SpawnBio)

  tot <- out %>%
    group_by(Yr) %>%
    summarise(SpawnBio = sum(SpawnBio),
              .groups = "drop") %>%
    mutate(Area = "Coastwide") %>%
    select(Yr, Area, SpawnBio)

  if(type == "SSB") {

    rbind(out, tot) %>% mutate(scen = scenario, y = SpawnBio)

  } else if(type == "SSB0") {

    SB0 <- replist$timeseries %>%
      filter(Era == "VIRG") %>%
      select(Area, SpawnBio) %>%
      mutate(Area = area_to_PFMA(Area)) %>%
      rename(SB0 = SpawnBio)

    out <- left_join(out, SB0, by = "Area")
    tot <- tot %>% mutate(SB0 = sum(SB0$SB0))

    rbind(out, tot) %>%
      mutate(scen = scenario, y = SpawnBio/SB0) %>%
      filter(Area == "Coastwide")

  } else if (type == "SSBMSY") {
    tot %>%
      mutate(SSBMSY = replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"],
             scen = scenario,
             y = SpawnBio/SSBMSY) %>%
      filter(Area == "Coastwide")
  } else {
    replist$timeseries %>%
      filter(Era == "TIME") %>%
      #mutate(Area = area_to_PFMA(Area)) %>%
      select(Yr, `SmryBio_SX:1_GP:1`, `SmryBio_SX:2_GP:1`) %>%
      rename(Female = `SmryBio_SX:1_GP:1`, Male = `SmryBio_SX:2_GP:1`) %>%
      reshape2::melt(id.vars = "Yr") %>%
      rename(y = value, Area = variable) %>%
      mutate(scen = scenario)

  }
}

SS3_B <- function(x, scenario, type = c("SSB", "SSBMSY", "SSB0", "B"), posterior = FALSE, probs = c(0.025, 0.5, 0.975)) {
  type <- match.arg(type)

  if (posterior) {
    stopifnot(type != "SSB")
    out <- Map(function(y, sc) {
      lapply(1:length(y), function(z) .SS3_B(y[[z]], scenario = z, type = type)) %>%
        bind_rows() %>%
        rename(Sim = scen) %>%
        mutate(scen = sc) %>%
        group_by(Yr, Area, scen) %>%
        summarise(est = quantile(y, probs = probs[2]),
                  lwr = quantile(y, probs = probs[1]),
                  upr = quantile(y, probs = probs[3]),
                  .groups = "drop")
    }, y = x, sc = scenario) %>%
      bind_rows()

    g <- ggplot(out, aes(Yr)) +
      geom_line(aes(y = est)) +
      geom_line(linetype = 2, aes(y = lwr)) +
      geom_line(linetype = 2, aes(y = upr)) +
      expand_limits(y = 0) +
      facet_wrap(vars(scen), ncol = 2) +
      labs(x = "Year", y = switch(type,
                                  "SSB" = "Spawning Biomass",
                                  "SSBMSY" = expression(B/B[MSY]),
                                  "SSB0" = expression(B/B[0]),
                                  "B" = "Biomass (Age 1+)")) +
      gfplot::theme_pbs()
  } else {
    out <- Map(.SS3_B, replist = x, scenario = scenario, type = type) %>%
      bind_rows()
    g <- ggplot(out, aes(Yr, y, linetype = Area)) +
      geom_line() +
      expand_limits(y = 0) +
      facet_wrap(vars(scen), ncol = 2) +
      labs(x = "Year", y = switch(type,
                                  "SSB" = "Spawning Biomass",
                                  "SSBMSY" = expression(B/B[MSY]),
                                  "SSB0" = expression(B/B[0]),
                                  "B" = "Biomass (Age 1+)")) +
      gfplot::theme_pbs() +
      scale_linetype_manual(values = c(1, 2, 3))
  }
  g
}

SS3_index <- function(replist, scenario = "OM 1", hist = FALSE, figure = TRUE) {
  out <- replist$cpue %>%
    filter(Use == 1) %>%
    mutate(Area = area_to_PFMA(Area), scen = scenario,
           Obs = ifelse(Use == 1, Obs, NA_real_)) %>%
    select(Yr, Area, Fleet_name, Obs, Exp, SE, scen)

  if(hist) {
    sel <- .SS3_sel(replist, scenario, fleet = unique(replist$cpue$Fleet))

    Ihist <- lapply(unique(replist$cpue$Fleet), function(ff) {
      ar <- replist$fleet_area[ff]
      #y <- 1918:2021
      y <- replist$startyr:(replist$cpue %>% filter(Fleet == ff, Use == 1) %>% pull(Yr) %>% min() %>% `-`(1))
      N <- replist$natage %>%
        filter(Area == ar, Yr %in% y, `Beg/Mid` == "B", Era == "TIME") %>%
        select(Yr, as.character(0:70)) %>%
        reshape2::melt(id.var = "Yr") %>%
        mutate(variable = as.character(variable) %>% as.numeric())

      #Z <- replist$Z_by_area %>% filter(Yr %in% y, Area == ar) %>%
      #  select(Yr, as.character(0:70)) %>%
      #  reshape2::melt(id.var = "Yr") %>%
      #  mutate(variable = as.character(variable) %>% as.numeric()) %>%
      #  mutate(Zrate2 = (1 - exp(-value))/value) %>%
      #  select(!value)

      v <- sel %>% filter(Fleet == ff) %>%
        ungroup() %>%
        select(variable, value) %>%
        rename(vul = value)

      wt <- replist$ageselex %>%
        filter(Fleet == ff, Factor == "bodywt") %>%
        select(Yr, as.character(0:70)) %>%
        reshape2::melt(id.var = "Yr") %>%
        mutate(variable = as.character(variable) %>% as.numeric()) %>%
        rename(weight = value)
      if (replist$survey_units[ff] == 0) wt$weight <- 1

      vul_stock <- left_join(N, v, by = "variable") %>%
        left_join(wt, by = c("Yr", "variable")) %>%
        #left_join(Z, by = c("Yr", "variable")) %>%
        group_by(Yr) %>%
        summarise(Vuln_index = sum(value * weight * vul))

      index_model <- replist$cpue %>% filter(Fleet == ff)
      #index_model <- replist$cpue %>% filter(Fleet == ff) %>%
      #  left_join(vul_stock, by = "Yr") %>%
      #  mutate(Q2 = mean(log(Obs[Obs>0.001]/Vuln_index[Obs>0.001]), na.rm = TRUE) %>% exp(),
      #         Exp2 = Vuln_index * Q2,
      #         Exp3 = Calc_Q * Vuln_index) %>%
      #  mutate(ratio = Exp/Exp2,
      #         ratio_vuln = Vuln_bio/Vuln_index)
      #par(mfrow = c(2, 1))
      #plot(Exp ~ Yr, index_model, ylim = c(0, 40), typ = 'l')
      #points(Obs ~ Yr, index_model)
      #plot(Exp2 ~ Yr, index_model, ylim = c(0, 50), typ = 'l')
      #points(Obs ~ Yr, index_model)
      #plot(ratio_vuln~Yr, index_model)

      q <- index_model %>% pull(Calc_Q) %>% unique()

      Ipred <- q * vul_stock$Vuln_index

      data.frame(Yr = y,
                 Area = area_to_PFMA(ar),
                 Fleet_name = replist$cpue %>% filter(Fleet == ff) %>% pull(Fleet_name) %>% unique(),
                 Obs = NA,
                 Exp = Ipred,
                 SE = NA,
                 scen = scenario)
    }) %>% bind_rows()

    out <- rbind(out, Ihist)
  }

  if(figure) {
    ggplot(out, aes(Yr, Obs, ymin = exp(log(Obs) - 1.96 * SE), ymax = exp(log(Obs) + 1.96 * SE))) +
      geom_pointrange(shape = 1) +
      geom_line(aes(y = Exp)) +
      expand_limits(y = 0) +
      facet_grid(vars(Fleet_name), vars(scen), scales = "free_y") +
      gfplot::theme_pbs() +
      labs(x = "Year", y = "Index")
  } else {
    return(out)
  }
}


SS3_recruitment <- function(x, scenario, dev = FALSE, prop = FALSE, posterior = FALSE, probs = c(0.025, 0.5, 0.975)) {

  if(posterior) {

    out <- Map(function(y, sc) {
      lapply(1:length(y), function(z) .SS3_recruitment(y[[z]], scenario = z, dev = dev, prop = prop)) %>%
        bind_rows() %>%
        rename(Sim = scen) %>%
        mutate(scen = sc) %>%
        group_by(Yr, Area, scen) %>%
        summarise(est = quantile(Recruit_0, probs = probs[2]),
                  lwr = quantile(Recruit_0, probs = probs[1]),
                  upr = quantile(Recruit_0, probs = probs[3]),
                  .groups = "drop")
    }, y = x, sc = scenario) %>%
      bind_rows()

    if (prop) {

    } else if (dev) {

    } else {

      out <- filter(out, Area == "Coastwide")

      df_poly <- local({
        v_low <- out %>% rename(y = lwr) %>% select(Yr, scen, y)
        v_high <- out %>% rename(y = upr) %>% select(Yr, scen, y)
        rbind(v_low,
              v_high[order(v_high$Yr, decreasing = TRUE), ])
      })
      g <- out %>%
        ggplot(aes(Yr)) +
        geom_polygon(data = df_poly, fill = "grey80", aes(y = y)) +
        geom_line(aes(y = est)) +
        #geom_line(linetype = 3, aes(y = lwr)) +
        #geom_line(linetype = 3, aes(y = upr)) +
        expand_limits(y = 0) +
        facet_wrap(vars(scen), ncol = 2) +
        labs(x = "Year", y = "Recruitment") +
        gfplot::theme_pbs()
      return(g)
    }

  } else {
    out <- Map(.SS3_recruitment, replist = x, scenario = scenario, dev = dev, prop = prop) %>%
      bind_rows()
  }
  if (prop) {
    ggplot(out, aes(Yr, p, linetype = Area)) +
      #geom_col() +
      geom_line() +
      coord_cartesian(ylim = c(0, 1)) +
      facet_wrap(vars(scen), ncol = 2) +
      gfplot::theme_pbs() +
      labs(x = "Year", y = "Recruitment proportion")
  } else if (dev) {
    ggplot(out, aes(Yr, dev)) +
      #expand_limits(y = 0) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_line() +
      facet_wrap(vars(scen), ncol = 2) +
      gfplot::theme_pbs() +
      labs(x = "Year", y = "Recruitment deviations")
  } else {
    out %>%
      filter(Area == "Coastwide") %>%
      ggplot(aes(Yr, Recruit_0)) +
      geom_line() +
      expand_limits(y = 0) +
      facet_wrap(vars(scen), ncol = 2) +
      gfplot::theme_pbs() +
      labs(x = "Year", y = "Recruitment")

  }
}

.SS3_recruitment <- function(replist, scenario = "OM 1", dev = FALSE, prop = FALSE) {

  if(dev) {

    replist$recruit %>%
      mutate(dev = ifelse(is.na(dev), 0, dev),
             scen = scenario)

  } else {

    out <- replist$timeseries %>%
      filter(Era == "TIME") %>%
      mutate(Area = area_to_PFMA(Area)) %>%
      select(Yr, Area, Recruit_0)

    if (prop) {

      pr <- out %>%
        group_by(Yr) %>%
        mutate(p = Recruit_0/sum(Recruit_0)) %>%
        select(Yr, Area, Recruit_0, p) %>%
        mutate(scen = scenario)
      return(pr)

    } else {

      tot <- out %>%
        group_by(Yr) %>%
        summarise(Recruit_0 = sum(Recruit_0)) %>%
        mutate(Area = "Coastwide") %>%
        select(Yr, Area, Recruit_0)

      return(rbind(out, tot) %>% mutate(scen = scenario))
    }
  }

}


.SS3_sel <- function(replist, scenario = "OM 1", fleet = 1:8,
                     bin_width = 5,
                     scale_max_1 = FALSE,
                     type = c("Asel2", "Asel", "Lsel"),
                     fleet_name = NULL,
                     check_comp = TRUE) {
  type <- match.arg(type)

  if (check_comp) {
    fleet_lencomp <- unique(replist$lendbase$Fleet)
    fleet_agecomp <- unique(replist$agedbase$Fleet)
    fleet <- intersect(fleet, c(fleet_lencomp, fleet_agecomp))
  }

  if(is.null(fleet_name)) fleet_name <- replist$FleetNames

  if (type == "Lsel") {

    out <- replist$sizeselex %>%
      filter(Yr == replist$endyr, Factor == type, Fleet %in% fleet) %>%
      mutate(FleetName = fleet_name[Fleet] %>% factor(levels = fleet_name[fleet])) %>%
      select(Factor, FleetName, Fleet, Sex, as.character(replist$lbins + 0.5 * bin_width)) %>%
      reshape2::melt(id.var = c("Fleet", "FleetName", "Factor", "Sex")) %>%
      mutate(variable = as.character(variable) %>% as.numeric(), scen = scenario)

  } else {

    out <- replist$ageselex %>%
      filter(Yr == replist$endyr, Factor == type, Fleet %in% fleet) %>%
      mutate(FleetName = fleet_name[Fleet] %>% factor(levels = fleet_name[fleet])) %>%
      select(Factor, FleetName, Fleet, Sex, as.character(0:70)) %>%
      reshape2::melt(id.var = c("Fleet", "FleetName", "Factor", "Sex")) %>%
      mutate(variable = as.character(variable) %>% as.numeric(), scen = scenario)

  }
  if (scale_max_1) {

    out <- out %>%
      group_by(Fleet, FleetName) %>%
      mutate(value = value/max(value))

  }

  return(out)
}

.SS3_mat <- function(replist, scenario, type = c("Asel2", "Asel", "Lsel")) {
  type <- match.arg(type)

  if (type == "Lsel") {
    mat <- data.frame(
      variable = replist$biology %>% pull(Len_mean),
      value = replist$biology %>% pull(Mat)
    )
  } else {
    mat <- data.frame(
      variable = replist$endgrowth %>% filter(Sex == 1) %>% pull(int_Age),
      value = replist$endgrowth %>% filter(Sex == 1) %>% pull(Len_Mat)
    )
  }

  mutate(mat, scen = scenario)
}

SS3_sel <- function(x, sc, fleet_name = NULL, type = c("Asel2", "Asel", "Lsel"), bin_width, scale_max_1 = FALSE,
                    do_mat = TRUE) {
  type <- match.arg(type)
  out <- Map(.SS3_sel, replist = x, scenario = sc, MoreArgs = list(type = type, fleet_name = fleet_name, bin_width = bin_width, scale_max_1 = scale_max_1)) %>%
    bind_rows() %>%
    mutate(Sex = ifelse(Sex == 1, "Female", "Male"))

  g <- ggplot(out, aes(variable, value, colour = FleetName, linetype = Sex)) +
    geom_line() +
    coord_cartesian(ylim = c(0, 1)) +
    facet_grid(vars(FleetName), vars(scen)) +
    gfplot::theme_pbs() +
    labs(x = ifelse(type == "Lsel", "Length", "Age"), y = "Selectivity") +
    guides(colour = "none") +
    theme(panel.spacing = unit(0, "in"),
          legend.position = "bottom")

  if (do_mat) {

    mat <- Map(.SS3_mat, replist = x, scenario = sc, MoreArgs = list(type = type)) %>%
      bind_rows()
    g <- g +
      geom_line(data = mat, colour = "black", linetype = 2)

  }

  g
}


.SS3_vuln <- function(replist, scenario) {

  vuln <- replist$catch %>%
    select(Yr, vuln_num, Fleet_Name) %>%
    reshape2::dcast(list("Yr", "Fleet_Name"), value.var = "vuln_num") %>%
    mutate(scen = scenario)
  return(vuln)
}

SS3_vuln <- function(x, scen) {
  dat <- Map(.SS3_vuln, replist = x, scenario = scen) %>%
    bind_rows() %>%
    filter(Yr >= 1937)

  g <- dat %>%
    select(Yr, x[[1]]$FleetNames[x[[1]]$IsFishFleet], scen) %>%
    reshape::melt(id.vars = c("Yr", "scen")) %>%
    ggplot(aes(Yr, value, colour = variable, linetype = variable)) +
    geom_line() +
    facet_wrap(vars(scen)) +
    gfplot::theme_pbs() +
    labs(x = "Year", y = "Vulnerable abundance", colour = "Gear", linetype = "Gear")

  g
}

.SS3_F <- function(replist, scenario = "OM 1") {

  total_catch <- replist$timeseries %>%
    filter(Era == "TIME", Yr <= 2023) %>%
    select(Yr, starts_with("dead(B)")) %>%
    reshape2::melt(id.vars = c("Yr")) %>%
    summarise(deadB = sum(value), .by = Yr)

  # Vuln_bio is calculated at midpoint of year
  hr_fleet <- replist$catch %>%
    mutate(hr = ifelse(dead_bio, dead_bio/(dead_bio + vuln_bio), 0),
           scen = scenario) %>%
    select(Yr, hr, Fleet_Name) %>%
    reshape2::dcast(list("Yr", "Fleet_Name"), value.var = "hr")

  # Harvest rate
  # Bio_smry is calculated at beginning of year
  out <- replist$timeseries %>%
    filter(Era == "TIME", Yr <= 2023) %>%
    select(Yr, Bio_smry) %>%
    left_join(total_catch, by = "Yr") %>%
    left_join(hr_fleet, by = "Yr") %>%
    mutate(F_std = deadB/Bio_smry, #total harvest rates
           scen = scenario)

  #out <- replist$exploitation %>%
  #  select(Yr, F_std, replist$FleetNames[replist$IsFishFleet]) %>%
  #  mutate(F_std = ifelse(is.na(F_std), 0, F_std),
  #         FMSY = replist$derived_quants %>% filter(Label == "annF_MSY") %>% pull(Value),
  #         scen = scenario,
  #         F_FMSY = F_std/FMSY) %>%
  #  filter(Yr <= 2023)

  out
}


SS3_F <- function(replist, scenario = "OM 1", type = c("F", "fleet", "FMSY"),
                  figure = TRUE,
                  posterior = FALSE,
                  probs = c(0.025, 0.5, 0.975)) {
  type <- match.arg(type)

  if (posterior) {

    out <- Map(function(y, sc) {
      lapply(1:length(y), function(z) .SS3_F(y[[z]], scenario = z)) %>%
        bind_rows() %>%
        rename(Sim = scen) %>%
        mutate(scen = sc) %>%
        group_by(Yr, scen) %>%
        summarise(est = quantile(value, probs = probs[2]),
                  lwr = quantile(value, probs = probs[1]),
                  upr = quantile(value, probs = probs[3]),
                  .groups = "drop")
    }, y = replist, sc = scenario) %>%
      bind_rows()

    if (figure) {

      df_poly <- local({
        v_low <- out %>% rename(y = lwr) %>% select(Yr, scen, y)
        v_high <- out %>% rename(y = upr) %>% select(Yr, scen, y)
        rbind(v_low, v_high[order(v_high$Yr, decreasing = TRUE), ])
      })
      g <- ggplot(out, aes(Yr)) +
        geom_polygon(data = df_poly, fill = "grey80", aes(y = y)) +
        geom_line(aes(y = est)) +
        #geom_line(linetype = 3, aes(y = lwr)) +
        #geom_line(linetype = 3, aes(y = upr)) +
        expand_limits(y = 0) +
        facet_wrap(vars(scen), ncol = 2) +
        gfplot::theme_pbs() +
        labs(x = "Year", y = ifelse(type == "FMSY", expression(F/F[MSY]), "Fishing mortality"))
      return(g)
    }

  } else {
    out <- Map(.SS3_F, replist = replist, scenario = scenario) %>%
      bind_rows()

    if (figure) {

      if (type == "F") {

        g <- ggplot(out, aes(Yr, F_std)) +
          geom_line() +
          facet_wrap(vars(scen)) +
          gfplot::theme_pbs() +
          labs(x = "Year", y = "Harvest rate")
        #xx <- replist[[1]]$derived_quants %>% filter(grepl("F_", Label))

      } else if (type == "FMSY") {

        g <- ggplot(out, aes(Yr, F_FMSY)) +
          geom_line() +
          facet_wrap(vars(scen)) +
          gfplot::theme_pbs() +
          labs(x = "Year", y = expression(F/F[MSY]))

      } else {

        g <- out %>%
          select(Yr, replist[[1]]$FleetNames[replist[[1]]$IsFishFleet], scen) %>%
          reshape::melt(id.vars = c("Yr", "scen")) %>%
          ggplot(aes(Yr, value, colour = variable, linetype = variable)) +
          geom_line() +
          facet_wrap(vars(scen)) +
          gfplot::theme_pbs() +
          labs(x = "Year", y = "Apical F", colour = "Gear", linetype = "Gear")

      }

      return(g)
    }
  }
  out
}

SS3_Kobe <- function(x, scenario) {

  FMSY <- Map(.SS3_F, replist = x, scenario = scenario) %>%
    bind_rows() %>%
    select(Yr, F_FMSY, scen) %>%
    rename(y = F_FMSY)
  BMSY <- Map(.SS3_B, replist = x, scenario = scenario, type = "SSBMSY") %>%
    bind_rows() %>%
    select(Yr, y, scen) %>%
    rename(x = y)

  out <- left_join(FMSY, BMSY, by = c("Yr", "scen"))

  yr_label <- filter(out, Yr %in% seq(1980, 2020, 5))

  ggplot(out, aes(x, y)) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_vline(xintercept = 1, linetype = 2) +
    geom_path() +
    ggrepel::geom_text_repel(data = yr_label,
                             aes(label = Yr),
                             size = 2,
                             min.segment.length = 0) +
    geom_point(shape = 21, aes(fill = Yr)) +
    facet_wrap(vars(scen)) +
    gfplot::theme_pbs() +
    expand_limits(x = 0, y = 0) +
    coord_cartesian(expand = FALSE) +
    scale_fill_viridis_c() +
    labs(x = expression(B/B[MSY]), y = expression(F/F[MSY]), fill = "Year")
}

SS3_SR <- function(x, scenario) {
  srr_pars <- Map(.SS3_SR, x, scenario)

  ts <- lapply(srr_pars, getElement, "ts") %>%
    bind_rows()

  extrap <- lapply(srr_pars, getElement, "extrap") %>%
    bind_rows() %>%
    rename(SpawnBio = SSB, Recruit_0 = Recruitment)

  extrap2 <- extrap %>%
    left_join(ts %>% group_by(scen) %>%
                summarise(SpawnBio_min = min(SpawnBio))) %>%
    filter(SpawnBio > SpawnBio_min)

  yr_label <- filter(ts, Yr %in% seq(1980, 2020, 5))

  g <- ggplot(ts, aes(SpawnBio, Recruit_0)) +
    ggrepel::geom_text_repel(data = yr_label,
                             aes(label = Yr),
                             size = 2,
                             min.segment.length = 0) +
    geom_point(shape = 21, alpha = 0.5, aes(fill = Yr)) +
    geom_line(data = extrap, linetype = 3) +
    #geom_line(data = extrap2, linewidth = 1) +
    gfplot::theme_pbs() +
    facet_wrap(vars(scen)) +
    expand_limits(y = 0, x = 0) +
    scale_fill_viridis_c(direction = -1) +
    labs(fill = "Year", x = "Spawning biomass", y = "Recruitment")
  g
}


.SS3_SR <- function(x, scenario) {
  ts <- x$timeseries %>%
    filter(Era == "TIME") %>%
    group_by(Yr) %>%
    summarise(SpawnBio = sum(SpawnBio),
              Recruit_0 = sum(Recruit_0)) %>%
    mutate(scen = scenario)

  extrap <- x$SPAWN_RECR_CURVE %>%
    filter(`SSB/SSB_virgin` < 1) %>%
    mutate(scen = scenario)

  unfished <- x$timeseries %>%
    filter(Era == "VIRG") %>%
    summarise(SpawnBio = sum(SpawnBio),
              Recruit_0 = sum(Recruit_0)) %>%
    mutate(scen = scenario)

  list(ts = ts, extrap = extrap, unfished = unfished)
}



SS3_lencomp <- function(replist, scenario = "OM 1", fleet = 7, mean_length = TRUE, ghost = FALSE, hist = FALSE) {
  comp <- replist$lendbase %>%
    filter(Fleet %in% fleet) %>%
    mutate(FleetName = replist$FleetNames[Fleet]) %>%
    select(Yr, Sex, Fleet, FleetName, Bin, Obs, Exp, Nsamp_in) %>%
    mutate(scen = scenario)

  if (ghost) {
    ghost_comp <- replist$ghostlendbase %>%
      filter(Fleet %in% fleet) %>%
      mutate(FleetName = replist$FleetNames[Fleet]) %>%
      select(Yr, Sex, Fleet, FleetName, Bin, Obs, Exp, Nsamp_in) %>%
      mutate(scen = scenario)
    if (nrow(ghost_comp)) comp <- rbind(comp, ghost_comp)
  }

  if(mean_length) {

    mean_length_comp <- comp %>%
      group_by(Yr, Fleet, FleetName, Sex) %>%
      summarise(Obs = weighted.mean(Bin, Obs),
                Exp = weighted.mean(Bin, Exp))

    if (hist) {

      sel <- .SS3_sel(replist, scenario, fleet = fleet)

      # Must recalculate with length-based selectivity
      mean_length_hist <- lapply(unique(mean_length_comp$Fleet), function(ff) {

        ar <- replist$fleet_area[ff]
        ymin <- mean_length_comp %>%
          filter(Fleet == ff) %>%
          pull(Yr) %>%
          min()
        y <- seq(replist$startyr, ymin - 1)
        N <- replist$natage %>%
          filter(Area == ar, Yr %in% y, `Beg/Mid` == "B") %>%
          select(Yr, as.character(0:70)) %>%
          reshape2::melt(id.vars = "Yr") %>%
          mutate(variable = as.character(variable) %>% as.numeric())

        v <- sel %>% filter(Fleet == ff) %>% rename(vul = value)
        Nvul <- left_join(N, v, by = "variable") %>%
          mutate(value = value * vul) %>%
          reshape2::acast(list("Yr", "variable"), value.var = 'value')

        ALK <- replist$ALK[, , 2]
        len_bin <- rownames(ALK) %>% as.numeric()
        Nvul_len <- matrix(0, length(y), length(len_bin))
        for(a in 1:ncol(ALK)) {
          for(yy in 1:length(y)) {
            Nvul_len[yy, ] <- Nvul_len[yy, ] + Nvul[yy, a] * ALK[, a]
          }
        }

        mean_length <- apply(Nvul_len, 1, function(w) weighted.mean(len_bin, w))

        data.frame(Yr = y,
                   Fleet = ff,
                   FleetName = mean_length_comp$FleetName %>% unique(),
                   Obs = NA,
                   Exp = mean_length)

      }) %>% bind_rows()
    } else {
      mean_length_hist <- data.frame()
    }

    return(rbind(mean_length_hist, mean_length_comp) %>%
             mutate(scen = scenario, Sex = ifelse(Sex == 1, "Female", "Male")))
  } else {
    if (!nrow(comp)) return(NULL)
    return(comp %>% mutate(scen = scenario, Sex = ifelse(Sex == 1, "Female", "Male")))
  }
}



SS3_agecomp <- function(replist, scenario = "OM 1", fleet = 7, y) {
  N <- replist$natage %>%
    filter(`Beg/Mid` == "B", Era == "TIME", Yr %in% y) %>%
    select(Yr, Sex, as.character(0:70)) %>%
    reshape2::melt(id.vars = c("Yr", "Sex"))

  sel <- replist$ageselex %>%
    filter(Factor == "Asel2", Fleet == fleet, Yr %in% y) %>%
    select(Yr, Sex, as.character(0:70)) %>%
    reshape2::melt(id.vars = c("Yr", "Sex"), value.name = "sel")

  Nvul <- full_join(N, sel, by = c("Yr", "Sex", "variable")) %>%
    mutate(pred = value * sel, Age = as.character(variable) %>% as.numeric()) %>%
    mutate(p = pred/sum(pred), .by = Yr) %>%
    mutate(Sex = ifelse(Sex == 1, "Female", "Male")) %>%
    mutate(p = ifelse(Sex == "Male", -1 * p, p)) %>%
    mutate(FleetName = replist$FleetNames[fleet])

  g <- ggplot(Nvul, aes(Age, p, fill = Sex)) +
    geom_col(colour = "grey60", width = 1, alpha = 0.75) +
    facet_grid(vars(FleetName), vars(Yr)) +
    scale_y_continuous(labels = abs) +
    theme(legend.position = "bottom",
          panel.spacing = unit(0, "in")) +
    scale_fill_manual(values = c("grey80", "white")) +
    #xlim(xlim[[ff]]) +
    labs(x = "Age", y = "Proportion") +
    guides(colour = guide_legend(nrow = 2))

  g
}


.SS3_N <- function(replist, scenario = "OM 1", type = c("age", "length"), age = c(0, 15, 30, 45, 60), len = seq(10, 115, 5)) {
  if (type == "age") {

    dat <- replist$natage %>%
      filter(Era == "TIME", `Beg/Mid` == "B") %>%
      select(Yr, Sex, as.character(age)) %>%
      reshape2::melt(id.vars = c("Yr", "Sex")) %>%
      mutate(Sex = ifelse(Sex == 1, "Female", "Male"),
             scen = scenario)
  } else {

    dat <- replist$natlen %>%
      filter(Era == "TIME", `Beg/Mid` == "B") %>%
      select(Yr, Sex, as.character(len)) %>%
      reshape2::melt(id.vars = c("Yr", "Sex")) %>%
      mutate(Sex = ifelse(Sex == 1, "Female", "Male"),
             scen = scenario)
  }
  dat
}

SS3_N <- function(x, scenario, type = c("age", "length"), age = c(0, 15, 30, 45, 60), len = seq(10, 115, 5), sex_ratio = FALSE) {
  type <- match.arg(type)

  dat <- Map(.SS3_N, replist = x, scenario = scenario, MoreArgs = list(age = age, len = len, type = type)) %>%
    bind_rows()

  if (sex_ratio) {
    ratio <- dat %>%
      mutate(value = value/sum(value), .by = c(Yr, variable, scen)) %>%
      filter(Sex == "Female")

    g <- ggplot(ratio, aes(Yr, value, colour = variable)) +
      geom_line() +
      facet_wrap(vars(scen)) +
      gfplot::theme_pbs() +
      labs(x = "Year", y = "Proportion female", colour = ifelse(type == "age", "Age", "Length")) +
      theme(panel.spacing = unit(0, "in"))

    #ratio <- dat %>%
    #  summarise(value = sum(value), .by = c(Yr, Sex, scen)) %>%
    #  mutate(value = value/sum(value), .by = c(Yr, scen)) %>%
    #  filter(Sex == "Female")

    #g <- ggplot(ratio, aes(Yr, value)) +
    #  geom_line() +
    #  facet_wrap(vars(scen)) +
    #  gfplot::theme_pbs() +
    #  labs(x = "Year", y = "Proportion female", colour = "Age") +
    #  theme(panel.spacing = unit(0, "in"))

  } else {
    g <- ggplot(dat, aes(Yr, value, colour = variable)) +
      geom_line() +
      facet_grid(vars(scen), vars(Sex)) +
      gfplot::theme_pbs() +
      labs(x = "Year", y = "Abundance", colour = ifelse(type == "age", "Age", "Length")) +
      theme(panel.spacing = unit(0, "in"))

  }
  g
}


# Steepness
SS3_steep <- function(replist) {

  B0 <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_Virgin"]
  R0 <- replist$derived_quants$Value[replist$derived_quants$Label == "Recr_unfished"]

  S0 <- R0/B0
  #stopifnot(S0 < 1) # Can happen if M is high, i.e., there's an upper bound on M!

  #phi0 <- 1/S0

  z0 <- -log(S0)
  zfrac <- replist$parameters$Value[replist$parameters$Label == "SR_surv_zfrac"]
  beta <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Beta"]
  M <- replist$parameters$Value[grepl("NatM_uniform_Fem_GP_1", replist$parameters$Label)]

  h <- 0.2 * exp(z0 * zfrac * (1 - 0.2^beta))
  hmax <- 0.2 * exp(z0)

  return(c("h" = h, "hmax" = hmax, "S0" = S0, "M" = M))
}

# Population fecundity
.SS3_fecundity <- function(replist, scen) {
  NF <- replist$natage %>%
    filter(Sex == 1, `Beg/Mid` == "B", Era == "TIME") %>%
    select(Yr, as.character(0:70)) %>%
    reshape2::melt(id.vars = "Yr") %>%
    mutate(variable = as.character(variable) %>% as.integer())

  mat <- replist$endgrowth %>%
    filter(Sex == 1) %>%
    select(int_Age, Len_Mat)

  pups <- replist$recruit %>%
    select(Yr, SpawnBio)

  dat <- NF %>%
    left_join(mat, by = c("variable" = "int_Age")) %>%
    mutate(value_mature = Len_Mat * value) %>%
    summarise(N_mature = sum(value_mature),
              p_mature = sum(value_mature)/sum(value), .by = Yr) %>%
    left_join(pups, by = c("Yr")) %>%
    mutate(PPP = SpawnBio/N_mature) %>%
    mutate(scen = scen)

  return(dat)
}

SS3_fecundity <- function(x, scenario, type = c("PP", "mat")) { # PP = pup production, mat
  type <- match.arg(type)

  dat <- Map(.SS3_fecundity, replist = x, scen = scenario) %>%
    bind_rows()

  if (type == "PP") {
    g <- ggplot(dat, aes(Yr, PPP)) +
      labs(x = "Year", y = "Pups per mature female")
  } else {
    g <- ggplot(dat, aes(Yr, p_mature)) +
      labs(x = "Year", y = "Proportion females mature")
  }

  g <- g +
    facet_wrap(vars(scen)) +
    geom_line() +
    gfplot::theme_pbs()

  g
}


SS3_prof <- function(x, val, variable) {
  dat <- lapply(1:length(x), function(i) {
    mutate(x[[i]]$recruit, value = val[i]) %>%
      filter(.data$era %in% c("Fixed", "Main"))
  }) %>%
    bind_rows() %>%
    mutate(dep = SpawnBio/SpawnBio[1], .by = value)

  g <- ggplot(dat, aes(Yr, {{ variable }}, colour = factor(value))) +
    geom_line()
  g
}

SS3_likelihoods <- function(x, scenario, by_fleet = FALSE) {

  if (by_fleet) {
    res <- lapply(1:length(x), function(i) {
      out <- x[[i]][["likelihoods_by_fleet"]] %>%
        filter(!is.na(ALL)) %>%
        select(!ALL) %>%
        reshape2::melt(id.vars = c("Label"))
      colnames(out)[3] <- scenario[i]
      return(out)
    })

  } else {
    res <- lapply(1:length(x), function(i) {
      out <- x[[i]][["likelihoods_used"]] %>%
        mutate(Component = rownames(.)) %>%
        select(Component, values)
      colnames(out)[2] <- scenario[i]
      return(out)
    })
  }

  Reduce(dplyr::left_join, res)
}

SS3_prof_like <- function(x, par, xval = c("par", "steep", "dep"), component = c("Survey", "Length_comp"), by_fleet = TRUE) {
  xval <- match.arg(xval)

  if (by_fleet) {
    like <- SS3_likelihoods(x, par, by_fleet = by_fleet) %>%
      rename(Fleet = variable, Component = Label) %>%
      reshape2::melt(id.vars = c("Fleet", "Component")) %>%
      mutate(variable = variable %>% as.character() %>% as.numeric()) %>%
      mutate(value = value - min(value), .by = c(Component, Fleet))
  } else {
    like <- SS3_likelihoods(x, par, by_fleet = by_fleet) %>%
      reshape2::melt(id.vars = "Component") %>%
      mutate(variable = variable %>% as.character() %>% as.numeric()) %>%
      mutate(value = value - min(value), .by = Component)
  }

  if (xval == "steep") {
    hdf <- lapply(x, SS3_steep) %>%
      bind_rows() %>%
      mutate(variable = par)

    like <- left_join(like, hdf, by = "variable") %>%
      mutate(variable = h)
  } else if (xval == "dep") {

    ddf <- SS3_B(x, par, type = "SSB0") %>%
      getElement("data") %>%
      filter(Yr == max(Yr)) %>%
      select(scen, y) %>%
      rename(variable = scen, dep = y)

    like <- left_join(like, ddf, by = "variable") %>%
      mutate(variable = dep)
  }

  if (by_fleet) {
    g <- like %>%
      filter(Component %in% component) %>%
      ggplot(aes(variable, value, colour = Fleet)) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(Component)) + #geom_line(data = like %>% filter(Component == "TOTAL"), linewidth = 1, colour = "black") +
      labs(y = "Change in likelihood", colour = "Fleet")
  } else {
    g <- like %>%
      filter(Component %in% component) %>%
      ggplot(aes(variable, value, colour = Component)) +
      geom_line(linetype = 2) +
      geom_point() +
      geom_line(data = like %>% filter(Component == "TOTAL"), linewidth = 1, colour = "black") +
      labs(y = "Change in likelihood", colour = "Component")
  }
  g
}



SS3_retro <- function(ret) {
  g1 <- SS3_prof(ret, abs(ypeel), SpawnBio) +
    labs(x = "Year", y = "Spawning output", colour = "Years peeled") +
    expand_limits(y = 0)
  g2 <- SS3_prof(ret, abs(ypeel), pred_recr) +
    labs(x = "Year", y = "Recruitment", colour = "Years peeled") +
    expand_limits(y = 0)
  g3 <- SS3_prof(ret, abs(ypeel), dep) +
    labs(x = "Year", y = "Spawning depletion", colour = "Years peeled") +
    expand_limits(y = 0)
  g <- ggpubr::ggarrange(g1, g2, g3, ncol = 1, common.legend = TRUE, legend = "bottom")
  g
}




SS3_meanweight <- function(replist, scenario = "OM 1", fleet = 7, hist = FALSE) {
  dataseries <- replist$mnwgt %>%
    filter(Fleet %in% fleet) %>%
    rename(FleetName = Fleet_Name) %>%
    select(Yr, Fleet, FleetName, Obs, Exp) %>%
    mutate(scen = scenario)

  if (hist && (!nrow(dataseries) || min(dataseries$Yr) > replist$startyr)) {

    sel <- .SS3_sel(replist, scenario, fleet = fleet)

    mean_weight_hist <- lapply(fleet, function(ff) {

      ar <- replist$fleet_area[ff]

      if(!nrow(dataseries)) {
        ymin <- replist$endyr + 1
      } else {
        ymin <- dataseries %>%
          filter(Fleet == ff) %>%
          pull(Yr) %>%
          min()
      }
      y <- seq(replist$startyr, ymin - 1)
      wt <- replist$ageselex %>%
        filter(Factor == "bodywt", Yr == replist$endyr, Fleet == ff) %>%
        select(as.character(0:70)) %>%
        as.numeric()
      N <- replist$natage %>%
        filter(Area == ar, Yr %in% y, `Beg/Mid` == "M") %>%
        select(as.character(0:70)) %>%
        as.matrix()
      v <- sel %>% filter(Fleet == ff) %>% pull(value)

      mean_weight <- apply(t(N) * v, 2, function(w) weighted.mean(x = wt, w = w))

      data.frame(Yr = y,
                 Fleet = ff,
                 FleetName = sel$FleetName %>% unique(),
                 Obs = NA,
                 Exp = mean_weight)

    }) %>% bind_rows() %>%
      mutate(scen = scenario)

    return(rbind(mean_weight_hist, dataseries))

  } else {
    return(dataseries)
  }
}



.SS3_age_structure <- function(replist, scenario = "OM 1", FM = seq(0.01, 1, 0.005),
                               sel = c("ind", "pop"),
                               ff = 7, vars = c("LRP", "Current", "Unfished", "USR")) {
  sel <- match.arg(sel)
  vars <- match.arg(vars, several.ok = TRUE)

  np <- replist$nareas
  maxage <- 70
  R0 <- replist$timeseries %>% filter(Era == "VIRG") %>% pull(Recruit_0) %>% sum()
  SSB0 <- replist$timeseries %>% filter(Era == "VIRG") %>% pull(SpawnBio) %>% sum()
  phi0 <- SSB0/R0
  Mat_age <- replist$endgrowth$Age_Mat
  Wt_age <- replist$endgrowth$Wt_Beg
  Len_age <- replist$endgrowth$Len_Beg
  SD_len_age <- replist$endgrowth$SD_Beg
  M_age <- replist$endgrowth$M
  M <- 0.056
  h <- replist$parameters %>%
    filter(Label == "SR_BH_steep") %>%
    pull("Value")

  fleet_ind <- list(c(1, 3, 5), c(2, 4, 6))
  F_age <- sapply(1:np, function(p) {
    FF <- replist$ageselex %>% filter(Factor == "F", Fleet %in% fleet_ind[[p]], Yr == replist$endyr) %>%
      select(Yr, Fleet, as.character(0:maxage)) %>%
      reshape2::melt(id.vars = c("Yr", "Fleet")) %>%
      group_by(Yr, variable) %>%
      summarise(value = sum(value)) %>%
      reshape2::acast(list("Yr", "variable"))
    FF
  }, simplify = "array")

  N <- sapply(1:np, function(p) {
    replist$natage %>% filter(Area == p, Yr == replist$endyr, `Beg/Mid` == "B") %>%
      select(as.character(0:70)) %>% as.matrix()
  }, simplify = "array")

  Nnext <- sapply(1:np, function(p) {
    Nout <- numeric(maxage + 1)
    Nout[-1] <- N[1, 1:maxage, p] * exp(-M - F_age[1, 1:maxage, p])
    #Nout[maxage + 1] <- Nout[maxage + 1] + N[1, maxage + 1, p] * exp(-M - F_age[1, maxage + 1, p])
    Nout
  })

  F_across <- -log(rowSums(Nnext)[-1]/rowSums(N[1, -c(maxage+1), ]) * exp(M))
  V <- F_across/max(F_across)
  V <- c(V, V[length(V)])

  YC <- sapply(c(-100, log(FM)),
               MSEtool:::MSYCalcs,
               M_at_Age = M_age,
               Wt_at_Age = Wt_age,
               Mat_at_Age = Mat_age,
               Fec_at_Age = Wt_age * Mat_age,
               V_at_Age = V,
               maxage = maxage,
               relRfun = function() invisible(),
               SRRpars = data.frame(),
               R0x = R0,
               SRrelx = 1,
               hx = h,
               SSBpR = SSB0/R0,
               opt = 2,
               plusgroup = 1)

  if(sel == "pop") {
    Ivul <- rep(1, maxage + 1)
  } else {
    Ivul <- .SS3_sel(replist, fleet = ff) %>% pull(value)
  }
  out <- data.frame(Age = 0:maxage)

  if ("LRP" %in% vars) {

    # Age structure at LRP 40% BMSY
    SSBMSY <- YC["SB", which.max(YC["Yield", ])]
    FLRP <- YC["F", which.min(abs(YC["SB", ] - 0.4 * SSBMSY))]
    ZLRP <- M_age + V * FLRP
    NPR <- numeric(maxage + 1)
    NPR[1] <- 1
    for(a in 1:maxage) NPR[a+1] <- NPR[a] * exp(-ZLRP[a])
    NPR[maxage + 1] <- NPR[maxage + 1]/(1 - exp(-ZLRP[maxage + 1]))

    out$LRP <- NPR * Ivul
  }

  if ("Unfished" %in% vars) {

    # Age structure at unfished
    NPR0 <- numeric(maxage + 1)
    NPR0[1] <- 1
    for(a in 1:maxage) NPR0[a+1] <- NPR0[a] * exp(-M_age[a])
    NPR0[maxage + 1] <- NPR0[maxage + 1]/(1 - exp(-M_age[maxage + 1]))

    out$Unfished <- NPR0 * Ivul
  }

  if ("USR" %in% vars) {

    # Age structure at USR 80% BMSY
    FUSR <- YC["F", which.min(abs(YC["SB", ] - 0.8 * SSBMSY))]
    ZUSR <- M_age + V * FUSR
    NPRUSR <- numeric(maxage + 1)
    NPRUSR[1] <- 1
    for(a in 1:maxage) NPRUSR[a+1] <- NPRUSR[a] * exp(-ZUSR[a])
    NPRUSR[maxage + 1] <- NPRUSR[maxage + 1]/(1 - exp(-ZUSR[maxage + 1]))

    out$USR <- NPRUSR * Ivul
  }

  if ("Current" %in% vars) {
    Curr <- replist$natage %>%
      filter(Yr == replist$endyr, `Beg/Mid` == "B") %>%
      select(Area, starts_with(as.character(0:maxage))) %>%
      reshape2::melt(id.vars = "Area") %>%
      group_by(variable) %>%
      summarise(Current = sum(value)) %>%
      mutate(variable = as.character(variable) %>% as.numeric())

    out <- left_join(out, Curr, by = c("Age" = "variable")) %>%
      mutate(Current = Current * Ivul)
  }

  out <- reshape2::melt(out, id.vars = "Age")

  if(sel == "pop") {
    out <- out %>%
      group_by(variable) %>%
      mutate(y = value/value[1], scenario = scenario) # Numbers per recruit/survival
  } else {
    age_comp <- SS3_agecomp(replist, scenario = scenario, fleet = ff, mean_age = FALSE) %>%
      filter(Yr == max(Yr)) %>%
      select(Yr, Bin, Obs)
    out <- left_join(out, age_comp, by = c("Age" = "Bin")) %>%
      mutate(Obs = ifelse(is.na(Obs), 0, Obs)) %>%
      group_by(variable) %>%
      mutate(y = value/max(value),
             Obs = Obs/max(Obs),
             scenario = scenario) # Proportion
  }
  return(out)
}


SS3_agestructure <- function(x, scenario, ff, vars = c("LRP", "Current", "Unfished", "USR"), french = FALSE) {
  vars <- match.arg(vars, several.ok = TRUE)
  require(rosettafish)

  agestructure <- Map(.SS3_age_structure, replist = x, scenario = scenario,
                      MoreArgs = list(ff = ff, vars = vars)) %>%
    bind_rows()

  Obs <- filter(agestructure, variable == "Current")

  vars_col <- c(
    "LRP" = 2,
    "USR" = "orange",
    "Unfished" = 4,
    "Current" = 1
  )
  vars_lab <- c(
    "LRP" = "LRP (equilibrium)",
    "USR" = "USR (equilibrium)",
    "Unfished" = "Unfished (equilibrium)",
    "Current" = "Current (estimated)"
  )

  g <- ggplot(agestructure, aes(Age, y)) +
    geom_col(data = Obs, aes(y = Obs), fill = "grey60") +
    geom_line(aes(colour = variable)) +
    facet_wrap(vars(scenario), ncol = 2) +
    gfplot::theme_pbs() +
    xlab(en2fr("Age", french)) +
    ylab(en2fr("Relative frequency", french)) +
    scale_colour_manual("Age structure",
                        labels = vars_lab[names(vars_lab) %in% vars],
                        values = vars_col[names(vars_col) %in% vars])
  g
}



pars_fn <- function(replist, OM_name) {

  x <- replist$parameters %>%
    filter(Label %in% c("SR_LN(R0)", "SR_BH_steep", "NatM_uniform_Fem_GP_1", "NatM_uniform_Mal_GP_1")) %>%
    select(Label, Value) %>%
    #mutate(Value = round(Value, 3)) %>%
    rename(Parameter = Label)

  y <- replist$derived_quants %>%
    filter(Label %in% c("SSB_2023", "F_2022", "annF_MSY", "SSB_unfished", "SSB_MSY")) %>%
    select(Label, Value) %>%
    #mutate(Value = round(Value, 3)) %>%
    rename(Parameter = Label)

  Fmult <- replist$equil_yield %>%
    filter(Tot_Catch == max(Tot_Catch)) %>%
    filter(Iter == min(Iter)) %>%
    pull(Fmult)

  z <- data.frame(Parameter = "Fmult", Value = Fmult)

  out <- rbind(x, y, z)
  names(out)[2] <- OM_name

  return(out)
}
