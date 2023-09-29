
library(dplyr)

geom_mean <- function(x) {
  ln_x <- log(x)
  ln_x <- ln_x[!is.infinite(ln_x)]
  exp(mean(ln_x))
}

expand_comp <- function(d, dsets, len_bin = seq(20, 120, 10)) {

  # Number of length samples per set
  dsamp <- d %>%
    summarize(nlength = n(), .by = c(fishing_event_id))

  # Merge with set data ----
  dprop <- dsets %>%
    select(fishing_event_id, year, survey_abbrev, catch_count, cpue_set, area_swept, longitude, latitude, depth_m) %>%
    left_join(dsamp, by = "fishing_event_id")

  # Some sets don't have total catch recorded (weight only) (set p = 1)
  #dprop %>% filter(is.infinite(p))
  #dprop2 <- mutate(dprop, p = ifelse(is.infinite(p), 1, p))

  # Calculate numbers at length bin, calculate proportions, merge with set data, and expand numbers ----
  dbin <- d %>%
    mutate(length = ifelse(length < min(len_bin), min(len_bin), length),
           length = ifelse(length > max(len_bin), max(len_bin), length),
           bin = len_bin[findInterval(length, len_bin)]) %>%
    summarise(n = n(), .by = c(fishing_event_id, sex, bin)) %>% # Numbers
    mutate(p = n/sum(n), .by = c(fishing_event_id)) %>%         # Proportions
    right_join(dprop %>% select(fishing_event_id, catch_count, cpue_set, nlength),
               by = "fishing_event_id") %>%                     # Merge with set data and CPUE by weight
    filter(!(cpue_set > 0 & !nlength)) %>%                      # Remove sets that caught fish but did not measure any length
    mutate(nexpand = p * cpue_set)                              # Expand length numbers by CPUE of the set

  #filter(dbin, cpue_set == 0) # Zero sets
  #filter(dbin, is.na(nexpand)) # Zero sets

  # Convert to table
  dbin_table <- reshape2::dcast(dbin,
                                fishing_event_id + sex ~ bin,
                                value.var = "nexpand", fill = 0) %>%
    select(!"NA")

  # Create rows with entries for males and females when there was zero catch in the set
  dbin_with_empty_sets <- lapply(unique(dbin_table$fishing_event_id), function(i) {
    x <- filter(dbin_table, fishing_event_id == i)
    if (all(is.na(x$sex))) {
      x1 <- x2 <- x
      x1$sex <- "M"
      x2$sex <- "F"
      rbind(x1, x2)
    } else x
  }) %>%
    bind_rows()

  # Full table of bins by sex for each set ----
  dbin2 <- dbin_with_empty_sets %>%
    reshape2::melt(id.vars = c("fishing_event_id", "sex"),
                   variable.name = "bin", value.name = "nexpand") %>%
    left_join(dsets %>% select(fishing_event_id, year, survey_abbrev, area_swept, longitude, latitude, grouping_code, area_km2),
              by = "fishing_event_id") %>%
    mutate(cpue = nexpand/area_swept * 1e3 * 1e3) # CPUE is in numbers/sq km.

  return(dbin2)
}







#library(VAST)
#library(TMB)
#stopifnot(.Platform$OS.type == "windows")
#vast_executable <- system.file("executables", package = "VAST") %>% file.path("VAST_v14_0_1_TMBad.dll")
#if (!file.exists(vast_executable)) TMB::compile(vast_executable, framework = "TMBad")
#dyn.load(vast_executable)

fit_VAST_model <- function(DataFrame,
                           input_grid,
                           category_names,
                           Aniso = TRUE,
                           n_x = 300,
                           X1_formula = ~ 0,
                           X2_formula = ~ 0,
                           covariate_data = NULL,
                           Q1_formula = ~ 0,
                           Q2_formula = ~ 0,
                           catchability_data = NULL,
                           Q1config_k = NULL,
                           Q2config_k = NULL,
                           ObsModel_ez = c(4, 0),    # Delta lognormal
                           Omega = c(0, 0),          # Number of spatial fields for presence/absence
                           Epsilon = c(0, 0),        # Number of spatiotemporal fields for presence/absence
                           Beta = c("IID", 0),       # Number of intercepts? "IID" means one per category I believe
                           RhoConfig = c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 3, "Epsilon2" = 3),
                           Version = "VAST_v14_0_1", # FishStatsUtils::get_latest_version(package = "VAST")
                           CheckForErrors = TRUE,
                           run_model = TRUE,
                           SD = FALSE) {

  require(VAST)

  settings <- FishStatsUtils::make_settings(
    n_x = n_x,
    Region = "user",
    purpose = "ordination",
    fine_scale = TRUE,
    zone = 32609,
    n_categories = length(category_names),
    max_cells = n_x
  )

  extrapolation_list <- FishStatsUtils::make_extrapolation_info(
    Region = "user",
    input_grid = input_grid,
    Save_results = FALSE
  )

  spatial_list <- FishStatsUtils::make_spatial_info(
    n_x = n_x,
    Lon_i = DataFrame$longitude,
    Lat_i = DataFrame$latitude,
    Extrapolation_List = extrapolation_list,
    Method = "Mesh",
    Save_Results = FALSE,
    fine_scale = TRUE,
    cutoff = 12,
    nstart = 1
  )

  settings$FieldConfig["Omega", ] <- Omega
  settings$FieldConfig["Epsilon", ] <- Epsilon
  settings$FieldConfig["Beta", ] <- Beta

  #### Year effect:
  # Default RhoConfig = c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 3, "Epsilon2" = 3)
  # 0 = each year as fixed effect (FE)
  # 1 = each year is IID as random effect (RE)
  # 2 = Random walk RE
  # 3 = single intercept FE
  # 4 = AR1 FE
  # 5 = AR1 FE by category
  settings$RhoConfig <- RhoConfig
  settings$Options["Calculate_Cov_SE"] <- FALSE # Saves a lot of ADREPORTing

  data_list <- VAST::make_data(
    b_i = as_units(DataFrame$cpue, "kg"),                     # Response
    a_i = as_units(rep(1, nrow(DataFrame)), "km^2"),          # Offset
    t_i = DataFrame$year,                                     # Time
    c_iz = DataFrame$cat_int,                                 # Category (sex x bin)
    e_i = DataFrame$err_int,                                  # SD per sex x bin
    FieldConfig = settings$FieldConfig,
    OverdispersionConfig = settings$OverdispersionConfig,
    RhoConfig = settings$RhoConfig,
    VamConfig = settings$VamConfig,
    ObsModel_ez = ObsModel_ez,
    covariate_data = covariate_data,
    X1_formula = X1_formula,
    X2_formula = X2_formula,
    catchability_data = catchability_data,
    Q1_formula = Q1_formula,
    Q2_formula = Q2_formula,
    Q1config_k = Q1config_k,
    Q2config_k = Q2config_k,
    spatial_list = spatial_list,
    Options = settings$Options,
    Aniso = Aniso,
    CheckForErrors = CheckForErrors,
    Version = Version
  )

  tmb_list <- VAST::make_model(
    TmbData = data_list,
    Version = Version,
    Method = "Mesh",
    RhoConfig = settings$RhoConfig,
    loc_x = spatial_list$loc_x
  )
  tmb_list$Obj$env$beSilent()

  if (run_model) {

    message("Fitting model...")
    parameter_estimates1 <- TMBhelper::fit_tmb(
      obj = tmb_list$Obj,
      #lower = tmb_list$Lower,
      #upper = tmb_list$Upper,
      control = list(eval.max = 10000, iter.max = 10000),
      loopnum = 0,
      quiet = TRUE,
      getsd = FALSE
    )
    message("Model finished.")

    if (SD) {
      message("Calculating covariance matrix...")
      parameter_estimates1[["SD"]] <- TMB::sdreport(tmb_list$Obj)
      message("TMB::sdreport finished.")
    }
    ParHat = tmb_list$Obj$env$parList(parameter_estimates1$par)
  } else {
    parameter_estimates1 <- list()
    ParHat <- tmb_list$Obj$env$parList()
  }

  Report <- tmb_list$Obj$report(tmb_list$Obj$env$last.par.best)
  Report <- FishStatsUtils::amend_output(
    Report = Report,
    TmbData = data_list,
    Map = tmb_list$Map,
    Sdreport = NULL,
    year_labels = DataFrame$year %>% unique(),
    category_names = category_names,
    extrapolation_list = extrapolation_list
  )

  Return = list(
    data_frame = data.frame(
      Lat_i = DataFrame$latitude,
      Lon_i = DataFrame$longitude,
      a_i = 1,
      v_i = rep(0, nrow(DataFrame)),
      b_i = DataFrame$cpue,
      t_i = DataFrame$year,
      c_iz = DataFrame$cat_int
    ),
    extrapolation_list = extrapolation_list,
    spatial_list = spatial_list,
    data_list = data_list,
    tmb_list = tmb_list,
    parameter_estimates = parameter_estimates1,
    Report = Report,
    ParHat = ParHat,
    year_labels = DataFrame$year %>% unique(),
    years_to_plot = 1:length(unique(DataFrame$year)),
    category_names = category_names,
    settings = settings
  ) %>%
    structure(class = "fit_model")
  ##input_args = input_args,
  #X1config_cp = X1config_cp,
  #X2config_cp = X2config_cp,
  #covariate_data = covariate_data,
  #X1_formula = X1_formula,
  #X2_formula = X2_formula,
  #Q1config_k = Q1config_k,
  #Q2config_k = Q1config_k,
  #catchability_data = catchability_data,
  #Q1_formula = Q1_formula,
  #Q2_formula = Q2_formula)

  return(Return)
}



plot_VAST_factor <- function(fit,
                             working_dir = getwd(),
                             save_figure = TRUE,
                             var,
                             year_labels = fit$year_labels, years_to_plot = fit$years_to_plot,
                             category_names = fit$category_names,
                             sp_size = c(6, 6),
                             map_size = c(6, 8),
                             corr_size = c(5, 6),
                             load_size = c(5, 3),
                             rel_size = 1, ...) {

  require(VAST)

  coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
    sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

  lab <- c("Epsilon1", "Epsilon2", "Omega1", "Omega2")
  Lvec <- c("L_epsilon1_cf", "L_epsilon2_cf", "L_omega1_cf", "L_omega2_cf")
  Om <- c("Epsiloninput1_gft", "Epsiloninput2_gft", "Omegainput1_gf", "Omegainput2_gf")
  #covvec <- paste0("lowercov_uppercor_", tolower(lab))

  # Plot individual fields
  if (missing(var)) var <- lab
  for (i in 1:length(var)) {
    i2 <- which(var[i] == lab)

    Loadings <- fit$Report[[Lvec[i2]]]

    if (length(Loadings)) {

      message("For ", lab[i2], ":")

      # Corr matrix ----
      message("Plotting correlation matrix...")
      #cov_j <- fit$Report[[covvec[i2]]]
      cov_j <- Loadings %*% t(Loadings)
      for(ii in 1:(nrow(cov_j)-1)) { # Remove upper diagonal
        for(j in (ii+1):nrow(cov_j)) cov_j[ii, j] <- NA
      }

      corr_j <- cov_j %>%
        cov2cor() %>%
        structure(dimnames = list(Var1 = category_names, Var2 = category_names)) %>%
        signif(2) %>%
        reshape2::melt() %>%
        filter(!is.na(value)) %>%
        mutate(Var1 = factor(Var1, levels = category_names %>% rev()),
               Var2 = factor(Var2, levels = category_names))

      gcorr <- corr_j %>%
        ggplot(aes(Var2, Var1)) +
        geom_tile(height = 1, width = 1, colour = "black", aes(fill = value), alpha = 0.5) +
        geom_text(aes(label = value), size = rel(rel_size)) +
        coord_cartesian(expand = FALSE) +
        scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
        labs(x = NULL, y = NULL) +
        guides(fill = "none")

      if (save_figure) {
        ggsave(file.path(working_dir, paste0(lab[i2], "_corr.png")), gcorr,
               height = corr_size[1], width = corr_size[2])
      }

      Psi_sjt <- fit$Report[[Om[i2]]]
      logkappa = unlist(fit$ParHat[c("logkappa1", "logkappa2")])[c(1, 2, 1, 2)[i2]]
      stopifnot(fit$data_list$Options_list$Options_vec[8] == 0)
      tau = 1/(exp(logkappa) * sqrt(4 * pi))

      Var_rot = FishStatsUtils::rotate_factors(
        L_pj = Loadings,
        Psi_sjt = Psi_sjt/tau,
        RotationMethod = "PCA",
        testcutoff = 1e-4,
        quiet = TRUE
      )

      # Loadings ----
      message("Plotting loadings matrix...")
      L_rot <- Var_rot$L_pj_rot %>%
        structure(dimnames = list(Category = category_names, Factor = 1:ncol(.))) %>%
        reshape2::melt() %>%
        mutate(Category = factor(Category, levels = rev(category_names)))

      gload <- L_rot %>%
        ggplot(aes(factor(Factor), Category)) +
        geom_tile(aes(fill = value), alpha = 0.7) +
        geom_text(aes(label = round(value, 2))) +
        scale_fill_viridis_c() +
        coord_cartesian(expand = FALSE) +
        guides(fill = "none") +
        theme(legend.position = "bottom") +
        labs(x = "Factor", y = "Category", fill = expression("Loadings"~psi*minute))

      if (save_figure) {
        ggsave(file.path(working_dir, paste0(lab[i2], "_Loadings.png")), gload,
               height = load_size[1], width = load_size[2])
      }

      # Factors ----
      message("Plotting spatial factors...")
      Psi_rot <- Var_rot$Psi_rot %>%
        structure(
          dimnames = list(
            Cell = 1:dim(.)[1],
            Factor = 1:dim(.)[2],
            Year = if(i2 < 3) fit$year_labels else 1
          )
        ) %>%
        reshape2::melt() %>%
        left_join(fit$spatial_list$latlon_g %>% as.data.frame() %>% mutate(Cell = 1:nrow(.)), by = "Cell")

      if(length(unique(Psi_rot$Year)) > 1) {

        if (save_figure) {
          for(ff in 1:max(Psi_rot$Factor)) {
            g <- Psi_rot %>%
              filter(Factor == ff) %>%
              ggplot(aes(Lon, Lat)) +
              geom_sf(data = coast, inherit.aes = FALSE) +
              coord_sf(xlim = range(Psi_rot$Lon), ylim = range(Psi_rot$Lat), expand = FALSE) +
              geom_tile(height = 0.025, width = 0.025, aes(fill = value, colour = value)) +
              facet_wrap(vars(Year)) +
              scale_fill_gradient2(high = scales::muted("red"), low = scales::muted("blue")) +
              scale_colour_gradient2(high = scales::muted("red"), low = scales::muted("blue")) +
              labs(x = "Longitude", y = "Latitude", colour = paste("Factor", ff), fill = paste("Factor", ff)) +
              theme(panel.spacing = unit(0, "in"))
            ggsave(file.path(working_dir, paste0(lab[i2], "_Factor_", ff, ".png")), g, height = map_size[1], width = map_size[2])
          }
        } else {
          gfact <- Psi_rot
        }

      } else {

        gfact <- Psi_rot %>%
          ggplot(aes(Lon, Lat)) +
          geom_sf(data = coast, inherit.aes = FALSE) +
          coord_sf(xlim = range(Psi_rot$Lon), ylim = range(Psi_rot$Lat), expand = FALSE) +
          geom_tile(height = 0.025, width = 0.025, aes(fill = value, colour = value)) +
          facet_wrap(vars(paste("Factor", Factor))) +
          scale_fill_gradient2(high = scales::muted("red"), low = scales::muted("blue")) +
          scale_colour_gradient2(high = scales::muted("red"), low = scales::muted("blue")) +
          labs(x = "Longitude", y = "Latitude", colour = "Value", fill = "Value") +
          theme(panel.spacing = unit(0, "in"))

        if (save_figure) {
          ggsave(file.path(working_dir, paste0(lab[i2], "_Factors.png")), gfact,
                 height = map_size[1], width = map_size[2])
        }
      }
    }
  }

  invisible(list(corr = gcorr, loadings = gload, factors = gfact, corr_tot = gcorr))
}

plot_spatial_cpue <- function(fit, dbin2, category = "F_80", type = c("obs", "pred"), cell_size = 0.025) {

  type <- match.arg(type)

  if (type == "pred") {

    attr(fit$Report$D_gct, "units") <- NULL
    df <- fit$Report$D_gct %>%
      structure(class = "array") %>%
      reshape2::melt() %>%
      filter(Category == category) %>%
      rename(Year = Time) %>%
      left_join(fit$spatial_list$latlon_g %>% as.data.frame() %>% mutate(Site = 1:nrow(.)), by = "Site") %>%
      left_join(fit$spatial_list$loc_g %>% as.data.frame() %>% mutate(Site = 1:nrow(.)), by = "Site")

  } else {

    df <- dbin2 %>%
      rename(value = cpue, Lon = longitude, Lat = latitude, Category = category, Year = year) %>%
      filter(Category == category) %>%
      filter(value > 0)

  }

  coast <- rnaturalearth::ne_countries(scale = 10, continent = "north america", returnclass = "sf") %>%
    sf::st_crop(xmin = -134, xmax = -125, ymin = 48, ymax = 55)

  g <- ggplot(df, aes(Lon, Lat)) +
    geom_sf(data = coast, inherit.aes = FALSE) +
    coord_sf(expand = FALSE) +
    geom_tile(height = cell_size, width = cell_size, aes(fill = value, colour = value)) +
    facet_wrap(vars(Year)) +
    scale_fill_viridis_c(trans = "log", direction = -1) +
    scale_colour_viridis_c(trans = "log", direction = -1) +
    labs(x = "Longitude", y = "Latitude", colour = "Log-Density", fill = "Log-Density") +
    theme(panel.spacing = unit(0, "in"))

  g

}

plot_lencomp <- function(len_std, len_nom, y = unique(len_std$year), bin_width = 5,
                         by_survey = "survey_abbrev" %in% names(len_std)) {

  len_std <- len_std %>% filter(year %in% y)
  len_nom <- len_nom %>% filter(year %in% y)

  if (by_survey) {

    len_std <- len_std %>% mutate(p = ifelse(sex == "F", p, -1 * p))
    len_nom <- len_nom %>% mutate(p = ifelse(sex == "F", p, -1 * p))

    N <- len_nom %>%
      summarize(n = sum(n), .by = c(year, survey_abbrev))

    g <- len_std %>%
      ggplot(aes(bin, p, fill = sex)) +
      geom_area(data = len_nom, linetype = 3, alpha = 0.4, position = "identity") +
      geom_line(aes(colour = sex)) +
      geom_label(data = N, aes(label = n), inherit.aes = FALSE,
                 x = Inf, y = Inf, hjust = "inward", vjust = "inward") +
      facet_grid(vars(year), vars(survey_abbrev)) +
      labs(x = "Length", y = "Proportion", colour = "Sex", fill = "Sex") +
      theme(panel.spacing = unit(0, "in"), legend.position = "bottom") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = abs) +
      #coord_cartesian(ylim = c(0, 0.3)) +
      #scale_fill_manual(values = c("Nominal" = "grey80", "Standardized" = "white")) +
      scale_shape_manual(values = c(1, 16))

  } else {

    g <- len_std %>%
      ggplot(aes(bin, p, shape = type, fill = type)) +
      geom_col(data = len_nom, width = bin_width, colour = 1, linewidth = 0.1) +
      geom_point(data = len_nom, shape = 1) +
      geom_point() +
      geom_line() +
      facet_grid(vars(year), vars(sex)) +
      labs(x = "Length", y = "Proportion", fill = "Series", shape = "Series") +
      theme(panel.spacing = unit(0, "in")) +
      scale_fill_manual(values = c("Nominal" = "grey80", "Standardized" = "white")) +
      scale_shape_manual(values = c("Nominal" = 1, "Standardized" = 16))

  }

  g
}

plot_dendrogram <- function(fit,
                            k = 2,
                            Lvec = c("L_epsilon1_cf", "L_epsilon2_cf", "L_omega1_cf", "L_omega2_cf"),
                            category_names = fit$category_names,
                            clust_method = c("silhouette", "wss", "gap_stat"),
                            FUNcluster = stats::kmeans,
                            rel_size = 1) {
  Lvec <- match.arg(Lvec, several.ok = TRUE)
  clust_method <- match.arg(clust_method)

  require(factoextra)

  # "Total variance" as the sum of various loadings matrices
  # See Omori and Thorson (2022), equation 9: https://doi.org/10.1093/icesjms/fsac015
  # Should calculate separately for Omegas and Epsilons
  V_factor <- lapply(Lvec, function(x) {
    L <- fit$Report[[x]]
    L2 <- L %*% t(L)
    return(L2)
  })
  V_total <- Reduce("+", V_factor)

  # Plot correlation matrix
  g_corr <- local({
    for(ii in 1:(nrow(V_total)-1)) { # Remove upper diagonal
      for(j in (ii+1):nrow(V_total)) V_total[ii, j] <- NA
    }

    corr_j <- V_total %>%
      cov2cor() %>%
      structure(dimnames = list(Var1 = category_names, Var2 = category_names)) %>%
      signif(2) %>%
      reshape2::melt() %>%
      filter(!is.na(value)) %>%
      mutate(Var1 = factor(Var1, levels = category_names %>% rev()),
             Var2 = factor(Var2, levels = category_names))
    corr_j %>%
      ggplot(aes(Var2, Var1)) +
      geom_tile(height = 1, width = 1, colour = "black", aes(fill = value), alpha = 0.5) +
      geom_text(aes(label = round(value, 2) %>% format()), size = rel(rel_size)) +
      coord_cartesian(expand = FALSE) +
      scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL) +
      guides(fill = "none")
  })

  # Calculate distance matrix, see equation 11
  gamma <- sapply(1:length(fit$category_names), function(i) {
    sapply(1:length(fit$category_names), function(j) {
      #browser(expr = i > j)
      v1 <- diag(V_total)[i]
      v2 <- diag(V_total)[j]
      cov <- V_total[i, j]
      sqrt(v1 + v2 - 2 * cov)
    })
  }) %>%
    structure(dimnames = list(fit$category_names, fit$category_names))

  # Determine number of clusters
  g_nclust <- factoextra::fviz_nbclust(gamma, FUNcluster = FUNcluster, method = clust_method, nboot = 500)

  # Get distance matrix
  #dist_matrix <- factoextra::get_dist(gamma, method = "euclidean", stand = FALSE)
  #factoextra::fviz_dist(dist_matrix)

  # Make dendrogram from (group by k)
  fvec <- c("L_epsilon1_cf" = "Epsilon1_gct", "L_epsilon2_cf" = "Epsilon2_gct",
            "L_omega1_cf" = "Omega1_gc", "L_omega2_cf" = "Omega2_gc")
  field <- lapply(fvec[Lvec], function(x) fit$Report[[x]])
  ff <- Reduce("+", field)
  if (length(dim(ff)) > 2) {
    ffout <- array(0, dim(ff)[1:2]) %>%
      structure(dimnames = dimnames(ff)[1:2])
    for(t in 2:dim(ff)[3]) ffout[] <- ffout + ff[, , t] - ff[, , t-1]
    ff <- ffout
  }
  dist_matrix <- factoextra::get_dist(t(ff), method = "euclidean", stand = TRUE)

  clust <- stats::hclust(dist_matrix)
  #plot(clust)
  clust_k <- stats::cutree(clust, k = k)
  clust2 <- stats::hclust(dist_matrix, members = clust_k)
  g_clust <- factoextra::fviz_dend(clust2, k = k)

  list(g_corr = g_corr, gamma = gamma, g_nclust = g_nclust, dist = dist, g_clust = g_clust)
}


plot_PCA <- function(fit, L = c("L_epsilon1_cf", "L_epsilon2_cf")) {
  PCA <- local({
    V_factor <- lapply(L, function(x) {
      L <- fit$Report[[x]]
      L2 <- L %*% t(L)
      return(L2)
    })
    V_total <- do.call("+", V_factor)
    stats::prcomp(V_total)
  })
  plot(PCA$rotation[, 1], PCA$rotation[, 2], xlab = "Component 1", ylab = "Component 2")
  text(PCA$rotation[, 1], PCA$rotation[, 2], rownames(PCA$rotation), pos = 3)
  abline(h = 0, v = 0, lty = 2)
  invisible()
}


# Variance calculations for proportions
calculate_SE <- function(fit) {

  attr(fit$Report$Index_ctl, "units") <- NULL
  Index <- fit$Report$Index_ctl[, , 1] %>%
    structure(class = "matrix") %>%
    reshape2::melt() %>%
    mutate(p = value/sum(value), .by = Time)

  derived_quants <- fit$parameter_estimates$SD$value
  Var_I <- fit$parameter_estimates$SD$cov[names(derived_quants) == "Index_ctl", names(derived_quants) == "Index_ctl"]

  # Function that calculates annual proportions
  f <- function(I, y) {
    proportion <- tapply(I, y, FUN = function(x) x/sum(x))
    do.call(c, proportion)
  }

  jac <- numDeriv::jacobian(f, x = Index$value, y = Index$Time)   # Jacobian
  Var_Prop <- jac %*% Var_I %*% t(jac)                            # Delta method
  mutate(Index, SE_p = diag(Var_Prop) %>% sqrt())                 # Only report the diagonal (no covariances)
}

calculate_Neff <- function(p, se, FUN = median) FUN(p * (1 - p)/se/se)
