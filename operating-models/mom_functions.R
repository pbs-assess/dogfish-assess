


SS2MOM_dogfish <- function(...) {
  MOM <- MSEtool::SS2MOM(...)
  np <- length(MOM@Stocks)
  nf <- length(MOM@Fleets[[1]])

  # Need to explicitly provide these parameters because apical selectivity is not 1 for males
  for(p in 1:np) {
    for(f in 1:nf) {
      SLarray <- MOM@cpars[[p]][[f]]$SLarray[1, , 1]
      L5 <- MSEtool:::LinInterp(x = SLarray,
                                y = MOM@cpars[[p]][[f]]$CAL_binsmid,
                                xlev = 0.05 * max(SLarray),
                                ascending = TRUE)

      LFS <- MSEtool:::LinInterp(x = SLarray,
                                 y = MOM@cpars[[p]][[f]]$CAL_binsmid,
                                 xlev = 0.95 * max(SLarray),
                                 ascending = TRUE)

      Vmaxlen <- SLarray[length(SLarray)]

      if(L5 >= LFS) L5 <- LFS - 0.1

      #print(c(L5, LFS, Vmaxlen))

      MOM@cpars[[p]][[f]][["L5_y"]] <-
        MOM@cpars[[p]][[f]][["LR5_y"]] <- matrix(L5, MOM@nsim, MOM@Fleets[[1]][[1]]@nyears + MOM@proyears)

      MOM@cpars[[p]][[f]][["LFS_y"]] <-
        MOM@cpars[[p]][[f]][["LFR_y"]] <- matrix(LFS, MOM@nsim, MOM@Fleets[[1]][[1]]@nyears + MOM@proyears)

      MOM@cpars[[p]][[f]][["Vmaxlen_y"]] <-
        MOM@cpars[[p]][[f]][["Rmaxlen_y"]] <- matrix(Vmaxlen, MOM@nsim, MOM@Fleets[[1]][[1]]@nyears + MOM@proyears)
    }
  }

  return(MOM)
}


MOM_agg_fleets_dogfish <- function(MOM) {

  nsim <- MOM@nsim
  MOM_aggfleet <- MOM
  n.stock <- length(MOM@Stocks)

  sel_par <- lapply(MOM@cpars, MSEtool:::calculate_single_fleet_dynamics)

  fleet <- MOM@Fleets[[1]][[1]]
  MOM_aggfleet@Fleets[[1]] <- list()
  MOM_aggfleet@Fleets[[2]] <- list()

  MOM_aggfleet@Fleets[[1]][[1]] <- fleet
  MOM_aggfleet@Fleets[[1]][[1]]@Name <- 'Aggregated Female'

  MOM_aggfleet@Fleets[[2]][[1]] <- fleet
  MOM_aggfleet@Fleets[[2]][[1]]@Name <- 'Aggregated Male'

  MOM_aggfleet@cpars[[1]] <- list()
  MOM_aggfleet@cpars[[2]] <- list()


  MOM_aggfleet@cpars[[1]][[1]] <- MOM@cpars[[1]][[1]]
  MOM_aggfleet@cpars[[2]][[1]] <- MOM@cpars[[2]][[1]]

  # No discarding, don't care about length selectivity right now, ignore these fields
  sel_names <- names(sel_par[[1]])
  vars_exclude <- c("Fdisc", "Fdisc_array1", "Fdisc_array2", "SLarray", "retA", "retL")

  MOM_aggfleet@cpars[[1]][[1]][vars_exclude] <-
    MOM_aggfleet@cpars[[2]][[1]][vars_exclude] <- lapply(vars_exclude, function(...) NULL)

  # Female dynamics
  MOM_aggfleet@cpars[[1]][[1]][c("Find", "V_real")] <- sel_par[[1]][c("Find", "V_real")]
  #range(sel_par[[1]]$V_real)

  # Male F at age
  nf <- length(MOM@Fleets[[1]])
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  Male_F_age <- sapply(1:nf, function(f) {
    FM <- MOM@cpars[[2]][[f]]$qs * MOM@cpars[[2]][[f]]$Find
    V <- MOM@cpars[[2]][[f]]$V[, , 1:nyears]
    sapply(1:71, function(a) V[, a, ] * FM, simplify = "array")
  }, simplify = "array") %>%
    apply(1:3, sum)

  # Female apical F
  Female_apical_F <- sel_par[[1]]$Find

  # Male selectivity from female apical F
  Male_sel <- sapply(1:nyears, function(y) {
    denom <- Female_apical_F[, y]
    denom[denom < 1e-8] <- 1e-8
    Male_F_age[, y, ]/denom
  }, simplify = "array")
  #range(Male_sel)

  MOM_aggfleet@cpars[[2]][[1]]["Find"] <- sel_par[[1]]["Find"]
  MOM_aggfleet@cpars[[2]][[1]][["V_real"]] <- local({
    Male_sel_proyears <- Male_sel[, , nyears] %>% array(c(dim(.)[1:2], MOM@proyears))
    abind::abind(Male_sel, Male_sel_proyears, along = 3)
  })

  MOM_aggfleet@CatchFrac <- vector('list', 2)
  # update cpars
  for (st in 1:2) {
    # update aggegrated selectivity
    #for (nm in names(sel_par[[st]])) {
    #  MOM_aggfleet@cpars[[st]][[1]][[nm]] <- sel_par[[st]][[nm]]
    #}
    # weighted average empirical catch-weight over fleets
    MOM_aggfleet@cpars[[st]][[1]]$Wt_age_C  <- MSEtool:::calc_weightedmean_c(MOM@cpars[[st]])
    MOM_aggfleet@CatchFrac[[st]] <- matrix(apply(MOM@CatchFrac[[st]], 1, sum), nrow=nsim, ncol=1)
  }
  MOM_aggfleet
}

