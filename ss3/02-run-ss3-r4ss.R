

model_dir <- "model1a_dwLen"
model_dir <- "model4_estH"
ss_home <- here::here("ss3")
ss_home <- "C:/users/quang/Desktop/dogfish_ss3"

#### Fit ss3 model
fit_ss3 <- function(model_dir = "model1",
                    hessian = FALSE,
                    ss_home = here::here("ss3"),
                    max_phase) {
  dir_cur <- getwd()
  dir_run <- file.path(ss_home, model_dir)
  setwd(dir_run)
  on.exit(setwd(dir_cur))

  cmd <- "ss.exe -nox"
  if (!hessian) {
    cmd <- paste(cmd, "-nohess")
  }
  if (!missing(max_phase) && is.integer(max_phase)) {
    cmd <- paste(cmd, "-maxI", max_phase)
  }
  message("File directory: ", dir_run)
  message("Command: ", cmd)

  system(cmd)
}

fit_ss3(model_dir, hessian = FALSE, ss_home = ss_home, max_phase = 10L)





# Load r4ss list
replist <- r4ss::SS_output(file.path(ss_home, model_dir),
                           verbose = FALSE,
                           printstats = FALSE,
                           hidewarn = TRUE)
# Save report list
#saveRDS(replist, file = file.path(ss_home, paste0("r4ss_", model_dir, ".rds")))

# Generate HTML report
r4ss::SS_plots(replist,
               verbose = FALSE)


# Report and plot various output in R
replist$estimated_non_dev_parameters %>%
  select(Value, Parm_StDev) %>%
  mutate(CV = abs(Parm_StDev/Value) %>% round(2)) %>%
  View()

r4ss::SSplotSelex(replist, fleets = 1)
r4ss::SSplotTimeseries(replist, subplot = 1)
r4ss::SSplotRecdevs(replist)
r4ss::SSplotCatch(replist)
r4ss::SSplotIndices(replist)
r4ss::SSplotComps(replist)
replist$likelihoods_used


# Compare SS models
mods <- c("model1", "model1a_dwLen", "model4_estH", "model4a_estH_dwLen")

multi_rep <- lapply(mods, function(x) r4ss::SS_output(file.path(ss_home, x),
                                                      verbose = FALSE,
                                                      printstats = FALSE,
                                                      hidewarn = TRUE))

likelihoods <- lapply(1:length(mods), function(x) {
  xx <- multi_rep[[x]][["likelihoods_used"]] %>%
    mutate(name = rownames(.)) %>%
    select(name, values)
  colnames(xx)[2] <- mods[x]
  xx
}) %>%
  Reduce(dplyr::left_join, .)

# Plot comparisons
multi_rep %>%
  r4ss::SSsummarize() %>%
  r4ss::SSplotComparisons(legendlabels = mods)
