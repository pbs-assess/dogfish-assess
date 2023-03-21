
library(MSEtool)
source("operating-models/mom_functions.R")

model_dir <- "model4_estH"

ss_home <- here::here("ss3")
#ss_home <- "C:/users/quang/Desktop/dogfish_ss3"

replist <- r4ss::SS_output(file.path(ss_home, model_dir),
                           verbose = FALSE,
                           printstats = FALSE,
                           hidewarn = TRUE)

# Two-sex and three-fleet operating model
MOM <- SS2MOM_dogfish(SSdir = replist, nsim = 2)

# Aggregate fleets. Need a custom function because apical male selectivity is not one
# Thus need to calculate male selectivity from apical F experienced by females
MOM_agg <- MOM_agg_fleets_dogfish(MOM)

# No process error
MOM_agg@cpars[[1]][[1]]$Perr_y[] <- MOM_agg@cpars[[2]][[1]]$Perr_y[] <- 1

multiHist_agg <- SimulateMOM(MOM_agg, parallel = FALSE, silent = TRUE)
saveRDS(MOM_agg, file.path("operating-models", "mom", paste0("mom_", model_dir, ".rds")))
saveRDS(multiHist_agg, file.path("operating-models", "mom", paste0("multihist_", model_dir, ".rds")))

## Compare the 3-fleet and single fleet MOM's
multiHist <- SimulateMOM(MOM, parallel = FALSE, silent = TRUE)
#plot_SS2MOM(multiHist, replist)
#plot_SS2MOM(multiHist_agg, replist)

MSEtool:::compareNmulti(replist, multiHist)
MSEtool:::compareNmulti(replist, multiHist_agg)

MSEtool:::compareSBmulti(replist, multiHist)
MSEtool:::compareSBmulti(replist, multiHist_agg)

MSEtool:::compareBmulti(replist, multiHist)
MSEtool:::compareBmulti(replist, multiHist_agg)


MSEtool:::compareC_overallmulti(replist, multiHist)
MSEtool:::compareC_overallmulti(replist, multiHist_agg)

MSEtool:::compareRecmulti(replist, multiHist)
MSEtool:::compareRecmulti(replist, multiHist_agg)

# Selectivity in projection
np <- 2
g <- lapply(1:np, function(p, y) {
  V <- multiHist[[p]][[1]]@SampPars$Fleet$V_real[1, , y]
  data.frame(Age = 1:length(V) - 1,
             Selectivity = V,
             Sex = ifelse(p == 1, "Female", "Male"))
}, y = 136) %>%
  bind_rows() %>%
  ggplot(aes(Age, Selectivity, linetype = Sex)) +
  geom_line()
ggsave("figs/om/sel_proj.png", g, height = 2, width = 4)
