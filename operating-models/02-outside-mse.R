
library(MSEtool)

model_dir <- "model4a_estH_dwLen"
multiHist <- readRDS(file.path("operating-models", "mom", paste0("multihist_", model_dir, ".rds")))

# Search for FMSY
multiHist[[1]][[1]]@Misc$MOM@interval <- 200

F_MP <- function(x, DataList, reps) {
  np <- 2
  nf <- 1
  Fmult <- seq(0, 20, length.out = 75)

  mRec <- lapply(1:np, function(p) {
    lapply(1:nf, function(f) {
      Rec <- new("Rec")
      Rec@Effort <- Fmult[x]
      return(Rec)
    })
  })
  return(mRec)
}
class(F_MP) <- "MMP"

MMSE <- ProjectMOM(multiHist, MPs = "F_MP")
saveRDS(MMSE, file = file.path("operating-models", "mse", paste0("mse_fmp_", model_dir, ".rds")))

#MMSE <- readRDS(file = "C:/users/qhuynh/Desktop/MMSE.rds")
#MMSE@SSB[, 1, 1, ] %>% t() %>% matplot(typ = 'l')
#MMSE@SSB %>% str()
#MMSE@FM[, 1, 1, 1, ] %>% t() %>% matplot(typ = 'l')
