mods <- c(
  "A1", "A0",
  "A2_USgrowth",
  "A3_highmat",
  "A4_USgrowth_highmat",
  "A5_highdiscard",
  "A6_IPHC+CPUE",
  "A7_SYNonly",
  "A8_HBLLonly",
  #"A9_lowM",
  #"A10_highM",
  "A11_low_zfrac",
  "A12_high_zfrac",
  "A13_extraSD",
  "A14_lowdiscard",
  "A15_100discard",
  "B1_1990inc",
  "B2_2010step",
  "B3_2005step"
  #"B4_1990inc_lowM",
  #"B5_2010step_lowM"
)

for (i in seq_along(mods)) {
  f <- readLines(paste0("ss3/", mods[i], "/control.ss"))
  ii <- grep("NatM_p_1_Fem_GP_1", f)
  f[ii] <- gsub("0\\.074", "0.065", f[ii])
  writeLines(f, paste0("ss3/", mods[i], "/control.ss"))
}

mods <- c(
  "A10_highM"
)

for (i in seq_along(mods)) {
  f <- readLines(paste0("ss3/", mods[i], "/control.ss"))
  ii <- grep("NatM_p_1_Fem_GP_1", f)
  f[ii] <- gsub("0\\.074", "0.065", f[ii])
  writeLines(f, paste0("ss3/", mods[i], "/control.ss"))
}

