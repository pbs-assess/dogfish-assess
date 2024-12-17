if (FALSE) {
  mods <- c("A1", "A0",
    "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_highdiscard",
    "A6_IPHC+CPUE", "A7_SYNonly", "A8_HBLLonly", "A9_lowM", "A10_highM",
    "A11_low_zfrac", "A12_high_zfrac", "A13_extraSD", "A14_lowdiscard",
    "A15_100discard",
    "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM")

  for (i in seq_along(mods)) {
    f <- readLines(paste0("ss3/", mods[i], "/control.ss"))
    x1 <- grep("Size_DblN_peak_HookLine_Landings\\(4\\)", f)
    x2 <- grep("SzSel_Male_Peak_HookLine_Landings\\(4\\)", f)
    f[x1] <- gsub("28\\.5", "14.25", f[x1])
    f[x2] <- gsub("28\\.5", "14.25", f[x2])
    writeLines(f, paste0("ss3/", mods[i], "/control.ss"))
  }
}


if (FALSE) {
  mods <- c("A1", "A0",
    "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_highdiscard",
    "A6_IPHC+CPUE", "A7_SYNonly", "A8_HBLLonly", "A9_lowM", "A10_highM",
    "A11_low_zfrac", "A12_high_zfrac", "A13_extraSD", "A14_lowdiscard",
    "A15_100discard",
    "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM")

  for (i in seq_along(mods)) {
    f <- readLines(paste0("ss3/", mods[i], "/control.ss"))
    x1 <- grep("Size_DblN_peak_HookLine_Landings\\(4\\)", f)
    x2 <- grep("SzSel_Male_Peak_HookLine_Landings\\(4\\)", f)
    f[x1] <- gsub("28\\.5", "14.25", f[x1])
    f[x2] <- gsub("28\\.5", "14.25", f[x2])
    writeLines(f, paste0("ss3/", mods[i], "/control.ss"))
  }
}