mods <- c("A1", "A0",
  "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_highdiscard",
  "A6_IPHC+CPUE", "A7_SYNonly", "A8_HBLLonly", "A9_lowM", "A10_highM",
  "A11_low_zfrac", "A12_high_zfrac", "A13_extraSD", "A14_lowdiscard",
  "A15_100discard",
  "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM")

for (i in seq_along(mods)) {
  f <- readLines(paste0("ss3/", mods[i], "/data.ss"))
  f[grep("1 -1 1 2 1 IPHC  # 6", f)] <- "1 -1 1 1 1 IPHC  # 6"
  f[grep("1 -1 1 2 1 HBLL  # 7", f)] <- "1 -1 1 1 1 HBLL  # 7"
  f[grep("1 -1 1 2 1 iRec # 9", f)] <- "1 -1 1 1 1 iRec # 9"
  f[grep("1 -1 1 2 1 Salmon_Bycatch # 10", f)] <- "1 -1 1 1 1 Salmon_Bycatch # 10"
  writeLines(f, paste0("ss3/", mods[i], "/data.ss"))
}
