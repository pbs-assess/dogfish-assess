mods <- c(
  "A1", "A0",
  "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_highdiscard",
  "A6_IPHC+CPUE", "A7_SYNonly", "A8_HBLLonly", "A9_lowM", "A10_highM", "A11_low_zfrac",
  "A12_high_zfrac", "A13_extraSD", "A14_lowdiscard", "A15_100discard",
  "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM"
)

model_name <- c(
  "(A1) Base with estimated zfrac",
  "(A0) Base",
  "(A2) US growth",
  "(A3) BC growth, high maturity",
  "(A4) US growth, high maturity",
  "(A5) High discard mortality",
  "(A6) IPHC + CPUE",
  "(A7) SYN only",
  "(A8) HBLL only",
  # "(A9) M = 0.05",
  "(A9) Low M",
  # "(A10) M = 0.082",
  "(A10) High M",
  # "(A11) zfrac = 0.2, Beta = 0.6",
  "(A11) Low productivity",
  # "(A12) zfrac = 0.6, Beta = 2",
  "(A12) High productivity",
  "(A13) Extra SD on IPHC",
  "(A14) Low discard mortality",
  "(A15) 100% discard mortality",
  "(B1) M = 0.074, inc. 1990",
  "(B2) M = 0.074, step 2010",
  "(B3) M = 0.074, step 2005",
  "(B4) M = 0.05, inc. 1990",
  "(B5) M = 0.05, inc. 2010"
)