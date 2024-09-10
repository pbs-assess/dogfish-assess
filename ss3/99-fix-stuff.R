if (FALSE) {
  mods <- c("A1", "A0",
    "A2_USgrowth", "A3_highmat", "A4_USgrowth_highmat", "A5_highdiscard",
    "A6_IPHC_CPUE", "A7_SYNonly", "A8_HBLLonly", "A9_lowM", "A10_highM",
    "A11_low_zfrac", "A12_high_zfrac", "A13_extraSD", "A14_lowdiscard",
    "A15_100discard",
    "B1_1990inc", "B2_2010step", "B3_2005step", "B4_1990inc_lowM", "B5_2010step_lowM")

  # for (i in seq_along(mods)) {
  #   f <- readLines(paste0("ss3/", mods[i], "/control.ss"))
  #   ii <- grep("NatM_p_1_Fem_GP_1", f)
  #   f[ii] <- gsub("0\\.074", "0.065", f[ii])
  #   writeLines(f, paste0("ss3/", mods[i], "/control.ss"))
  # }
  #
  # mods <- c(
  #   "A10_highM"
  # )
  #
  # for (i in seq_along(mods)) {
  #   f <- readLines(paste0("ss3/", mods[i], "/control.ss"))
  #   ii <- grep("NatM_p_1_Fem_GP_1", f)
  #   f[ii] <- gsub("0\\.074", "0.065", f[ii])
  #   writeLines(f, paste0("ss3/", mods[i], "/control.ss"))
  # }
  #
  #

  # for (i in seq_along(mods)) {
  #   f <- readLines(paste0("ss3/", mods[i], "/data.ss"))
  #   f[grep("1 -1 1 2 1 IPHC  # 6", f)] <- "1 -1 1 1 1 IPHC  # 6"
  #   f[grep("1 -1 1 2 1 HBLL  # 7", f)] <- "1 -1 1 1 1 HBLL  # 7"
  #   f[grep("1 -1 1 2 1 iRec # 9", f)] <- "1 -1 1 1 1 iRec # 9"
  #   f[grep("1 -1 1 2 1 Salmon_Bycatch # 10", f)] <- "1 -1 1 1 1 Salmon_Bycatch # 10"
  #   writeLines(f, paste0("ss3/", mods[i], "/data.ss"))
  # }

  for (i in seq_along(mods)) {
    f <- readLines(paste0("ss3/", mods[i], "/data.ss"))

    # previous:
    # 2003	1	8	2.938	0.244
    # 2004	1	8	4.053	0.113
    # 2005	1	8	4.526	0.188
    # 2006	1	8	4.684	0.101
    # 2007	1	8	3.595	0.223
    # 2008	1	8	2.974	0.114
    # 2009	1	8	3.49	0.224
    # 2010	1	8	4.982	0.133
    # 2011	1	8	3.412	0.214
    # 2012	1	8	2.702	0.144
    # 2013	1	8	2.822	0.247
    # 2014	1	8	2.643	0.197
    # 2015	1	8	1.875	0.265
    # 2016	1	8	1.589	0.149
    # 2017	1	8	1.649	0.195
    # 2018	1	8	1.567	0.126
    # 2019	1	8	1.289	0.184
    # 2020	1	8	0.801	0.236
    # 2021	1	8	0.495	0.123
    # 2022	1	8	0.813	0.142
    # 2023	1	8	0.872	0.341


    start <- grep("2003	1	8	2.938", f)
    end <- grep("2023	1	8	0.872", f)

    replacement <-
      c(
        "2003	1	8	2.669	0.228",
        "2004	1	8	2.839	0.119",
        "2005	1	8	2.938	0.169",
        "2006	1	8	2.888	0.094",
        "2007	1	8	2.218	0.157",
        "2008	1	8	1.696	0.106",
        "2009	1	8	2.198	0.161",
        "2010	1	8	3.144	0.12",
        "2011	1	8	2.299	0.157",
        "2012	1	8	1.781	0.121",
        "2013	1	8	1.754	0.149",
        "2014	1	8	1.527	0.135",
        "2015	1	8	1.257	0.176",
        "2016	1	8	1.22	0.119",
        "2017	1	8	1.209	0.143",
        "2018	1	8	1.013	0.107",
        "2019	1	8	0.766	0.178",
        "2020	1	8	0.612	0.193",
        "2021	1	8	0.48	0.096",
        "2022	1	8	0.651	0.128",
        "2023	1	8	0.664	0.256"
      )
    stopifnot(length(f[start:end])==length(replacement))

    f[start:end] <- replacement

    writeLines(f, paste0("ss3/", mods[i], "/data.ss"))
  }
}