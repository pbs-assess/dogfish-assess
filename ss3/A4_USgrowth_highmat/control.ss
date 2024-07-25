#V3.30.16.00;_2020_09_03;_safe;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.2
#Stock Synthesis (SS) is a work of the U.S. Government and is not subject to copyright protection in the United States.
#Foreign copyrights may apply. See copyright.txt for more information.
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
#_data_and_control_files: data.ss // control.ss
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS)
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Platoon_within/between_stdev_ratio (no read if N_platoons=1)
#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern 
# begin and end years of blocks
#
# controls for all timevary parameters 
1 #_time-vary parm bound check (1=warn relative to base parm bounds; 3=no bound check); Also see env (3) and dev (5) options to constrain with base bounds
#
# AUTOGEN
 1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen time-varying parms of this category; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: P(y)=f(TVP,env_Zscore) w/ logit to stay in min-max;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  5: like 4 with logit transform to stay in base min-max
#_DevLinks(more):  21-25 keep last dev for rest of years
#
#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma
#
# setup for M, growth, wt-len, maturity, fecundity, (hermaphro), recr_distr, cohort_grow, (movement), (age error), (catch_mult), sex ratio 
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
2 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0.018
0.025
0.035
0.047
0.062
0.081
0.103
0.128
0.155
0.186
0.217
0.25
0.283
0.314
0.345
0.372
0.397
0.419
0.438
0.453
0.465
0.475
0.482
0.488
0.492
0.495
0.497
0.498
0.499
0.499
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
0.5
24 #_First_Mature_Age
4 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
2 #_parameter_offset_approach for M, G, CV_G:  1- direct, no offset; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
 0.01 0.114 0.074 0 0 0 -50 0 0 0 0 0 0 0 # NatM_p_1_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 1 55 22.07 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 30 120 119 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0.01 0.2 0.0277 0 0 0 -50 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0.01 10 2.82 0 0 0 -50 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0.01 10 6.30 0 0 0 -50 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
# Sex: 1  BioPattern: 1  WtLen
 0 0.1 1.89e-06 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_1_Fem_GP_1
 2 4 3.19 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_2_Fem_GP_1
# Sex: 1  BioPattern: 1  Maturity&Fecundity
 0 100 97.6 0 0 0 -50 0 0 0 0 0 0 0 # Mat50%_Fem_GP_1
 -1 0 -0.168 0 0 0 -50 0 0 0 0 0 0 0 # Mat_slope_Fem_GP_1
 -14.7 3 -9.96 0 0 0 -50 0 0 0 0 0 0 0 # Eggs_intercept_Fem_GP_1
 -3 3 0.176 0 0 0 -50 0 0 0 0 0 0 0 # Eggs_slope_len_Fem_GP_1
# Sex: 2  BioPattern: 1  NatMort
 0 0 0 0 0 0 -50 0 0 0 0 0 0 0 # NatM_p_1_Mal_GP_1
# Sex: 2  BioPattern: 1  Growth
 -2 2 0 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amin_Mal_GP_1
 -1 1 -0.195 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amax_Mal_GP_1
 -2 2 0.368 0 0 0 -50 0 0 0 0 0 0 0 # VonBert_K_Mal_GP_1
 -1 1 -0.186 0 0 0 -50 0 0 0 0 0 0 0 # CV_young_Mal_GP_1
 -1 1 0.056 0 0 0 -50 0 0 0 0 0 0 0 # CV_old_Mal_GP_1
# Sex: 2  BioPattern: 1  WtLen
 0 0.1 3.54e-06 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_1_Mal_GP_1
 2 4 3.03 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_2_Mal_GP_1
# Hermaphroditism
#  Recruitment Distribution  
#  Cohort growth dev base
 -5 5 1 0 0 0 -50 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
#  Age Error from parameters
#  catch multiplier
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_1_Bottom_Trawl_Landings
 0 2 3.333 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_2_Bottom_Trawl_Discards
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_3_MidwaterTrawl
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_4_HookLine_Landings
 0 2 3.333 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_5_HookLine_Discards
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_6_IPHC
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_7_HBLL
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_8_SYN
 0 2 3.333 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_9_iRec
 0 2 3.333 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_10_Salmon_Bycatch
#  fraction female, by GP
 0 1 0.5 0 0 0 -50 0 0 0 0 0 0 0 # FracFemale_GP_1
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
7 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
1  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
             5            15            12             0             0             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
             0             1           0.4           0.5      0.287717             2          1          0          0          0          0          0          0          0 # SR_surv_zfrac
           0.2             5             1             0             0             0        -50          0          0          0          0          0          0          0 # SR_surv_Beta
           0.2             1           0.4             0             0             0        -50          0          0          0          0          0          0          0 # SR_sigmaR
            -1             1             0             0             0             0        -50          0          0          0          0          0          0          0 # SR_regime
            -1             1             0             0             0             0        -50          0          0          0          0          0          0          0 # SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1960 # first year of main recr_devs; early devs can preceed this era
2022 # last year of main recr_devs; forecast devs start in following year
-3 #_recdev phase 
0 # (0/1) to read 13 advanced options
#_Cond 0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
#_Cond -4 #_recdev_early_phase
#_Cond -4 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
#_Cond 1 #_lambda for Fcast_recr_like occurring before endyr+1
#_Cond 937 #_last_yr_nobias_adj_in_MPD; begin of ramp
#_Cond 1867 #_first_yr_fullbias_adj_in_MPD; begin of plateau
#_Cond 2022 #_last_yr_fullbias_adj_in_MPD
#_Cond 2024 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
#_Cond 1 #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
#_Cond 0 #_period of cycles in recruitment (N parms read below)
#_Cond -5 #min rec_dev
#_Cond 5 #max rec_dev
#_Cond 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  2023F 2024F
#  0 0
# implementation error by year in forecast:  0
#
#Fishing Mortality info 
0.05 # F ballpark value in units of annual_F
-1920 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
3 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
3  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#2024 2074
# F rates by fleet
# Yr:  1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Bottom_Trawl_Landings 0.0117295 0.000659224 0.015774 0.0302641 0.073238 0.213306 0.252333 1.06903 1.17686 0.669138 0.988949 0.927557 1.30832 0.123164 0.256403 0.141541 0.149692 0.0603201 0.0780591 0.0581896 0.108148 0.0476602 0.105365 0.0683003 0.271753 0.00935161 0.00488712 0 0 0 0 0 0 0.000264151 0 0.000525784 0.0472019 0.000865223 0.00127286 0.000202265 0.00542679 0.0152505 0.0295437 0.181325 0.0303249 0.10744 0.0542833 0.0567367 0.178509 0.207966 0.112056 0.20359 0.112575 0.178278 0.140026 0.0778338 0.0117973 0.0244501 0.0401394 0.00261085 0.00241761 0.00680006 0.000868925 0.00133783 0.00227139 0.0129784 0.0267437 0.0102951 0.00893247 0.0128163 0.00437812 0.00124878 0.00730137 0.00203816 0.00165549 0.00195843 0.00120876 0.00129818 0.00117029 0.000938738 0.00112631 0.00183042 0.00437092 0.0034171 0.000310021 0.000679791 0.000647331 0.000647331
# Bottom_Trawl_Discards 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.0031412 0.00424509 0.00122773 0.000989808 0.00250568 0.00367283 0.00057459 0.00471641 0.00120868 0.0180971 0 0.0161396 0.0175118 0.0176351 0.0301743 0.0208585 0.0265996 0.015383 0.0161622 0.0195788 0.0109502 0.0204113 0.0226604 0.0231624 0.0318524 0.0375494 0.0404056 0.0356147 0.0339716 0.0225415 0.0264986 0.0169703 0.0250951 0.0125332 0.0213732 0.0136619 0.0195057 0.0110908 0.019445 0.0135746 0.0157465 0.011636 0.00802296 0.0201787 0.0115048 0.013226 0.00910531 0.0117729 0.00577454 0.00663527 0.00461937 0.00524128 0.00540168 0.00730147 0.00618697 0.0076813 0.00287026 0.00342542 0.00342542
# MidwaterTrawl 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4.73056e-06 1.07516e-05 3.40912e-06 8.26066e-06 1.01813e-06 0.000589968 2.49853e-05 0.00114248 0.000162793 0.00107679 0.00125758 0.000237562 0.0015919 0.000220433 0.000375349 0.00217788 0.00305303 0.00192154 0.00193258 0.000510308 0.000133085 0.000274323 0.00422274 0.00479977 0.00352045 0.000909778 0.000915939 0.000915939
# HookLine_Landings 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6.09432e-05 0 0 0 0 0.000567937 0.00321087 0.00242841 0.00926851 0.0023335 0.00651356 0.0136299 0.0061424 0.0403498 0.0166526 0.160984 0.287866 0.146152 0.183388 0.242006 0.236407 0.0500558 0.143172 0.18633 0.270564 0.116859 0.206596 0.312813 0.650886 0.639432 0.662075 1.01955 1.26263 1.39735 0.758525 0.0697692 0.0575877 0.0642826 0.0851701 0.0161882 0.038231 8.49304e-05 0.00259348 0.000306446 0 2.30013e-05 0 4.36812e-05 1.16841e-06 0 0 0 0
# HookLine_Discards 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.0030183 0.00254138 0.0063902 0.00383128 0.00692032 0.000145733 0.00231963 0.00171896 0.000508853 0.000271696 0.000211025 0.000684762 0.00122946 0.000806113 0.000746621 5.26028e-05 0.00041303 5.80531e-05 0.000108715 0.000394574 0.000196747 0.000189414 0.000443012 0.000443012
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         6         1         0         0         0         1  #  IPHC
         7         1         0         0         0         1  #  HBLL
         8         1         0         0         0         1  #  SYN
         11        1         0         0         0         1  #  HS_MSA
         12        1         0         0         0         1  #  Bottom_Trawl_CPUE
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -10            10      -3.95461             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_IPHC(6)
           -10            10      -2.64842             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_HBLL(7)
           -10            10      -8.95677             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_SYN(8)
           -10            10      -8.95677             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_HS_MSA(11)
           -10            10s      -8.95677             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_Bottom_Trawl_CPUE(12)
#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0;  parm=0; selex=1.0 for all sizes
#Pattern:_1;  parm=2; logistic; with 95% width specification
#Pattern:_5;  parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_11; parm=2; selex=1.0  for specified min-max population length bin range
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6;  parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8;  parm=8; double_logistic with smooth transitions and constant above Linf option
#Pattern:_9;  parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in size
#Pattern:_27; parm=3+special; cubic spline 
#Pattern:_42; parm=2+special+3; // like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 24 0 3 0 # 1 Bottom_Trawl_Landings
 24 0 3 0 # 2 Bottom_Trawl_Discards
 24 0 3 0 # 3 MidwaterTrawl
 24 0 3 0 # 4 HookLine_Landings
 15 0 0 2 # 5 HookLine_Discards
 24 0 3 0 # 6 IPHC
 15 0 0 6 # 7 HBLL
 24 0 3 0 # 8 SYN
 15 0 0 2 # 9 iRec
 15 0 0 2 # 10 Salmon_Bycatch
 15 0 0 8 # 11 HS_MSA
 15 0 0 2 # 12 Bottom_Trawl_CPUE
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (average over bin range)
#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder
#_Pattern Discard Male Special
 0 0 0 0 # 1 Bottom_Trawl_Landings
 0 0 0 0 # 2 Bottom_Trawl_Discards
 0 0 0 0 # 3 MidwaterTrawl
 0 0 0 0 # 4 HookLine_Landings
 0 0 0 0 # 5 HookLine_Discards
 0 0 0 0 # 6 IPHC
 0 0 0 0 # 7 HBLL
 0 0 0 0 # 8 SYN
 0 0 0 0 # 9 iRec
 0 0 0 0 # 10 Salmon_Bycatch
 0 0 0 0 # 11 HS_MSA
 0 0 0 0 # 12 Bottom_Trawl_CPUE
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   Bottom_Trawl_Landings LenSelex
            35           150       106.698           100            30             6          3          0          0          0          0          0          0          0  #  Size_DblN_peak_Bottom_Trawl_Landings(1)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_Bottom_Trawl_Landings(1)
           -10            10        5.4429          5.05           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_Bottom_Trawl_Landings(1)
           -10            50            15             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_Bottom_Trawl_Landings(1)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_Bottom_Trawl_Landings(1)
          -999           999          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_Bottom_Trawl_Landings(1)
          -100             0      -21.7446           -20            30             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_Bottom_Trawl_Landings(1)
           -10            10      -1.26652          1.03           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_Bottom_Trawl_Landings(1)
           -20            10      -7.95226         -11.8           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_Bottom_Trawl_Landings(1)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_Bottom_Trawl_Landings(1)
             0             1           0.1           0.5      0.287717             2          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_Bottom_Trawl_Landings(1)
# 2   Bottom_Trawl_Discards LenSelex
            35           110       62.6939            60            18             6          3          0          0          0          0          0          0          0  #  Size_DblN_peak_Bottom_Trawl_Discards(2)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_Bottom_Trawl_Discards(2)
           -10            10       5.79581          4.61           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_Bottom_Trawl_Discards(2)
           -10            50        9.3691             6           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_Bottom_Trawl_Discards(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_Bottom_Trawl_Discards(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_Bottom_Trawl_Discards(2)
          -100           100       9.75306             5            18             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_Bottom_Trawl_Discards(2)
           -10            10       6.11314           0.4           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_Bottom_Trawl_Discards(2)
           -10            10      -4.93226          -1.4           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_Bottom_Trawl_Discards(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_Bottom_Trawl_Discards(2)
             0             1      0.776155           0.5      0.287717             2          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_Bottom_Trawl_Discards(2)
# 3   MidwaterTrawl LenSelex
            35           110       52.6577            55          16.5             6          3          0          0          0          0          0          0          0  #  Size_DblN_peak_MidwaterTrawl(3)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_MidwaterTrawl(3)
           -10            10       5.45677           4.6           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_MidwaterTrawl(3)
           -10            10       5.28424             4           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_MidwaterTrawl(3)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_MidwaterTrawl(3)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_MidwaterTrawl(3)
          -100           100       19.3615             0          16.5             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_MidwaterTrawl(3)
           -10            50       9.93806          -0.6           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_MidwaterTrawl(3)
           -10            10     -0.825551           1.4           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_MidwaterTrawl(3)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_MidwaterTrawl(3)
             0             1      0.884402           0.5      0.287717             2          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_MidwaterTrawl(3)
# 4   HookLine_Landings LenSelex
            35           110       101.432            95          28.5             6          3          0          0          0          0          0          0          0  #  Size_DblN_peak_HookLine_Landings(4)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_HookLine_Landings(4)
           -10            10       4.79594             4           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_HookLine_Landings(4)
           -10            50            15             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_HookLine_Landings(4)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_HookLine_Landings(4)
          -999           999          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_HookLine_Landings(4)
          -100             0      -20.2021           -10          28.5             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_HookLine_Landings(4)
           -10            10      -1.96691             0           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_HookLine_Landings(4)
           -50            10       2.46477         -13.2           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_HookLine_Landings(4)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_HookLine_Landings(4)
             0             1           0.1           0.5      0.287717             2          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_HookLine_Landings(4)
# 5   HookLine_Discards LenSelex
# 6   IPHC LenSelex
            35           200           150            95          28.5             6          3          0          0          0          0          0          0          0  #  Size_DblN_peak_IPHC(6)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_IPHC(6)
           -10            10       6.25389           5.7           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_IPHC(6)
           -10            50            15             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_IPHC(6)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_IPHC(6)
          -999           999          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_IPHC(6)
          -100             0      -32.0789           -20          28.5             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_IPHC(6)
           -10            10       -9.9887          -1.7           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_IPHC(6)
           -50            10       1.68605         -11.8           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_IPHC(6)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_IPHC(6)
             0             1      0.639827           0.5      0.287717             2          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_IPHC(6)
# 7   HBLL LenSelex
# 8   SYN LenSelex
            35           110       66.5916            65          19.5             6          3          0          0          0          0          0          0          0  #  Size_DblN_peak_SYN(8)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_SYN(8)
           -10            10        5.5921           4.6           0.3             6          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_SYN(8)
           -10            50            15             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_SYN(8)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_SYN(8)
          -999           999          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_SYN(8)
          -100           100       4.91395             0          19.5             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_SYN(8)
           -10            10        1.0886             0           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_SYN(8)
           -50            10      -5.64713         -10.4           0.3             6          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_SYN(8)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_SYN(8)
             0             1       0.81194           0.5      0.287717             2          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_SYN(8)
# 1   Bottom_Trawl_Landings AgeSelex
# 2   Bottom_Trawl_Discards AgeSelex
# 3   MidwaterTrawl AgeSelex
# 4   HookLine_Landings AgeSelex
# 5   HookLine_Discards AgeSelex
# 6   IPHC AgeSelex
# 7   HBLL AgeSelex
# 8   SYN AgeSelex
#_no timevary selex parameters
#
0   #  use 2D_AR1 selectivity(0/1)
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read and autogen if tag data exist; 1=read
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# no timevary parameters
#
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
      4      1       0.035
      4      2       0.040
      4      3       0.018
      4      4       0.047
      4      6       0.105
      4      8       0.057
 -9999   1    0  # terminator
#
1 #_maxlambdaphase
0 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 0 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value  sizefreq_method
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  0 #_CPUE/survey:_2
#  0 #_CPUE/survey:_3
#  0 #_CPUE/survey:_4
#  0 #_CPUE/survey:_5
#  1 #_CPUE/survey:_6
#  1 #_CPUE/survey:_7
#  1 #_CPUE/survey:_8
#  1 #_lencomp:_1
#  1 #_lencomp:_2
#  1 #_lencomp:_3
#  1 #_lencomp:_4
#  0 #_lencomp:_5
#  1 #_lencomp:_6
#  0 #_lencomp:_7
#  1 #_lencomp:_8
#  1 #_init_equ_catch1
#  1 #_init_equ_catch2
#  1 #_init_equ_catch3
#  1 #_init_equ_catch4
#  1 #_init_equ_catch5
#  1 #_init_equ_catch6
#  1 #_init_equ_catch7
#  1 #_init_equ_catch8
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M and Dyn Bzero
 # 0 2 0 0 # Selectivity: (1) fleet, (2) 1=len/2=age/3=both, (3) year, (4) N selex bins
 # 0 0 # Growth: (1) growth pattern, (2) growth ages
 # 0 0 0 # Numbers-at-age: (1) area(-1 for all), (2) year, (3) N ages
 # -1 # list of bin #'s for selex std (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 # -1 # list of ages for NatAge std (-1 in first bin to self-generate)
999

