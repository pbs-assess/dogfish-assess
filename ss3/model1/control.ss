#V3.30.20.00;_fast(opt);_compile_date:_Sep 30 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.0
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis
#_data_and_control_files: data.ss // control.ss
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS3)
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
#_NATMORT
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity;_6=Lorenzen_range
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
5 #_First_Mature_Age
4 #_fecundity_at_length option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach for M, G, CV_G:  1- direct, no offset**; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)
#_** in option 1, any male parameter with value = 0.0 and phase <0 is set equal to female parameter
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
 0.01 0.1 0.073 0 0 0 -50 0 0 0 0 0 0 0 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 1 55 40.4 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 30 100 97.4 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0.01 0.2 0.05 0 0 0 -50 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0.01 0.2 0.18 0 0 0 -50 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0.01 0.2 0.18 0 0 0 -50 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
# Sex: 1  BioPattern: 1  WtLen
 0 0.1 1.76e-06 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_1_Fem_GP_1
 2 4 3.2 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_2_Fem_GP_1
# Sex: 1  BioPattern: 1  Maturity&Fecundity
 0 100 97.6 0 0 0 -50 0 0 0 0 0 0 0 # Mat50%_Fem_GP_1
 -1 0 -0.168 0 0 0 -50 0 0 0 0 0 0 0 # Mat_slope_Fem_GP_1
 -14.7 3 -14.7 0 0 0 -50 0 0 0 0 0 0 0 # Eggs_intercept_Fem_GP_1
 -3 3 0.214 0 0 0 -50 0 0 0 0 0 0 0 # Eggs_slope_len_Fem_GP_1
# Sex: 2  BioPattern: 1  NatMort
 0.01 0.1 0.07 0 0 0 -50 0 0 0 0 0 0 0 # NatM_uniform_Mal_GP_1
# Sex: 2  BioPattern: 1  Growth
 1 55 50.7 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amin_Mal_GP_1
 30 100 83.7 0 0 0 -50 0 0 0 0 0 0 0 # L_at_Amax_Mal_GP_1
 0.01 0.2 0.08 0 0 0 -50 0 0 0 0 0 0 0 # VonBert_K_Mal_GP_1
 0.01 0.2 0.147 0 0 0 -50 0 0 0 0 0 0 0 # CV_young_Mal_GP_1
 0.01 0.2 0.147 0 0 0 -50 0 0 0 0 0 0 0 # CV_old_Mal_GP_1
# Sex: 2  BioPattern: 1  WtLen
 0 0.1 4.37e-06 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_1_Mal_GP_1
 2 4 2.97 0 0 0 -50 0 0 0 0 0 0 0 # Wtlen_2_Mal_GP_1
# Hermaphroditism
#  Recruitment Distribution 
#  Cohort growth dev base
 -5 5 1 0 0 0 -50 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
#  Age Error from parameters
#  catch multiplier
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_1_BottomTrawl
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_2_MidwaterTrawl
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_3_HookLine
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_7_Trawl_3565
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_8_LL_3565
 0 2 1 0 0 0 -50 0 0 0 0 0 0 0 # Catch_Mult:_9_Trawl_6695
#  fraction female, by GP
 0 1 0.5 0 0 0 -50 0 0 0 0 0 0 0 # FracFemale_GP_1
#  M2 parameter for each predator fleet
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
1  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
             3            20       9.16561             0             0             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1           0.3             0             0             0        -50          0          0          0          0          0          0          0 # SR_BH_steep
           0.2             1           0.4             0             0             0        -50          0          0          0          0          0          0          0 # SR_sigmaR
            -1             1             0             0             0             0        -50          0          0          0          0          0          0          0 # SR_regime
            -1             1             0             0             0             0        -50          0          0          0          0          0          0          0 # SR_autocorr
#_no timevary SR parameters
0 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
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
#_Cond 2023 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS3 sets bias_adj to 0.0 for fcast yrs)
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
#  2023F
# #
#Fishing Mortality info 
0.05 # F ballpark value in units of annual_F
-1920 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope midseason rate; 2=F as parameter; 3=F as hybrid; 4=fleet-specific parm/hybrid (#4 is superset of #2 and #3 and is recommended)
3 # max F (methods 2-4) or harvest fraction (method 1)
3  # N iterations for tuning in hybrid mode; recommend 3 (faster) to 5 (more precise if many fleets)
#
#_initial_F_parms; for each fleet x season that has init_catch; nest season in fleet; count = 0
#_for unconstrained init_F, use an arbitrary initial catch and set lambda=0 for its logL
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#
# F rates by fleet x season
# Yr:  1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# BottomTrawl 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.123147 0.0755935 0.108129 0.0500531 0.0811943 0.0499588 0.0697743 0.0449207 0.0581407 0.0393321 0.0407282 0.0251488 0.0172366 0.0399091 0.0225498 0.0240795 0.0132032 0.0156071 0.00679781 0.00786852 0.00616148 0.00718546 0.00710108 0.00620958 0.00396948 0.00602263 0.00262469 0.00262469
# MidwaterTrawl 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1.88341e-06 4.1093e-06 1.25055e-06 2.89738e-06 3.39843e-07 0.000187005 7.47482e-06 0.000320049 4.24185e-05 0.000262352 0.00028994 5.21972e-05 0.00033235 4.36869e-05 7.0906e-05 0.000392814 0.000526489 0.000318072 0.000308596 7.89374e-05 2.00282e-05 4.02844e-05 0.000605776 0.000673388 0.000484374 0.00012026 0.00012026
# HookLine 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.000125059 0 0 0 0 0.0010318 0.00573208 0.00426019 0.0159525 0.00394152 0.0108045 0.0221487 0.00977324 0.0625707 0.0249868 0.233687 0.401819 0.196082 0.237288 0.300546 0.282044 0.0579165 0.161079 0.202016 0.281043 0.116695 0.198492 0.286129 0.553893 0.513822 0.481964 0.674292 0.704244 0.676965 0.309542 0.0357862 0.0279399 0.0256675 0.0319902 0.00641979 0.015355 0.00297884 0.00270692 0.00168198 0.000105082 0.000777792 0.000105115 0.000202129 0.000653362 0.000312657 0.000290681 0.000290681
# Trawl_3565 0.00978207 0.00054996 0.0131653 0.0252912 0.0613801 0.180229 0.215978 0.952106 1.10936 0.649432 0.972964 0.91506 1.28143 0.119207 0.246425 0.135053 0.141795 0.0566912 0.0727241 0.053675 0.0986311 0.0429154 0.0935322 0.0596781 0.233129 0.00786637 0.0040339 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# LL_3565 0.0120458 0.000677533 0.0162286 0.0312276 0.0760721 0.225767 0.275524 1.28649 1.69154 1.10763 1.82729 1.91299 2.99996 0.290469 0.578622 0.306091 0.308678 0.118273 0.145286 0.102991 0.182569 0.0768082 0.162296 0.100752 0.386382 0.0127968 0.00639234 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# Trawl_6695 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.0265446 0.0345834 0.00966889 0.00755284 0.0189541 0.0265281 0.00477025 0.0958066 0.00933257 0.122219 0.000255472 0.110718 0.130003 0.146296 0.400517 0.164971 0.28386 0.153009 0.157574 0.299049 0.267921 0.225139 0.312592 0.2291 0.323784 0.315723 0.276163 0.195554 0.186847 0.133715 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         4         1         0         0         0         1  #  IPHC
         5         1         0         0         0         1  #  HBLL
         6         1         0         0         0         1  #  SYN
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -10            10       -5.1281             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_IPHC(4)
            -5             5      -4.02718             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_HBLL(5)
            -5             5      -10.4101             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_SYN(6)
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
#Pattern:_2;  parm=6; double_normal with sel(minL) and sel(maxL), using joiners, back compatibile version of 24 with 3.30.18 and older
#Pattern:_25; parm=3; exponential-logistic in length
#Pattern:_27; parm=special+3; cubic spline in length; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=special+3+2; cubic spline; like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 24 0 3 0 # 1 BottomTrawl
 24 0 3 0 # 2 MidwaterTrawl
 24 0 3 0 # 3 HookLine
 24 0 3 0 # 4 IPHC
 15 0 0 4 # 5 HBLL
 24 0 3 0 # 6 SYN
 15 0 0 1 # 7 Trawl_3565
 15 0 0 3 # 8 LL_3565
 15 0 0 1 # 9 Trawl_6695
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic. Recommend using pattern 18 instead.
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (average over bin range)
#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder
#_Pattern Discard Male Special
 10 0 0 0 # 1 BottomTrawl
 10 0 0 0 # 2 MidwaterTrawl
 10 0 0 0 # 3 HookLine
 10 0 0 0 # 4 IPHC
 10 0 0 0 # 5 HBLL
 10 0 0 0 # 6 SYN
 10 0 0 0 # 7 Trawl_3565
 10 0 0 0 # 8 LL_3565
 10 0 0 0 # 9 Trawl_6695
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   BottomTrawl LenSelex
            14           120           120             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_BottomTrawl(1)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_BottomTrawl(1)
           -10            10        6.2266             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_BottomTrawl(1)
           -10            10             0             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_BottomTrawl(1)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_BottomTrawl(1)
          -999           999             5             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_BottomTrawl(1)
          -100             0      -26.6751             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_BottomTrawl(1)
           -10            10     -0.358849             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_BottomTrawl(1)
           -10            10       -1.7671             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_BottomTrawl(1)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_BottomTrawl(1)
             0             1     0.0511047             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_BottomTrawl(1)
# 2   MidwaterTrawl LenSelex
            14           120       59.3269             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_MidwaterTrawl(2)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_MidwaterTrawl(2)
           -10            10       5.26216             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_MidwaterTrawl(2)
           -10            10       5.35528             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_MidwaterTrawl(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_MidwaterTrawl(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_MidwaterTrawl(2)
          -100           100       23.2128             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_MidwaterTrawl(2)
           -10            10       9.95621             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_MidwaterTrawl(2)
           -10            10      -1.29589             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_MidwaterTrawl(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_MidwaterTrawl(2)
             0             1      0.747595             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_MidwaterTrawl(2)
# 3   HookLine LenSelex
            14           120       106.791             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_HookLine(3)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_HookLine(3)
           -10            10       4.02673             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_HookLine(3)
           -10            10             0             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_HookLine(3)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_HookLine(3)
          -999           999             5             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_HookLine(3)
          -100             0      -22.4719             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_HookLine(3)
           -10            10      -2.56037             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_HookLine(3)
           -10            10       5.07239             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_HookLine(3)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_HookLine(3)
             0             1    0.00118937             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_HookLine(3)
# 4   IPHC LenSelex
            14           120       93.9103             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_IPHC(4)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_IPHC(4)
           -10            10       6.17768             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_IPHC(4)
           -10            10      -6.07498             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_IPHC(4)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_IPHC(4)
          -999           999          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_IPHC(4)
          -100             0      -27.3733             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_IPHC(4)
           -10            10      -2.04601             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_IPHC(4)
           -10            10       9.65958             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_IPHC(4)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_IPHC(4)
             0             1      0.331736             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_IPHC(4)
# 5   HBLL LenSelex
# 6   SYN LenSelex
            14           120       69.3508             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_SYN(6)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_SYN(6)
           -10            10       5.28293             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_SYN(6)
           -10            10       9.99979             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_SYN(6)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_SYN(6)
          -999           999          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_SYN(6)
          -100           100       12.0158             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_SYN(6)
           -10            10       1.09169             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_SYN(6)
           -10            10      -6.12597             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_SYN(6)
          -999           100          -999             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_SYN(6)
             0             1      0.929375             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_SYN(6)
# 7   Trawl_3565 LenSelex
# 8   LL_3565 LenSelex
# 9   Trawl_6695 LenSelex
# 1   BottomTrawl AgeSelex
# 2   MidwaterTrawl AgeSelex
# 3   HookLine AgeSelex
# 4   IPHC AgeSelex
# 5   HBLL AgeSelex
# 6   SYN AgeSelex
# 7   Trawl_3565 AgeSelex
# 8   LL_3565 AgeSelex
# 9   Trawl_6695 AgeSelex
#_No_Dirichlet parameters
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
#  1 #_CPUE/survey:_4
#  1 #_CPUE/survey:_5
#  1 #_CPUE/survey:_6
#  0 #_CPUE/survey:_7
#  0 #_CPUE/survey:_8
#  0 #_CPUE/survey:_9
#  1 #_lencomp:_1
#  1 #_lencomp:_2
#  1 #_lencomp:_3
#  1 #_lencomp:_4
#  0 #_lencomp:_5
#  1 #_lencomp:_6
#  0 #_lencomp:_7
#  0 #_lencomp:_8
#  0 #_lencomp:_9
#  1 #_init_equ_catch1
#  1 #_init_equ_catch2
#  1 #_init_equ_catch3
#  1 #_init_equ_catch4
#  1 #_init_equ_catch5
#  1 #_init_equ_catch6
#  1 #_init_equ_catch7
#  1 #_init_equ_catch8
#  1 #_init_equ_catch9
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M,Dyn. Bzero, SmryBio
 # 0 2 0 0 # Selectivity: (1) fleet, (2) 1=len/2=age/3=both, (3) year, (4) N selex bins
 # 0 0 # Growth: (1) growth pattern, (2) growth ages
 # 0 0 0 # Numbers-at-age: (1) area(-1 for all), (2) year, (3) N ages
 # -1 # list of bin #'s for selex std (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 # -1 # list of ages for NatAge std (-1 in first bin to self-generate)
999

