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
             3            12       6.77797             0             0             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1          0.67             0             0             0        -50          0          0          0          0          0          0          0 # SR_BH_steep
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
#_Cond 954 #_last_yr_nobias_adj_in_MPD; begin of ramp
#_Cond 1884 #_first_yr_fullbias_adj_in_MPD; begin of plateau
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
# Yr:  1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# BottomTrawl 0.00487078 0.0227572 0.00919947 0.00449821 0.00586255 0.11788 0.308799 0.161896 0 0 0 0.00141447 0 0 1.38572e-05 0 0 0 0 0.372965 0.000191394 0.00072374 0.00163545 0.00324275 0.0248983 0.0507521 0.242916 0.0372774 0.191874 0.016414 0.084525 0.033687 0.0730512 0.0448812 0.0509378 0.0174826 0.018137 0.0180805 0.0185925 0 0.00939211 0.00602749 0.704944 0.483482 0.767916 0.387735 0.671825 0.437043 0.648497 0.455932 0.642245 0.456799 0.48872 0.316182 0.225492 0.545443 0.32354 0.35552 0.198107 0.23669 0.103077 0.118606 0.0921544 0.106543 0.104552 0.0909226 0.0578426 0.0873983 0.037854 0.037854
# MidwaterTrawl 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.000258919 0.000161568 2.34453e-06 0 0 0 0 0 0 0 0 0 0.000571943 0 0 0 7.48128e-05 0.00155415 0 1.8271e-05 4.05582e-05 1.25233e-05 2.93903e-05 3.49049e-06 0.00194776 7.91208e-05 0.00344712 0.000464366 0.0029159 0.00327569 0.000597786 0.00387214 0.00051854 0.000854111 0.00479848 0.00652137 0.00398502 0.00389963 0.00100421 0.000255993 0.000516932 0.00781385 0.00874002 0.00631758 0.00157098 0.00157098
# HookLine 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.0203822 0.0842515 0.018691 0.0677419 0.0683194 0.0652099 0.37246 0.0039612 0.0176115 0.0123224 0.00341212 0.325637 0.117961 0.00990474 0.00180847 0.0060076 0.0189063 0.157175 0.0103649 0.0739179 0.0912312 0.273051 0.16137 0.495913 1.18746 1.19147 0.717021 0.629503 0.639953 0.529048 0.516278 0.688522 0.142692 0.344174 0.0666547 0.0594036 0.0359338 0.00218272 0.0157165 0.0020739 0.0039031 0.0123627 0.00581105 0.00530291 0.00530291
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
           -10            10      -2.79548             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_IPHC(4)
            -5             5      -1.63004             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_HBLL(5)
            -5             5      -7.82061             0             0             0        -50          0          0          0          0          0          0          0  #  LnQ_base_SYN(6)
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
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   BottomTrawl LenSelex
            -1           120       119.348             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_BottomTrawl(1)
           -10            50            50             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_BottomTrawl(1)
           -10            10       6.28328             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_BottomTrawl(1)
             0             0             0             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_BottomTrawl(1)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_BottomTrawl(1)
          -999           999            10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_BottomTrawl(1)
          -100             0      -29.1562             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_BottomTrawl(1)
           -10            10      -0.50151             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_BottomTrawl(1)
           -10            10      0.132231             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_BottomTrawl(1)
         -1009           100      -13.4394             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Final_BottomTrawl(1)
             0             1     0.0571262             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_BottomTrawl(1)
# 2   MidwaterTrawl LenSelex
            -1           120       59.2541             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_MidwaterTrawl(2)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_MidwaterTrawl(2)
           -10            10       5.25276             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_MidwaterTrawl(2)
           -10            10       5.32708             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_MidwaterTrawl(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_MidwaterTrawl(2)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_MidwaterTrawl(2)
          -100           100       23.0172             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_MidwaterTrawl(2)
           -10            10       9.96935             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_MidwaterTrawl(2)
           -10            10      -1.23973             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_MidwaterTrawl(2)
          -999            70             0             0             0             0        -50          0          0          0          0          0          0          0  #  SzSel_Male_Final_MidwaterTrawl(2)
             0             1      0.745148             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_MidwaterTrawl(2)
# 3   HookLine LenSelex
            -1           120        106.35             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_HookLine(3)
           -10            50            50             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_HookLine(3)
           -10            10       4.01196             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_HookLine(3)
             0             0             0             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_HookLine(3)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_HookLine(3)
          -999           999            10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_HookLine(3)
          -100             0      -24.4129             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_HookLine(3)
           -10            10      -9.99058             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_HookLine(3)
           -10            10        2.4981             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_HookLine(3)
         -1009           100      -6.78036             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Final_HookLine(3)
             0             1    0.00180432             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_HookLine(3)
# 4   IPHC LenSelex
            -1           120         90.69             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_IPHC(4)
           -10            50            50             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_IPHC(4)
           -10            10       6.03683             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_IPHC(4)
             0             0             0             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_descend_se_IPHC(4)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_IPHC(4)
          -999           999            10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_IPHC(4)
          -100             0       -26.323             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_IPHC(4)
           -10            10      -1.75305             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_IPHC(4)
           -10            10       1.79547             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_IPHC(4)
         -1009           100       59.3618             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Final_IPHC(4)
             0             1      0.194126             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_IPHC(4)
# 5   HBLL LenSelex
# 6   SYN LenSelex
            -1           120       70.7492             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_peak_SYN(6)
           -10            50           -10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_top_logit_SYN(6)
           -10            10         5.364             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_SYN(6)
           -10            10     -0.614548             0             0             0          3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_SYN(6)
          -999            70          -999             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_start_logit_SYN(6)
          -999           999            10             0             0             0        -50          0          0          0          0          0          0          0  #  Size_DblN_end_logit_SYN(6)
          -100           100       10.6443             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Peak_SYN(6)
           -10            10        1.0445             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Ascend_SYN(6)
           -10            10       4.46946             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Descend_SYN(6)
         -1009           100       -14.751             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Final_SYN(6)
             0             1      0.815875             0             0             0          3          0          0          0          0          0          0          0  #  SzSel_Male_Scale_SYN(6)
# 1   BottomTrawl AgeSelex
# 2   MidwaterTrawl AgeSelex
# 3   HookLine AgeSelex
# 4   IPHC AgeSelex
# 5   HBLL AgeSelex
# 6   SYN AgeSelex
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
#  1 #_lencomp:_1
#  1 #_lencomp:_2
#  1 #_lencomp:_3
#  1 #_lencomp:_4
#  0 #_lencomp:_5
#  1 #_lencomp:_6
#  1 #_init_equ_catch1
#  1 #_init_equ_catch2
#  1 #_init_equ_catch3
#  1 #_init_equ_catch4
#  1 #_init_equ_catch5
#  1 #_init_equ_catch6
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

