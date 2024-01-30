
library(tidyverse)
library(readxl)
library(gfplot)

#length is tail extended in cm

#data from NW US survey in 2010, from Ian and Vladlena at NWFSC
#see paper Taylor et al Spine based ageing
datnwus <- readxl::read_excel("data/raw/NWFSC_Dogfish_2021_age_data_foranalysis.xlsx")|>
  dplyr::select(Age1, SampleYear, Sex, NaturalLength, Age_Ketchen, Age_Exponential) |>
  drop_na(Age_Exponential, Age_Ketchen, NaturalLength,SampleYear, Sex) |>
  filter(Sex %in% c(1,2)) |>
  mutate(length = 1.02 * NaturalLength, # convert to TL extended (see Tribuzio and Gerseva for refs)
         sex = ifelse(Sex == 1, "Male", "Female")) |>
  rename(Age = Age1)

ggplot(datnwus, aes(Age, length)) + facet_wrap(~sex, scales = "free") + geom_smooth() + geom_point()
ggplot(datnwus, aes(Age_Exponential, length)) + facet_wrap(~sex, scales = "free") + geom_smooth() + geom_point()
ggplot(datnwus, aes(Age_Ketchen, length)) + facet_wrap(~sex, scales = "free") + geom_smooth() + geom_point()

#data from Jackie/DFO
#these were aged using the Ketchen method
dat <- readxl::read_excel("data/raw/Age.Length.Data.xlsx") %>%
  filter(!is.na(Age),
         sex != 7,
         !is.na(length)) %>%
  mutate(Area = ifelse(Area == "4A", "4B", Area),
         length = 0.1 * length, # convert to cm
         sex = ifelse(sex == 1, "Male", "Female"))

ggplot(dat, aes(Age, length)) + facet_wrap(~sex, scales = "free") + geom_smooth() + geom_point()


# combine to visualise ----------------------------------------------------
dat2 <- dat |> dplyr::select(sex, Age, length, Area) |> mutate(type = "DFO")
datnwus2 <- datnwus |> dplyr::select(sex, Age, length) |> mutate(type = "nwus1")
datnwus3 <- datnwus |> dplyr::select(sex, Age_Ketchen, length) |> rename (Age =  Age_Ketchen) |> mutate(type = "nwus_Ketchen")
datnwus4 <- datnwus |> dplyr::select(sex, Age_Exponential , length) |> rename (Age =  Age_Exponential ) |> mutate(type = "nwus_exponential")
datc <- rbind(datnwus2, datnwus3, datnwus4)
datc <- rbind(dat2, datnwus3)

ggplot(datc, aes(Age, length, colour = type)) +
  facet_wrap(~sex, scales = "free") + geom_smooth() + geom_point(alpha = 0.5)

# Combine to fit model
dat_all <- rbind(
  dat %>%
    rename(Year = `Sample year`) %>%
    dplyr::select(Year, sex, Age, length, Area) %>%
    mutate(type = paste("DFO -", Area)) %>% select(-Area),
  datnwus |>
    mutate(Year = 2010) %>%
    dplyr::select(Year, sex, Age_Exponential , length) |> rename (Age =  Age_Exponential ) |> mutate(type = "NWFSC")
)

#### Plot data (DFO + NWFSC)
# By Area
g <- ggplot(dat_all, aes(Age, length, colour = sex)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = c(80, 95), linetype = 2, colour = "grey60") +
  gfplot::theme_pbs() +
  facet_wrap(vars(type)) +
  expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE) +
  theme(panel.spacing = unit(0, "in")) +
  labs(x = "Age", y = "Length (cm)", colour = "Sex")
ggsave("figs/length-age-area.png", g, height = 4, width = 6)

# By Year
g <- ggplot(dat_all, aes(Age, length, colour = sex)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = c(80, 95), linetype = 2, colour = "grey60") +
  gfplot::theme_pbs() +
  facet_wrap(vars(Year)) +
  expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE) +
  labs(x = "Age", y = "Length (cm)", colour = "Sex")
ggsave("figs/length-age-year.png", g, height = 4, width = 6)

# Samples by area and year
dat_all %>%
  summarise(n = n(), .by = c(Year, type)) %>%
  reshape2::acast(list("Year", "type"), fill = 0)

#### Fit growth models

# Model 1 - All DFO samples
vb_f <- dat_all %>%
  filter(sex == "Female", grepl("DFO", type)) %>%
  mutate(sex = 2,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "female",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

vb_m <- dat_all %>%
  filter(sex == "Male", grepl("DFO", type)) %>%
  mutate(sex = 1,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "male",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

g <- gfplot::plot_growth(object_f = vb_f,
                         object_m = vb_m,
                         lab_x = 0.4,
                         lab_x_gap = 0.35,
                         pt_alpha = 0.8,
                         col = c(Female = "black", Male = "black"),
                         french = FALSE,
                         jitter = TRUE) +
  facet_wrap(vars(sex)) +
  guides(col = "none", lty = "none") +
  theme(panel.spacing = unit(0, "in")) +
  ggtitle("DFO samples")
ggsave("figs/length-age-vb-dfo.png", g, height = 3, width = 6)


# Model 2 - DFO + NWFSC samples
vb_f <- dat_all %>%
  filter(sex == "Female") %>%
  mutate(sex = 2,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "female",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

vb_m <- dat_all %>%
  filter(sex == "Male") %>%
  mutate(sex = 1,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "male",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

g <- gfplot::plot_growth(object_f = vb_f,
                         object_m = vb_m,
                         lab_x = 0.4,
                         lab_x_gap = 0.35,
                         pt_alpha = 0.8,
                         col = c(Female = "black", Male = "black"),
                         french = FALSE,
                         jitter = TRUE) +
  facet_wrap(vars(sex)) +
  guides(col = "none", lty = "none") +
  theme(panel.spacing = unit(0, "in")) +
  ggtitle("DFO + NWFSC samples")
ggsave("figs/length-age-vb-dfo-nwfsc.png", g, height = 3, width = 6)

# Model 3 - DFO + NWFSC samples - ex pregnant female
vb_f <- dat_all %>%
  filter(sex == "Female") %>%
  filter(ifelse(grepl("DFO", type), ifelse(grepl("4B", type), TRUE, length < 95),
                length < 80)) %>%
  mutate(sex = 2,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "female",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

vb_m <- dat_all %>%
  filter(sex == "Male") %>%
  mutate(sex = 1,
         specimen_id = 1:n()) %>%
  rename(age = Age) %>%
  gfplot::fit_vb(usability_codes = NULL,
                 sex = "male",
                 tmb_inits = list(k = 0.1, linf = 100, log_sigma = log(0.2), t0 = -5))

g <- gfplot::plot_growth(object_f = vb_f,
                         object_m = vb_m,
                         lab_x = 0.4,
                         lab_x_gap = 0.35,
                         pt_alpha = 0.8,
                         col = c(Female = "black", Male = "black"),
                         french = FALSE,
                         jitter = TRUE) +
  facet_wrap(vars(sex)) +
  guides(col = "none", lty = "none") +
  theme(panel.spacing = unit(0, "in")) +
  ggtitle("DFO + NWFSC + exclude large females")
ggsave("figs/length-age-vb-dfo-nwfsc-ex-large-female.png", g, height = 3, width = 6)



#### Plot residual by area
#dat <- dat %>%
#  mutate(linf = ifelse(sex == "Female", 97.4, 83.7),
#         k = ifelse(sex == "Female", 0.05, 0.08),
#         t0 = ifelse(sex == "Female", -10.63, -11.07),
#         sigma = ifelse(sex == "Female", exp(-1.693), exp(-1.914)),
#         pred = linf * (1 - exp(-k * (Age - t0))) * exp(-0.5 * sigma * sigma),
#         resid = log(length/pred))
#g <- dat %>%
#  ggplot(aes(Age, resid, colour = sex)) +
#  geom_point(alpha = 0.4) +
#  gfplot::theme_pbs() +
#  facet_wrap(vars(Area)) +
#  geom_hline(yintercept = 0, linetype = 2) +
#  theme(panel.spacing = unit(0, "in")) +
#  coord_cartesian(ylim = c(-1, 1), expand = FALSE) +
#  labs(x = "Age", y = "Residual", colour = "Sex")
#ggsave("figs/length-age-residual-area.png", g, height = 5, width = 6)
#
#g <- dat %>%
#  ggplot(aes(Age, resid, colour = Area)) +
#  geom_jitter(alpha = 0.4) +
#  gfplot::theme_pbs() +
#  facet_wrap(vars(sex)) +
#  geom_hline(yintercept = 0, linetype = 2) +
#  theme(panel.spacing = unit(0, "in")) +
#  coord_cartesian(ylim = c(-1, 1), expand = FALSE) +
#  labs(x = "Age", y = "Residual", colour = NULL)
#ggsave("figs/length-age-residual.png", g, height = 3, width = 6)


###### Tried Schnute function but AIC does not support additional inflection parameter
#fit_growth <- function(dat, a1 = 0, a2 = max(dat$Age), p) {
#  require(TMB)
#  if (!file.exists(TMB::dynlib("tmb/schnute"))) TMB::compile("tmb/schnute.cpp")
#  dyn.load(TMB::dynlib("tmb/schnute"))
#  on.exit(dyn.unload(TMB::dynlib("tmb/schnute")))
#
#  map <- list()
#  if (!missing(p)) {
#    map$p <- factor(NA)
#  } else {
#    p <- 0.5
#  }
#
#  lapply(unique(dat$sex), function(s) {
#
#    dat_sex <- filter(dat, sex == s)
#    TMB_data <- list(len = dat_sex$length,
#                     age = dat_sex$Age,
#                     a1 = a1,
#                     a2 = a2)
#    TMB_parameters <- list(k = 0.1,
#                           L1 = 35,
#                           L2 = 100,
#                           log_sigma = log(0.2),
#                           p = p)
#
#    obj <- TMB::MakeADFun(data = TMB_data,
#                          parameters = TMB_parameters,
#                          map = map,
#                          DLL = "schnute",
#                          silent = TRUE)
#
#    opt <- stats::nlminb(obj$par, obj$fn, obj$gr)
#
#    SD <- TMB::sdreport(obj)
#
#    pred <- obj$report(obj$env$last.par.best)$eta
#    resid <- obj$report(obj$env$last.par.best)$resid
#
#    LAA <- list(len = seq(0, a2),
#                age = seq(0, a2),
#                a1 = a1,
#                a2 = a2)
#
#    obj2 <- TMB::MakeADFun(data = LAA,
#                           parameters = as.list(SD, what = "Estimate"),
#                           map = map,
#                           DLL = "schnute",
#                           silent = TRUE)
#
#    list(obj = obj, opt = opt, SD = SD, LAA = data.frame(Age = LAA$age, length = obj2$report()$eta))
#  }) %>%
#    structure(names = unique(dat$sex))
#}
#
#
#schnute_pars <- fit_growth(dat)
#vb_pars <- fit_growth(dat, p = 1)
#
#get_LAA <- function(pars) {
#  lapply(names(pars), function(x) {
#    pars[[x]]$LAA %>% mutate(sex = x)
#  }) %>% bind_rows()
#}
#
#get_AIC <- function(pars) {
#  lapply(names(pars), function(x) {
#    2 * (pars[[x]]$opt$objective + length(pars[[x]]$opt$par))
#  })
#}
#
## Schnute function not much better, if at all
#get_AIC(schnute_pars)
#get_AIC(vb_pars)

# Single figure
#g <- ggplot(dat, aes(Age, length)) +
#  geom_point(alpha = 0.25, aes(colour = sex)) +
#  geom_line(data = get_LAA(schnute_pars)) +
#  gfplot::theme_pbs() +
#  facet_wrap(vars(sex)) +
#  expand_limits(y = 0) +
#  labs(x = "Age", y = "Length (cm)", colour = "Sex")
#ggsave("length-age-schnute.png", g, height = 3, width = 6)
#
## Single figure
#g <- ggplot(dat, aes(Age, length)) +
#  geom_point(alpha = 0.25, aes(colour = sex)) +
#  geom_line(data = get_LAA(vb_pars)) +
#  gfplot::theme_pbs() +
#  facet_wrap(vars(sex)) +
#  expand_limits(y = 0) +
#  labs(x = "Age", y = "Length (cm)", colour = "Sex")
#ggsave("length-age-vb.png", g, height = 3, width = 6)
