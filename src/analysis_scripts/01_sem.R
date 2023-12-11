################################################################################
# Accompanying code for the following research project: 
#   Drivers of invasibility in urban meadow restoration
#
#
# Corresponding authors for this script:  
#   Garland Xie      (1)
#
# Affiliations: 
#   (1) Department of Biological Sciences, 
#       University of Toronto Scarborough,
#       1265 Military Trail, Toronto, ON, M1C 1A4, Canada
#       email: garland.xie@mail.utoronto.ca, 
#              nicholas.sookhan@mail.utoronto.ca
#              scott.macivor@mail.utoronto.ca
#
# Purpose of this R script: to run the structural equation models, including
# diagnostics 

#
# IMPORTANT: Please refresh your R session before you run this script
# Why? See https://rstats.wtf/save-source.html

# libraries --------------------------------------------------------------------
library(here)          # for creating relative file-paths
library(piecewiseSEM)  # for doing piece-wise structural equation modelling
library(dplyr)         # for manipulating data
library(lme4)          # for doing mixed effects models 
library(DHARMa)        # for running diagnostic tests on mixed effects models
library(lmerTest)      # for getting p-values for the mixed effect models
library(ggplot2)       # for visualizing datasets

# import -----------------------------------------------------------------------
sem_df <- read.csv(
  here("data", "analysis_data", "sem.csv"), 
  row.names = 1
)

# check packaging --------------------------------------------------------------
str(sem_df)
head(sem_df, n = 5)
tail(sem_df, n = 5)

# clean data -------------------------------------------------------------------

sem_tidy <- sem_df %>%
  mutate(
    treatment = case_when(
      treatment == "RES" ~ 0, 
      treatment == "TIL" ~ 1), 
    treatment = as.numeric(treatment),
    site = factor(site) 
    )

# remove an outlier for seed bank density
# this is likely plot with a seed bank with a high abundance of common mullein 
sem_tidy_no_out <- dplyr::filter(sem_tidy, !(sb_density == max(sb_density))) 

# apply a logit transformation on invasibility and degree of invasion
# this is done to accommodate the proportional data type
# since piecewise R package (v.2.3.0) does provide standardized coefficients
# under a beta distribution when using glmmTMB 

sem_tidy_no_out <- sem_tidy_no_out %>%
  mutate(
    logit_ie = car::logit(i_e_chal), 
    logit_di = car::logit(guo_di_exo)
  )

sem_tidy <- sem_tidy %>%
  mutate(
    logit_ie = car::logit(i_e_chal), 
    logit_di = car::logit(guo_di_exo),
    log_sb_density = log(sb_density)
  )

# original: sem model 1 --------------------------------------------------------

## |- seed bank <- restoration stage -------------------------------------------

mgt_sb_lm1 <- glmer(
  sb_density ~ treatment + (1|site), 
  family = poisson(link = "log"),
  data = sem_tidy_no_out
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = mgt_sb_lm1, plot = T) 

# check model fit using Nakagawa's marginal and conditional R2
piecewiseSEM::rsquared(mgt_sb_lm1_rev)

# check model output
summary(mgt_sb_lm1_rev) 

## |- invasibility <- seed bank density + restoration stage --------------------

# fit model
sb_inv_lm1 <- lmer(
  logit_ie ~ sb_density + treatment + (1|site), 
  data = sem_tidy_no_out
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = sb_inv_lm1, plot = TRUE)

# check model fit using Nakagawa's marginal and conditional R2
piecewiseSEM::rsquared(sb_inv_lm1)

## |- degree of invasion <- invasibility + seed rain of DSV --------------------

# fit model
inv_di_lm1 <- lmer(
  logit_di ~ logit_ie + seed_rain_dsv + (1|site), 
  data =  sem_tidy_no_out
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = inv_di_lm1, plot = TRUE)

# check model fit using Nakagawa's marginal and conditional R2
piecewiseSEM::rsquared(inv_di_lm1)

## |- seed rain of dsv <- restoration stage ------------------------------------

# fit model 
mgt_sr_lm1 <- lmer(
  seed_rain_dsv ~ treatment + (1|site), 
  data = sem_tidy_no_out
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = mgt_sr_lm1, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(mgt_sr_lm1)

# check model output
summary(mgt_sr_lm1)

## |- run sem ----------------------------------------------------------------------

# run piecewise structural equation modelling for simple version
sem_1 <- piecewiseSEM::psem(
  inv_di_lm1, 
  mgt_sb_lm1,
  mgt_sr_lm1, 
  sb_inv_lm1
  )

summary(sem_1, conserve = TRUE)

# check for any important missing pathways
piecewiseSEM::dSep(sem_1, conserve = TRUE)

# check Fischer's C statistics
# alternative model fits using log-likelihoods
piecewiseSEM::fisherC(sem_1, conserve = TRUE)

# grab unstandardized coefficients
coefs(modelList = sem_1, standardize = "none")

# grab standardized coefficients
# use observation-empirical approach to account for non-normal distributions
# un the response variable 
coefs(modelList = sem_1, standardize = "scale", standardize.type = "Menard.OE")

# revised: sem model 1 ---------------------------------------------------------

## |- seed bank <- restoration stage + seed rain of DSV ------------------------

# specify model 
# note: I added seed_rain_dsv based on a previous d-sep test that suggested
# this pathway may be missing from the first run; biologically, this makes
# sense since the seed rain of DSV can easily contribute to the seed banks
mgt_sb_lm1_rev <- glmer(
  sb_density ~ treatment + seed_rain_dsv + (1|site), 
  family = poisson(link = "log"),
  data = sem_tidy_no_out
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = mgt_sb_lm1_rev, plot = T) 

# check model fit using Nakagawa's marginal and conditional R2
piecewiseSEM::rsquared(mgt_sb_lm1_rev)

# check model output
summary(mgt_sb_lm1_rev)

## |- run revised sem model ----------------------------------------------------

sem_1_rev <- piecewiseSEM::psem(
  inv_di_lm1, 
  mgt_sb_lm1_rev,
  mgt_sr_lm1, 
  sb_inv_lm1
)

summary(sem_1_rev, conserve = TRUE)

# check for any important missing pathways
piecewiseSEM::dSep(sem_1_rev, conserve = TRUE)

# check Fischer's C statistics
# alternative model fits using log-likelihoods
piecewiseSEM::fisherC(sem_1_rev, conserve = TRUE)

# grab unstandardized coefficients
coefs(modelList = sem_1_rev, standardize = "none")

# grab standardized coefficients
# use observation-empirical approach to account for non-normal distributions
# un the response variable 
coefs(modelList = sem_1_rev, standardize = "scale", standardize.type = "Menard.OE")

# original: sem model 2 ------------------------------------------------------------------

## |- management regime -> community biomass -----------------------------------
# specify model 
comm_biomass_lm2 <- lmer(
  comm_biomass_g ~ treatment + (1|site), 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = comm_biomass_lm2, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(comm_biomass_lm2)

# check model output
summary(comm_biomass_lm2)

## |- community biomass -> litter biomass --------------------------------------

# specify model 
litter_lm2 <- lmer(
  litter_mass_g ~ comm_biomass_g + (1|site), 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = litter_lm2, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(litter_lm2)

# check model output
summary(litter_lm2)

## |- litter biomass -> seed bank density --------------------------------------

# specify model 
sb_lm2 <- lmer(
   log_sb_density ~ litter_mass_g + (1|site), 
   data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = sb_lm2, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(sb_lm2)

# check model output
summary(sb_lm2)

## |- di <- seed bank density + seed bank richness -----------------------------

# specify model 
di_lm2 <- lmer(
  logit_di ~ sb_richness + log_sb_density + (1|site), 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = di_lm2, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(di_lm2)

# check model output
summary(di_lm2)

## |- management regime -> community richness ----------------------------------

# specify model 
comm_rich_lm2 <- glmer(
  species_richness ~ treatment + (1|site), 
  family = "poisson", 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = comm_rich_lm2, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(comm_rich_lm2)

# check model output
summary(comm_rich_lm2)

## |- species richness -> seed bank richness -----------------------------------

# specify model 
sr_lm2 <- glmer(
  sb_richness ~ species_richness + (1|site), 
  family = "poisson", 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = sr_lm2, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(sr_lm2)

## |- run original sem model 1 -------------------------------------------------

sem_2 <- piecewiseSEM::psem(
  di_lm2, 
  litter_lm2, 
  sb_lm2, 
  comm_rich_lm2, 
  sr_lm2, 
  comm_biomass_lm2 
)

# check for any important missing pathways
piecewiseSEM::dSep(sem_2, conserve = TRUE)
piecewiseSEM::fisherC(sem_2, conserve = TRUE)

coefs(modelList = sem_2, standardize = "scale", standardize.type = "Menard.OE")

# revised: sem model 2 ---------------------------------------------------------

## |- community biomass -> litter biomass + restoration stage ------------------

# specify model 
# added treatment based on a previous d-separation test
litter_lm2_rev <- lmer(
  litter_mass_g ~ comm_biomass_g + treatment + (1|site), 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = litter_lm2_rev, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(litter_lm2_rev)

# check model output
summary(litter_lm2_rev)

## |- seed bank density <- litter + seed bank richness -------------------------

# specify model 
# added sb_richness based on a previous d-tests of separation
sb_lm2_rev <- lmer(
  log_sb_density ~ litter_mass_g + sb_richness + (1|site), 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = sb_lm2_rev, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(sb_lm2_rev)

# check model output
summary(sb_lm2_rev)

## |- di -> seed bank richness + seed bank density + restoration stage ---------

# specify model 
# added treatment from a previous d-tests of separation
di_lm2_rev <- lmer(
  logit_di ~ sb_richness + log_sb_density + treatment + (1|site), 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = di_lm2_rev, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(di_lm2_rev)

# check model output
summary(di_lm2_rev)

## |- species richness <- seed bank richness + restoration stage ---------------

# specify model 
# added treatment based on a previous d-tests of separation
sr_lm2_rev <- glmer(
  sb_richness ~ species_richness + treatment + (1|site), 
  family = "poisson", 
  data = sem_tidy
)

# check model diagnostics
DHARMa::simulateResiduals(fittedModel = sr_lm2_rev, plot = TRUE)

# check model fit
piecewiseSEM::rsquared(sr_lm2_rev)

## |- run revised sem model 2 --------------------------------------------------

sem_2_rev <- piecewiseSEM::psem(
  di_lm2_rev, 
  litter_lm2_rev, 
  sb_lm2_rev, 
  comm_rich_lm2, 
  sr_lm2_rev, 
  comm_biomass_lm2 
)

# save to disk -----------------------------------------------------------------

ggsave(
  plot = di_vs_inv_by_trt,
  file = here("output", "results", "di_vs_inv.pdf"),
  device = "pdf", 
  units = "in", 
  height = 5, 
  width = 7
)