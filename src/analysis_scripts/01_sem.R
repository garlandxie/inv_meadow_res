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

# sem model 1: simple ----------------------------------------------------------

## |- management regime -> seed bank -------------------------------------------

# specify model 
mgt_sb_lm1 <- glmer(
  sb_density ~ treatment + (1|site), 
  family = poisson(link = "log"),
  data = sem_tidy_no_out
  )

# check model diagnostics
sim_mgt_sb <- DHARMa::simulateResiduals(fittedModel = mgt_sb_lm, plot = F)
plot(sim_mgt_sb)

# check model fit using Nakagawa's marginal and conditional R2
piecewiseSEM::rsquared(mgt_sb_lm)

# check model output
summary(mgt_sb_lm)

## |- seed bank -> invasibility ------------------------------------------------

# fit model
sb_inv_lm1 <- lmer(
  car::logit(i_e_chal) ~ sb_density + treatment + (1|site), 
  data = sem_tidy_no_out
)

# check model diagnostics
sim_inv_lm1 <- DHARMa::simulateResiduals(fittedModel = sb_inv_lm, plot = F)
plot(sim_inv_lm)

# check model fit using Nakagawa's marginal and conditional R2
piecewiseSEM::rsquared(sb_inv_lm)

## |- invasibility -> degree of invasion ---------------------------------------

# fit model
inv_di_lm1 <- lmer(
  car::logit(guo_di_exo) ~ i_e_chal + treatment + seed_rain_dsv + (1|site), 
  data = sem_tidy_no_out
)

# check model diagnostics
sim_inv_di<- DHARMa::simulateResiduals(fittedModel = inv_di_lm, plot = F)
plot(sim_inv_di)

# check model fit using Nakagawa's marginal and conditional R2
piecewiseSEM::rsquared(inv_di_lm)

## |- management regime -> seed rain DSV ---------------------------------------

# fit model 
mgt_sr_lm1 <- lmer(
  seed_rain_dsv ~ treatment + (1|site), 
  data = sem_tidy_no_out
)

# check model diagnostics
sim_mgt_sr <- simulateResiduals(fittedModel = mgt_sr_lm, plot = F)
plot(sim_mgt_sr)

# check model fit
piecewiseSEM::rsquared(mgt_sr_lm)

# check model output
summary(mgt_sr_lm)

## |- run sem ----------------------------------------------------------------------

# run piecewise structural equation modelling for simple version
sem_1 <- piecewiseSEM::psem(
  inv_di_lm1, 
  mgt_sb_lm1,
  mgt_sr_lm1, 
  sb_inv_lm1)

# check for any important missing pathways
d_seps <- piecewiseSEM::dSep(sem_1, conserve = TRUE)

# check Fischer's C statistics
# alternative model fits using log-likelihoods
piecewiseSEM::fisherC(sem_1, conserve = TRUE)

# grab unstandardized coefficients
coefs(modelList = sem_1, standardize = "none")

# grab standardized coefficients
# use observation-empirical approach to account for non-normal distributions
# un the response variable 
coefs(modelList = sem_1, standardize = "scale", standardize.type = "Menard.OE")

# sem model 2: complex ---------------------------------------------------------

## |- management regime -> community biomass -----------------------------------
# specify model 
comm_biomass_lm2 <- lmer(
  comm_biomass_g ~ treatment + (1|site), 
  data = sem_tidy
)

# check model diagnostics
sim_comm_biomass <- simulateResiduals(fittedModel = comm_biomass_lm2, plot = F)
plot(sim_comm_biomass)

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
sim_litter_bm <- simulateResiduals(fittedModel = litter_lm2, plot = F)
plot(sim_litter_bm)

# check model fit
piecewiseSEM::rsquared(litter_lm2)

# check model output
summary(litter_lm2)

## |- litter biomass -> seed bank density --------------------------------------

# specify model 
sb_lm2 <- glmer(
   sb_density ~ scale(litter_mass_g) + (1|site), 
   family = "poisson",
  data = sem_tidy
)

# check model diagnostics
sim_sb <- simulateResiduals(fittedModel = sb_lm2, plot = F)
plot(sim_sb)

# check model fit
piecewiseSEM::rsquared(sb_lm2)

# check model output
summary(sb_lm2)

## |- seed bank density + seed bank richness -> degree of invasion ----------------

# specify model 
di_lm2 <- lmer(
s  data = sem_tidy
)

# check model diagnostics
sim_di_lm2 <- simulateResiduals(fittedModel = di_lm2, plot = F)
plot(sim_di_lm2)

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
sim_comm_rich <- simulateResiduals(fittedModel = comm_rich_lm2, plot = F)
plot(sim_comm_rich)

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
sim_sr_lm2 <- simulateResiduals(fittedModel = sr_lm2, plot = F)
plot(sim_sr_lm2)

# check model fit
piecewiseSEM::rsquared(sr_lm2)

## |- run sem ------------------------------------------------------------------

sem_2 <- piecewiseSEM::psem(
  di_lm2, 
  litter_lm2, 
  sb_lm2, 
  comm_rich_lm2, 
  sr_lm2, 
  comm_biomass_lm2 
)

# check for any important missing pathways
d_seps <- piecewiseSEM::dSep(sem_2, conserve = TRUE)
piecewiseSEM::fisherC(sem_2, conserve = TRUE)

# plot -------------------------------------------------------------------------

## |- TRCA meeting -------------------------------------------------------------
sem_tidy %>%
  filter(!(sb_density == max(sb_density))) %>% 
  ggplot(aes(x = i_e_chal, y = guo_di_exo, fill = factor(treatment))) + 
  geom_point(aes(col = factor(site))) + 
  geom_smooth(method = "lm", color = "black") + 
  scale_fill_discrete(
    name = "Management Regime", 
    labels = c("Section 4 (Restored)", "Section 2 (Tilled)")
  ) + 
  xlim(0, 1) + 
  ylim(0, 1) + 
  labs(
    x = "Susceptibility to Plant Invasion",
    y = "Extent of Invasion") + 
  theme_bw()

sem_tidy %>%
  filter(!(sb_density == max(sb_density))) %>% 
  ggplot(aes(x = sb_density, y = i_e_chal, fill = factor(treatment))) + 
  geom_point(aes(color = factor(treatment))) + 
  geom_smooth(method = "lm", color = "black") + 
  scale_fill_discrete(
    name = "Management Regime", 
    labels = c("Section 4 (Restored)", "Section 2 (Tilled)")  
  ) +
  theme_bw()

# save to disk -----------------------------------------------------------------

ggsave(
  plot = di_vs_inv_by_trt,
  file = here("output", "results", "di_vs_inv.pdf"),
  device = "pdf", 
  units = "in", 
  height = 5, 
  width = 7
)






