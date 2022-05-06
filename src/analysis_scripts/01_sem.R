# libraries ----
library(here)          # for creating relative file-paths
library(piecewiseSEM)  # for doing piece-wise structural equation modelling
library(dplyr)         # for manipulating data
library(lme4)          # for doing mixed effects models 
library(DHARMa)        # for running diagnostic tests on mixed effects models

# import ----
sem_df <- read.csv(
  here("data", "analysis_data", "sem.csv"), 
  row.names = 1
)

# check packaging ----
str(sem_df)
head(sem_df, n = 5)
tail(sem_df, n = 5)

# clean data ----

sem_tidy <- sem_df %>%
  mutate(
    scale_litter_mass_g = scale(litter_mass_g),
    treatment = case_when(
      treatment == "RES" ~ 0, 
      treatment == "TIL" ~ 1), 
    treatment = as.numeric(treatment),
    site = factor(site) 
    )

# fit individual regression models ----

## |- seed bank ----

# standardize litter mass due to convergence issues
# original model (w/o standardizing) => unidentified => large eigenvalue ratio
lm_sb <- glmer(
  sb_density ~ treatment + scale_litter_mass_g + (1|site),
  family = "poisson",
  data = sem_tidy
)

## |- litter ----
lm_litter <- lmer(
  litter_mass_g ~ treatment + (1|site), 
  data = sem_tidy
)

## |- seed rain ----
lm_sr <- lmer(
  seed_rain_dsv ~ treatment + (1|site), 
  data = sem_tidy, 
)

## |- invasibility -----

lm_ie <- glmer(
  i_e_chal ~ 
    
    # fixed effects
    sb_density + 
    seed_rain_dsv + 
    treatment + 
    
    # random effects
    (1|site), 
  
    family = "binomial",
  
  data = sem_tidy
)

# check diagnostics ----

## explanatory power -----

# Nakagawa's marginal and conditional r-squared values
# marginal => variance explained by fixed effects
# conditional => variance explained by random and fixed effects
# observation-level variance derived by either delta or trigamma approximation 
rsquared(lm_sb)
rsquared(lm_litter)
rsquared(lm_sr)
rsquared(lm_ie)

## influential outliers ----
performance::check_outliers(lm_sb)
performance::check_outliers(lm_litter)
performance::check_outliers(lm_sr)
performance::check_outliers(lm_ie)

## diagnostic plots ----

lm_sb_sim <- simulateResiduals(fittedModel = lm_sb)
lm_li_sim <- simulateResiduals(fittedModel = lm_litter)
lm_sr_sim <- simulateResiduals(fittedModel = lm_sr)
lm_ie_sim <- simulateResiduals(fittedModel = lm_ie)

plot(lm_sb_sim)
plot(lm_li_sim)
plot(lm_sr_sim)
plot(lm_ie_sim)

## overdispersion ----

# for generalized linear mixed effects models
testDispersion(lm_sb_sim, type = "DHARMa")
testDispersion(lm_ie_sim, type = "DHARMa")

# fit SEM ----
sem <- psem(lm_sb,lm_litter, lm_sr, lm_ie)
summary(sem)
