# libraries ----
library(here)
library(piecewiseSEM)
library(dplyr)
library(lme4)
library(DHARMa)

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
    treatment = case_when(
      treatment == "RES" ~ 0, 
      treatment == "TIL" ~ 1), 
    treatment = as.numeric(treatment),
    site = factor(site) 
    )

# fit individual regression models ---

## |- seed bank ----
lm_sb <- glmer(
  sb_density ~ treatment + litter_mass_g + (1|site),
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
rsquared(lm_sb)
rsquared(lm_litter)
rsquared(lm_sr)
rsquared(lm_ie)

## influential outliers ----
check_outliers(lm_ie)

## overdispersion ----
lm_sb_sim <- simulateResiduals(fittedModel = lm_sb)
testDispersion(lm_sb_sim, type = "DHARMa")

lm_ie_sim <- simulateResiduals(fittedModel = lm_ie)
testDispersion(lm_ie_sim, type = "DHARMa")


# fit SEM ----
sem <- psem(lm_sb,lm_litter, lm_sr, lm_ie)
summary(sem_unst)
rsquared(sem_unst)
