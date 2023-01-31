# libraries --------------------------------------------------------------------
library(here)          # for creating relative file-paths
library(piecewiseSEM)  # for doing piece-wise structural equation modelling
library(dplyr)         # for manipulating data
library(lme4)          # for doing mixed effects models 
library(DHARMa)        # for running diagnostic tests on mixed effects models
library(partR2)        # for calculating part R2 for an individual predictor

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

# sem model 1: simple ----------------------------------------------------------

## |- management regime -> seed bank -------------------------------------------

# fit model 
mgt_sb_lm <- glmer(
  sb_density ~ treatment + (1|site), 
  family = poisson(link = "log"),
  data = sem_tidy
  )

## |- seed bank -> invasibility ------------------------------------------------

# fit model
sb_inv_lm <- glmer(
  i_e_chal ~ scale(sb_density) + (1|site), 
  family = binomial(link = "log"),
  data = sem_tidy
)

## |- invasibility -> degree of invasion ---------------------------------------

# fit model
sb_inv_lm <- glmer(
  d ~ scale(sb_density) + (1|site), 
  family = binomial(link = "log"),
  data = sem_tidy
)

