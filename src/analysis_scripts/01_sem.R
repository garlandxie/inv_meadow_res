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
inv_di_lm <- lmer(
  car::logit(guo_di_exo) ~ i_e_chal + treatment + (1|site), 
  data = sem_tidy
)

# plot -------------------------------------------------------------------------

## |- TRCA meeting -------------------------------------------------------------
di_vs_inv_by_trt <- sem_tidy %>%
  filter(!(sb_density == max(sb_density))) %>% 
  ggplot(aes(x = i_e_chal, y = guo_di_exo, fill = factor(treatment))) + 
  geom_point() + 
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






