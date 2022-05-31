# libraries ----
library(here)     # for creating relative file-paths 
library(ggplot2)  # for visualizing data
library(dplyr)    # for manipulating data
library(glmmTMB)  # for running regression models

# import ----

guo_di <- read.csv(
  here("data", "intermediate_data", "guo_di.csv"), 
  row.names = 1
)

guo_inv <- read.csv(
  here("data", "intermediate_data", "guo_inv_with_outliers.csv"), 
  row.names = 1
) 

guo_inv_out <- read.csv(
  here("data", "intermediate_data", "guo_inv_exclude_outliers.csv"), 
  row.names = 1
) 

# check packaging ----
dplyr::glimpse(guo_di)
dplyr::glimpse(guo_inv)
dplyr::glimpse(guo_inv_out)

# data clean ----

di_inv <- guo_di %>%
  inner_join(guo_inv, by = c("section", "site", "treatment", "plot")) %>%
  inner_join(guo_inv_out, by = c("section", "site", "treatment", "plot")) %>%
  select(
    section, site, treatment, plot,
    guo_di_exo, 
    i_e_chal, i_e_bnsh) 

# regression models ----

# compare alternative link functions 
# since the model has continuous predictors 
# see Douma et al. 2019. MEE

glm_di_inv_logit_link <- glmmTMB(
   guo_di_exo ~ i_e_chal + treatment +  (1|site), 
   family = beta_family(link = "logit"), 
   data = di_inv)

glm_di_inv_probit_link <- glmmTMB(
  guo_di_exo ~ i_e_chal + treatment +  (1|site), 
  family = beta_family(link = "probit"), 
  data = di_inv)

glm_di_inv_loglog_link <- glmmTMB(
  guo_di_exo ~ i_e_chal + treatment +  (1|site), 
  family = beta_family(link = "cloglog"), 
  data = di_inv)

AIC(
  glm_di_inv_logit_link, 
  glm_di_inv_probit_link, 
  glm_di_inv_loglog_link
  )

## diagnostics ----
glm_exo_sim <- simulateResiduals(fittedModel = glm_di_inv_exo)
plot(glm_exo_sim)
performance::check_outliers(glm_di_inv_exo, method = "cook")

# main plot ----

# back-transform regression coefficients 
# from beta regression model
# so that I can plot the slope with the raw data

# controlling for management regime
effects_ie <- as.data.frame(
   effects::effect(
   term= "i_e_chal", 
   mod = glm_di_inv_exo
   )
)

(di_exo_inv_plot <- ggplot() + 
   
    geom_point(
       data = di_inv,
       aes(
          x = i_e_chal, 
          y = guo_di_exo, 
          col = treatment 
       )
    ) + 
      
   geom_line(
      data = effects_ie, 
      aes(
         x = i_e_chal, 
         y = fit, 
      ),
      col = "black"
   ) + 

   geom_ribbon(
      data = effects_ie, 
      aes(x = i_e_chal, ymin = lower, ymax = upper), 
      alpha = 0.2, 
      ) +
      
    labs(
       x = "Unified Metric of Invasibility",
       y = "Degree of Invasion") + 
    xlim(0.1, 0.9) + 
    ylim(0.1, 0.9) + 
    scale_color_discrete(
       name = "Management Regime", 
       labels = c("Undisturbed", "Tilling")
    ) + 
 
    theme_bw()
)

# save to disk ----

ggsave(
   filename = here("output", "results", "di_vs_inv.png"),
   plot = di_exo_inv_plot, 
   device = "png", 
   units = "in", 
   height = 5, 
   width = 6
)
