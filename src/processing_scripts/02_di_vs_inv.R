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

# plots: treatment effect on exotic species ----

## |- includes extreme outliers ----
(di_exo_inv_plot <- di_inv %>%
  ggplot(
    aes(
      x = i_e_chal, 
      y = guo_di_exo, 
      col = treatment 
      )
    ) +
  geom_smooth(method = "lm") + 
  geom_point(aes(shape = site)) + 
  labs(
    title = "Includes extreme outliers from maximum biomass", 
    x = "Unified Metric of Invasibility",
    y = "Degree of Invasion (Exotic Species)") + 
  xlim(0, 1) + 
  ylim(0, 1) + 
  scale_color_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  scale_shape_discrete(
    name = "Site"
  ) + 
  theme_bw()
)

## |- excludes extreme outliers ----

(di_exo_inv_out_plot <- di_inv %>%
   ggplot(
     aes(
       x = i_e_bnsh, 
       y = guo_di_exo, 
       col = treatment 
     )
   ) +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_point(aes(shape = site)) + 
   labs(
     title = "Excludes extreme outliers from maximum biomass", 
     x = "Unified Metric of Invasibility",
     y = "Degree of Invasion") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Tilling")
   ) + 
   scale_shape_discrete(
     name = "Site"
   ) + 
   theme_bw()
)

# plots: treatment effect on invasives species ----

## |- includes extreme outliers ----
(di_inv_plot <- di_inv %>%
   ggplot(
     aes(
       x = i_e_chal, 
       y = guo_di_inv, 
       col = treatment 
     )
   ) +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_point(aes(shape = site)) + 
   labs(
     title = "Includes extreme outliers from maximum biomass", 
     x = "Unified Metric of Invasibility",
     y = "Degree of Invasion (Invasive Species)") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Tilling")
   ) + 
   scale_shape_discrete(
     name = "Site"
   ) + 
   theme_bw()
)

## |- excludes extreme outliers ----

(di_inv_out_plot <- di_inv %>%
   ggplot(
     aes(
       x = i_e_bnsh, 
       y = guo_di_inv, 
       col = treatment 
     )
   ) +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_point(aes(shape = site)) + 
   labs(
     title = "Excludes extreme outliers from maximum biomass", 
     x = "Unified Metric of Invasibility",
     y = "Degree of Invasion (Invasive Species)") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Tilling")
   ) + 
   scale_shape_discrete(
     name = "Site"
   ) + 
   theme_bw()
)

# plots: site effect on exotic species -----

## |- includes extreme outliers ----
(di_exo_inv_site_chal <- di_inv %>%
   ggplot(
     aes(
       x = i_e_chal, 
       y = guo_di_exo, 
       col = treatment
     )
   ) +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_point() + 
   labs(
     title = "Includes extreme outliers from maximum biomass", 
     x = "Unified Metric of Invasibility",
     y = "Degree of Invasion (Exotic Species)") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   #scale_color_discrete(name = "Site") + 
   theme_bw()
)

## |- excludes extreme outliers ----
(di_exo_inv_site_bnsh <- di_inv %>%
   ggplot(
     aes(
       x = i_e_bnsh, 
       y = guo_di_exo, 
       col = site 
     )
   ) +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_point() + 
   labs(
     title = "Excludes extreme outliers from maximum biomass", 
     x = "Unified Metric of Invasibility",
     y = "Degree of Invasion (Exotic Species)") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   scale_color_discrete(name = "Site") + 
   theme_bw()
)

# plots: site effect on invasive species -----

## |- includes extreme outliers ----
(di_inv_site_chal <- di_inv %>%
   ggplot(
     aes(
       x = i_e_chal, 
       y = guo_di_inv, 
       col = site 
     )
   ) +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_point() + 
   labs(
     title = "Includes extreme outliers from maximum biomass", 
     x = "Unified Metric of Invasibility",
     y = "Degree of Invasion (Invasive Species)") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   scale_color_discrete(name = "Site") + 
   theme_bw()
)

## |- excludes extreme outliers ----
(di_inv_site_bnsh <- di_inv %>%
   ggplot(
     aes(
       x = i_e_bnsh, 
       y = guo_di_inv, 
       col = site 
     )
   ) +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_point() + 
   labs(
     title = "Excludes extreme outliers from maximum biomass", 
     x = "Unified Metric of Invasibility",
     y = "Degree of Invasion (Invasive Species)") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   scale_color_discrete(name = "Site") + 
   theme_bw()
)

# regression models ----

lm_di_inv_exo <- glmmTMB(
   guo_di_exo ~ i_e_chal + treatment +  (1|site), 
   family = beta_family(link = "logit"), 
   data = di_inv)

## diagnostics ----
lm_exo_sim <- simulateResiduals(fittedModel = lm_di_inv_exo)
plot(lm_exo_sim)
performance::check_outliers(lm_exo_sim)
