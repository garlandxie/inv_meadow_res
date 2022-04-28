# libraries ----
library(here)
library(ggplot2)
library(dplyr)

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
    guo_di_exo, guo_di_inv, 
    i_e_chal, i_e_bnsh)

# plots: exotic species ----

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
   geom_smooth(method = "lm") + 
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