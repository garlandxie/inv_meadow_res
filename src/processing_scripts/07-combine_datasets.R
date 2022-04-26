# libraries ----
library(here)
library(dplyr)
library(ggplot2)

# import ----

## invasibility metrics ----
guo_inv <- read.csv(
  here("data", "intermediate_data", "guo_inv_incl_morphospp.csv"), 
  row.names = 1
)

## seed bank metrics -----
seed_bank <- read.csv(
  here("data", "intermediate_data", "seed_bank_tidy.csv"), 
  row.names = 1
)

## litter mass ----
litter <- read.csv(
  here("data", "intermediate_data", "litter_tidy.csv"), 
  row.names = 1
)

## plot coordinates ----
plot_coords <- read.csv(
  here("data", "intermediate_data", "plot_coords.csv"), 
  row.names = 1
)

## seed rain for DSV ----
seed_rain_dsv <- read.csv(
  here("data", "intermediate_data", "seed_rain_tidy.csv"), 
  row.names = 1
)

# clean data ----

## create SEM df ----

multi_key_id <- c("section", "site", "treatment", "plot")

sem_df <- guo_inv %>%
  left_join(seed_bank, by = multi_key_id) %>%
  left_join(litter, by = multi_key_id) %>%
  left_join(plot_coords, by = multi_key_id) %>%
  left_join(seed_rain_dsv, by = multi_key_id) %>%
  mutate(
    across(
      .cols = c("sb_richness", "sb_density", "litter_mass_g"),
      ~ tidyr::replace_na(.x, 0)
      )
    ) %>%
  mutate(
    site = factor(
      site, 
      levels = c("VICP", "TIMH", "KENN", "GRNB", "BNSH", "DAVE")
    )
  ) %>%
  select(
    section, site, treatment, plot, 
    lat_dd, lon_dd, 
    i_e, sb_density, sb_richness, sb_pp_viro, litter_mass_g, seed_rain_dsv
  )

# main figures ----



# exploring data ----

## invasibility versus potential propagule pressure ----

(inv_vs_pp  <- sem_df %>%
   ggplot(aes(x = seed_rain_dsv, y = i_e, col = treatment)) +
   geom_point() + 
   geom_smooth(method = "lm") + 
   labs(
     x = "Potential Propagule Pressure", 
     y = "Unified Metric of Invasibility") + 
   ylim(0, 1) + 
   scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Seed Drilling")
   ) + 
   facet_wrap(~site) +
   theme_bw()
)

## litter versus propagule density ----

(litter_vs_sb_density <- sem_df %>%
  ggplot(aes(x = litter_mass_g, y = sb_density, col = treatment)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x = "Litter Mass (in grams)", 
    y = "Propagule Density in Soil Seed Bank"
  ) + 
  scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Seed Drilling")
  ) + 
  facet_wrap(~site) + 
  theme_bw()
)

## litter versus propagule richness ----

(litter_vs_sb_richness <- sem_df %>%
   ggplot(aes(x = litter_mass_g, y = sb_richness, col = treatment)) + 
   geom_point() + 
   geom_smooth(method = "lm", se = FALSE) + 
   labs(
     x = "Litter Mass (in grams)", 
     y = "Propagule Richness in Soil Seed Bank"
   ) + 
   scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Seed Drilling")
   ) + 
   facet_wrap(~site) + 
   theme_bw()
)

## invasibility versus seed bank density ----
(inv_vs_sb_density <- sem_df %>%
  
  # removed an outlier 
  filter(sb_density != max(sem_df$sb_density)) %>%
  ggplot(aes(x = sb_density, y = i_e, col = treatment)) + 
  geom_point(aes(shape = site)) + 
  geom_smooth(method = "lm") + 
  ylim(0, 1) + 
  labs(
    title = "Without influential outliers", 
    x = "Propagule Density in Soil Seed Bank", 
    y = "Unified Metric of Invasibility") + 
  scale_color_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Seed Drilling")
  ) +
  scale_shape_discrete(
    name = "Site"
  ) + 
  theme_bw()
)

## invasibility versus seed bank richness ----
inv_vs_sb_richness <- sem_df %>%

  # removed an outlier 
  ggplot(aes(x = sb_richness, y = i_e, col = treatment)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  ylim(0, 1) + 
  labs(
    x = "Propagule Richness in Soil Seed Bank", 
    y = "Unified Metric of Invasibility") +
  scale_color_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Seed Drilling")
  ) + 
  facet_wrap(~site) + 
  theme_bw()

# save to disk ----

## invasibility versus propagule density ----
ggsave(
  filename = here("output", "results", "inv_vs_sb_density.png"), 
  plot = inv_vs_sb_density, 
  device = "png", 
  units = "in", 
  height = 5, 
  width = 6
)


## litter versus propagule density -----
ggsave(
  filename = here("output", "results", "litter_vs_sb_density.png"), 
  plot = litter_vs_sb_density, 
  device = "png", 
  units = "in", 
  height = 5, 
  width = 6
)

## litter versus propagule richness -----
ggsave(
  filename = here("output", "results", "litter_vs_sb_richness.png"), 
  plot = litter_vs_sb_richness, 
  device = "png", 
  units = "in", 
  height = 5, 
  width = 6
)

# save to disk -----

write.csv(
  x = sem_df, 
  file = here("data", "analysis_data", "sem.csv")
)