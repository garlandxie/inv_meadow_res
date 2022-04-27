# libraries ----
library(here)
library(readr)
library(dplyr)
library(ggplot2)

# import ----

biomass <- read.csv(
  here("data", "analysis_data", "biomass_analysis_data.csv"), 
  row.names = 1
)

# check packaging ----
dplyr::glimpse(biomass)

# analysis: with the influential outlier ----

## get carrying capacity ----

# maximum biomass (in grams)
# basically, the plot with all of the lambquarter's (Chenopodium album)
bm_max_chal <- max(biomass$comm_biomass_g)

# maximum richness 
# TODO: compare to TRCA Meadoway's biomonitoring datasets
sr_maximum <- max(biomass$species_richness)

## get relative fractions (species richness and biomass) ----

biomass_tidy <- biomass %>%
  rename(
    bm_observed_g = comm_biomass_g, 
    sr_observed = species_richness
         ) %>%
  mutate(
    frac_bm_obs_max = bm_observed_g/bm_max_chal, 
    frac_sr_obs_max = sr_observed/sr_maximum
  )

## get variation of invasibility ----

# there should be a better way of doing this
bm_5_chal <- biomass_tidy %>%
  arrange(desc(bm_observed_g)) %>%
  head(n=5)

sr_5_chal <- biomass_tidy %>%
  arrange(desc(sr_observed)) %>%
  head(n=5)

h_df_chal <- rbind(bm_5_chal, sr_5_chal)

lm_chal <- lm(frac_sr_obs_max ~ frac_bm_obs_max, data = h_df_chal)
h_chal <- coef(lm_chal)["frac_bm_obs_max"]

abs_h_chal <- abs(h_chal)

## get unified metric of invasibility ----

# includes unidentified morphospecies
inv_incl_mspp <- mutate(
  biomass_tidy,
  i_e_chal = 1-(abs_h_chal*frac_sr_obs_max + (1-abs_h_chal)*frac_bm_obs_max)
  )

## plots -----

(rel_fracs_chal <- inv_incl_mspp %>%
  ggplot(aes(x = frac_sr_obs_max, y = frac_bm_obs_max, col = site)) +
  geom_point() +
  labs(
    x = expression("S"["obs"]/"S"["max"]),
    y = expression("B"["obs"]/"B"["max"])
  ) + 
  theme_bw()
)

(site_vs_ie_chal <- inv_incl_mspp %>%
  ggplot(aes(x = site, y = i_e, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.1) +
  labs(
    x = "Site", 
    y = "Unified Metric of Invasibility"
  ) + 
  scale_fill_discrete(
    name = "Treatment", 
    label = c("Undisturbed", "Tilling")) + 
  theme_bw()
)

# save to disk -----

ggsave(
  plot = rel_fracs, 
  filename = here("output", "results", "relative_fractions.png"), 
  device = "png",
  units = "in", 
  height = 5, 
  width = 5
)

ggsave(
  plot = site_vs_ie, 
  filename = here("output", "results", "site_vs_ie.png"), 
  device = "png",
  units = "in", 
  height = 4, 
  width = 5
)

write.csv(
  x = inv_incl_mspp, 
  file = here("data", "intermediate_data", "guo_inv_incl_morphospp.csv")
)


