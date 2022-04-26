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

# calculate unified metric of invasibility: incl. morphospecies ----

## get carrying capacity ----

# maximum biomass
bm_maximum_g <- max(biomass$comm_biomass_g)

# maximum richness 
# TODO: compare to TRCA Meadoway's biomonitoring datasets
sr_maximum <- max(biomass$species_richness)

## get relative fractions ----

##  observed values ----
biomass_tidy <- biomass %>%
  rename(bm_observed_g = comm_biomass_g, 
         sr_observed = species_richness
         ) %>%
  mutate(
    frac_bm_obs_max = bm_observed_g/bm_maximum_g, 
    frac_sr_obs_max = sr_observed/sr_maximum
  )

## get variation of invasibility ----

# there should be a better way of doing this
bm_5 <- biomass_tidy %>%
  arrange(desc(bm_observed_g)) %>%
  head(n=5)

sr_5 <- biomass_tidy %>%
  arrange(desc(sr_observed)) %>%
  head(n=5)

h_df <- rbind(bm_5, sr_5)

h <- coef(lm(frac_sr_obs_max ~ frac_bm_obs_max, data = h_df))["frac_bm_obs_max"]
abs_h <- abs(h)

## get unified metric of invasibility ----

# includes unidentified morphospecies
inv_incl_mspp <- biomass_tidy %>%
  mutate(i_e = 1-(abs_h*frac_sr_obs_max + (1-abs_h)*frac_bm_obs_max))

# plots -----

rel_fracs <- inv_incl_mspp %>%
  ggplot(aes(x = frac_sr_obs_max, y = frac_bm_obs_max, col = site)) +
  geom_point() +
  labs(
    x = expression("S"["obs"]/"S"["max"]),
    y = expression("B"["obs"]/"B"["max"])
  ) + 
  theme_bw()

site_vs_ie <- inv_incl_mspp %>%
  ggplot(aes(x = site, y = i_e, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.1) +
  labs(
    x = "Site", 
    y = "Unified Metric of Invasibility"
  ) + 
  scale_fill_discrete(
    name = "Treatment", 
    label = c("Undisturbed", "Seed Tillage")) + 
  theme_bw()

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


