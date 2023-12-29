# libraries ----
library(here)     # for creating relative file-paths
library(dplyr)    # for manipulating data
library(ggplot2)  # for visualising data 

# import ----

biomass <- read.csv(
  here("data", "intermediate_data", "comm_biomass_data.csv"), 
  row.names = 1
)

# check packaging ----
dplyr::glimpse(biomass)

# analysis: with the extreme outlier for maximum biomass ----

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

# relative fractions and possible habitat saturation
(rel_fracs_chal <- inv_incl_mspp %>%
  ggplot(aes(x = frac_sr_obs_max, y = frac_bm_obs_max, col = site)) +
  geom_point() +
  labs(
    x = expression("S"["obs"]/"S"["max"]),
    y = expression("B"["obs"]/"B"["max"])
  ) + 
  theme_bw()
)

# unified metric of invasibility per site
(site_vs_ie_chal <- inv_incl_mspp %>%
  ggplot(aes(x = site, y = i_e_chal, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.1) +
  labs(
    title = "Includes extreme outliers for maximum biomass",
    x = "Site", 
    y = "Unified Metric of Invasibility"
  ) + 
  scale_fill_discrete(
    name = "Treatment", 
    label = c("Undisturbed", "Tilling")) + 
  theme_bw()
)

# analysis: without extreme outlier for maximum biomass ----

## remove extreme outlier ----
biomass_no_out <- dplyr::filter(biomass, comm_biomass_g != bm_max_chal)

## get new carrying capacity ----
bm_max_bnsh <- max(biomass_no_out$comm_biomass_g)

## get relative fractions (species richness and biomass) ----

biomass_tidy2 <- biomass_no_out %>%
  rename(
    bm_observed_g = comm_biomass_g, 
    sr_observed = species_richness
  ) %>%
  mutate(
    frac_bm_obs_max = bm_observed_g/bm_max_bnsh, 
    frac_sr_obs_max = sr_observed/sr_maximum
  )

## get variation in invasibility ---- 

bm_5_bnsh <- biomass_tidy2 %>%
  arrange(desc(bm_observed_g)) %>%
  head(n=5)

sr_5_bnsh <- biomass_tidy2 %>%
  arrange(desc(sr_observed)) %>%
  head(n=5)

h_df_bnsh <- rbind(bm_5_bnsh, sr_5_bnsh)

# obtain slope (coefficient)
lm_bnsh <- lm(frac_bm_obs_max ~ frac_sr_obs_max, data = h_df_bnsh)
h_bnsh <- coef(lm_bnsh)["frac_sr_obs_max"]
abs_h_bnsh <- abs(h_bnsh)

## get unified metric of invasibility ----

# includes unidentified morphospecies
inv_bnsh <- mutate(
  biomass_tidy2,
  i_e_bnsh = 1-(abs_h_bnsh*frac_sr_obs_max + (1-abs_h_bnsh)*frac_bm_obs_max)
)

## plots ----

# relative fractions and possible habitat saturation
(rel_fracs_bnsh <- inv_bnsh %>%
   ggplot(aes(x = frac_sr_obs_max, y = frac_bm_obs_max, col = site)) +
   geom_point() +
   geom_abline(intercept = 1.50, slope = as.double(h_bnsh)) + 
   
   # note: remove y-axis tick labels 1.5 to 2.0 (if possible)
   ylim(0, 2) + 
   labs(
     title = "Excludes extreme outliers for maximum biomass",
     x = expression("S"["obs"]/"S"["max"]),
     y = expression("B"["obs"]/"B"["max"])
   ) + 
   theme_bw()
)

# unified metric of invasibility per site
(site_vs_ie_bnsh <- inv_bnsh %>%
    ggplot(aes(x = site, y = i_e_bnsh, fill = treatment)) + 
    geom_boxplot() + 
    geom_point(alpha = 0.1) +
    labs(
      title = "Excludes extreme outliers for maximum biomass",
      x = "Site", 
      y = "Unified Metric of Invasibility"
    ) + 
    scale_fill_discrete(
      name = "Treatment", 
      label = c("Undisturbed", "Tilling")) + 
    theme_bw()
)

# sensitivity analysis ----

inv <- inv_incl_mspp %>%
  inner_join(inv_bnsh, by = c("section", "site", "treatment", "plot")) %>%
  mutate(id = paste(site, plot, sep = "-")) %>%
  select(section, site, treatment, plot, id, i_e_bnsh, i_e_chal) 

# plot differences between invasibility metrics that includes and excludes
# extreme outliers of maximum biomass
# by site
(inv_sens <- inv %>%
  mutate(
    treatment = case_when(
      treatment == "RES" ~ "Undisturbed", 
      treatment == "TIL" ~ "Tilling"
    )
  ) %>%
  ggplot() +
  geom_segment(aes(x = i_e_bnsh, xend = i_e_chal, y = id, yend = id), alpha = 0.2) + 
  geom_point(aes(x = i_e_chal, y = id, col = "i_e", shape = treatment)) + 
  geom_point(aes(x = i_e_bnsh, y = id, col = "i_e_bnsh", shape = treatment)) + 
  labs(x = "Unified Metric of Invasibility") + 
  xlim(0, 1) + 
  scale_color_discrete(
    name = "Sensitivity Analysis",
    labels = c("With outliers", "Without outliers")
  ) + 
  scale_shape_discrete(
    name = "Management Regime"
  ) + 
  facet_wrap(~site) + 
  theme_bw() + 
  theme(axis.text.y = element_blank())
)

## histograms ----

inv %>%
ggplot(aes(x = i_e_chal)) +
  geom_histogram() + 
  geom_vline(xintercept = mean(inv$i_e_chal), linetype = "dashed") + 
  xlim(0, 1) + 
  labs(
    title = "Includes extreme outliers of maximum biomass",
    x = "Unified Metric of Invasibility") + 
  theme_bw()

inv %>%
  ggplot(aes(x = i_e_bnsh)) +
  geom_histogram() + 
  xlim(0, 1) + 
  geom_vline(xintercept = mean(inv$i_e_bnsh), linetype = "dashed") + 
  labs(
    title = "Excludes extreme outliers of maximum biomass",
    x = "Unified Metric of Invasibility"
    ) + 
  theme_bw()

# save to disk -----

ggsave(
  plot = rel_fracs_chal, 
  filename = here("output", "results", "relative_fractions.png"), 
  device = "png",
  units = "in", 
  height = 5, 
  width = 5
)

ggsave(
  plot = site_vs_ie_chal, 
  filename = here("output", "results", "site_vs_ie.png"), 
  device = "png",
  units = "in", 
  height = 4, 
  width = 5
)

# data frame with: 
# (1) observed values of species richness and community biomass, 
# (2) relative fractions of species richness and community biomass, 
# (3) Guo's unified metric of invasibility (including extreme outliers)
write.csv(
  x = inv_incl_mspp, 
  file = here("data", "intermediate_data", "guo_inv_with_outliers.csv")
)

# data frame with: 
# (1) observed values of species richness and community biomass, 
# (2) relative fractions of species richness and community biomass, 
# (3) Guo's unified metric of invasibility (excluding extreme outliers)
write.csv(
  x = inv_bnsh, 
  file = here("data", "intermediate_data", "guo_inv_exclude_outliers.csv")
)