################################################################################
# Accompanying code for the project: 
#   Drivers of invasibility in urban meadow restoration
#
# Corresponding authors for this script: Garland Xie (1)
#
# Affiliations: 
#   (1) Department of Biological Sciences, 
#       University of Toronto Scarborough,
#       1265 Military Trail, Toronto, ON, M1C 1A4, Canada
#       email: garland.xie@mail.utoronto.ca, 
#     
# Purpose of this R script: to process community biomass field data within 
# the Meadoway

# libraries --------------------------------------------------------------------
library(here)      # for creating relative file-paths
library(dplyr)     # for manipulating data
library(janitor)   # for creating r-friendly column names
library(ggplot2)   # for visualizing data
library(forcats)   # for manipulating factor variables 

# import -----------------------------------------------------------------------

biomass <- read.csv(
  here(
    "data", 
    "input_data",
    "aboveground_biomass", 
    "meadoway_plants_aboveground_biomass_raw_data.csv"
    )
)

# check packaging --------------------------------------------------------------
dplyr::glimpse(biomass)

# clean data -------------------------------------------------------------------

## get species-specific biomass per plot ---------------------------------------
biomass_tidy <- biomass %>%
  janitor::clean_names() %>%
  group_by(section, site, treatment, plot, spp_code) %>%
  summarize(biomass_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup()

## get community-level metrics per plot ----------------------------------------
biomass_summ <- biomass_tidy %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # species richness
    species_richness = dplyr::n_distinct(spp_code), 
    
    # community-level biomass
    comm_biomass_g = sum(biomass_g, na.rm = TRUE)
    
    ) %>%
  ungroup() 

# figures ----------------------------------------------------------------------

## biomass ---------------------------------------------------------------------
biomass_summ %>%
  mutate(
    site = factor(
      site, 
      levels = c("VICP", "TIMH", "KENN", "GRNB", "BNSH", "DAVE"))
  ) %>%
  ggplot(aes(x = site, y = comm_biomass_g, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  labs(x = "Site", y = "Community Biomass (in grams)") + 
  scale_fill_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  theme_bw()

## species richness ------------------------------------------------------------
biomass_summ %>%
  mutate(
    site = factor(
      site, 
      levels = c("VICP", "TIMH", "KENN", "GRNB", "BNSH", "DAVE"))
  ) %>%
  ggplot(aes(x = site, y = species_richness, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  labs(x = "Site", y = "Species Richness") + 
  scale_fill_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  theme_bw()

## correlation: richness and biomass -------------------------------------------

# can community biomass and species richness act as surrogates?
biomass_summ %>%
  mutate(
    treatment = case_when(
      treatment == "RES" ~ "Undisturbed", 
      treatment == "TIL" ~ "Tilling")
  ) %>%
  ggplot(aes(x = species_richness, y = comm_biomass_g)) + 
  geom_point(aes(col = site)) + 
  geom_smooth(method = "lm") + 
  labs(x = "Species Richness", y = "Community Biomass (in grams)") + 
  facet_wrap(~ treatment) + 
  scale_color_discrete(name = "Site") + 
  theme_bw()

# save to disk -----------------------------------------------------------------

write.csv(
  x = biomass_summ, 
  file = here(
    "data", 
    "analysis_data",
    "biomass_analysis_data.csv"
  )
)