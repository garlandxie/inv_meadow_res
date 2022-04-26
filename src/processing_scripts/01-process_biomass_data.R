# libraries ----
library(here)    # for creating relative file-paths
library(dplyr)   # for manipulating data
library(janitor) # for creating r-friendly column names

# import ----

biomass <- read.csv(
  here(
    "data", 
    "input_data",
    "aboveground_biomass", 
    "meadoway_plants_aboveground_biomass_raw_data.csv"
    )
)


# check packaging ----
dplyr::glimpse(biomass)

# clean data ----

## get species-specific biomass per plot ----
biomass_tidy <- biomass %>%
  janitor::clean_names() %>%
  group_by(section, site, treatment, plot, spp_code) %>%
  summarize(biomass_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup()

## get community-level metrics per plot ----
biomass_summ <- biomass_tidy %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # species richness
    species_richness = dplyr::n_distinct(spp_code), 
    
    # community-level biomass
    comm_biomass_g = sum(biomass_g, na.rm = TRUE)
    
    ) %>%
  ungroup() 

# save to disk ----
write.csv(
  x = biomass_summ, 
  file = here(
    "data", 
    "analysis_data",
    "biomass_analysis_data.csv"
    )
)

