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
# Purpose of this R script: to merge many datasets as preparation for the 
# structural equation modelling analysis

# libraries --------------------------------------------------------------------
library(here)    # for creating relative file-paths
library(dplyr)   # for manipulating data 
library(ggplot2) # for visualizing data 

# import -----------------------------------------------------------------------

## |- above-ground community biomass and richness ------------------------------

comm_biomass <- read.csv(
  here("data", "intermediate_data", "comm_biomass_data.csv"), 
  row.names = 1
)

## |- invasibility -------------------------------------------------------------
guo_inv <- read.csv(
  here("data", "intermediate_data", "guo_inv_with_outliers.csv"), 
  row.names = 1
)

## |- degree of invasion -------------------------------------------------------  

guo_di <- read.csv(
  here("data", "intermediate_data", "guo_di.csv"), 
  row.names = 1
)

## |- seed bank metrics --------------------------------------------------------
seed_bank <- read.csv(
  here("data", "intermediate_data", "seed_bank_tidy.csv"), 
  row.names = 1
)

seed_bank <- rename(seed_bank, site = site_name)

##

## |- litter mass --------------------------------------------------------------

litter <- read.csv(
  here("data", "intermediate_data", "litter_tidy.csv"), 
  row.names = 1
)

## |- seed rain for DSV --------------------------------------------------------
seed_rain_dsv <- read.csv(
  here("data", "intermediate_data", "seed_rain_tidy.csv"), 
  row.names = 1
)

# clean data: create data frame for SEM  ---------------------------------------

multi_key_id <- c("section", "site", "treatment", "plot")

sem_df <- guo_inv %>%
  left_join(comm_biomass, by = multi_key_id) %>%
  left_join(seed_bank, by = multi_key_id) %>%
  left_join(litter, by = multi_key_id) %>%
  left_join(seed_rain_dsv, by = multi_key_id) %>%
  left_join(guo_di, by = multi_key_id) %>%
  mutate(
    across(
      .cols = c("litter_mass_g"),
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
    i_e_chal, guo_di_exo, 
    sb_density, sb_richness, sb_pp_viro, 
    litter_mass_g, seed_rain_dsv
  )

# save to disk -----------------------------------------------------------------

write.csv(
  x = sem_df, 
  file = here("data", "analysis_data", "sem.csv")
)