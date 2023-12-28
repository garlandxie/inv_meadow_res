# libraries --------------------------------------------------------------------
library(here)      # for creating relative file-paths
library(vegan)     # for calculating similarity indices
library(dplyr)     # for manipulating data
library(janitor)   # for cleaning column names
library(tidyr)     # for changing from long to wide tables
library(tibble)
library(stringr)

# import -----------------------------------------------------------------------

sb_spring <- read.csv(
  here(
    "data", 
    "input_data", 
    "seed_bank", 
    "seed_bank_spring.csv")
  )

sb_fall <- read.csv(
  here(
    "data", 
    "input_data", 
    "seed_bank",
    "seed_bank_fall.csv")
)
  
abg_biomass <- read.csv(
  here(
    "data", 
    "input_data",
    "aboveground_biomass", 
    "meadoway_plants_aboveground_biomass_raw_data.csv"
  )
)

# clean data -------------------------------------------------------------------

## |- presence/absence data ----------------------------------------------------

sb_tidy <- sb_fall %>%
  dplyr::select(-Typed_by) %>%
  rbind(sb_spring) %>%
  janitor::clean_names() %>%
  dplyr::select(treatment, site = site_name, plot, spp_code, abund) %>%
  filter(treatment %in% c("RES", "TIL")) %>%
  mutate(layer = "sb")
  
abg_tidy <- abg_biomass %>%
  janitor::clean_names() %>%
  dplyr::select(treatment, site, plot, spp_code, abund = biomass_g) %>%
  mutate(layer = "ab")

comm_incid <- rbind(sb_tidy, abg_tidy) %>%
  group_by(layer, site, treatment, plot, spp_code) %>%
  summarize(incidence = 1) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = spp_code, values_from = incidence) %>%
  mutate(across(CAREX:STME, ~tidyr::replace_na(., 0))) %>%
  mutate(plot_id = paste(layer, site, treatment, plot, sep = "-")) 

# calculate indices ------------------------------------------------------------

## |- Jaccard ------------------------------------------------------------------

j_dist_til <- comm_incid  %>%
  dplyr::filter(treatment == "TIL") %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "jaccard")

j_dist_res <- comm_incid  %>%
  dplyr::filter(treatment == "RES") %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "jaccard")

## |- Sorensen -----------------------------------------------------------------

# clean distance matrices ------------------------------------------------------

## |- Jaccard's ----------------------------------------------------------------

j_til_df <- j_dist_til %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "jaccard"
    ) 

j_til_tidy <- j_til_df %>%
  dplyr::filter(
    jaccard > 0 &
    stringr::str_detect(plot_1, pattern = "sb") &
    stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(plot_1_rep == plot_2_rep)

j_res_df <- j_dist_res %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "jaccard"
  ) 

j_res_tidy <- j_res_df %>%
  dplyr::filter(
    jaccard > 0 &
      stringr::str_detect(plot_1, pattern = "sb") &
      stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(plot_1_rep == plot_2_rep)



