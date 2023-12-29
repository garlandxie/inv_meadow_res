# libraries --------------------------------------------------------------------
library(here)      # for creating relative file-paths
library(vegan)     # for calculating similarity indices
library(dplyr)     # for manipulating data
library(janitor)   # for cleaning column names
library(tidyr)     # for changing from long to wide tables
library(tibble)    # for converting column to row-names
library(stringr)   # for manipulating string characters
library(ggplot2)   # for visualizing data

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

comm_incid_til <- rbind(sb_tidy, abg_tidy) %>%
  group_by(layer, site, treatment, plot, spp_code) %>%
  summarize(incidence = 1) %>%
  ungroup() %>%
  dplyr::filter(treatment == "TIL") %>%
  tidyr::pivot_wider(names_from = spp_code, values_from = incidence) %>%
  mutate(across(ALPE:PONO, ~tidyr::replace_na(., 0))) %>%
  mutate(plot_id = paste(layer, site, treatment, plot, sep = "-")) 

comm_incid_res <- rbind(sb_tidy, abg_tidy) %>%
  group_by(layer, site, treatment, plot, spp_code) %>%
  summarize(incidence = 1) %>%
  ungroup() %>%
  dplyr::filter(treatment == "RES") %>%
  tidyr::pivot_wider(names_from = spp_code, values_from = incidence) %>%
  mutate(across(CAREX:FRVE, ~tidyr::replace_na(., 0))) %>%
  mutate(plot_id = paste(layer, site, treatment, plot, sep = "-")) 

# calculate indices ------------------------------------------------------------

## |- Jaccard ------------------------------------------------------------------

j_dist_til <- comm_incid_til  %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "jaccard")

j_dist_res <- comm_incid_res  %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "jaccard")

## |- Bray-Curtis --------------------------------------------------------------

b_dist_til <- comm_incid_til  %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "bray")

b_dist_res <- comm_incid_res %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "bray")

## |- Chao ---------------------------------------------------------------------

c_dist_til <- comm_incid_til  %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "chao")

c_dist_res <- comm_incid_res  %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "chao")

## |- Raup ---------------------------------------------------------------------

r_dist_til <- comm_incid_til  %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "raup")

r_dist_res <- comm_incid_res  %>%
  tibble::column_to_rownames(var = "plot_id") %>%
  dplyr::select(-c("layer", "site", "treatment", "plot")) %>%
  as.matrix() %>%
  vegan::vegdist(method = "raup")

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
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep, 
    site_1_rep == site_2_rep
    ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

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
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep,
    site_1_rep == site_2_rep
    ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

## |- Bray-Curtis --------------------------------------------------------------

b_til_df <- b_dist_til %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "bray"
  ) 

b_til_tidy <- b_til_df %>%
  dplyr::filter(
    bray > 0 &
      stringr::str_detect(plot_1, pattern = "sb") &
      stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep,
    site_1_rep == site_2_rep
    ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

b_res_df <- b_dist_res %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "bray"
  ) 

b_res_tidy <- b_res_df %>%
  dplyr::filter(
    bray > 0 &
      stringr::str_detect(plot_1, pattern = "sb") &
      stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep,
    site_1_rep == site_2_rep
    ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

## |- Chao ---------------------------------------------------------------------

c_til_df <- c_dist_til %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "chao"
  ) 

c_til_tidy <- c_til_df %>%
  dplyr::filter(
    chao >= 0 &
      stringr::str_detect(plot_1, pattern = "sb") &
      stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep,
    site_1_rep == site_2_rep
  ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

c_res_df <- c_dist_res %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "chao"
  ) 

c_res_tidy <- c_res_df %>%
  dplyr::filter(
    chao >= 0 &
      stringr::str_detect(plot_1, pattern = "sb") &
      stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep,
    site_1_rep == site_2_rep
  ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

## |- Raup ---------------------------------------------------------------------

r_til_df <- r_dist_til %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "raup"
  ) 

r_til_tidy <- r_til_df %>%
  dplyr::filter(
    raup >= 0 &
      stringr::str_detect(plot_1, pattern = "sb") &
      stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep,
    site_1_rep == site_2_rep
  ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

r_res_df <- r_dist_res %>%
  as.matrix() %>%
  as_tibble(rownames = "plot_id") %>%
  rename(plot_1 = plot_id) %>%
  tidyr::pivot_longer(
    cols = -plot_1, 
    names_to = "plot_2", 
    values_to = "raup"
  ) 

r_res_tidy <- r_res_df %>%
  dplyr::filter(
      raup >= 0 &
      stringr::str_detect(plot_1, pattern = "sb") &
      stringr::str_detect(plot_2, pattern = "ab")
  ) %>%
  mutate(
    site_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 2),
    site_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 2),
    plot_1_rep = str_split(plot_1, pattern = "-") %>% sapply("[", 4), 
    plot_2_rep = str_split(plot_2, pattern = "-") %>% sapply("[", 4)
  ) %>% 
  dplyr::filter(
    plot_1_rep == plot_2_rep,
    site_1_rep == site_2_rep
  ) %>%
  select(-c("plot_1_rep", "plot_2_rep", "site_1_rep", "site_2_rep"))

# summarize --------------------------------------------------------------------

sim_res <- b_res_tidy %>%
  inner_join(j_res_tidy, by = c("plot_1", "plot_2")) %>%
  inner_join(c_res_tidy, by = c("plot_1", "plot_2")) %>%
  inner_join(r_res_tidy, by = c("plot_1", "plot_2"))

sim_til <- b_til_tidy %>%
  inner_join(j_til_tidy, by = c("plot_1", "plot_2")) %>%
  inner_join(c_til_tidy, by = c("plot_1", "plot_2")) %>%
  inner_join(r_til_tidy, by = c("plot_1", "plot_2"))

sim <- rbind(sim_res, sim_til) %>%
  mutate(across(bray:raup, ~round(., digits = 2)))

# get mean and sd's for raup-crick similarity indices (per restoration stage)
median(sim_res$raup)
IQR(sim_res$raup)

median(sim_til$raup)
IQR(sim_til$raup)




