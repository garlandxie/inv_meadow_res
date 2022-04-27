# libraries ----
library(here)
library(dplyr)
library(stringr)

# import ----

## biomass ----
biomass <- read.csv(
  here(
    "data", 
    "input_data",
    "aboveground_biomass", 
    "meadoway_plants_aboveground_biomass_raw_data.csv"
  )
) 

## taxonomy ----
taxon <- read.csv(
  here(
    "data", 
    "input_data", 
    "aboveground_biomass", 
    "meadoway_plants_taxonomy.csv"
  )
)

## seed mix ----

seed_mix <- read.csv(
  here("data", "input_data", "seed_mix", "meadoway_seed_mix.csv")
)

## plants of toronto ----

# import dataset using R DRYAD API  
dryad_doi <- "10.5061/dryad.1ns1rn8sg"
dryad_link <- rdryad::dryad_download(dryad_doi)
plants_to <- read.csv(unlist(dryad_link))

# data clean ----

## clean seed mix ----

seed_mix_tidy <- seed_mix %>%
  janitor::clean_names() %>%
  mutate(
    
    binom_latin = paste(genus, species, sep = "_"),
    
    seed_mix_1 = str_replace(
      seed_mix_1,
      pattern = "^$", 
      replace = "No"
      ), 
    
    seed_mix_2 = str_replace(
      seed_mix_2, 
      pattern = "^$", 
      replace = "No"
    ), 
    
    seed_mix_1_2 = case_when(
      seed_mix_1 == "Yes" | seed_mix_2 == "Yes" ~ "Yes", 
      TRUE ~ "No"
    )
  ) %>%
  select(binom_latin, seed_mix_1_2) 

## obtain exotic/native status ----
plants_to_tidy <- plants_to %>%
  janitor::clean_names() %>%
  select(scientific_name, exotic_native) %>%
  mutate(binom_latin = str_replace(
    scientific_name, 
    pattern = " ", 
    replace = "_")
  )

## link exotic/native status with biomass ----
biomass_tidy <- biomass %>%
  janitor::clean_names() %>%
  left_join(taxon, by = c("spp_code" = "Code")) %>%
  mutate(binom_latin = paste(Genus, species, sep = "_")) %>%
  left_join(plants_to_tidy, by = "binom_latin") %>%
  select(section, site, treatment, plot, spp_code, biomass_g, exotic_native) %>%
  
  # manually assign exotic/native status 
  mutate(exotic_native = case_when(
    
    # Cerastium pumilum 
    spp_code == "CEPU"  ~ "E", 
    
    # Conzya canadensis
    # synonym with Erigeron canadensis
    # located in the plants of TO database
    spp_code == "COCA"  ~ "N", 
    spp_code == "SEGL"  ~ "E", 
    
    # Chenopodium glaucum
    # synonym with Oxybasis glauca
    # located in the plants of TO database
    spp_code == "CHGL" ~ "N",
    TRUE ~ exotic_native)
  )

# 
