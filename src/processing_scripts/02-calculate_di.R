# libraries ----
library(here)
library(rdryad)
library(dplyr)
library(janitor)
library(stringr)

# import ----

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

## plants of toronto ----

# import dataset using R DRYAD API  
dryad_doi <- "10.5061/dryad.1ns1rn8sg"
dryad_link <- rdryad::dryad_download(dryad_doi)
plants_to <- read.csv(unlist(dryad_link))

# check packaging ----
dplyr::glimpse(plants_to)
dplyr::glimpse(biomass)

# data clean ---
plants_to_tidy <- plants_to %>%
  janitor::clean_names() %>%
  select(scientific_name, exotic_native) %>%
  mutate(binomial_latin = str_replace(
    scientific_name, 
    pattern = " ", 
    replace = "_")
    )

