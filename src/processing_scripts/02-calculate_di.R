# libraries ----
library(here)
library(rdryad)
library(dplyr)
library(janitor)
library(stringr)

# import ----

## plants of toronto ----

# import dataset using R DRYAD API  
dryad_doi <- "10.5061/dryad.1ns1rn8sg"
dryad_link <- rdryad::dryad_download(dryad_doi)
plants_to <- read.csv(unlist(dryad_link))

# check packaging ----
dplyr::glimpse(plants_to)

# data clean ---
plants_to_tidy <- plants_to %>%
  janitor::clean_names() %>%
  select(scientific_name, exotic_native) %>%
  mutate(binomial_latin = str_replace(
    scientific_name, 
    pattern = " ", 
    replace = "_")
    )

