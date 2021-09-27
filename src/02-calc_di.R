# libraries ----
library(rdryad)
library(skimr)
library(dplyr)
library(janitor)
library(stringr)

# import ----

# dryad citation
# Cadotte, Marc (2020), The list of vascular plants for the city of Toronto
# Dryad, Dataset, https://doi.org/10.5061/dryad.1ns1rn8sg
# license: Creative Commons Zero 
doi <- "10.5061/dryad.1ns1rn8sg"
link <- rdryad::dryad_download(doi)
plants <- read.csv(unlist(link))

# check packaging ----

str(plants)
skimr::skim(plants)

# data cleaning ----

# some metadata on the plants of Toronto database: 
# (1) 1937 taxa from 146 families, of which 822 are non-indigenous

# (2) compiled from multiple data sources: 
# - Cadotte lab research, 
# - Rouge National Urban Park, 
# - Royal Ontario Museum, 
# - Toronto Region Conservation Authority, 
# - GBIF, 
# - Ken Sproule 

# (3) species were checked against Taxonomic Name Resolution Services 
# and Canadensys 

# (4) species were cross-referenced against Ministry of Natural Resources 
# and Forestry’s Natural Heritage Information Centre for native or introduced 
# (non-indigenous) status. Any other species not in this database were
# cross-referenced with the Plants USDA database 
# Note that Non-indigenous species are those not known to occur in 
# the province of Ontario prior to European settlement (Myers & Bazely 2003)

# relevant variables: 
# (1) scientific name, 
# (2) exotic_native

plants_tidy <- plants %>%
  janitor::clean_names() %>%
  select(
    taxa = scientific_name, 
    exotic_native
    ) %>%
  mutate(taxa = str_replace(taxa, pattern = " ", replacement = "_"))

