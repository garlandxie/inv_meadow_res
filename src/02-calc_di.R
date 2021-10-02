# libraries --------------------------------------------------------------------
library(rdryad)           # for importing a dryad repo (see below)
library(dplyr)            # for manipulating tidy data
library(janitor)          # for cleaning column names
library(stringr)          # for manipulating string characters
library(googlesheets4)    # for reading Google spreadsheet files
library(here)             # for creating relative file paths
library(tidyr)            # for creating tidy data

# import -----------------------------------------------------------------------

# dryad citation
# Cadotte, Marc (2020), The list of vascular plants for the city of Toronto
# Dryad, Dataset, https://doi.org/10.5061/dryad.1ns1rn8sg
# license: Creative Commons Zero 
doi <- "10.5061/dryad.1ns1rn8sg"
link <- rdryad::dryad_download(doi)
plants <- read.csv(unlist(link))

# list of species collected in the meadoway
# binomial latin names and their associated codes
gs_link <- "https://docs.google.com/spreadsheets/d/1ctIxX6FHW2vZS3yyO5A-pE8jYKXIO2FFq5D_cq7x39s/edit?usp=sharing"
plants_mw <- googlesheets4::read_sheet(gs_link, sheet = 1)

# biomass data 
bm_link <- "https://docs.google.com/spreadsheets/d/1U6IvXmukXMR8Gwxs1Yi8RyUJnUvqdeBXCVNBQo91Qys/edit?usp=sharing"
bm <- read_sheet(bm_link, sheet = "raw_data")

# check packaging --------------------------------------------------------------

str(plants)

# data cleaning: plants of Toronto ---------------------------------------------

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
  select(taxa = scientific_name, exotic_native) %>%
  mutate(taxa = str_replace(taxa, pattern = " ", replacement = "_"))

# data cleaning: plants of the Meadoway ----------------------------------------

mw_tidy <- plants_mw %>%
  janitor::clean_names() %>%
  mutate(taxa = paste(genus, species, sep = "_")) %>%
  select(code, taxa) %>%
  
  # fix synonyms for botanical nomenclature 
  # so that is consistent with plants of Toronto database 
  mutate(taxa = case_when(
    taxa == "Conzya_canadensis" ~ "Erigeron_canadensis", 
    taxa == "Solidago_spp." ~ "Solidago_canadensis",
    taxa == "Chenopodium_glaucum" ~ "Oxybasis_glauca", 
    taxa == "Setaria_glauca" ~ "Setaria_pumila",
    TRUE ~ taxa
    )
  )

# data clean: biomass ----------------------------------------------------------

# aggregate to species-level biomass per plot
bm_tidy <- bm %>%
  janitor::clean_names() %>%
  group_by(section, site, treatment, plot, spp_code) %>%
  summarize(spp_biomass_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup() 

# assign status ----------------------------------------------------------------

# non-native/invasive status
ex_in_nn <- mw_tidy %>%
  left_join(plants_tidy, by = "taxa") %>%
  
  # one species was not recorded in plants of Toronto
  # assign as exotic
  # ref: https://plants.usda.gov/home/plantProfile?symbol=CEPU4
  mutate(exotic_native = case_when(
    taxa == "Cerastium_pumilum" ~ "E", 
    TRUE ~ exotic_native)
    ) %>%
  
  # assign invasive status
  # should back up with refs? 
  mutate(invasive = case_when(
    taxa == "Alliaria_petiolata" ~ "Y",
    taxa == "Vincetoxicum_rossicum" ~ "Y",
    taxa == "Cirsium_arvense" ~ "Y", 
    TRUE ~ "N")
  ) %>%
  
  # assign non-native status (excluding invasive spp)
  # should back up with refs? 
  mutate(non_natives = case_when(
    exotic_native == "E" & invasive == "N" ~ "Y",
    exotic_native == "E" & invasive == "Y" ~ "N",
    exotic_native == "N" ~ "N",
    TRUE ~ "N")
  )
  
# get biomass and exotic/non-native/invasive status in the same df
bm_en <- bm_tidy %>% 
  left_join(ex_in_nn, by = c("spp_code" = "code")) 

# calculate degree of invasion (invasive + non-natives) ------------------------

# tricky to do so just make dfs and do joins

# get total biomass 
tot_fracs <- bm_en %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    tot_rich      = n_distinct(spp_code),
    tot_biomass_g = sum(spp_biomass_g, na.rm = TRUE)) %>%
  ungroup()

di_exo <- bm_en %>%
  filter(!is.na(exotic_native)) %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # exotic richness
    exo_rich = sum(exotic_native == "E", na.rm = TRUE)
    
  ) %>%
  ungroup()
  
exo_bio <- bm_en %>%
  filter(exotic_native == "E") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # exotic biomass 
    exo_biomass_g = sum(spp_biomass_g, na.rm = TRUE)
    
    ) %>%
  ungroup()

di_exo_final <- di_exo %>%
  left_join(exo_bio, by = c("section", "site", "treatment", "plot")) %>%
  left_join(tot_fracs, by = c("section", "site", "treatment", "plot")) %>%
  mutate(
  
    # Guo's degree of invasion
    guo_di = ((exo_rich/tot_rich) + (exo_biomass_g/tot_biomass_g))*0.5
    
    )

# calculate degree of invasion (for just invasive species) ---------------------

# prep for relative fractions
# 1. invasive richness
# 2. invasive biomass

# list of invasive spp: 
# 1. Vinceotoxicum rossicum
# 2. Alliara petiolata
# 3. Cirsium arvense

di_inv <- bm_en %>%
  
  # remove morphospecies and litter since I cannot assign a native status
  filter(!is.na(invasive)) %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # invasive richness 
    inv_rich = sum(invasive == "Y", na.rm = TRUE)
    
  ) %>%
  ungroup()

inv_bio <- bm_en %>%
  filter(invasive == "Y") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # invasive biomass 
    inv_bio = sum(spp_biomass_g, na.rm = TRUE)
    
    ) %>%
  
  # sites with zero invasive spp should have zero invasive biomass 
  mutate(inv_bio = tidyr::replace_na(inv_bio, 0))


di_inv_final <- di_inv %>%
  left_join(inv_bio, by = c("section", "site", "treatment", "plot")) %>%
  left_join(tot_fracs, by = c("section", "site", "treatment", "plot")) %>%
  mutate(
    
    # sites with zero invasive spp should have zero invasive biomass 
    inv_bio = tidyr::replace_na(inv_bio, 0),
    
    # modified Guo's degree of invasion
    # only applies to invasive spp
    # which is a subset of exotic spp (non-native + invasive)
    guo_di_inv = ((inv_rich/tot_rich) + (inv_bio/tot_biomass_g))*0.5
    
    )

# calculate degree of invasion (for non-native spp) ----------------------------

# prep for relative fractions
# 1. non-native richness
# 2. non-native biomass

di_nn <- bm_en %>%
  filter(!is.na(non_natives)) %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # non-native community richness
    nn_rich = sum(non_natives == "Y", na.rm = TRUE),
    
    # total community richness (incl. native and invasive species)
    tot_rich = n_distinct(spp_code),
    
    # total community biomass 
    tot_biomass_g = sum(spp_biomass_g, na.rm = TRUE)
    
  ) %>%
  ungroup()

nn_bio <- bm_en %>%
  filter(non_natives == "Y") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(nn_biomass_g = sum(spp_biomass_g, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # sites with zero invasive spp should have zero invasive biomass 
  mutate(nn_biomass_g = tidyr::replace_na(nn_biomass_g, 0))

# degree of invasion
# for non-native species 
di_nn_final <- di_nn %>%
  left_join(nn_bio, by = c("section", "site", "treatment", "plot")) %>%
  mutate(
    
    # sites with zero invasive spp should have zero invasive biomass 
    nn_bio = tidyr::replace_na(nn_biomass_g, 0), 
    
    # modified Guo's degree of invasion
    # only applies to non-native spp (excludes invasive spp)
    guo_di_nn = ((nn_rich/tot_rich) + (nn_biomass_g/tot_biomass_g))*0.5
    
    )

# save to disk -----------------------------------------------------------------

# DI (incl. non-natives + invasive spp)
write.csv(
  x = di_exo_final, 
  file = here('data', 'final', 'guo_di.csv')
)

# DI (incl. only invasive spp)
write.csv(
  x = di_inv_final, 
  file = here('data', 'final', 'di_inv.csv')
)

# DI (incl. non-native spp, but excludes invasive spp)
write.csv(
  x = di_nn_final, 
  file = here('data', 'final', 'di_nn.csv')
)

