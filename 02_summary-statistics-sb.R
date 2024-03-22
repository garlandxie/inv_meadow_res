################################################################################
# Accompanying code for the following research project: 
#   Invasibility in the Meadoway (Scarborough, Canada)
#
# Corresponding authors for this script:  
#   Garland Xie      (1)
#
# Affiliations: 
#   (1) Department of Biological Sciences, 
#       University of Toronto Scarborough,
#       1265 Military Trail, Toronto, ON, M1C 1A4, Canada
#       email: garland.xie@mail.utoronto.ca, 
#              scott.macivor@mail.utoronto.ca
#
# Purpose of this R script: to process seed bank data for obtaining 
# plant status, such as non-native, native, and invasive species
#
# IMPORTANT: Please refresh your R session before you run this script
# Why? See https://rstats.wtf/save-source.html

# libraries --------------------------------------------------------------------
library(here)        # for creating relative file-paths
library(dplyr)       # for manipulating data 
library(readxl)      # for reading excel files 
library(stringr)     # for manipulating string characters
library(rdryad)      # for importing dryad repo files

# import -----------------------------------------------------------------------

## seed bank -------------------------------------------------------------------
sb_spr <- read.csv(
  here("data", "input_data", "seed_bank", "seed_bank_spring.csv")
  )

sb_fall <- read.csv(
  here("data", "input_data", "seed_bank", "seed_bank_fall.csv")
)

## exotic/native status --------------------------------------------------------

# import dataset using R Dryad API 
# Cadotte. 2021. Ecological Solutions and Evidence
# https://doi.org/10.1002/2688-8319.12036
dryad_cadotte_doi <- "10.5061/dryad.1ns1rn8sg"
dryad_cadotte_link <- rdryad::dryad_download(dryad_cadotte_doi)
plants_to <- read.csv(unlist(dryad_cadotte_link))

## invasive status ----

# import dataset using R Dryad API
# Potgieter et al. 2022. Journal of Applied Ecology
#  https://doi.org/10.1111/1365-2664.14103
dryad_potgieter_doi <- "10.5061/dryad.h9w0vt4k3"
dryad_potgieter_link <- rdryad::dryad_download(dryad_potgieter_doi)
dryad_potgieter_excel <- unlist(dryad_potgieter_link)[2]

invasive_plants <- 
  readxl::read_excel(
    dryad_potgieter_excel, 
    sheet = "Combined Species Ranking"
  )

## taxonomy --------------------------------------------------------------------

# note: for some odd reason, DRYAD would not let me pass the tabular data check
# for this particular dataset. So, switched over to the .xls format since that 
# was one of the options for uploading datasets using that platform 

sb_taxon <- read.csv(
  here("data", "input_data", "seed_bank", 
       "seed_bank_taxonomy.csv")
)

## seed mix --------------------------------------------------------------------

seed_mix <- read.csv(
  here("data", "input_data", "seed_bank", "seed_mix.csv")
)

# clean data: obtain plant status ----------------------------------------------

## native_status ---------------------------------------------------------------

# obtain list of native plants in Toronto, Canada
# can include plants in the TRCA curated seed mix as well
native_plants_tidy <- plants_to %>%
  janitor::clean_names() %>%
  filter(exotic_native == "N") %>%
  mutate(native_status = "Yes") %>%
  dplyr::select(species = scientific_name, native_status)

## exotic status ---------------------------------------------------------------

# obtain list of exotic plants in Toronto, Canada
# can include invasive plants as well 
exotic_plants_tidy <- plants_to %>%
  janitor::clean_names() %>%
  filter(exotic_native == "E") %>%
  mutate(exotic_status = "Yes") %>%
  dplyr::select(species = scientific_name, exotic_status)

## invasive status -------------------------------------------------------------

# obtain list of fifty invasive species in Toronto, Canada
invasive_plants_tidy <- invasive_plants %>%
  mutate(invasive_status = "Yes") %>%
  dplyr::select(species = Species, invasive_status)

## seed mix status -------------------------------------------------------------

# obtain list of native plants across all curated seed mixes
# curated by the Toronto Regional Conservation Authority
# https://themeadoway.ca/resources/
seed_mix_tidy <- seed_mix %>%
  janitor::clean_names() %>%
  mutate(species = paste(genus, species, sep = " ")) %>%
  mutate(seed_mix_status = "Yes") %>%
  dplyr::select(species, seed_mix_status)

# clean data: taxonomy ---------------------------------------------------------

sb_taxon_tidy <- sb_taxon %>%
  janitor::clean_names() %>%
  mutate(binom_latin = paste(genus, species, sep = " ")) %>%
  dplyr::select(code, binom_latin, authority)

# clean data: join taxonomy with all plant statuses ----------------------------

# do a left join of all plant status to the taxonomy df
# plant status: native, exotic, invasive, and seed mix
# each column will have a plant status 
# for transparency, in case there's a mistake that needs to be corrected

all_status <- sb_taxon_tidy %>%
  left_join(native_plants_tidy,   by = c("binom_latin" = "species")) %>%
  left_join(exotic_plants_tidy,   by = c("binom_latin" = "species")) %>%
  left_join(invasive_plants_tidy, by = c("binom_latin" = "species")) %>%
  left_join(seed_mix_tidy,        by = c("binom_latin" = "species"))

# manually assign status (due to synonyms or missing from database)

all_status_tidy <- all_status %>%
  mutate(
    native_status = case_when(
      
      # synonymous with Acalypha virginica
      # from Cadotte. 2021. Ecological Solutions and Evidence
      binom_latin == "Acalypha rhomboidea" ~ "Yes",
      
      # synonymous with Erigeron canadensis
      # from Cadotte. 2021. Ecological Solutions and Evidence
      binom_latin == "Conyza canadensis" ~ "Yes",
      
      #https://plants.usda.gov/home/plantProfile?symbol=CHSES
      binom_latin == "Euphorbia serpyllifolia" ~ "Yes",
      
      # synonymous with Mentha canadensis
      # Cadotte. 2021. Ecological Solutions and Evidence
      binom_latin == "Mentha arvensis" ~ "Yes",
      
      # https://plants.usda.gov/home/plantProfile?symbol=PYMU
      binom_latin == "Pycnanthemum muticum" ~ "Yes",
      
      TRUE ~ native_status
    )
  ) %>%
  
  mutate(
    exotic_status = case_when(
      
      # https://plants.usda.gov/home/plantProfile?symbol=CHSES
      binom_latin == "Lactuca sativa" ~ "Yes",
      
      #https://plants.usda.gov/home/plantProfile?symbol=PAMI2
      # typo: should be Panicum miliaceum
      binom_latin == "Panicum milliaceum" ~ "Yes",  
      
      #https://plants.usda.gov/home/plantProfile?symbol=PEGL2
      binom_latin == "Pennisetum glaucum" ~ "Yes",
      
      # synonymous with Helminthotheca echioides
      # Cadotte. 2021. Ecological Solutions and Evidence
      binom_latin == "Picris echioides" ~ "Yes",
      
      #https://plants.usda.gov/home/plantProfile?symbol=SIAL5
      binom_latin == "Sinapis alba" ~ "Yes",
      
      # https://plants.usda.gov/home/plantProfile?symbol=VEAG
      binom_latin == "Veronica agrestis" ~ "Yes",
      
      TRUE ~ exotic_status
    )
  )

# clean data: assign plant status (as a single column) -------------------------

# create a variable that assigns a particular status 
# this will be used to calculate proportions in another R script
# e.g., proportion of invasive species in a single seed bank plot

# classifications (for reference)
# exotic spontaneous (from nearby patches or previous land use) = E
# native spontaneous (from nearby patches or previous land use) = N
# native from seed mix (curated + sown after tilling) = SM
# invasive spontaneous (from nearby patches or previous land use)  = I

final_df <- all_status_tidy %>%
  mutate(status = case_when(
    
    invasive_status == "Yes" ~ "I",
    exotic_status == "Yes" & invasive_status == "Yes" ~ "I", 
    exotic_status == "Yes" ~ "E",
    
    native_status == "Yes" & seed_mix_status == "Yes" ~ "SM", 
    native_status == "Yes" ~ "N", 
    seed_mix_status == "Yes" ~ "SM"
  )
  )
