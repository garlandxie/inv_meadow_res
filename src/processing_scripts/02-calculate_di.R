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

# data clean ----

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

# calculate degree of invasion ----

# obtain total richness and community biomass ----
max_df <- biomass_tidy %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    sr_tot = dplyr::n_distinct(spp_code), 
    bm_tot = sum(biomass_g, na.rm = TRUE)
  )

# observed exotic richness and biomass ----
obs_df <- biomass_tidy %>%
  filter(exotic_native == "E") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    sr_exo = dplyr::n_distinct(spp_code), 
    bm_exo = sum(biomass_g, na.rm = TRUE)
  )

# join total and observed values ----
bm_df <- max_df %>%
  inner_join(obs_df,by = c("section", "site", "treatment", "plot")) %>%
  mutate(guo_di = (sr_exo/sr_tot + bm_exo/bm_tot)*0.5)



