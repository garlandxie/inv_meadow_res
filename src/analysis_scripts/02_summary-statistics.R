################################################################################
# Accompanying code for the following research project: 
#   Drivers of invasibility in urban meadow restoration
#
#
# Corresponding authors for this script:  
#   Garland Xie      (1)
#
# Affiliations: 
#   (1) Department of Biological Sciences, 
#       University of Toronto Scarborough,
#       1265 Military Trail, Toronto, ON, M1C 1A4, Canada
#       email: garlandxie@gmail.com
#
# Purpose of this R script: to run summary statistics for the results section
#
# IMPORTANT: Please refresh your R session before you run this script
# Why? See https://rstats.wtf/save-source.html

# libraries --------------------------------------------------------------------
library(here)      # for creating relative file-paths
library(dplyr)     # for manipulating data
library(stringr)   # for manipulating string characters
library(rdryad)    # for importing DRYAD repositories 
library(ggplot2)   # for visualizing data
library(patchwork) # for visualizing ggplot2 layouts

# import data ------------------------------------------------------------------

## |- biomass ------------------------------------------------------------------
biomass <- read.csv(
  here(
    "data", 
    "input_data",
    "aboveground_biomass", 
    "meadoway_plants_aboveground_biomass_raw_data.csv"
  )
)

## |- taxonomy -----------------------------------------------------------------
taxon <- read.csv(
  here(
    "data", 
    "input_data", 
    "aboveground_biomass", 
    "meadoway_plants_taxonomy.csv"
  )
)

## |- seed mix -----------------------------------------------------------------

seed_mix <- read.csv(
  here(
    "data", 
    "input_data", 
    "seed_mix", 
    "meadoway_seed_mix.csv"
    )
)

## |- plants of toronto --------------------------------------------------------

# import dataset using R DRYAD API  
dryad_doi_1 <- "10.5061/dryad.1ns1rn8sg"
dryad_link_1 <- rdryad::dryad_download(dryad_doi_1)
plants_to <- read.csv(unlist(dryad_link_1))

## |- invasive species in TO ---------------------------------------------------

dryad_doi_2 <- "10.5061/dryad.h9w0vt4k3"
dryad_link_2 <- rdryad::dryad_download(dryad_doi_2)
inv_spp_rank_wa <- dryad_link_2[[1]][2] 
inv_spp_to <- readxl::read_excel(
  unlist(inv_spp_rank_wa), 
  sheet = "Combined Species Ranking"
)

# clean data -------------------------------------------------------------------

## |- clean biomass ------------------------------------------------------------------
biomass_tidy <- biomass %>%
  janitor::clean_names() %>%
  group_by(section, site, treatment, plot, spp_code) %>%
  summarize(biomass_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup()

biomass_summ <- biomass_tidy %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    
    # species richness
    species_richness = dplyr::n_distinct(spp_code), 
    
    # community-level biomass
    comm_biomass_g = sum(biomass_g, na.rm = TRUE)
    
  ) %>%
  ungroup() 

## |- clean native/non-native status ------------------------------------------

# clean seed mix

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

# clean invasive species of TO 

inv_to_tidy <- inv_spp_to %>%
  janitor::clean_names() %>%
  mutate(binom_latin = str_replace(
    species, 
    pattern = " ", 
    replace = "_")
  ) %>%
  mutate(invasive_status = "I") 

# obtain exotic/native status 
plants_to_tidy <- plants_to %>%
  janitor::clean_names() %>%
  select(scientific_name, exotic_native) %>%
  mutate(binom_latin = str_replace(
    scientific_name, 
    pattern = " ", 
    replace = "_")
  )

# link exotic/native status with biomass
biomass_status <- biomass %>%
  janitor::clean_names() %>%
  left_join(taxon, by = c("spp_code" = "Code")) %>%
  mutate(binom_latin = paste(Genus, species, sep = "_")) %>%
  left_join(plants_to_tidy, by = "binom_latin") %>%
  left_join(seed_mix_tidy, by = "binom_latin") %>%
  left_join(inv_to_tidy, by = "binom_latin") %>%
  select(
    section, site, treatment, plot, 
    spp_code,
    biomass_g, 
    exotic_native, 
    seed_mix_1_2, 
    invasive_status) %>%
  
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
    
    
    # Bellis sylvestris
    spp_code == "BESP" ~ "E",
    TRUE ~ exotic_native)
  ) 

# clean status: spontaneous, seed mix, invasives 
biomass_status <- biomass_status %>%
  mutate(status = case_when(
    
    # native species in the TRCA seed mix
    exotic_native == "N" & seed_mix_1_2 == "Yes" ~ "SM", 
    
    # invasive alien species with TRCA management priority 
    invasive_status == "I" ~ "SI",
    
    # spontaneous exotic species 
    exotic_native == "E" ~ "SE", 
    
    # spontaneous native species
    exotic_native == "N" ~ "SN",
    
    TRUE ~ "U")
  ) %>%
  dplyr::select(
    section, 
    site, 
    treatment, 
    plot, 
    spp_code, 
    biomass_g, 
    status
  )

# summary statistics -----------------------------------------------------------

## |- plant id: aboveground inventory ------------------------------------------

# total number of sampled plants from above-ground inventory
# includes two genus-level ids: Carex and Solidago
# Carex being difficult to id without flowers
# Solidago because of the Canadensis-Altissima-Giganetus complex 

sr_ab_plants <- biomass %>%
  janitor::clean_names() %>%
  pull(spp_code) %>%
  unique() 

# number of morphospecies
sr_ab_morphospecies <- biomass %>%
  janitor::clean_names() %>%
  dplyr::filter(str_detect(spp_code, pattern = ".*[0-9].*")) %>%
  pull(spp_code) %>%
  unique() 

# number of non-native invasive species
sr_inv_plants <- biomass_status %>%
  dplyr::filter(status == "SI") %>%
  pull(spp_code) %>%
  unique()

# number of spontaneous exotic species
sr_exo_plants <- biomass_status %>%
  dplyr::filter(status == "SE") %>%
  pull(spp_code) %>%
  unique()

# number of native species included in the seed mix
sr_sm_plants <- biomass_status %>%
  dplyr::filter(status == "SM") %>%
  pull(spp_code) %>%
  unique()

# number of native species not included in seed mix
sr_sn_plants <- biomass_status %>%
  dplyr::filter(status == "SN") %>%
  pull(spp_code) %>%
  unique()

## |- average estimates --------------------------------------------------------

avg_estimates <- biomass_summ %>%
  group_by(treatment) %>%
  summarize(
    mean_sr = mean(species_richness, na.rm = TRUE) %>% round(digits = 0), 
    sd_sr = sd(species_richness, na.rm = TRUE) %>% round(digits = 0), 
    mean_biomass = mean(comm_biomass_g, na.rm = TRUE) %>% round(digits = 0), 
    sd_biomass = sd(comm_biomass_g, na.rm = TRUE) %>% round(digits = 0)
  )

## |- outliers -----------------------------------------------------------------

bm_outliers <- filter(biomass_summ, comm_biomass_g == max(comm_biomass_g))
sr_outliers <- filter(biomass_summ, species_richness == max(species_richness))

# visualize data ---------------------------------------------------------------

## |- biomass ------------------------------------------------------------------

(bm_plot_til <- biomass_summ %>%
  filter(treatment == "TIL") %>%
  mutate(plot = factor(plot)) %>%
  ggplot(aes(x = plot, y = comm_biomass_g)) + 
  geom_col() +
  ylim(0, 2000) + 
  facet_wrap(~site) + 
  labs(
    title = "Newly Established",
    x = "Plot ID", 
    y = "Community Biomass (in grams)"
  ) + 
  theme(axis.text.x = element_blank()) 
)

(bm_plot_res <- biomass_summ %>%
    filter(treatment == "RES") %>%
    mutate(plot = factor(plot)) %>%
    ggplot(aes(x = plot, y = comm_biomass_g)) + 
    geom_col() +
    ylim(0, 2000) + 
    facet_wrap(~site) + 
    labs(
      title = "Restored",
      x = "Plot ID", 
      y = NULL
    ) + 
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank()) 
)

bm_plot <- bm_plot_til + bm_plot_res

## |- species richness ---------------------------------------------------------

(sr_plot_til <- biomass_summ %>%
   filter(treatment == "TIL") %>%
   mutate(plot = factor(plot)) %>%
   ggplot(aes(x = plot, y = species_richness)) + 
   geom_col() +
   ylim(0, 30) + 
   facet_wrap(~site) + 
   labs(
     title = "Newly Established",
     x = "Plot ID", 
     y = "Species Richness"
   ) + 
   theme(axis.text.x = element_blank()) 
)

(sr_plot_res <- biomass_summ %>%
    filter(treatment == "RES") %>%
    mutate(plot = factor(plot)) %>%
    ggplot(aes(x = plot, y = species_richness)) + 
    geom_col() +
    ylim(0, 30) + 
    facet_wrap(~site) + 
    labs(
      title = "Restored",
      x = "Plot ID", 
      y = NULL
    ) + 
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
      ) 
)

(sr_plot <- sr_plot_til + sr_plot_res)

# save to disk -----------------------------------------------------------------

ggsave(
  filename = here("output", "data_appendix_output", "bm_plot.png"),
  plot = bm_plot, 
  device = "png", 
  units = "in",
  height = 3.5, 
  width = 6
)

ggsave(
  filename = here("output", "data_appendix_output", "sr_plot.png"),
  plot = sr_plot, 
  device = "png", 
  units = "in",
  height = 3.5, 
  width = 6
)