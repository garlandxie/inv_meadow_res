# libraries ----
library(here)     # for creating relative file-paths
library(dplyr)    # for manipulating data
library(stringr)  # for manipulating string characters
library(tidyr)    # for replacing missing values
library(rdryad)   # for importing DRYAD repositories
library(readxl)   # for importing excel spreadsheets
library(ggplot2)  # for visualising data 

# import ----

## |- biomass ----
biomass <- read.csv(
  here(
    "data", 
    "input_data",
    "aboveground_biomass", 
    "meadoway_plants_aboveground_biomass_raw_data.csv"
  )
) 

## |- taxonomy ----
taxon <- read.csv(
  here(
    "data", 
    "input_data", 
    "aboveground_biomass", 
    "meadoway_plants_taxonomy.csv"
  )
)

## |- seed mix ----

seed_mix <- read.csv(
  here("data", "input_data", "seed_mix", "meadoway_seed_mix.csv")
)

## |- plants of toronto ----

# import dataset using R DRYAD API  
dryad_doi_1 <- "10.5061/dryad.1ns1rn8sg"
dryad_link_1 <- rdryad::dryad_download(dryad_doi_1)
plants_to <- read.csv(unlist(dryad_link_1))

## |- invasive species in TO ----

dryad_doi_2 <- "10.5061/dryad.h9w0vt4k3"
dryad_link_2 <- rdryad::dryad_download(dryad_doi_2)
inv_spp_rank_wa <- dryad_link_2[[1]][2] 
inv_spp_to <- readxl::read_excel(
  unlist(inv_spp_rank_wa), 
  sheet = "Combined Species Ranking"
  )

# data clean ----

## |- clean seed mix ----

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

## |- clean invasive species of TO -----

inv_to_tidy <- inv_spp_to %>%
  janitor::clean_names() %>%
  mutate(binom_latin = str_replace(
      species, 
      pattern = " ", 
      replace = "_")
    ) %>%
  mutate(invasive_status = "I") 

## |- obtain exotic/native status ----
plants_to_tidy <- plants_to %>%
  janitor::clean_names() %>%
  select(scientific_name, exotic_native) %>%
  mutate(binom_latin = str_replace(
    scientific_name, 
    pattern = " ", 
    replace = "_")
  )

## |- link exotic/native status with biomass ----
biomass_tidy <- biomass %>%
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
    TRUE ~ exotic_native)
  ) 
  
## |- clean status: spontaneous, seed mix, invasives -----
biomass_tidy <- biomass_tidy %>%
  mutate(status = case_when(
    
    # native species in the TRCA seed mix
    exotic_native == "N" & seed_mix_1_2 == "Yes" ~ "SM", 
    
    # invasive alien species with TRCA management priority 
    invasive_status == "I" ~ "SI",
    
    # spontaneous exotic species 
    exotic_native == "E" ~ "SE", 
    
    # spontaneous native species
    exotic_native == "N" ~ "SN",

    TRUE ~ "U"
  )
)

# |- calculate proportions (for abundance) ----

all_tot <- biomass_tidy %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    abund_tot = sum(biomass_g, na.rm = TRUE),
    rich_tot = dplyr::n_distinct(spp_code)
    ) %>%
  ungroup()

sm_tot <- biomass_tidy %>%
  filter(status == "SM") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    abund_sm = sum(biomass_g, na.rm = TRUE),
    rich_sm = dplyr::n_distinct(spp_code)) %>%
  ungroup()

se_tot <- biomass_tidy %>%
  filter(status == "SE") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    abund_se = sum(biomass_g, na.rm = TRUE),
    rich_se  = dplyr::n_distinct(spp_code)) %>%
  ungroup()

sn_tot <- biomass_tidy %>%
  filter(status == "SN") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    abund_sn = sum(biomass_g, na.rm = TRUE),
    rich_sn  = dplyr::n_distinct(spp_code)) %>%
  ungroup()

si_tot <- biomass_tidy %>%
  filter(status == "SI") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    abund_si = sum(biomass_g, na.rm = TRUE),
    rich_si  = dplyr::n_distinct(spp_code)) %>%
  ungroup()

multi_key_id <- c("section", "site", "treatment", "plot")
tot <- all_tot %>%
  left_join(sm_tot, by = multi_key_id) %>%
  left_join(se_tot, by = multi_key_id) %>%
  left_join(sn_tot, by = multi_key_id) %>%
  left_join(si_tot, by = multi_key_id) %>%
  mutate(
    
    # abundance
    prop_sm_bm = abund_sm/abund_tot, 
    prop_sn_bm = abund_sn/abund_tot, 
    prop_se_bm = abund_se/abund_tot, 
    prop_si_bm = abund_si/abund_tot,
    
    # richness
    prop_sm_sr = rich_sm/rich_tot, 
    prop_sn_sr = rich_sn/rich_tot, 
    prop_se_sr = rich_se/rich_tot, 
    prop_si_sr = rich_si/rich_tot
  ) 

# plot ----

## |- proportions of abundance ----

(prop_sm_bm_plot <- tot %>%
  ggplot(aes(x = site, y = prop_sm_bm, col = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  ylim(0, 1) + 
  labs(x = "Site", y = "Proportion of natives in seed mix") + 
  scale_color_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  coord_flip() + 
  theme_bw()
)

(prop_se_bm_plot <- tot %>%
  ggplot(aes(x = site, y = prop_se_bm, col = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  ylim(0, 1) + 
  labs(x = "Site", y = "Proportion of exotics species") + 
  scale_color_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  coord_flip() + 
  theme_bw()
)

(prop_sn_bm_plot <- tot %>%
    ggplot(aes(x = site, y = prop_sn_bm, col = treatment)) + 
    geom_boxplot() + 
    geom_point(alpha = 0.2) + 
    ylim(0, 1) + 
    labs(x = "Site", y = "Proportion of native abundance") + 
    scale_color_discrete(
      name = "Management Regime", 
      labels = c("Undisturbed", "Tilling")
    ) +
    coord_flip() + 
    theme_bw()
)

(prop_si_bm_plot <- tot %>%
    ggplot(aes(x = site, y = prop_si_bm, col = treatment)) + 
    geom_boxplot() + 
    geom_point(alpha = 0.2) + 
    ylim(0, 1) + 
    labs(
      title = "Analysis does not include V. rossicum",
      x = "Site",
      y = "Proportion of invasive abundance") + 
    scale_color_discrete(
      name = "Management Regime", 
      labels = c("Undisturbed", "Tilling")
    ) + 
    coord_flip() + 
    theme_bw()
)

## |- proportions of richness ----

(prop_sm_sr_plot <- tot %>%
   ggplot(aes(x = site, y = prop_sm_sr, col = treatment)) + 
   geom_boxplot() + 
   geom_point(alpha = 0.2) + 
   ylim(0, 1) + 
   labs(x = "Site", y = "Proportion of native abundance in seed mix") + 
   scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Tilling")
   ) + 
   coord_flip() + 
   theme_bw()
)

(prop_se_sr_plot <- tot %>%
    ggplot(aes(x = site, y = prop_se_sr, col = treatment)) + 
    geom_boxplot() + 
    geom_point(alpha = 0.2) + 
    ylim(0, 1) + 
    labs(x = "Site", y = "Proportion of exotic richness") + 
    scale_color_discrete(
      name = "Management Regime", 
      labels = c("Undisturbed", "Tilling")
    ) + 
    coord_flip() + 
    theme_bw()
)

(prop_sn_sr_plot <- tot %>%
    ggplot(aes(x = site, y = prop_sn_sr, col = treatment)) + 
    geom_boxplot() + 
    geom_point(alpha = 0.2) + 
    ylim(0, 1) + 
    labs(x = "Site", y = "Proportion of native richness") + 
    scale_color_discrete(
      name = "Management Regime", 
      labels = c("Undisturbed", "Tilling")
    ) + 
    coord_flip() + 
    theme_bw()
)

(prop_si_sr_plot <- tot %>%
    ggplot(aes(x = site, y = prop_si_sr, col = treatment)) + 
    geom_boxplot() + 
    geom_point(alpha = 0.2) + 
    ylim(0, 1) + 
    labs(
      title = "Analysis does not include V. rossicum",
      x = "Site", 
      y = "Proportion of invasive richness") + 
    scale_color_discrete(
      name = "Management Regime", 
      labels = c("Undisturbed", "Tilling")
    ) + 
    coord_flip() + 
    theme_bw()
)

# save to disk ----

write.csv(
  x = biomass_tidy, 
  file = here("data", "intermediate_data", "biomass_spp_status.csv")
) 
  
