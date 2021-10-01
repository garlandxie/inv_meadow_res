# libraries ----
library(rdryad)
library(skimr)
library(dplyr)
library(janitor)
library(stringr)
library(googlesheets4)
library(here)
library(tidyr)

# import ----

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

# check packaging ----

str(plants)
skimr::skim(plants)

# data cleaning: plants of Toronto ----

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

# data cleaning: plants of the Meadoway ----

mw_tidy <- plants_mw %>%
  janitor::clean_names() %>%
  mutate(taxa = paste(genus, species, sep = "_")) %>%
  select(code, taxa) %>%
  mutate(taxa = case_when(
    taxa == "Conzya_canadensis" ~ "Erigeron_canadensis", 
    taxa == "Solidago_spp." ~ "Solidago_canadensis",
    taxa == "Chenopodium_glaucum" ~ "Oxybasis_glauca", 
    taxa == "Setaria_glauca" ~ "Setaria_pumila",
    TRUE ~ taxa
    )
  )

# data clean: biomass ----------------------------------------------------------

bm_tidy <- bm %>%
  janitor::clean_names() %>%
  group_by(section, site, treatment, plot, spp_code) %>%
  summarize(spp_biomass_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup() 

# exploring ---

# exotics, natives, and unidentified (??)
table(plants_tidy$exotic_native)

# filter out unidentified (??)
u_list <- plants_tidy %>%
  filter(exotic_native == "U") %>%
  pull(taxa)

# joins ----

mw_en <- mw_tidy %>%
  left_join(plants_tidy, by = "taxa") %>%
  mutate(exotic_native = case_when(
    taxa == "Cerastium_pumilum" ~ "E", 
    TRUE ~ exotic_native)
    )

bm_en <- bm_tidy %>% 
  left_join(mw_en, by = c("spp_code" = "code")) 

# calculate degree of invasion ----

# tricky to do so just make dfs and do joins

di_df <- bm_en %>%
  filter(!is.na(exotic_native)) %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    er = sum(exotic_native == "E", na.rm = TRUE),
    sr = length(unique(spp_code)),
    tot_bio = sum(spp_biomass_g, na.rm = TRUE)
  ) %>%
  ungroup()
  
e_bio <- bm_en %>%
  filter(exotic_native == "E") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(e_bio = sum(spp_biomass_g, na.rm = TRUE)) %>%
  ungroup()

di <- di_df %>%
  left_join(e_bio, by = c("section", "site", "treatment", "plot")) %>%
  mutate(
    guo_di = ((er/sr) + (e_bio/tot_bio))*0.5
  )

# calculate degree of invasion (for just invasive species) ---------------------

# assign invasive status
# should back up with refs? 

mw_i <- mw_en %>%
  mutate(invasive = case_when(
    taxa == "Alliaria_petiolata" ~ "Y",
    taxa == "Vincetoxicum_rossicum" ~ "Y",
    taxa == "Cirsium_arvense" ~ "Y", 
    TRUE ~ "N")
  )

# prep for relative fractions
# 1. invasive richness
# 2. invasive biomass

bm_i <- bm_tidy %>% 
  left_join(mw_i, by = c("spp_code" = "code")) 

di_df2 <- bm_i %>%
  filter(!is.na(invasive)) %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    ir = sum(invasive == "Y", na.rm = TRUE),
    sr = length(unique(spp_code)),
    tot_bio = sum(spp_biomass_g, na.rm = TRUE)
  ) %>%
  ungroup()

i_bio <- bm_i %>%
  filter(invasive == "Y") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(i_bio = sum(spp_biomass_g, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(i_bio = tidyr::replace_na(i_bio, 0))

# degree of invasion
# where invasive spp are: 
# (1) Alliaria petiolata, (2) Vincetoxicum rossicum, (3) Cirsium arvense

di_inv <- di_df2 %>%
  left_join(i_bio, by = c("section", "site", "treatment", "plot")) %>%
  mutate(i_bio = tidyr::replace_na(i_bio, 0)) %>%
  mutate(guo_di_inv = ((ir/sr) + (i_bio/tot_bio))*0.5)

# save to disk -----------------------------------------------------------------

write.csv(
  x = di, 
  file = here('data', 'final', 'guo_di.csv')
)




