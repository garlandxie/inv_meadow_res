# library ----
library(googlesheets4)
library(visdat)
library(dplyr)
library(ggplot2)
library(here)
library(readr)
library(tidyr)
library(tibble)
library(vegan)
library(BiodiversityR)
library(ggrepel)
library(gghighlight)
library(stringr)

# import ----

link <- "https://docs.google.com/spreadsheets/d/1U6IvXmukXMR8Gwxs1Yi8RyUJnUvqdeBXCVNBQo91Qys/edit?usp=sharing"
bm <- read_sheet(link, sheet = "raw_data")

# check packaging ----

str(bm)
head(bm, n=5)
tail(bm, n=5)

# check for missing values ----

vis_dat(bm)
vis_miss(bm)

# clean data ----

bm_tidy <- bm %>%
  janitor::clean_names() %>%
  filter(spp_code != "LITTER") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    sr = length(unique(spp_code)), 
    bm_g = sum(biomass_g, na.rm = TRUE)
  )

# plot ----

plot_id <- c(
  "P01", "P03", "P05",
  "P11", "P13", "P15", 
  "P21", "P23", "P25"
  )

# species richness
bm_tidy %>%
  filter(plot %in% plot_id) %>%
  ggplot(aes(x = plot, y = sr, fill = site)) + 
    geom_bar(stat = "identity") + 
    labs(x = NULL, y = "Species Richness") + 
    facet_wrap(site~treatment) +
    coord_flip() + 
    theme_bw()

# community biomass
bm_tidy %>%
  filter(plot %in% plot_id) %>%
  ggplot(aes(x = plot, y = bm_g, fill = site)) + 
  geom_bar(stat = "identity") + 
  labs(x = NULL, y = "Community Biomass (in grams)") + 
  facet_wrap(site~treatment) +
  coord_flip() + 
  theme_bw()

# litter

litter <- bm %>%
  janitor::clean_names() %>%
  filter(spp_code == "LITTER") 

(site_vs_lit <- litter %>%
  group_by(section, treatment, site, plot) %>%
  summarize(litter_bm_g = sum(biomass_g, na.rm = TRUE)) %>%
  ggplot(aes(x = site, y = litter_bm_g, fill = site)) + 
    geom_violin() +  
    geom_point(alpha = 0.1) + 
    labs(
      x = NULL, 
      y = "Litter biomass (in grams)"
    ) + 
    theme_bw() + 
    theme(legend.position = "none") 
)

# nmds for community composition -----------------------------------------------

# create community matrix 
comm_matrix <- bm %>%
  janitor::clean_names() %>%
  filter(spp_code != "LITTER") %>%
  group_by(site, plot, spp_code) %>%
  summarize(bm_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = spp_code, values_from = bm_g) %>%
  mutate(across(.cols = everything(), replace_na, 0)) %>%
  mutate(id = paste(site, plot, sep = "-")) %>%
  select(-c(site, plot)) %>%
  column_to_rownames(var = "id")

nmds <- vegan::metaMDS(comm_matrix, k = 2, distance = "bray")
stressplot(nmds)

site <- comm_matrix %>%
  rownames() %>%
  str_extract(pattern = "[A-Z]{1,4}")

ordiplot(nmds, type = "n")
ordihull(nmds, groups = site, draw="polygon", label= TRUE)
dev.off()
plot.new()
  
# rank-abundance curve: VICP ---------------------------------------------------

# VICP 
vicp_comm <- bm %>%
  janitor::clean_names() %>%
  group_by(section, site, treatment, plot, spp_code) %>%
  summarize(bm_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(site == "VICP") %>%
  select(
    plot, 
    spp_code, 
    bm_g
  ) %>%
  pivot_wider(names_from = spp_code, values_from = bm_g) %>%
  mutate(across(.cols = everything(), replace_na, 0)) %>%
  column_to_rownames(var = "plot")

# helpful for seeing all plots in VICP
rad_vicp <- vegan::radfit(vicp_comm, family = Gamma)
plot(rad_vicp)

# helpful for labeling species
(RA <- vicp_comm %>%
  BiodiversityR::rankabundance() %>%
  as.data.frame() %>%
  rownames_to_column(var = "species") %>%
  ggplot(aes(x = rank, y = abundance)) + 
  geom_line() + 
  geom_point() + 
  gghighlight(species %in% c("MELU", "CEFO", "TRPR", "CHAL", "TRAE")) + 
  geom_text_repel(aes(label = species)) + 
  labs(title = "Victoria Park") + 
  theme_bw()
)

# rank-abundance curve: GRNB ---------------------------------------------------

# GRNB
grnb_comm <- bm %>%
  janitor::clean_names() %>%
  filter(spp_code != "LITTER") %>%
  group_by(section, site, treatment, plot, spp_code) %>%
  summarize(bm_g = sum(biomass_g, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(site == "GRNB") %>%
  select(
    plot, 
    spp_code, 
    bm_g
  ) %>%
  pivot_wider(names_from = spp_code, values_from = bm_g) %>%
  mutate(across(.cols = everything(), replace_na, 0)) %>%
  column_to_rownames(var = "plot")

# helpful for seeing all plots in GRNB
# current gamma distribution fit results in non-convergence for glm's 
# ignore for now.. 
rad_grnb <- vegan::radfit(grnb_comm, family = Gamma)
plot(rad_grnb)

# helpful for labeling species
(RA_GRNB <- grnb_comm %>%
    BiodiversityR::rankabundance() %>%
    as.data.frame() %>%
    rownames_to_column(var = "species") %>%
    ggplot(aes(x = rank, y = abundance)) + 
    geom_line() + 
    geom_point() + 
    gghighlight(species %in% c("SOSP", "HEGI", "HEHE", "DACA")) + 
    geom_text_repel(aes(label = species)) + 
    labs(title = "GRNB") + 
    theme_bw()
)

# write to disk -----

readr::write_csv(
  x = bm_tidy, 
  here("data/final", "biomass_tidy.csv")
)
