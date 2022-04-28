# libraries ----
library(here)    # for creating relative file paths
library(rdryad)  # for importing dryad repos
library(dplyr)   # for manipulating data
library(janitor) # for cleaning column names
library(stringr) # for manipulating string data
library(ggplot2) # for visualising data 

# import ----

biomass_tidy <- read.csv(
  here(
    "data", 
    "intermediate_data",
    "biomass_tidy.csv"
  ), 
  row.names = 1
)

# check packaging ----
dplyr::glimpse(biomass_tidy)

# calculate degree of invasion ----

# TO DO: modify existing Guo's degree of invasion by partitioning into 
# invasive alien and non-invasive alien species

# obtain total richness and community biomass ----
max_df <- biomass_tidy %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    sr_tot = dplyr::n_distinct(spp_code), 
    bm_tot = sum(biomass_g, na.rm = TRUE)
  )

# observed exotic richness and biomass ----
exo_df <- biomass_tidy %>%
  filter(status == "SE") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    sr_exo = dplyr::n_distinct(spp_code), 
    bm_exo = sum(biomass_g, na.rm = TRUE)
  )

# observed invasive richness and biomass ----
inv_df <- biomass_tidy %>%
  filter(status == "SI") %>%
  group_by(section, site, treatment, plot) %>%
  summarize(
    sr_inv = dplyr::n_distinct(spp_code), 
    bm_inv = sum(biomass_g, na.rm = TRUE)
  )

# join total and observed values ----
bm_df <- max_df %>%
  inner_join(exo_df, by = c("section", "site", "treatment", "plot")) %>%
  inner_join(inv_df, by = c("section", "site", "treatment", "plot")) %>%
  mutate(
    sr_exo_frac = sr_exo/sr_tot, 
    bm_exo_frac = bm_exo/bm_tot,
    sr_inv_frac = sr_inv/sr_tot, 
    bm_inv_frac = bm_inv/bm_tot, 
    guo_di_exo = (sr_exo_frac + bm_exo_frac)*0.5, 
    guo_di_inv = (sr_inv_frac + bm_inv_frac)*0.5
    )

# plots ----

# relative fractions
(rel_fracs <- bm_df %>%
   ggplot(aes(
     x = sr_frac, 
     y = bm_frac,
     col = treatment, 
     shape = site)
     ) +
   geom_point() +
   xlim(0, 1) + 
   ylim(0, 1) + 
   labs(
     x = expression("S"["exo"]/"S"["tot"]),
     y = expression("B"["exo"]/"B"["tot"])
   ) + 
   scale_color_discrete(
     name = "Management Regime", 
     labels = c("Undisturbed", "Tilling")
   ) + 
   scale_shape_discrete(name = "Site") + 
   theme_bw()
)

# guo's degree of invasion per site
di_vs_site <- bm_df %>% 
  ggplot(aes(x = site, y = guo_di, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  ylim(0, 1) + 
  labs(x = "Site", y = "Guo's Degree of Invasion") + 
  scale_fill_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  theme_bw() 

# save to disk ----

write.csv(
  x = bm_df, 
  file = here("data", "intermediate_data", "guo_di.csv")
)