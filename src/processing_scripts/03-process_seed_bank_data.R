# libraries ----
library(here)
library(dplyr)
library(janitor)
library(ggplot2)

# import ----
sb_spring <- read.csv(
  here(
    "data", 
    "input_data", 
    "seed_bank", 
    "seed_bank_spring.csv")
  )

sb_fall <- read.csv(
  here(
    "data", 
    "input_data", 
    "seed_bank",
    "seed_bank_fall.csv")
)

# check packaging ----
dplyr::glimpse(sb_spring)
dplyr::glimpse(sb_fall)

# clean data ----

## |- combine both spring and fall sampling seasons ----
sb_spr_fall <- sb_fall %>%
  select(-Typed_by) %>%
  rbind(sb_spring)

## |- calculate seed bank density ----
sb_spring_tidy <- sb_spr_fall %>%
  janitor::clean_names() %>%
  dplyr::filter(
    site_name %in% c("BNSH", "GRNB", "DAVE", "KENN", "TIMH", "VICP")
    ) %>%
  group_by(section, site_name, treatment, plot) %>%
  summarize(
    sb_density = sum(abund, na.rm = TRUE), 
    sb_richness = dplyr::n_distinct(spp_code)) %>%
  ungroup()

## |- calculate propagule pressure of V. rossicum ----

# no seeds of V.rossicum emerged from the greenhouse soil seed banks
sb_spring_tidy$sb_pp_viro <- 0

# save to disk ----

write.csv(
  x = sb_spring_tidy, 
  file = here("data", "intermediate_data", "seed_bank_tidy.csv")
)

# figures: box-plots ----

## |- seed bank density ----
sb_spring_tidy %>%
  ggplot(aes(x = site_name, y = sb_density, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) +
  labs(x = "Site", y = "Seed Bank Density") + 
  scale_fill_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  theme_bw()

## |- seed bank richness ----
sb_spring_tidy %>%
  ggplot(aes(x = site_name, y = sb_richness, fill = treatment)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) +
  labs(x = "Site", y = "Seed Bank Richness") + 
  scale_fill_discrete(
    name = "Management Regime", 
    labels = c("Undisturbed", "Tilling")
  ) + 
  theme_bw()
