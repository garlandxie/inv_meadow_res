# libraries ---
library(here)
library(dplyr)
library(janitor)

# import ----

litter <- read.csv(
  here("data", "input_data", "litter", "meadoway_litter_raw_data.csv")
) 

# check packaging ----

dplyr::glimpse(litter)

# clean data ----

litter_tidy <- litter %>%
  janitor::clean_names() %>%
  group_by(section, site, treatment, plot) %>%
  summarize(litter_mass_g = sum(mass_g, na.rm = TRUE)) %>%
  ungroup()

# save to disk ----

write.csv(
  x = litter_tidy, 
  file = here("data", "intermediate_data", "litter_tidy.csv")
)

  