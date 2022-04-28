# libraries ----
library(here)
library(dplyr)
library(ggplot2)
library(vegan)
library(ggrepel)
library(janitor)

# import ----

spr_sb <- read.csv(
  here("data", "input_data", "seed_bank", "seed_bank_spring.csv")
  )

fall_sb <- read.csv(
  here("data", "input_data", "seed_bank", "seed_bank_fall.csv")
  )

# check packaging ----
dplyr::glimpse(spr_sb)
dplyr::glimpse(fall_sb)

# clean data ----

sb <- fall_sb %>%
  select(-Typed_by) %>%
  rbind(spr_sb) %>%
  janitor::clean_names() %>%
  
  # remove one plot in BRIM site
  # since only spring seed bank data was collected
  filter(!(site_name == "BRIM" & plot == 5)) %>%
  
  filter(site_name %in% c(
    "VICP", "TIMH", "KENN", 
    "GRNB", "BNSH", "DAVE")
    )
  
# clean data: community data matrix -----

# convert raw data into community data matrix
# preparation for redundancy analysis
comm_matrix <- sb %>%
  janitor::clean_names() %>%
  
  # get plot-level abundance
  group_by(treatment, site_name, plot, spp_code) %>%
  summarize(abund = sum(abund, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # convert into community data matrix
  # each row represents a plot within a given site
  group_by(treatment, site_name) %>%
  tidyr::pivot_wider(names_from = spp_code, values_from = abund) %>%
  ungroup() %>%
    
  # replace missing values
  mutate(across(.cols = everything(), ~ tidyr::replace_na(.x, 0)))

# redundancy analysis -----

## prep ----

Y <- comm_matrix %>%
  dplyr::select(-"treatment", -"site_name", -"plot") %>%
  vegan::decostand("hellinger")

## run RDA ----
comm_comp_rda <- rda(Y ~ site_name, data = comm_matrix)

## grab eigenvalues ----
rda_summ <- summary(comm_comp_rda)

## get r squared ----

# unadjusted R^2 retrieved from rda object
R2 <- RsquareAdj(comm_comp_rda)$r.squared

# adjusted R^2 retrieved from rda object
R2adj <- RsquareAdj(comm_comp_rda)$adj.r.squared

## anova ----

anova(comm_comp_rda, permutations = how(nperm = 999))
anova(comm_comp_rda, by = "axis", permutations = how(nperm = 999))

# plot ----

# manually extract scores for the first two RDA axes
sp_scores <- as.data.frame(rda_summ$species[, c("RDA1", "RDA2")])
st_scores <- as.data.frame(rda_summ$sites[, c("RDA1", "RDA2")])
yz_scores <- as.data.frame(rda_summ$biplot[, c("RDA1", "RDA2")]) 

# plot the scores using ggplot2 syntax
ggplot_rda <- ggplot() + 
  
  # species scores
  geom_text_repel(
    aes(x = RDA1, y  = RDA2), 
    label = row.names(sp_scores), 
    max.overlaps = 50, 
    alpha = 0.3, 
    data = sp_scores) +
  geom_point(
    aes(x = RDA1, y  = RDA2), 
    alpha = 0.5, 
    data = sp_scores) +
  
  # environmental scores 
  geom_text_repel(
    aes(x = RDA1, y = RDA2), 
    label = row.names(yz_scores), 
    data = yz_scores
  ) + 
  geom_hline(yintercept = 0, linetype = 3, size = 0.1) + 
  geom_vline(xintercept = 0, linetype = 3, size = 0.1) + 
  xlim(-1, 1) + 
  ylim(-1, 1) + 
  theme_bw()

