# library ----
library(googlesheets4)
library(visdat)
library(dplyr)
library(ggplot2)

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
  filter(Spp_Code != "LITTER") %>%
  group_by(Section, Site, Treatment, Plot) %>%
  summarize(
    sr = length(unique(Spp_Code)), 
    bm_g = sum(Biomass_g, na.rm = TRUE)
  )

# plot ----

plot_id <- c(
  "P01", "P03", "P05",
  "P11", "P13", "P15", 
  "P21", "P23", "P25"
  )

# species richness
bm_tidy %>%
  filter(
    Plot %in% plot_id) %>%
  ggplot(aes(x = Plot, y = sr, fill = Site)) + 
    geom_bar(stat = "identity") + 
    labs(x = NULL, y = "Species Richness") + 
    facet_wrap(Site~Treatment) +
    coord_flip() + 
    theme_bw()

# community biomass
bm_tidy %>%
  filter(Plot %in% plot_id) %>%
  ggplot(aes(x = Plot, y = bm_g, fill = Site)) + 
  geom_bar(stat = "identity") + 
  labs(x = NULL, y = "Community Biomass (in grams)") + 
  facet_wrap(Site~Treatment) +
  coord_flip() + 
  theme_bw()

# Guo's invasibility

sr_max <- max(bm_tidy$sr)
bm_max <- max(bm_tidy$bm_g)


bm_tidy$bm_obs_max <- bm_tidy$bm_g/bm_max
bm_tidy$sr_obs_max <- bm_tidy$sr/sr_max

bm_tidy %>%
ggplot(aes(x = sr_obs_max, y = bm_obs_max, shape = Site, col = Site)) +
  geom_point() +
  labs(
    x = expression("S"["obs"]/"S"["max"]),
    y = expression("B"["obs"]/"B"["max"])
    ) + 
  theme_bw()
