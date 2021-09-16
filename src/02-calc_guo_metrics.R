# libraries ----
library(here)
library(readr)

# import ----

bm <- read_csv(here("data/final", "biomass_tidy.csv"))

# check packaging ----

str(bm)
head(bm, n = 5)
tail(bm, n = 5)


# Guo's invasibility

sr_max <- max(bm_tidy$sr)
bm_max <- max(bm_tidy$bm_g)

bm_tidy$bm_obs_max <- bm_tidy$bm_g/bm_max
bm_tidy$sr_obs_max <- bm_tidy$sr/sr_max

bm_tidy %>%
  ggplot(aes(x = sr_obs_max, y = bm_obs_max)) +
  geom_point() +
  labs(
    x = expression("S"["obs"]/"S"["max"]),
    y = expression("B"["obs"]/"B"["max"])
  ) + 
  theme_bw()