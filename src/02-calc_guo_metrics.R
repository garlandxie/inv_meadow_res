# libraries ----
library(here)
library(readr)
library(dplyr)
library(ggplot2)

# import ----

bm <- read_csv(here("data/final", "biomass_tidy.csv"))

# check packaging ----

str(bm)
head(bm, n = 5)
tail(bm, n = 5)

# calculate unified measure of invasibility ----

sr_max <- max(bm$sr)
bm_max <- max(bm$bm_g)

bm$bm_obs_max <- bm$bm_g/bm_max
bm$sr_obs_max <- bm$sr/sr_max

# plot Guo's invasibility ----

bm %>%
  ggplot(aes(x = sr_obs_max, y = bm_obs_max, col = site)) +
  geom_point() +
  labs(
    x = expression("S"["obs"]/"S"["max"]),
    y = expression("B"["obs"]/"B"["max"])
  ) + 
  theme_bw()