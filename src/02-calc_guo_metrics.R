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

# get maximum biomass and richness
sr_max <- max(bm$sr)
bm_max <- max(bm$bm_g)

# get relative fractions
bm$bm_obs_max <- bm$bm_g/bm_max
bm$sr_obs_max <- bm$sr/sr_max

# get variation in invasibility (h)
# maybe there's a better way of doing this...
bm_5 <- bm %>%
  arrange(desc(bm_g)) %>%
  head(n=5)

sr_5 <- bm %>%
  arrange(desc(sr)) %>%
  head(n=5)

h_df <- rbind(bm_5, sr_5) %>%
  mutate(
    bm_obs_max = bm_g/bm_max, 
    sr_obs_max = sr/sr_max
  )

h <- coef(lm(sr_obs_max ~ bm_obs_max, data = h_df))["bm_obs_max"]
abs_h <- abs(h)

# unified metric of invasibility 
bm <- bm %>%
  mutate(
    i_e = 1-(abs_h*sr_obs_max + (1-abs_h)*bm_obs_max)
  )

# plot Guo's invasibility ----

bm %>%
  ggplot(aes(x = sr_obs_max, y = bm_obs_max, col = site)) +
  geom_point() +
  labs(
    x = expression("S"["obs"]/"S"["max"]),
    y = expression("B"["obs"]/"B"["max"])
  ) + 
  theme_bw()