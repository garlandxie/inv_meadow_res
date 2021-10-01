# libraries -------------------------------------------------------------------
library(here)
library(ggplot2)
library(dplyr)

# import -----------------------------------------------------------------------

# degree of invasion (incl. non-natives + invasive spp)
guo_di <- read.csv(
  here("data", "final", "guo_di.csv"),
  row.names = 1
  )

# degree of invasion (just invasive spp)
guo_di_inv <- read.csv(
  here("data", "final", "di_inv.csv"),
  row.names = 1
)

# degree of invasion (non-natives, excl. invasive spp)
guo_di_nn <- read.csv(
  here("data", "final", "di_nn.csv"),
  row.names = 1
)

# unified measure of invasibility 
guo_inv <- read.csv(
  here("data", "final", "guo_inv.csv"),
  row.names = 1
  ) 

# joins ------------------------------------------------------------------------

custom_join <- purrr::partial(
  inner_join, 
  by = c("section", "site", "treatment", "plot")
  )

# recursively apply join operations using a multi-id key
all_dfs <- list(guo_di, guo_di_inv, guo_di_nn, guo_inv)
guo <- Reduce(custom_join, all_dfs)

# Degree of invasion (non-native + invasive): relative fractions ---------------

(di_exo_fracs <- guo %>%
  mutate(
    exo_rich_frac = exo_rich/tot_rich,
    exo_bm_frac   = exo_biomass_g/bm_g
    ) %>%
  ggplot(aes(x = exo_rich_frac, y = exo_bm_frac, col = site)) + 
  geom_point() + 
  labs(
    title = "Exotics (incl. non-native and invasive spp)",
    x = expression("S"["exo"]/"S"["tot"]),
    y = expression("B"["exo"]/"B"["tot"])
  ) + 
  scale_color_discrete("Site") + 
  theme_bw()
)

# Degree of invasion (invasive): relative fractions ----------------------------

(di_inv_fracs <- guo %>%
   mutate(
     inv_rich_frac = inv_rich/tot_rich,
     inv_bm_frac   = inv_bio/bm_g
   ) %>%
   ggplot(aes(x = inv_rich_frac, y = inv_bm_frac, col = site)) + 
   geom_point() + 
   labs(
     title = "Invasive spp only",
     x = expression("S"["inv"]/"S"["tot"]),
     y = expression("B"["inv"]/"B"["tot"])
   ) + 
   scale_color_discrete("Site") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   theme_bw()
)

# Degree of invasion (non-natives): relative fractions -------------------------

(di_nn_fracs <- guo %>%
   mutate(
     nn_rich_frac = nn_rich/tot_rich,
     nn_bm_frac   = nn_biomass_g/bm_g
   ) %>%
   ggplot(aes(x = nn_rich_frac, y = nn_bm_frac, col = site)) + 
   geom_point() + 
   labs(
     title = "Non-native spp only",
     x = expression("S"["nn"]/"S"["tot"]),
     y = expression("B"["nn"]/"B"["tot"])
   ) + 
   scale_color_discrete("Site") + 
   xlim(0, 1) + 
   ylim(0, 1) + 
   theme_bw()
)

# DI (exotics) vs invasibility -------------------------------------------------

# degree of invasion versus invasibility 
# add seed rain later on
(di_vs_inv <- guo %>%
  ggplot(aes(x = i_e, y = guo_di, col = site)) + 
  geom_point() + 
  labs(
    x = "Invasibility", 
    y = "DI"
  ) + 
  scale_color_discrete("Site") + 
  theme_bw()
)

# DI (non-natives) vs invasibility ---------------------------------------------

# degree of invasion versus invasibility 
# add seed rain later on
(di_nn_vs_inv <- guo %>%
   ggplot(aes(x = i_e, y = guo_di_nn, col = site)) + 
   geom_point() + 
   labs(
     x = "Invasibility", 
     y = "DI (non-natives)"
   ) + 
   scale_color_discrete("Site") + 
   theme_bw()
)

# save to disk ------

ggsave(
  filename = here("data", "final", "inv_vs_di.png"), 
  plot = di_vs_inv, 
  device = "png", 
  units = "in", 
  height = 5, 
  width = 5
)
  