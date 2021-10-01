# libraries ----
library(here)
library(ggplot2)
library(dplyr)

# import ----

# degree of invasion
guo_di  <- read.csv(
  here("data", "final", "guo_di.csv"),
  row.names = 1
  )

# unified measure of invasibility 
guo_inv <- read.csv(
  here("data", "final", "guo_inv.csv"),
  row.names = 1
  ) 

# joins ----

di_inv <- guo_di %>%
  inner_join(
    guo_inv,
    by = c("section", "site", "treatment", "plot")
    ) 

# plot ------

# add seed rain later on
(di_vs_inv <- di_inv %>%
  ggplot(aes(x = i_e, y = guo_di, col = site)) + 
  geom_point() + 
  labs(
    x = "Invasibility", 
    y = "DI"
  ) + 
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
  