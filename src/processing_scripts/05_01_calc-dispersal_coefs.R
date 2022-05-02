# libraries ----
library(dplyr)    # for manipulating data
library(here)     # for creating relative file-paths
library(ggplot2)  # for visualizing data 
library(MESS)     # for calculating area under curves

# import ----

# can't download zip files from the R DRYAD API
# so, manual download and import into R
# doi: 10.5061/dryad.mq2ff

disp_df <- read.csv(
  here("data", "input_data", "seed_rain", "Plantdispersaldata.csv")
  )

# clean data ----

# doi for this specific dispersal kernel dataset
# https://doi.org/10.1674/0003-0031(2002)148[0263:SOTIAV]2.0.CO;2

# should avoid "hardcoding" so this solution make work
# for getting the Vincetoxicum rossicum dataset
dsv_1 <- row.names(disp_df)[disp_df$Species.name == "Vincetoxicum rossicum"]
dsv_1 <- as.numeric(dsv_1)
dsv_2 = dsv_1 + 1

disp_tidy <- disp_df %>%
  
  # grab the dispersal dataset for Vincetoxicum rossicum 
  slice(dsv_1:dsv_2) %>%
  select(-"Dataset", -"Species.name", -"Source") %>%
  
  # transpose data set so that we have distance and density as columns 
  t() %>%
  as.data.frame() %>%
  dplyr::filter(V1 != "Density (m-2)") %>%
  
  # remove additional missing values
  na.omit() %>%
  
  # make distance and density into numeric values 
  mutate(
    V1 = stringr::str_replace(V1, pattern = " ", replace = ""), 
    V1 = as.numeric(V1), 
    V2 = as.numeric(V2)
  ) %>%
  rename(distance_m = V1, density_m2 = V2) %>%
  filter(distance %in% c(0:10))

# plot ----

# dispersal location kernel
  disp_tidy %>%
    ggplot(aes(x = distance_m, y = density_m2)) + 
    geom_point() + 
    geom_line() + 
    labs(
      title = "Dispersal location kernel for Vincetoxicum rossicum",
      x = "Distance (m)", 
      y = "Density (m^2)") + 
    theme_bw()

# get dispersal coefficients -----

# calculate area under the curve using definite integrals
# natural spline interpolation
# from a vector of x and y coordinates

dispersal_coef <- MESS::auc(
  x = disp_tidy$distance_m, 
  y = disp_tidy$density_m2, 
  type = "spline")

# includes this parameter in calc_seed_rain_index()
# in the functions.R file 
dispersal_coef <- log(dispersal_coef)
