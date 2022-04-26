# Script written by Malaika Mitra and Garland Xie

# libraries ----
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# custom functions ----

ddm_to_dd <- function(dms) {
  
  # split strings to seprate degrees and minutes.m
  split1 <- str_split(as.vector(dms), pattern = " ")
  
  # extract degrees
  degrees <- substr(split1[[1]][2], 1, 2)
  degrees <- as.double(degrees)
  
  # extract minutes.m
  minutes.m <- str_replace(split1[[1]][3], pattern = "'", replace = "")
  minutes.m <- as.double(minutes.m)
  
  # convert to degrees
  dot_d <- minutes.m/60
  
  # calculate decimal degrees
  decimal_degrees = degrees + dot_d
  
  
  return(decimal_degrees)
}

# import ----

## sites in section 2 ----

VICP_coords <- read.csv(
  here("data", "input_data", "plot_coords", 
       "meadoway_site_VICP_plot_coordinates.csv")
)

TIMH_coords <- read.csv(
  here("data", "input_data", "plot_coords", 
       "meadoway_site_TIMH_plot_coordinates.csv")
)

KENN_coords <- read.csv(
  here("data", "input_data", "plot_coords", 
       "meadoway_site_KENN_plot_coordinates.csv")
)

## sites in section 4 ----

GRNB_coords <- read.csv(
  here("data", "input_data", "plot_coords", 
       "meadoway_site_GRNB_plot_coordinates.csv")
)

BNSH_coords <- read.csv(
  here("data", "input_data", "plot_coords", 
       "meadoway_site_BNSH_plot_coordinates.csv")
)

DAVE_coords <- read.csv(
  here("data", "input_data", "plot_coords", 
       "meadoway_site_DAVE_plot_coordinates.csv")
)

# data clean: row bind ----

coords <- do.call(
  "rbind", list(
    VICP_coords, TIMH_coords, KENN_coords, 
    GRNB_coords, BNSH_coords, DAVE_coords
    )
)


# data clean: convert DD to DMS ----

coords_dd <- coords %>%
  janitor::clean_names() %>%
  mutate(
    lat_dd = sapply(latitude, ddm_to_dd),
    lon_dd = sapply(longitude, ddm_to_dd) * -1
    ) %>%
  mutate(
    section = case_when(
      site %in% c("VICP", "TIMH", "KENN") ~ "S2",
      site %in% c("GRNB", "BNSH", "DAVE") ~ "S4"
    )
  ) %>%
  select(section, treatment, site, plot, lat_dd, lon_dd)
  
# save to disk -----

write.csv(
  x = coords_dd,
  file = here("data", "intermediate_data", "plot_coords.csv")
)