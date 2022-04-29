# libraries ----
library(here)
library(sf)
library(ggplot2)
library(dplyr)

# custom functions ----

source(here("src", "functions.R"))

# import: plot coordinates ----

plot_coords <- read.csv(
  here("data", "intermediate_data", "plot_coords.csv"), 
  row.names = 1
)

# import: seed rain ----

## site: KENN ----

# There are only two way-points for seed rain at this site
# So, remove this site from any subsequent analyses
# But, it is worth writing a few comments about this in the preprint/paper

## site: GRNB ----

# I think GRNB 1 was a test run for GPS device
# coordinates do not make any sense with reference to the actual site

sr_grnb_2 <- import_gpx("seed_rain_GRNB_waypoints_21-JUL-21_2.gpx")
sr_grnb_3 <- import_gpx("seed_rain_GRNB_waypoints_22-JUL-21_3.gpx")
sr_grnb_4 <- import_gpx("seed_rain_GRNB_waypoints_23-JUL-21_4.gpx")

## site: BNSH ----
sr_bnsh_1 <- import_gpx("seed_rain_BNSH_waypoints_04-AUG-21_1.gpx")
sr_bnsh_2 <- import_gpx("seed_rain_BNSH_waypoints_04-AUG-21_2.gpx")

## site: DAVE ----
sr_dave <- import_gpx("seed_rain_DAVE_waypoints_20-AUG-21.gpx")

# data clean ----

nad83_utm17n <- 26917

sr_tidy <- rbind(
  sr_grnb_2, sr_grnb_3, sr_grnb_4,
  sr_bnsh_1, sr_bnsh_2,
  sr_dave) %>%
  sf::st_transform(crs = nad83_utm17n)

# data clean: plot coordinates -----

plot_coords_sf <- st_as_sf(
  plot_coords, 
  coords = c("lon_dd", "lat_dd"), 
  crs = 4326)

plot_coords_utm <- st_transform(
  plot_coords_sf, 
  crs = nad83_utm17n
  )

# calculate seed rain index -----

# this code is sub-optimal but it works!
seed_rain <- vector()
for (plot in 1:nrow(plot_coords_utm)) {
  seed_rain <- c(
    seed_rain, 
    calc_seed_rain_index(plot_coords_utm[plot,], sr_tidy)
    )
}

# clean data ----

seed_rain_df <- plot_coords %>%
  mutate(seed_rain_dsv = seed_rain) %>%
  select(section, treatment, site, plot, seed_rain_dsv)

# save to disk ----

write.csv(
  x = seed_rain_df, 
  here("data", "intermediate_data", "seed_rain_tidy.csv")
)


