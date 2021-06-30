# libraries ----
library(ggplot2)
library(sf)
library(here)

# import ----

sections <- sf::read_sf(
  here("data/original/gis/meadoway_sections",
       "Meadoway.shp")
)

coords <- read.csv(
  here("data/original/gis", 
       "coordinates_dec_deg.csv"),
  stringsAsFactors = FALSE
)


st_crs(sections)
x <- sf::st_as_sf(coords, coords = c("Longitude","Latitude"),remove=F)


plot(sections$geometry)
plot(x$geometry,add=T,col="red")
plot(x$geometry)
plot(sections$geometry,add=T)
