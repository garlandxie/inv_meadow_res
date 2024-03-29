# TO DO: add roxygen documentation

import_gpx <- function(link) {
  link2 <- here("data", "input_data", "seed_rain", link)
  sf_seed_rain <- sf::st_read(link2, layer = "waypoints")
  return(sf_seed_rain)
}

calc_seed_rain_index<- function(plot, seed_rain, distance = 10) {
  
  # create buffer
  buffer <- st_buffer(plot, dist = distance)
  
  # clip seed rain to a specific buffer
  clip_sr <- st_intersection(seed_rain, buffer)
  
  # calculate matrix of distances from focal point to seed rain
  distance_sr <- st_distance(plot, clip_sr)
  distance_sr <- as.numeric(distance_sr)
  
  # calculate seed rain index
  dispersal_coef <- 2.870326
  
  # demographic parameters
  # key reference: Averill et al. 2017. 10.1614/IPSM-D-10-00034.1
  fecundity <- 130 
  seed_rain <- fecundity*exp((-dispersal_coef)*distance_sr)
  seed_rain_index <- sum(seed_rain)
  
  return(seed_rain_index)
  
}



