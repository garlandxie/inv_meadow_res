#' Convert the coordinates from a tibble of character types into a vector of 
#' numeric types
#' 
#' @param data_to_convert the tibble that you want to convert
#' @param col_number the column number of the column you want to convert
#' @return The column as numeric types in a vector
#' @examples 
#' A vector containing the degrees for coords
#' as.numeric(unlist(c(coords_lat3[2])))
#' [1] 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43 43
#' [25] 43 43 43 43 43 43 43 43 43 43 43 43

dms_col <- function(data_to_convert, col_number) {
  dms <- as.numeric(unlist(c(data_to_convert[col_number])))
  return (dms)
}

#' Converts DMS to decimal degrees
#' 
#' @param degrees degrees as a double, if NA it is taken as 0
#' @param minutes minutes as a double, if NA it is taken as 0
#' @param seconds seconds as a double, if NA it is taken as 0
#' @return the degrees, minutes, and seconds converted to decimal degrees as a 
#' double
#' @examples 
#' dms_to_dd(75, 32, 21)
#' 74.53917
#' dms_to_dd(34, 72, NA)
#' 35.2

dms_to_dd <- function(degrees, minutes, seconds) {
  if(is.na(degrees) == TRUE) {degrees = 0} else{degrees}
  if(is.na(minutes) == TRUE) {minutes = 0} else{minutes}
  if(is.na(seconds) == TRUE) {seconds = 0} else{seconds}
  converted <- degrees + (minutes/60) + (seconds/3600)
  return(converted)
}

