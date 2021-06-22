# libraries ---------------------------------
library(here)
library(readxl)
library(tibble)
library(tidyr)
library(googlesheets4)

# import google spreadsheets -------
# still needed: grnb (trash), brim (duplicate link with dvag), dave and bnsh (wrong ncol)
# vicp: plot and treatment columns are in the wrong order

kenn_link <- "https://docs.google.com/spreadsheets/d/1cfyrfu98_nMB16XCsjO12uY1J0GNLXzrN086WLIq83o/edit?usp=sharing"
vicp_link <- "https://docs.google.com/spreadsheets/d/1H6s3-4BEg0H8IykFCL17MEKWniHyhqw42SKPMz6suLY/edit?usp=sharing"
timh_link <- "https://docs.google.com/spreadsheets/d/1nwzzRC8Sbc-wPsly5bcxUEIXzIRcI6ocCu4bCxP31Vc/edit?usp=sharing"

dvag_link <- "https://docs.google.com/spreadsheets/d/1gQ-6Ys7rwSuYWWDmO9SaJwg-ZyjSrgjJwC77-k2Hnzw/edit?usp=sharing"
ambj_link <- "https://docs.google.com/spreadsheets/d/1FDeG27I1YaTDxRRQ2FF5JMsrIcdi73uBvKQEuk0FS_A/edit?usp=sharing"
brim_link <- "https://docs.google.com/spreadsheets/d/1rpeGYKqBTUJxt47FTyJ-C2X9tNwJrJXZouZV34u201o/edit?usp=sharing"

grnb_link <- "https://docs.google.com/spreadsheets/d/1Ltt8BvqmoSwsJt6MqZdLDEB197nmJgNKHA2id7L70Fo/edit?usp=sharing"
bnsh_link <- "https://docs.google.com/spreadsheets/d/1eKDivEUqAReffqI75pDQzSIcs0462LBLkwHc-lNo0is/edit?usp=sharing"
dave_link <- "https://docs.google.com/spreadsheets/d/1jma3Hghv1w1FcfY01-tf1TjKuiuUDlf7pvPMnI3PAuI/edit?usp=sharing"

kenn <- read_sheet(kenn_link, sheet = "raw_data")
vicp <- read_sheet(vicp_link, sheet = "raw_data")
timh <- read_sheet(timh_link, sheet = "raw_data")

dvag <- read_sheet(dvag_link, sheet = "raw_data")
brim <- read_sheet(brim_link, sheet = "raw_data")
ambj <- read_sheet(ambj_link, sheet = "raw_data")

bnsh <- read_sheet(bnsh_link, sheet = "raw_data")
dave <- read_sheet(dave_link, sheet = "raw_data")
grnb <- read_sheet(grnb_link, sheet = "raw_data")

# import functions ----
source(here("src", "functions.R"))

# row-bind -----

coords <- do.call("rbind", list(
  kenn, vicp, timh,
  brim, ambj, dvag, 
  dave, bnsh, ambj)
)

# sanity check
str(coords)

# extract the columns with latitude and longitude

coords_lat <- coords[, 6]
coords_lon <- coords[, 7]

# split the lat and lon into columns for degrees and minutes

coords_lat1 <- separate(coords_lat, col = 1, into = c("ns", "chd", "chm"), sep = " ")
coords_lat2 <- separate(coords_lat1, col = 2, into = c("chd", "deg"), sep = "°")
coords_lat3 <- separate(coords_lat2, col = 4, into = c("chm", "deg"), sep = "'")

coords_lon1 <- separate(coords_lon, col = 1, into = c("ns", "chd", "chm"), sep = " ")
coords_lon2 <- separate(coords_lon1, col = 2, into = c("chd", "deg"), sep = "°")
coords_lon3 <- separate(coords_lon2, col = 4, into = c("chm", "deg"), sep = "'")

# create columns for DMS and convert the degrees, minutes, and seconds to numeric

dd_lat <- dms_col(coords_lat3, 2)
mm_lat <- dms_col(coords_lat3, 3)
ss_lat <- dms_col(coords_lat3, 4)
ns_lat <- c(coords_lat3[1])

dd_lon <- dms_col(coords_lon3, 2)
mm_lon <- dms_col(coords_lon3, 3)
ss_lon <- dms_col(coords_lon3, 4)
ns_lon <- c(coords_lon3[1])

# convert DMS to decimal degrees

dec_deg_lat <- mapply(FUN = dms_to_dd, degrees = dd_lat, minutes = mm_lat, seconds = ss_lat)
dec_deg_lon <- mapply(FUN = dms_to_dd, degrees = dd_lon, minutes = mm_lon, seconds = ss_lon)