# Script pulling soil moisture data from target files to create a netCDF file at the end
## vswc = mass_fraction_of_unfrozen_water_in_soil_moisture

# loading necessary packages
library(readr)
library(dplyr)
library(ncdf4)

# set function inputs
homedir <- "/projectnb/dietzelab/cfranci1"
site_ID <- "BART"

call_vswc <- function(homedir, site_ID) {

# reading in terrestrial targets data at daily level
download.file("https://data.ecoforecast.org/targets/terrestrial/terrestrial_daily-targets.csv.gz",
              "terrestrial_daily-targets.csv.gz")

terr_daily <- read_csv(file.path(homedir, "terrestrial_daily-targets.csv.gz"),
  col_types = cols(
  time = col_datetime(format = ""),
  siteID = col_character(),
  nee = col_double(),
  le = col_double(),
  vswc = col_double(),
  vswc_sd = col_double()
))


## subset out rows that contain vswc data
 non_na_daily <- terr_daily[complete.cases(terr_daily$vswc), ] %>% 
   mutate(time = as.Date(time))

## find the most recent date with vswc data for each site at the daily target level
non_na_daily <- non_na_daily %>% 
  filter(siteID == site_ID)

max_time <- max(non_na_daily$time)


# extracting a VSWC value based on date and siteID columns
filter.date.daily <- dplyr::filter(non_na_daily, time == max_time & siteID == site_ID)
print(filter.date.daily)

# save the lastest vswc value into object and turn this into a netCDF
VSWC_dim <- ncdim_def(name = "VSWC", units = "mass fraction of unfrozen water in soil moisture" , vals = filter.date.daily$vswc)
VSWC_def <- ncvar_def(name = "volumetric soil water content", units = "mass fraction of unfrozen water in soil moisture", dim = VSWC_dim, 
                      missval = -999, longname = paste0(site_ID, "_", "VSWC"), prec = "integer")
soilmoisture.nc <- nc_create(paste0(site_ID, "_", "VSWC"), VSWC_def)
print(soilmoisture.nc)

}

# terrestrial targets data produce the same vswc values at the daily and 30-minute target level
############# doing the same process again, but at the 30-min terrestrial target level #############

# reading in terrestrial targets data at 30-min level
terr_30min <- read_csv("/projectnb/dietzelab/cfranci1/Targets/terrestrial/terrestrial_30min-targets.csv.gz", guess_max = 10000, 
  col_types = cols(
  time = col_datetime(format = ""),
  siteID = col_character(),
  nee = col_double(),
  le = col_double(),
  nee_sd_intercept = col_double(),
  nee_sd_slopeP = col_double(),
  nee_sd_slopeN = col_double(),
  le_sd_intercept = col_double(),
  le_sd_slopeP = col_double(),
  le_sd_slopeN = col_double(),
  vswc = col_double(),
  vswc_sd = col_double()
))

# review 30-min targets
summary(terr_30min)
## view dates that contain vswc data
non_na_30min <- terr_daily[complete.cases(terr_daily$vswc), ] %>% 
  mutate(time = as.Date(non_na_daily$time))

## find date range (most recent date) with vswc data for each site at the 30-min target level
non_na_30min_konz <- non_na_30min %>% 
  filter(siteID == "KONZ")
range(non_na_30min_konz$time)

KONZ = 2021-01-20

non_na_30min_bart <- non_na_30min %>% 
  filter(siteID == "BART")
range(non_na_30min_bart$time)

BART = 2021-06-28

non_na_30min_srer <- non_na_30min %>% 
  filter(siteID == "SRER")
range(non_na_30min_srer$time)

SRER = 2021-02-21

non_na_30min_osbs <- non_na_30min %>% 
  filter(siteID == "OSBS")
range(non_na_30min_osbs$time)

OSBS = 2020-12-08

# extracting a VSWC value based on date and siteID columns
filter.date.30min.KONZ <- dplyr::filter(non_na_30min, time == "2021-01-20" & siteID == "KONZ")
filter.date.30min.KONZ

filter.date.30min.BART <- dplyr::filter(non_na_30min, time == "2021-06-28" & siteID == "BART")
filter.date.30min.BART 

filter.date.30min.SRER <- dplyr::filter(non_na_30min, time == "2021-02-21" & siteID == "SRER")
filter.date.30min.SRER

filter.date.30min.OSBS <- dplyr::filter(non_na_30min, time == "2020-12-08" & siteID == "OSBS")
filter.date.30min.OSBS 

