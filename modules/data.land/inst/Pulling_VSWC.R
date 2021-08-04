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
depth <- ncdim_def(name = "depth", units = "meters" , vals = filter.date.daily$vswc)
time <- ncdim_def(name = "time", units = "yyyymmdd" , longname = "date", vals = as.numeric(gsub("-","", filter.date.daily$time)))

VSWC <- ncvar_def(name = "mass_fraction_of_unfrozen_water_in_soil_moisture", units = "volumetric soil water content", 
                  list(depth, time), missval = -999, longname = paste0(site_ID, "_", "VSWC"), prec = "float")

soilmoisture.nc <- nc_create(paste0(site_ID, "_", "VSWC.nc"), VSWC)

ncvar_put(nc=soilmoisture.nc, "mass_fraction_of_unfrozen_water_in_soil_moisture", vals = filter.date.daily$vswc)

print(soilmoisture.nc)

nc_close(soilmoisture.nc)

}
