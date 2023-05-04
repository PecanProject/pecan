#' Download Soil Moisture (VSWC)
#' @title download_vswc
#' @name download_vswc
#' @param outdir location where you want the soil moisture ncdf file saved
#' @param site_ID the NEON site ID
#'
#' @return a ncdf file with the soil moisture value for the most recent available day
#' @author Christina Francis and Alexis Helgeson
#'
#' @examples 
#' outdir <- "/projectnb/dietzelab/cfranci1/soilmoist" 
#' site_ID <- "BART"

download_vswc <- function(outdir, site_ID) {

# reading in terrestrial targets data at daily level
PEcAn.utils::download_file("https://data.ecoforecast.org/targets/terrestrial/terrestrial_daily-targets.csv.gz",
              "terrestrial_daily-targets.csv.gz")

terr_daily <- read.csv(file.path(outdir, "terrestrial_daily-targets.csv.gz"),
  col_types = cols(
  time = col_datetime(format = ""),
  siteID = col_character(),
  nee = col_double(),
  le = col_double(),
  vswc = col_double(),
  vswc_sd = col_double()
))


## subset out rows that contain vswc data
 non_na_daily <- terr_daily[stats::complete.cases(terr_daily$vswc), ] %>% 
   dplyr::mutate(time = as.Date(time))

## find the most recent date with vswc data for each site at the daily target level
non_na_daily <- non_na_daily %>% 
  dplyr::filter(siteID == site_ID)

max_time <- max(non_na_daily$time)


# extracting a VSWC value based on date and siteID columns
filter.date.daily <- dplyr::filter(non_na_daily, time == max_time & siteID == site_ID)
print(filter.date.daily)

# save the lastest vswc value into object and turn this into a netCDF
depth <- ncdf4::ncdim_def(name = "depth", units = "meters" , vals = filter.date.daily$vswc)
time <- ncdf4::ncdim_def(name = "time", units = "yyyymmdd" , longname = "date", vals = as.numeric(gsub("-","", filter.date.daily$time)))

VSWC <- ncdf4::ncvar_def(name = "mass_fraction_of_unfrozen_water_in_soil_moisture", units = "volumetric soil water content", 
                  list(depth, time), missval = -999, longname = paste0(site_ID, "_", "VSWC"), prec = "float")

soilmoisture.nc <- ncdf4::nc_create(paste0(site_ID, "_", "VSWC.nc"), VSWC)

ncdf4::ncvar_put(nc=soilmoisture.nc, "mass_fraction_of_unfrozen_water_in_soil_moisture", vals = filter.date.daily$vswc)

print(soilmoisture.nc)

ncdf4::nc_close(soilmoisture.nc)

}
