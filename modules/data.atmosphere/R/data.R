
## Drafts of documentation for package datasets
##
## Written by CKB 2020-05-03, then commented out when I realized that as
##  written we need to enable lazy-loading of package data to use these.
## TODO in this order:
##  * Inspect all datasets, determine whether lazy-loading them into package
##    namespace will cause any issues
##  * make any changes needed to resolve issues identified above
##  * Change DESCRIPTION line to read `LazyData: true`
##  * Uncomment this file, delete this header block
##  * run Roxygen, commit resulting Rd files

# #' 2010 CRUNCEP weather data for Urbana, IL
# #'
# #' Hourly 2010 meteorology for the 0.5-degree grid cell containing the
# #'  EBI Energy Farm (Urbana, IL), as obtained from the CRUNCEP
# #'  6-hourly product.
# #' Please see the `compare_narr_cruncep_met` vignette for details of a
# #'  comparison between this and the `narr`, `narr3h`, and `ebifarm` datasets.
# #'
# #' @format A data frame with 8736 rows and 10 columns:
# #'  \describe{
# #'    \item{date}{POSIXct timestamp}
# #'    \item{year, doy, hour}{integer, extracted from `date`}
# #'    \item{solarR}{solar radiation, in umol/h/m2}
# #'    \item{DailyTemp.C}{air temperature, in degrees C}
# #'    \item{RH}{relative humidity, in percent}
# #'    \item{WindSpeed}{wind speed, in m/s}
# #'    \item{precip}{precipitation rate, in mm/h}
# #'    \item{source}{dataset identifier, in this case always "cruncep"}}
# #' @seealso \code{\link{narr}} \code{\link{narr3h}} \code{\link{ebifarm}}
# "cruncep"
# 
# 
# #' Global 0.5 degree land/water mask for the CRUNCEP dataset
# #'
# #' For details, please see the CRUNCEP scripts included with this package:
# #'  `system.file("scripts/cruncep", package = "PEcAn.data.atmosphere")`
# #'
# #' @format a data frame with 259200 rows and 3 columns:
# #'  \describe{
# #'    \item{lat}{latitude, in decimal degrees}
# #'    \item{lon}{longitude, in decimal degrees}
# #'    \item{land}{logical. TRUE = land, FALSE = water}}
# "cruncep_landmask"
# 
# 
# #' 2010 weather station data from near Urbana, IL
# #'
# #' Hourly 2010 weather data collected at the EBI Energy Farm (Urbana, IL).
# #' Please see the `compare_narr_cruncep_met` vignette for details of a
# #'  comparison between this and the `narr`, `narr3h`, and `cruncep` datasets.
# #'
# #' @format A data frame with 8390 rows and 10 columns:
# #'  \describe{
# #'    \item{date}{POSIXct timestamp}
# #'    \item{year, doy, hour}{integer, extracted from `date`}
# #'    \item{Temp}{air temperature, in degrees C}
# #'    \item{RH}{relative humidity, in percent}
# #'    \item{precip}{precipitation rate, in mm/h}
# #'    \item{wind}{wind speed, in m/s}
# #'    \item{solar}{solar radiation, in umol/h/m2}
# #'    \item{source}{dataset identifier, in this case always "ebifarm"}}
# #' @seealso  \code{\link{cruncep}} \code{\link{narr}} \code{\link{narr3h}}
# "ebifarm"
# 
# 
# #' Codes and BeTY IDs for sites in the FLUXNET network
# #'
# #' @format a data frame with 698 rows and 2 columns:
# #'  \describe{
# #'    \item{FLUX.id}{character identifier used by FLUXNET,
# #'      e.g. Niwot Ridge USA is `US-NR1`}
# #'    \item{site.id}{identifier used in the `sites` table of the PEcAn
# #'      database. Integer, but stored as character}}
# "FLUXNET.sitemap"
# 
# 
# #' Global land/water mask for the NCEP dataset
# #'
# #' For details, please see the NCEP scripts included with this package:
# #'  `system.file("scripts/ncep", package = "PEcAn.data.atmosphere")`
# #'
# #' @format a data frame with 18048 rows and 3 columns:
# #'  \describe{
# #'    \item{lat}{latitude, in decimal degrees}
# #'    \item{lon}{longitude, in decimal degrees}
# #'    \item{land}{logical. TRUE = land, FALSE = water}}
# "landmask"
# 
# 
# #' Latitudes of 94 sites from the NCEP dataset
# #'
# #' For details, please see the NCEP scripts included with this package:
# #'  `system.file("scripts/ncep", package = "PEcAn.data.atmosphere")`
# #'
# #' @format a vector of 94 decimal values
# "Lat"
# 
# 
# #' Longitudes of 192 sites from the NCEP dataset
# #'
# #' For details, please see the NCEP scripts included with this package:
# #'  `system.file("scripts/ncep", package = "PEcAn.data.atmosphere")`
# #'
# #' @format a vector of 192 decimal values
# "Lon"
# 
# 
# #' 2010 NARR weather data for Urbana, IL
# #'
# #' Hourly 2010 meteorology for the 0.3-degree grid cell containing the
# #'  EBI Energy Farm (Urbana, IL), as obtained from the NARR daily product.
# #' Please see the `compare_narr_cruncep_met` vignette for details of a
# #'  comparison between this and the `cruncep`, `narr3h`, and `ebifarm` datasets.
# #'
# #' @format A data frame with 8760 rows and 10 columns:
# #'  \describe{
# #'    \item{date}{POSIXct timestamp}
# #'    \item{year, doy, hour}{integer, extracted from `date`}
# #'    \item{SolarR}{solar radiation, in umol/h/m2}
# #'    \item{Temp}{air temperature, in degrees C}
# #'    \item{RH}{relative humidity, in percent}
# #'    \item{WS}{wind speed, in m/s}
# #'    \item{precip}{precipitation rate, in mm/h}
# #'    \item{source}{dataset identifier, in this case always "narr"}}
# #' @seealso  \code{\link{cruncep}} \code{\link{ebifarm}} \code{\link{narr3h}}
# "narr"
# 
# 
# #' 2010 NARR 3-hourly weather data for Urbana, IL
# #'
# #' Hourly 2010 meteorology for the 0.25-degree grid cell containing the
# #'  EBI Energy Farm (Urbana, IL), as obtained from the NARR 3-hourly product.
# #' Please see the `compare_narr_cruncep_met` vignette for details of a
# #'  comparison between this and the `cruncep`, `narr`, and `ebifarm` datasets.
# #'
# #' @format A data frame with 8736 rows and 10 columns:
# #'  \describe{
# #'    \item{date}{POSIXct timestamp}
# #'    \item{year, doy, hour}{integer, extracted from `date`}
# #'    \item{solarR}{solar radiation, in umol/h/m2}
# #'    \item{DailyTemp.C}{air temperature, in degrees C}
# #'    \item{RH}{relative humidity, in percent}
# #'    \item{WindSpeed}{wind speed, in m/s}
# #'    \item{precip}{precipitation rate, in mm/h}
# #'    \item{source}{dataset identifier, in this case always "narr3h"}}
# #' @seealso  \code{\link{cruncep}} \code{\link{ebifarm}} \code{\link{narr}}
# "narr3h"
