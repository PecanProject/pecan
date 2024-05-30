#' Biomass and soil data from FluxNet sites
#'
#' Contains data from 246 Fluxnet sites.
#' Variables include aboveground and belowground biomass in various pools,
#'   plus soil texture/chemistry/horizonation/C&N stocks.
#'
#' @format ## `BADM`
#' A data frame with 12,300 rows and 13 columns:
#' \describe{
#'   \item{SITE_ID}{Fluxnet code for the site}
#'   \item{LOCATION_ELEV, LOCATION_LAT, LOCATION_LON}{site coordinates}
#'   \item{Date}{Measurement date}
#'   \item{GROUP_ID}{TODO}
#'   \item{VARIABLE_GROUP}{category, eg abovground biomass or soil chemistry}
#'   \item{VARIABLE, DATAVALUE}{key and value for each measured variable}
#'   \item{NA_L1CODE, NA_L1NAME, NA_L2CODE, NA_L2NAME}{
#'		numeric IDs and names for the Level 1 and level 2 ecoregions where
#'		this site is located}
#' }
#' @source Originally from Fluxnet <https://fluxnet.org/badm-data-product/>,
#'  but the provenence and age of this specific file is not clear.
"BADM"

#' Soil organic carbon (SOC) density based on eco-region level 2 code from the ISCN database.
#'
#' Contains 200 ensemble SOC data from 43 level 2 eco-regions across North America.
#' Variable include SOC densities in g/cm2.
#'
#' @format ## `iscn_soc`
#' A data frame with 200 rows and 43 columns:
#' \describe{
#'   \item{rows}{1 to 200 ensemble members}
#'   \item{columns}{43 level 2 ecoregion codes across North America}
#' }
#' @source https://iscn.fluxdata.org/wp-content/uploads/sites/23/2019/05/ISCN_ALL_DATA_DATASET_1-1.xlsx
"iscn_soc"


#' Default parameters for calculating soil properties from sand & clay content
#'
#'
#' @format ## `soil_class`
#' A list with 26 entries:
#' \describe{
#'   \item{air.cond, h2o.cond, sand.cond, silt.cond, clay.cond}{
#'		thermal conductivity, W m^-1 K^-1}
#'   \item{air.hcap, sand.hcap, silt.hcap, clay.hcap}{heat capacity,
#'		J m^-3 K^-1}
#'   \item{kair, ksand, ksilt, kclay}{relative conductivity factor}
#'   \item{fieldcp.K}{hydraulic conductance at field capacity, mm day^-1}
#'   \item{grav}{gravity acceleration, m s^-2}
#'   \item{soil.key}{Abbreviations for each of 18 soil texture classes, e.g.
#' 		"SiL", "LSa"}
#'   \item{soil.name}{Names for 18 soil texture classes, e.g. "Sand",
#'		"Silty clay"}
#'   \item{soilcp.MPa}{soil water potential when air-dry, MPa}
#'   \item{soilld.MPa}{soil water potential at critical water content, MPa}
#'   \item{soilwp.MPa}{soil water potential at wilting point, MPa}
#'   \item{stext.lines}{list of 18 lists, each giving minimum and maximum
#'	  	sand/silt/clay contents for a soil texture class}
#'   \item{stext.polygon}{list of 18 lists, each giving corner points in the
#'		soil texture triangle for a soil texture class}
#'   \item{texture}{data frame with 13 rows and 21 columns, giving default
#'		parameter values for 13 named soil textures}
#'   \item{theta.crit}{critical water content (fractional soil moisture at
#'		which plants start dropping leaves), m^3 m^-3}
#'   \item{xclay.def}{default volume fraction of sand in each of 18 soil
#'		texture classes}
#'   \item{xsand.def}{default volume fraction of clay in each of 18 soil
#'		texture classes}
#' }
#' @source
#' The hydraulic parameters are derived from Cosby et al 1984, "A Statistical
#' Exploration of the Relationships of Soil Moisture Characteristics to the
#' Physical Properties of Soils", Water Resources Research 20(6): 682-690.
#' This implementation comes from one provided by the ED2 model,
#' plus `texture.csv` from a source not recorded. Package `PEcAn.linkages`
#' contains an identical texture.csv, also with no obvious source label.
#' See also comments in soil_utils.R
"soil_class"
