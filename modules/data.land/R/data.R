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

#' Fertilizer Nutrient Composition Table
#' 
#' A dataset of fertilizer and organic matter addition types 
#' and their nitrogen and carbon composition, based on the SWAT model's
#' `fertilizer.frt` table and DayCent model defaults for organic matter
#' C:N ratio parameters.
#'
#' @format A tibble with one row per fertilizer type and the following columns:
#' \describe{
#'   \item{name}{\code{character}. Short identifier from SWAT (e.g., \code{"urea"}, \code{"manure"}).}
#'   \item{description}{\code{character}. Longer description of the fertilizer or manure type.}
#'   \item{fraction_mineral_n}{\code{numeric}. Fraction of total nitrogen in mineral form.}
#'   \item{fraction_nh3_n}{\code{numeric}. Fraction of fertilizer by mass that is ammonium-n (NH\eqn{_3}-N).}
#'   \item{fraction_no3_n}{\code{numeric}. Fraction of fertilizer by mass that is nitrate-N (NO\eqn{_3}-N).
#'     Computed as \code{fraction_mineral_n - fraction_nh3_n}.}
#'   \item{fraction_organic_n}{\code{numeric}. Fraction of organic matter that is nitrogen.}
#'   \item{fraction_c}{\code{numeric}. Fraction of mass that is carbon.}
#'   \item{cn_ratio}{\code{numeric}. Carbon-to-nitrogen ratio for organic matter.
#'     Assigned based on DayCent organic matter parameterterizations.}
#' }
#'
#' @details
#' This table is based on SWAT model's \code{fertilizer.frt} file, and uses 
#' C:N ratios (\code{cn_ratio}) from DayCent model default parameter files.
#' \code{fraction_nh3_n} and \code{fraction_no3_n} represent the fraction of 
#' fertilizer by mass that is ammonium-N and nitrate-N, respectively. This is different from
#' the SWAT model's definition of \code{fraction_nh3_n} as a fraction of the total mineral N.
#'
#' @source https://github.com/swat-model/swatplus
#' @source DayCent model default parameter file: `omad.100` obtained from the Soil Carbon Solutions Center, https://www.soilcarbonsolutionscenter.com
"fertilizer_composition_data"

#' LandIQ crop mapping codes
#'
#' LandIQ land-use class and subclass labels used for crop mapping.
#'
#' @format ## `landiq_crop_mapping_codes`
#' A data frame with 203 rows and 4 columns:
#' \describe{
#'   \item{CLASS}{LandIQ class code.}
#'   \item{class_name}{LandIQ class name.}
#'   \item{SUBCLASS}{LandIQ subclass code.}
#'   \item{subclass_name}{LandIQ subclass name.}
#' }
#' @source California Department of Water Resources. (2023). Statewide Crop Mapping—California 
#' Natural Resources Agency Open Data. Metadata retrieved from https://data.cnra.ca.gov/dataset/statewide-crop-mapping and manually extracted into `data-raw/landiq_crop_mapping_codes.tsv`.
"landiq_crop_mapping_codes"

#' BIS crop coefficients by crop
#'
#' Crop and growth stage specific coefficients (Kc) from the Basic Irrigation Scheduling
#' (BIS) Excel workbook (Snyder et. al., 2014).
#' The dataset is an export of the BISm.xlsx workbook's `CropRef` worksheet, with columns renamed
#' and columns added that map to LandIQ CADWR land use dataset 
#' (\code{\link{landiq_crop_mapping_codes}}; California Department of Water Resources, 2023).
#' This dataset provides the information needed to reconstruct a stage-based daily Kc curve when
#' combined with grass-reference evapotranspiration (ETo), such as that provided
#' by CIMIS (California Department of Water Resources, 2025).
#
#' @format A data frame with one row per crop and the following columns:
#' \describe{
#'   \item{crop_number}{Numeric crop identifier used internally by BIS.}
#'   \item{crop_name}{Crop name as listed in the `CropRef` worksheet.}
#'   \item{percent_season_B}{Percent-of-season location of growth date B.}
#'   \item{percent_season_C}{Percent-of-season location of growth date C.}
#'   \item{percent_season_D}{Percent-of-season location of growth date D.}
#'   \item{KcB}{Crop coefficient at growth date B (approximately Kc1 for field crops).}
#'   \item{KcC}{Crop coefficient at growth date C (mid-season plateau, Kc2).}
#'   \item{KcD}{Crop coefficient at growth date D (typically equal to KcC for most crops).}
#'   \item{KcE}{Crop coefficient at growth date E (late-season value, Kc3).}
#'   \item{planting_month}{Representative planting month used by BIS.}
#'   \item{planting_day}{Representative planting day used by BIS.}
#'   \item{harvest_month}{Representative harvest month used by BIS.}
#'   \item{harvest_day}{Representative harvest day used by BIS.}
#'   \item{landiq_class}{LandIQ class code matched by BISm crop number.}
#'   \item{landiq_subclass}{LandIQ subclass code matched by BISm crop number.}
#'   \item{landiq_subclass_name}{LandIQ subclass name matched by BISm crop number.}
#' }
#'
#' @details
#' BIS follows the crop-coefficient framework of Doorenbos and Pruitt (1977),
#' in which maximum crop evapotranspiration is calculated as
#' \deqn{ETc = Kc \times ETo.}
#'
#' Rather than specifying fixed durations for growth stages, BIS expresses
#' the locations of key growth dates (B, C, and D) as percentages of the total
#' season length between planting (A) and harvest or dormancy (E). Daily Kc values
#' are obtained by linear interpolation between the stage-specific coefficients
#' stored in this dataset.
#'
#' Growth-stage interpretation depends on crop type:
#'
#' \itemize{
#'   \item \strong{Field and row crops (Type 1):} A-B corresponds to initial growth
#'   from planting to roughly 10\% ground cover; B-C represents rapid canopy
#'   development with Kc increasing toward its mid-season value; C-D is the
#'   mid-season period at near-maximum Kc (typically around 75\% ground cover);
#'   and D-E represents late-season senescence, during which Kc may decline.
#'
#'   \item \strong{Deciduous tree and vine crops (Type 3):} there is no explicit
#'   initial A-B period; the season begins at leaf-out (B). Kc increases during
#'   B-C as the canopy develops, reaches a maximum at approximately 61-63\%
#'   ground cover during C-D, and declines during D-E toward leaf drop or the
#'   first hard freeze.
#' }
#'
#' @source
#' Snyder, R., Orang, M., Bali, K., Eching, S., Zaccaria, D. (2014).
#' \emph{BISm Basic Irrigation Scheduling Excel program (metric units)}.
#'
#' @references
#' Doorenbos, J., Pruitt, W.O. (1977).
#' \emph{Guidelines for predicting crop water requirements}.
#' FAO Irrigation and Drainage Paper 24.
#'
#' Snyder, R.L., Shackel, K.A., Sanden, B., Fulton, A.E., Suvočarev, K. (2024).
#' Irrigation scheduling. In \emph{Microirrigation for Crop Production}. Elsevier.
#'
#' California Department of Water Resources (2025).
#' \emph{California Irrigation Management Information System (CIMIS)}.
#'
#' @examples
#' data(bism_kc_by_crop)
#' head(bism_kc_by_crop)
#'
#' @keywords datasets
"bism_kc_by_crop"
