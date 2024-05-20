#' Title Identify pft for each site of a multi-site settings using NLCD and Eco-region
#'
#' @param settings a multi-site settings
#' @param Ecoregion path of Ecoregion data (*.shp)
#' @param NLCD path of NLCD img file
#' @param con connection to bety
#'
#' @return pft info with each site
#'
#' @examples
#' \dontrun{
#'  NLCD <- file.path(
#'    "/fs", "data1", "pecan.data", "input",
#'    "nlcd_2001_landcover_2011_edition_2014_10_10",
#'    "nlcd_2001_landcover_2011_edition_2014_10_10.img")
#'  Ecoregion <- file.path(
#'    "/projectnb", "dietzelab", "dongchen",
#'    "All_NEON_SDA", "NEON42", "eco-region", "us_eco_l3_state_boundaries.shp")
#'  settings <- PEcAn.settings::read.settings(
#'    "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/pecan.xml")
#'  con <- PEcAn.DB::db.open(settings$database$bety)
#'    site_pft_info <- Create_Site_PFT_CSV(settings, Ecoregion, NLCD, con)
#' }
#'
#' @export
Create_Site_PFT_CSV <- function(settings, Ecoregion, NLCD, con){

  # Bail out if packages in Suggests not available
  suggests_needed <- c("glue", "raster")
  suggests_found <- sapply(suggests_needed, requireNamespace, quietly = TRUE)
  if (!all(suggests_found)) {
    PEcAn.logger::logger.error(
      "Can't find package(s)",
      sQuote(suggests_needed[!suggests_found]),
      ", needed by PEcAnAssimSequential::Create_Site_PFT_CSV().",
      "Please install these and try again.")
  }

  #grab Site IDs from settings
  observations <- c()
  for (i in 1:length(settings)) {
    obs <- settings[[i]]$run$site$id
    observations <- c(observations,obs)
  }
  
  
  site_ID <- observations
  suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                              ids = site_ID, .con = con))
  suppressWarnings(qry_results <- PEcAn.DB::db.query(site_qry, con))
  site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                    lon=qry_results$lon, time_zone=qry_results$time_zone)
  
  #initialize data pool for NLCD
  sites <- as.data.frame(cbind(site_info$site_id,site_info$lon, site_info$lat))
  names(sites) <- c("id", "lon", "lat")
  sp::coordinates(sites) <- ~lon+lat
  raster::projection(sites) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
  cover <- raster::raster(NLCD)
  
  #
  sites = sp::spTransform(sites, CRS = raster::crs(cover))
  # make sure projections match
  data = raster::extract(cover, sites)
  sites$cover = data
  site_data = sites
  
  ecoregion = raster::shapefile(Ecoregion)
  ecoregion = sp::spTransform(ecoregion, CRS = raster::crs(site_data))
  eco_data = raster::extract(ecoregion, site_data)
  site_data$region = eco_data$NA_L1CODE
  site_data$name = eco_data$NA_L1NAME
  
  site_data = as.data.frame(site_data)
  names(site_data) = c("ID", "cover", "ecoregion", "name", "lon", "lat")
  site_data$pft = NA
  site_data$cover = as.numeric(site_data$cover)
  site_data$ecoregion = as.numeric(site_data$ecoregion)
  # remove sites that are categorized as unclassified, water, ice/snow, barren
  index = which(site_data$cover == 0 | site_data$cover == 11 | site_data$cover == 12 | site_data$cover == 31)
  site_data$pft[index] = NA
  
  # classify deciduous
  index = which(site_data$cover == 41)
  site_data$pft[index] = "deciduous"
  
  
  # classify evergreen/conifer
  index = which(site_data$cover == 42)
  site_data$pft[index] = "conifer"
  
  
  # classify mixed forest
  index = which(site_data$cover == 43)
  site_data$pft[index] = "mixed forest"
  
  # classify developed
  index = which(site_data$cover == 21 | site_data$cover == 22 | site_data$cover == 23 | site_data$cover == 24)
  site_data$pft[index] = "developed"
  
  # classify shrub/scrub
  index = which(site_data$cover == 52 & (site_data$ecoregion == 10 | site_data$ecoregion == 11 | site_data$ecoregion == 12 | site_data$ecoregion == 13 | site_data$ecoregion == 14))
  site_data$pft[index] = "arid grassland"
  
  index = which(site_data$cover == 52 & (site_data$ecoregion == 9 | site_data$ecoregion == 8 | site_data$ecoregion == 6 | site_data$ecoregion == 7))
  site_data$pft[index] = "mesic grassland"
  
  
  # classify herbaceous
  index = which(site_data$cover == 71 & (site_data$ecoregion == 10 | site_data$ecoregion == 11 | site_data$ecoregion == 12 | site_data$ecoregion == 13 | site_data$ecoregion == 14))
  site_data$pft[index] = "arid grassland"
  
  index = which(site_data$cover == 71 & (site_data$ecoregion == 9 | site_data$ecoregion == 15 | site_data$ecoregion == 7 | site_data$ecoregion == 8 | site_data$ecoregion == 5 | site_data$ecoregion == 6))
  site_data$pft[index] = "mesic grassland"
  
  
  # classify hay/pasture crops
  index = which((site_data$cover == 81 | site_data$cover == 82) & (site_data$ecoregion == 10 | site_data$ecoregion == 11 | site_data$ecoregion == 12 | site_data$ecoregion == 13 | site_data$ecoregion == 14))
  site_data$pft[index] = "arid grassland"
  
  index = which((site_data$cover == 81 | site_data$cover == 82) & (site_data$ecoregion == 9 | site_data$ecoregion == 8 | site_data$ecoregion == 7))
  site_data$pft[index] = "mesic grassland"
  
  # classify wetlands
  index = which(site_data$cover == 95)
  site_data$pft[index] = "mesic grassland"
  
  index = which(site_data$cover == 90)
  site_data$pft[index] = "woody wetland"
  
  # #rename temporally 
  # site_data$pft[site_data$pft=='mesic grassland'] <- 'semiarid.grassland_HPDA'
  # site_data$pft[site_data$pft=='woody wetland'] <- 'semiarid.grassland_HPDA'
  # site_data$pft[site_data$pft=='mixed forest'] <- 'temperate.deciduous.HPDA'
  # site_data$pft[site_data$pft=='deciduous'] <- 'temperate.deciduous.HPDA'
  # site_data$pft[site_data$pft=='conifer'] <- 'boreal.coniferous'
  # site_data$pft[site_data$pft=='arid grassland'] <- 'semiarid.grassland_HPDA'
  
  #write into csv file
  out.csv <- cbind(site_data$ID, site_data$pft)
  colnames(out.csv) <- c("site", "pft")
  utils::write.csv(out.csv, file = paste0(settings$outdir,"/site_pft.csv"), row.names=FALSE)
  return(site_data)
}