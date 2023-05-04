##'@name download_SMAP_gee2pecan.R
##'@description: 
##'Download SMAP data from GEE by date and site location
##'
##'Requires python3 and earthengine-api. 
##'Untill 'gee2pecan_smap' is integrated into PEcAn workflow,
##'follow GEE registration 'Installation Instructions' here:
##'https://github.com/PecanProject/pecan/pull/2645
##'
##'@param start  start date as YYYY-mm-dd (chr)
##'@param end  end date YYYY-mm-dd (chr)
##'@param site_info list of site info containing name (String), site_id (numeric), lat (numeric), and lon (numeric)
##'@param geoJSON_outdir  directory to store site GeoJSON, must be the location same as 'gee2pecan_smap.py'
##'@param smap_outdir  directory to store netCDF file of SMAP data, if directory folder does not exist it will be created
##'@return data.frame of SMAP data
##'
##'
##'@authors Juliette Bateman, Ayush Prasad (gee2pecan_smap.py), Joshua Bowers
##'
##'@examples
##'\dontrun{
##'test <- download_SMAP_from_gee(
##'start = "2019-11-01",
##'end = "2019-11-10",
##'site_info = list(site_id = 1126, name = "Harvard_Forest", lat = 42.531453, lon = -72.188896),
##'geoJSON_outdir = '/projectnb/dietzelab/jbowers1/geoFiles/',
##'smap_outdir = '/projectnb/dietzelab/jbowers1/smap_ncFiles/')
##'}


download_SMAP_gee2pecan <- function(start, end,
                                   site_info, 
                                   geoJSON_outdir, smap_outdir) {
  
  ## if site_info is only one id, connect to database and collect into  
  #################### Connect to BETY #################### 
  # 
  # con <- PEcAn.DB::db.open(
  #   list(user='bety', password='bety', host='localhost',
  #        dbname='bety', driver='PostgreSQL',write=TRUE))
  # site_ID <- as.character(site_id)
  # suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
  #                                             ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
  #                                             ids = site_ID, .con = con))
  # suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
  # suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
  # site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
  #                   lon=qry_results$lon, time_zone=qry_results$time_zone)
  # 
  
  #################### Begin Data Extraction #################### 
  
  # Create geoJSON file for site
  site_GeoJSON <- data.frame(site_info$lon, site_info$lat) %>%
    setNames(c("lon","lat")) %>% 
    leafletR::toGeoJSON(name = site_info$name, dest = geoJSON_outdir, overwrite = TRUE) %>%
    rgdal::readOGR()
  site_GeoJSON$name = site_info$name
  site_GeoJSON = site_GeoJSON[-1] %>%
    leafletR::toGeoJSON(name = site_info$name, dest = geoJSON_outdir, overwrite = TRUE)
  
  # Locate gee2pecan_smap.py function and load into 
  reticulate::source_python('~/pecan/modules/data.remote/inst/RpTools/RpTools/gee2pecan_smap.py')
  
  ## code taken out of this line of code (var = var ## an arg of gee2pecan_smap)
  var_filename <- paste0('smap_', site_info$name)
  nc.file <- gee2pecan_smap(geofile = site_GeoJSON, outdir = smap_outdir, 
                            filename = var_filename, start = start, end = end)
  
  # Run gee2pecan_smap function
  output <- nc_open(nc.file)
  smap.data = cbind((ncdf4::ncvar_get(output, "date")), ncdf4::ncvar_get(output, "ssm"), 
                    ncdf4::ncvar_get(output,"susm"), ncdf4::ncvar_get(output, "smp"), 
                    ncdf4::ncvar_get(output, "ssma"), ncdf4::ncvar_get(output,"susma")) %>%
    as.data.frame(stringsAsFactors = FALSE) %>% 
    setNames(c("Date", "ssm", "susm", "smp", "ssma", "susma")) %>%
    dplyr::mutate(Date = as.Date(Date)) %>% 
    dplyr::mutate_if(is.character, as.numeric) %>%
    tidyr::complete(Date = seq.Date(as.Date(start), as.Date(end), by="day"))
  
  #################### Convert to % Soil Moisture ####################
  
  ## If variable is ssm or susm, must convert unit from mm --> % 
  # SSM (surface soil moisture) represents top 0-5cm (50mm) of soil
  smap.data$ssm.vol = unlist((smap.data[,2] / 50) * 100) %>% as.numeric()
  # SUSM (subsurface soil moisture) represents top 0-100 cm (1000mm) of soil
  smap.data$susm.vol = unlist((smap.data[,2] / 1000) * 100) %>% as.numeric()
  
  
  #################### Date Entry Parameter Check #################### 
  
  ## Check if there is data for the date range entered
  if (all(is.na(smap.data[-1])) == TRUE) {
    
    PEcAn.logger::logger.error(
      "There are no SMAP data observations for this date range (", start, " to ", end,
      "), Please choose another date range. (SMAP data is not available before 2015-04-01.)")
    
  } else if (any(is.na(smap.data)) == TRUE) {
    
    ## NOTE: SMAP collects data every ~2-3 days. Missing observations are expected. 
    PEcAn.logger::logger.warn(
      "WARNING: There are some missing SMAP observations during this date range (", start, " to ", end, ").")
    
    return(na.omit(smap.data)) } 
  
}

