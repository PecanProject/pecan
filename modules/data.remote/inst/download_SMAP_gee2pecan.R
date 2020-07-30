##'@name download_SMAP_gee2pecan
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
##'@param site_id Bety site location id number(s)
##'@param geoJSON_outdir  directory to store site GeoJSON, must be the location same as 'gee2pecan_smap.py'
##'@param smap_outdir  directory to store netCDF file of SMAP data, if directory folder does not exist it will be created
##'@return data.frame of SMAP data w/ Date, NA's filling missing data
##'
##'
##'@authors Juliette Bateman, Ayush Prasad (gee2pecan_smap.py)
##'
##'@examples
##'\dontrun{
##'test <- download_SMAP_gee2pecan(
##'  start = "2019-11-01",
##'  end = "2019-11-10",
##'  site_id = 676,
##'  geoJSON_outdir = "/fs/data3/jbateman/pecan/modules/data.remote/inst", 
##'  smap_outdir = "/fs/data3/jbateman/pecan/modules/data.remote/inst")
##'}


download_SMAP_gee2pecan <- function(start, end,
                                    site_id, 
                                    geoJSON_outdir, smap_outdir) {
  
  
  #################### Connect to BETY #################### 
  
  bety <- list(user='bety', password='bety', host='localhost',
               dbname='bety', driver='PostgreSQL',write=TRUE)
  con <- PEcAn.DB::db.open(bety)
  bety$con <- con
  site_ID <- as.character(site_id)
  suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                              ids = site_ID, .con = con))
  suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
  suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
  site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                    lon=qry_results$lon, time_zone=qry_results$time_zone)
  
  
  #################### Begin Data Extraction #################### 
  
  # Create geoJSON file for site
  site_GeoJSON <- data.frame(site_info$lon, site_info$lat) %>%
    setNames(c("lon","lat")) %>% 
    leafletR::toGeoJSON(name = site_info$site_name, dest = geoJSON_outdir, overwrite = TRUE) %>%
    rgdal::readOGR()
  site_GeoJSON$name = site_info$site_name
  site_GeoJSON = site_GeoJSON[-1] %>%
    leafletR::toGeoJSON(name = site_info$site_name, dest = geoJSON_outdir, overwrite = TRUE)
  
  # Locate gee2pecan_smap.py function and load into R
  script.path = file.path(system.file("gee2pecan_smap.py", package = "PEcAn.data.remote"))
  reticulate::source_python(script.path)
  
  # Run gee2pecan_smap function 
  smap.out = gee2pecan_smap(geofile = site_GeoJSON, outdir = smap_outdir, start = start, end = end, var = var)
  output = ncdf4::nc_open(paste0(site_info$site_name,"_smap", ".nc"))
  smap.data = cbind((ncdf4::ncvar_get(output, "date")), ncdf4::ncvar_get(output, "ssm"), ncdf4::ncvar_get(output,"susm"), ncdf4::ncvar_get(output, "smp"), ncdf4::ncvar_get(output, "ssma"), ncdf4::ncvar_get(output,"susma")) %>%
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
    
    return(smap.data) } 
  
}

