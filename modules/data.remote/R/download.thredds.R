#
##' @title download.thredds.AGB
##' @name  download.thredds.AGB
##' 
##' @param outdir Where to place output
##' @param site_ids What locations to download data at? 
##' @param run_parallel Logical. Download and extract files in parallel?
##' @param ncores Optional. If run_parallel=TRUE how many cores to use?  If left as NULL will select max number -1
##' 
##' @return data.frame summarize the results of the function call
##' 
##' @examples
##' \dontrun{
##' outdir <- "~/scratch/abg_data/"

##' results <- PEcAn.data.remote::download.thredds.AGB(outdir=outdir, 
##'            site_ids = c(676, 678, 679, 755, 767, 1000000030, 1000000145, 1000025731), 
##'            run_parallel = TRUE, ncores = 8)
##' }
##' @export
##' @author Bailey Morrison
##'
download.thredds.AGB <- function(outdir = NULL, site_ids, run_parallel = FALSE, 
                                    ncores = NULL) {
  
  
  con <- PEcAn.DB::db.open(
    list(user='bety', password='bety', host='localhost',
    dbname='bety', driver='PostgreSQL',write=TRUE))
  site_ID <- as.character(site_ids)
  suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                              ids = site_ID, .con = con))
  suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
  suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
  site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                    lon=qry_results$lon, time_zone=qry_results$time_zone)
  
  mylat = site_info$lat
  mylon = site_info$lon
  
  # site specific URL for dataset --> these will be made to work for all THREDDS datasets in the future, but for now, just testing with
  # this one dataset. This specific dataset only has 1 year (2005), so no temporal looping for now.
  obs_file = "https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1221/agb_5k.nc4"
  obs_err = "https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1221/agb_SE_5k.nc4"
  files = c(obs_file, obs_err)
  
  # function to extract ncdf data from lat and lon values for value + SE URLs
  get_data = function(i)
  {
    data = ncdf4::nc_open(files[1])
    agb_lats = ncdf4::ncvar_get(data, "latitude")
    agb_lons = ncdf4::ncvar_get(data, "longitude")
    
    agb_x = which(abs(agb_lons- mylon[i]) == min(abs(agb_lons - mylon[i])))
    agb_y = which(abs(agb_lats- mylat[i]) == min(abs(agb_lats - mylat[i])))
   
    start = c(agb_x, agb_y)
    count = c(1,1)
    d  = ncdf4::ncvar_get(ncdf4::nc_open(files[1]), "abvgrndbiomass", start=start, count = count)
    if (is.na(d)) d <- NA
    sd = ncdf4::ncvar_get(ncdf4::nc_open(files[2]), "agbSE", start=start, count = count)
    if (is.na(sd)) sd <- NA
    date = "2005"
    site = site_ID[i]
    output = as.data.frame(cbind(d, sd, date, site))
    names(output) = c("value", "sd", "date", "siteID")
    
    # option to save output dataset to directory for user.
    if (!(is.null(outdir)))
    {
      utils::write.csv(output, file = paste0(outdir, "THREDDS_", sub("^([^.]*).*", "\\1",basename(files[1])), "_site_", site, ".csv"), row.names = FALSE)
    }
    
    return(output)
  }
  
  ## setup parallel
  if (run_parallel) {
    if (!is.null(ncores)) {
      ncores <- ncores
    } else {
      ncores <- parallel::detectCores() -1
    }
  
    PEcAn.logger::logger.info(paste0("Running in parallel with: ", ncores))
    cl = parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    data = foreach::foreach(i = seq_along(mylat), .combine = rbind) %dopar% get_data(i)
    parallel::stopCluster(cl)
    
  } else {
    # setup sequential run
    data = data.frame()
    for (i in seq_along(mylat))
    {
      data = rbind(data, get_data(i))
    }
  }
  
  return(data)
}
