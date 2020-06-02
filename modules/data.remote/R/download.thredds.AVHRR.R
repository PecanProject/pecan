##' @title get_site_info
##' @name  get_site_info
##' 
##' 
##' @param xmlfile full path to pecan xml settings file
##' 
##' 
##' @return a list of site information derived from BETY using a pecan .xml settings file with site_id, site_name, lat, lon, and time_zone.
##' 
##' @examples
##' \dontrun{
##' xmlfile <- the full path to a pecan .xml settings file.
##' 

##' site_info <- get_site_info(xmlfile = "/data/bmorrison/sda/lai/pecan_MultiSite_SDA_LAI_AGB_8_Sites_2009.xml")
##'  }          
##' @export
##' @author Bailey Morrison
##'
get_site_info <- function(xmlfile) {
  #require(PEcAn.all)
  
  settings <- read.settings(xmlfile)
  
  observation <- c()
  for (i in seq_along(1:length(settings$run))) {
    command <- paste0("settings$run$settings.", i, "$site$id")
    obs <- eval(parse(text = command))
    observation <- c(observation, obs)
  }
  
  
  PEcAn.logger::logger.info("**** Extracting LandTrendr AGB data for model sites ****")
  bety <- list(user = 'bety', password = 'bety', host = 'localhost',
               dbname = 'bety', driver = 'PostgreSQL', write = TRUE)
  con <- PEcAn.DB::db.open(bety)
  bety$con <- con
  site_ID <- observation
  suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                              ids = site_ID, .con = con))
  suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
  suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
  site_info <- list(site_id = qry_results$id, site_name = qry_results$sitename, lat = qry_results$lat,
                    lon = qry_results$lon, time_zone = qry_results$time_zone)
  return(site_info)
}


##' @title download.thredds
##' @name  download.thredds
##' 
##' 
##' @param outdir file location to place output
##' @param site_info list of information with the site_id, site_info, lat, lon, and time_zone. Derived from BETY using a PEcAn .xml settings file with site information. Can use the get_site_info function to generate this list. 
##' @param dates vector of start and end date for dataset as YYYYmmdd, YYYY-mm-dd, YYYYjjj, or date object.
##' @param varid character vector of shorthand variable name. i.e. LAI
##' @param dir_url catalog url of data from ncei.noaa.gov/thredds website
##' @param data_url opendap url of data from ncei.noaa.gov/thredds website
##' @param run_parallel Logical. Download and extract files in parallel?
##' 
##' @return data.frame summarize the results of the function call
##' 
##' @examples
##' \dontrun{
##' outdir <- directory to store downloaded data
##' site_info <- list that contains information about site_id, site_name, latitude, longitude, and time_zone
##' dates <- date range to download data. Should be a vector of start and end date for dataset as YYYYmmdd, YYYY-mm-dd, YYYYjjj, or date object.
##' varod <- character shorthand name of variable to download. Example: LAI for leaf area index.
##' dir_url <- catalog url from THREDDS that is used to determine which files are available for download using OPENDAP
##' data_url <- OpenDAP URL that actually downloads the netcdf file.
##' run_parallel <- optional. Can be used to speed up download process if there are more than 2 cores available on computer
##' 

##' results <- download_thredds(site_info = site_info, dates = c("19950201", "19961215"), varid = "LAI", dir_url = "https://www.ncei.noaa.gov/thredds/catalog/cdr/lai/files", data_url = "https://www.ncei.noaa.gov/thredds/dodsC/cdr/lai/files", run_parallel = FALSE, outdir = NULL)
##' }        
##' @importFrom foreach %do% %dopar%           
##' @export
##' @author Bailey Morrison
##'
download_thredds <- function(site_info, dates, varid, dir_url, data_url,run_parallel = FALSE, outdir = NULL) {
  
  #until the issues with parallel runs are fixed.
  run_parallel = FALSE
  #require("foreach")
  
  
  #### check that dates are within the date range of the dataset
  
  #first make sure dates are in date format. Correct if not.
  if (!(lubridate::is.Date(dates))){
    if (!(is.character(dates))) {
      dates = as.character(dates)
    }
    if (length(grep(dates, pattern = "-")) > 0) {
      dates <- c(as.Date(dates[1], "%Y-%m-%d"), as.Date(dates[2], "%Y-%m-%d"))
    } else {
      dates <- c(as.Date(dates[1], "%Y%m%d"), as.Date(dates[2], "%Y%m%d"))
    }
    # Julien Date
    if (any(nchar(dates) == 7)) {
      dates <- c(as.Date(dates[1], "%Y%j"), as.Date(dates[2], "%Y%j"))
    }
  }
  
  date_range = unique(lubridate::year(seq(dates[1], dates[2], by = '1 year')))
  
  output = data.frame()
  if (!(is.null(dir_url)))
  {
    for (i in seq_along(date_range))
    {
      result <- RCurl::getURL(paste(dir_url, date_range[i], "/catalog.html", sep = "/"), 
                              verbose=FALSE ,ftp.use.epsv = TRUE, dirlistonly = TRUE)
      files <- XML::getHTMLLinks(result)
      
      index_dates <- regexpr(pattern = "_[0-9]{8}_", files)
      files <- files[-(which(index_dates < 0))]
      index_dates <- index_dates[which(index_dates > 0)]
      
      dates_avail <- as.Date(substr(files, index_dates+1, index_dates+8), "%Y%m%d")
      
      if (!(is.null(data_url)))
      {
        urls <- sort(paste(data_url, substr(dates_avail, 1, 4), "/", basename(files), sep = ""))
        
        if (run_parallel)
        {
          #require("parallel")
          #require("doParallel")
          #ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
          # This is a failsafe for computers with low numbers of CPUS to reduce risk of blowing RAM.
          # if (ncores >= 3)
          # {
          #   # failsafe in case someone has a computer with 2-4 nodes.
          #   ncores <- ncores-2
          # }
          # # THREDDS has a 10 job limit. Will fail if you try to download more than 10 values at a time
          # if (ncores >= 10)
          # {
          #   ncores <- 9 # went 1 less becasue it still fails sometimes
          # }
          # cl <- parallel::makeCluster(ncores, outfile="")
          # doParallel::registerDoParallel(cl)
          # out <- foreach(i = urls, .combine = rbind) %dopar% 
          #   extract_thredds_nc(site_info = site_info, url_info = i)
          # parallel::stopCluster(cl)
        } else {
          #start_time <- Sys.time()
          out <- foreach::foreach(j = urls, .combine = rbind) %do% 
            extract_thredds_nc(site_info, url_info = j)
          # end_time <- Sys.time()
          # end_time - start_time
        }
        output = rbind(output, out)
        
        if (!(is.null(outdir)))
        {
          # this will need to be changed in the future if users want to be able to save data they haven't already extracted at different sites/dates.
          write.csv(out, file = paste(outdir, "/THREDDS_", varid, "_", dates[1], "-", dates[2], ".csv", sep = ""))
        } 
      }

    }
  }
  return(output)
}


##' @title extract_thredds_nc
##' @name  extract_thredds_nc
##' 
##' 
##' @param site_info list of information with the site_id, site_info, lat, lon, and time_zone. Derived from BETY using a PEcAn .xml settings file with site information. Can use the get_site_info function to generate this list. 
##' @param url a THREDDS url of a .nc file to extract data from.
##' 
##' 
##' @return a dataframe with the values for each date/site combination from a THREDDS file 
##' 
##' @examples
##' \dontrun{
##' site_info <- list of information with the site_id, site_info, lat, lon, and time_zone. Derived from BETY using a PEcAn .xml settings file with site information. Can use the get_site_info function to generate this list. 
##' url <- url a THREDDS url of a .nc file to extract data from.
##' 
##' output <- extract_thredds_nc(site_info = site_info, url_info = "https://www.ncei.noaa.gov/thredds/dodsC/cdr/lai/files/1995/AVHRR-Land_v005_AVH15C1_NOAA-14_19950201_c20180831220722.nc")
##'}            
##' @export
##' @author Bailey Morrison
##'
extract_thredds_nc <- function(site_info, url_info)
{
  #print(url)
  #require("foreach")
  #require("ncdf4")
  index = regexpr(pattern = "_[0-9]{8}_", url_info)
  date<- as.Date(substr(url_info, index+1, index+8), "%Y%m%d")
  
  mylats <- site_info$lat
  mylons <- site_info$lon
  sites <- site_info$site_id
  
  # open netcdf file and get the correct variable name based on varid parameter + var names of netcdf
  data <- ncdf4::nc_open(url_info)
  vars <- names(data$var)
  var <- vars[grep(vars, pattern = varid, ignore.case = TRUE)]
  
  # get list of all xy coordinates in netcdf
  lats <- ncdf4::ncvar_get(data, "latitude")
  lons <- ncdf4::ncvar_get(data, "longitude")
  
  # find the cell that site coordinates are located in
  dist_y <- foreach::foreach(i = mylats, .combine = cbind) %do% sqrt((lats - i)^2)
  dist_x <- foreach::foreach(i = mylons, .combine = cbind) %do% sqrt((lons - i)^2)
  y <- foreach::foreach(i = 1:ncol(dist_y), .combine = cbind) %do% which(dist_y[,i] == min(dist_y[,i]), arr.ind = TRUE)
  x <- foreach::foreach(i = 1:ncol(dist_x), .combine = cbind) %do% which(dist_x[,i] == min(dist_x[,i]), arr.ind = TRUE)
  
  scale <- data$var[[var]]$scaleFact
  
  d <- as.vector(foreach::foreach(i = seq_along(x), .combine = rbind) %do% ncdf4::ncvar_get(data, var, start = c(x[i], y[i], 1), count = c(1,1,1)))
  
  info <- as.data.frame(cbind(sites, mylons, mylats, as.character(rep(date, length(mylats))), d), stringsAsFactors = FALSE)
  names(info) <- c("site_id", "lon", "lat", "date", "value")
  
  na = which(is.na(info$value))
  if (length(na) != length(info$site_id) | length(na) != 0)
  {
    info = info[-na,]
  } else {
    info = info
  }
  
  return(info)
}

