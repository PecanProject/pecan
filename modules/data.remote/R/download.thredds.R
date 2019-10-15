#
##' @title download.thredds.data
##' @name  download.thredds.data
##' 
##' @param outdir file location to place output
##' @param site_info information about the site. i.e. site_id, latitude, longitude 
##' @param dates character vector of start and end date for dataset as YYYYmmdd
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
##' site_info <- dataframe that contains information about site_id, latitude, longitude, and site_names
##' dates <- date range to download data. Should be a character vector with start and end date as YYYYmmdd
##' varod <- character shorthand name of variable to download. Example: LAI for leaf area index.
##' dir_url <- catalog url from THREDDS that is used to determine which files are available for download using OPENDAP
##' data_url <- OpenDAP URL that actually downloads the netcdf file.
##' run_parallel <- optional. Can be used to speed up download process if there are more than 2 cores available on computer
##' 

##' results <- PEcAn.data.remote::download.thredds.AGB(outdir=outdir, 
##'            site_ids = c(676, 678, 679, 755, 767, 1000000030, 1000000145, 1000025731), 
##'            run_parallel = TRUE, ncores = 8)
##'            
##' @export
##' @author Bailey Morrison
##'
download.thredds.data <- function(outdir = NULL, site_info, dates = c("19950201", "19961215"), 
                                  varid = "LAI",
                                  dir_url = "https://www.ncei.noaa.gov/thredds/catalog/cdr/lai/files", 
                                  data_url = "https://www.ncei.noaa.gov/thredds/dodsC/cdr/lai/files",
                                  run_parallel = TRUE) {
  # require("XML")
  # require("RCurl") 
  require("foreach")
  
  # check that dates are within the date range of the dataset
  dates = c(as.Date(dates[1], "%Y%m%d"), as.Date(dates[2], "%Y%m%d"))
  if (!(is.null(dir_url)))
  {
    #https://www.ncei.noaa.gov/thredds/catalog/cdr/lai/files/1981/catalog.html -> link for directory files, not downloads
    result <- RCurl::getURL(paste(dir_url, "catalog.html", sep = "/"), verbose=F,ftp.use.epsv=TRUE, dirlistonly = TRUE)
    files = XML::getHTMLLinks(result)
    
    date_year_range = unique(range(c(year(as.Date(dates[1], "%Y")), year(as.Date(dates[2], "%Y")))))
    if (all((!(substr(files, 1, 4) %in% date_year_range))))
    {
      # give warning that dates aren't available
      print(test)
    }
    
  }
  
  # get list of catalog file links to determine actual dates that can be downloaded with in user range
  links = vector()
  for (i in 1:length(date_year_range))
  {
    links[i] = RCurl::getURL(paste(dir_url, date_year_range[i], "catalog.html", sep = "/"), verbose=F,ftp.use.epsv=T, dirlistonly = T)
  }
  
  # get list of all dates available from year range provided
  files = foreach(i = 1:length(links), .combine = c) %do% XML::getHTMLLinks(links[i])
  
  #remove files with no dates and get list of dates available.
  index_dates = regexpr(pattern = "[0-9]{8}", files)
  files = files[-(which(index_dates < 0))]
  index_dates = index_dates[which(index_dates > 0)]
  
  # get list of files that fall within the specific date range user asks for (Ymd, not Y)
  dates_avail = as.Date(substr(files, index_dates, index_dates+7), "%Y%m%d")
  date_range = seq(dates[1], dates[2], by = "day")
  get_dates =  date_range[which(date_range %in% dates_avail)]
  
  # only keep files that are within the true yyyymmdd date range user requested
  files = files[foreach(i = seq_along(get_dates), .combine = c) %do% grep(files, pattern = format(get_dates[i], '%Y%m%d'))]
  filenames = basename(files)
  
  # user must supply data_URL or the netcdf files cannot be downloaded through thredds. if user has supplied no data_url, the job will fail
  # supply a warning
  if (!(is.null(data_url)))
  {
    #https://www.ncei.noaa.gov/thredds/dodsC/cdr/lai/files/1981/AVHRR-Land_v005_AVH15C1_NOAA-07_19810624_c20181025194251.nc.html
    # this is what a link looks like to download threeds data.
    urls = sort(paste(data_url, substr(dates_avail, 1, 4), filenames, sep = "/"))
    
    extract_nc = function(site_info, url, run_parallel)
    {
      require("foreach")
      require("ncdf4")
      
      mylats = site_info$lat
      mylons = site_info$lon
      sites = site_info$site_id
      
      # open netcdf file and get the correct variable name based on varid parameter + var names of netcdf
      data = ncdf4::nc_open(url)
      vars = names(data$var)
      var = vars[grep(vars, pattern = varid, ignore.case = T)]
      
      # get list of all xy coordinates in netcdf
      lats = ncdf4::ncvar_get(data, "latitude")
      lons = ncdf4::ncvar_get(data, "longitude")
      
      # find the cell that site coordinates are located in
      dist_y = foreach(i = mylats, .combine = cbind) %do% sqrt((lats - i)^2)
      dist_x = foreach(i = mylons, .combine = cbind) %do% sqrt((lons - i)^2)
      y = foreach(i = 1:ncol(dist_y), .combine = c) %do% which(dist_y[,i] == min(dist_y[,i]), arr.ind = T)
      x = foreach(i = 1:ncol(dist_x), .combine = c) %do% which(dist_x[,i] == min(dist_x[,i]), arr.ind = T)
      
      scale = data$var[[var]]$scaleFact
      
      d = as.vector(foreach(i = seq_along(x), .combine = rbind) %do% ncdf4::ncvar_get(data, var, start = c(x[i], y[i], 1), count = c(1,1,1)))
      
      info = as.data.frame(cbind(sites, mylons, mylats, d), stringsAsFactors = F)
      names(info) = c("site_id", "lon", "lat", "value")
      
      return(info)
    }
    
    
    
    if (run_parallel)
    {
      require("parallel")
      require("doParallel")
      ncores = parallel::detectCores(all.tests = FALSE, logical = TRUE)
      if (ncores >= 3)
      {
        # failsafe in case someone has a computer with 2 nodes.
        ncores = ncores-2
      }
      # THREDDS has a 10 job limit. Will fail if you try to download more than 10 values at a time
      if (ncores >= 10)
      {
        ncores = 9 # went 1 less becasue it still fails sometimes
      }
      cl <- parallel::makeCluster(ncores, outfile="")
      doParallel::registerDoParallel(cl)
      output = foreach(i = urls, .combine = rbind) %dopar% extract_nc(site_info, i, run_parallel)
      stopCluster(cl)
    } else {
      output = foreach(i = urls, .combine = rbind) %do% extract_nc(site_info, i, run_parallel)
    }
    
    return(output)
    
  }
}
