##' Get MODIS data by date and location  
##' 
##' @name call_MODIS
##' @title call_MODIS
##' @export
##' @param outdir where the output file will be stored. Default is NULL and in this case only values are returned. When path is provided values are returned and written to disk.  
##' @param var the simple name of the modis dataset variable (e.g. lai)
##' @param site_info Bety list of site info for parsing MODIS data: list(site_id, site_name, lat, 
##' lon, time_zone)
##' @param product_dates a character vector of the start and end date of the data in YYYYJJJ
##' @param run_parallel optional method to download data paralleize. Only works if more than 1 
##' site is needed and there are >1 CPUs available.
##' @param ncores number of cpus to use if run_parallel is set to TRUE. If you do not know the 
##' number of CPU's available, enter NULL.
##' @param product string value for MODIS product number
##' @param band   string value for which measurement to extract
##' @param package_method string value to inform function of which package method to use to download 
##' modis data. Either "MODISTools" or "reticulate" (optional)
##' @param QC_filter Converts QC values of band and keeps only data values that are excellent or good 
##' (as described by MODIS documentation), and removes all bad values. qc_band must be supplied for this 
##' parameter to work. Default is False. Only MODISTools option.
##' @param progress TRUE reports the download progress bar of the dataset, FALSE omits the download 
##' progress bar. Default is TRUE. Only MODISTools option.
##' 
##' Requires Python3 for reticulate method option. There are a number of required python libraries. 
##' sudo -H pip install numpy suds netCDF4 json
##' depends on the MODISTools package version 1.1.0
##' 
##' @examples
##' \dontrun{
##' site_info <- list(
##'   site_id = 1,
##'   site_name = "test",
##'   lat = 44,
##'   lon = 90,
##'   time_zone = "UTC")
##' test_modistools <- call_MODIS(
##'   var = "lai",
##'   product = "MOD15A2H",
##'   band = "Lai_500m",
##'   site_info = site_info,
##'   product_dates = c("2001150", "2001365"),
##'   outdir = NULL,
##'   run_parallel = TRUE,
##'   ncores = NULL,
##'   package_method = "MODISTools",
##'   QC_filter = TRUE,
##'   progress = FALSE)
##' }
##' @importFrom foreach %do% %dopar%
##' @author Bailey Morrison
##'
call_MODIS <- function(var, product, 
                       band, site_info, 
                       product_dates,
                       outdir = NULL, 
                       run_parallel = FALSE, 
                       ncores = NULL,
                       package_method = "MODISTools", 
                       QC_filter = FALSE, 
                       progress = FALSE) {
  
  # makes the query search for 1 pixel and not for rasters chunks for now. Will be changed when we provide raster output support.
 size <- 0
  
  site_coords <- data.frame(site_info$lon, site_info$lat)
  names(site_coords) <- c("lon","lat")
  
  # set up CPUS for parallel runs.
  if (is.null(ncores)) {
    total_cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
    ncores <- total_cores-2
  }
  if (ncores > 10) # MODIS API has a 10 download limit / computer
  {
    ncores <- 10
  }
  
  # register CPUS if run_parallel = TRUE
  if (run_parallel){
    if (progress){
      cl <- parallel::makeCluster(ncores, outfile = "")
      doParallel::registerDoParallel(cl)
    } else {
      cl <- parallel::makeCluster(ncores)
      doParallel::registerDoParallel(cl)
    }
    
  }
  

  #################### if package_method == MODISTools option ####################
  
  if (package_method == "MODISTools")
  {
    #################### FUNCTION PARAMETER PRECHECKS #################### 
    #1. check that modis product is available
    products <- MODISTools::mt_products()
    if (!(product %in% products$product))
    {
      PEcAn.logger::logger.warn(products)
      stop("Product not available for MODIS API. Please chose a product from the list above.")
    } 
    
    #2. check that modis produdct band is available
    bands <- MODISTools::mt_bands(product = product)
    if (!(band %in% bands$band))
    {
      PEcAn.logger::logger.warn(bands$band)
      stop("Band selected is not avialable. Please selected from the bands listed above that correspond with the data product.")
    } 
    
    #3. check that dates asked for in function parameters are fall within dates available for modis product/bands.
    if (run_parallel)
    {
      modis_dates <- as.numeric(substr(sort(unique(foreach::foreach(i = seq_along(nrow(site_coords)), .combine = c) 
                                                   %dopar% MODISTools::mt_dates(product = product, lat = site_coords$lat[i], lon = site_coords$lon[i])$modis_date)), 2, 8))
    } else {
      modis_dates <- as.numeric(substr(sort(unique(foreach::foreach(i = seq_along(nrow(site_coords)), .combine = c) %do% 
                                                     MODISTools::mt_dates(product = product, lat = site_coords$lat[i], lon = site_coords$lon[i])$modis_date)), 2, 8))
    }
  

  # check if user asked for dates for data, if not, download all dates
  if (is.null(product_dates)) {
    dates <- sort(unique(foreach::foreach(i = seq_along(nrow(site_coords)), .combine = c) %do% 
                           MODISTools::mt_dates(product = product, lat = site_coords$lat[i], lon = site_coords$lon[i])$modis_date))
    #dates = as.Date(as.character(substr(dates, 2, nchar(dates))), format = "%Y%j")
  } else {
    # if user asked for specific dates, first make sure data is available, then inform user of any missing dates in time period asked for.
    start_date <- as.numeric(product_dates[1])
    end_date <- as.numeric(product_dates[2])
    
    # if all dates are available with user defined time period:
    if (start_date >= modis_dates[1] & end_date <= modis_dates[length(modis_dates)])
    {
      PEcAn.logger::logger.info("Check #2: All dates are available!")
      
      start_date <- modis_dates[which(modis_dates >= start_date)[1]]
      
      include <- which(modis_dates <= end_date)
      end_date <- modis_dates[include[length(include)]]
    }
    
    # if start and end dates fall completely outside of available modis_dates:
    if ((start_date < modis_dates[1] & end_date < modis_dates[1]) | start_date > modis_dates[length(modis_dates)] & end_date > modis_dates[length(modis_dates)])
    {
      PEcAn.logger::logger.severe(
        "Start and end date (", start_date, ", ", end_date,
        ") are not within MODIS data product date range (", modis_dates[1], ", ", modis_dates[length(modis_dates)],
        "). Please choose another date.")
    }
       
    # if start and end dates are larger than the available range, but part or full range:
    if ((start_date < modis_dates[1] & end_date > modis_dates[1]) | start_date < modis_dates[length(modis_dates)] & end_date > modis_dates[length(modis_dates)])
    {
      PEcAn.logger::logger.warn("WARNING: Dates are  partially available. Start and/or end date extend beyond modis data product availability.")
      start_date <- modis_dates[which(modis_dates >= start_date)[1]]
      
      include <- which(modis_dates <= end_date)
      end_date <- modis_dates[include[length(include)]]
    }
    
    dates <- modis_dates[which(modis_dates >= start_date & modis_dates <= end_date)]
      
  }
  
  modis_dates <- as.Date(as.character(modis_dates), format = "%Y%j")
  dates <- as.Date(as.character(dates), format = "%Y%j")
  
  #### Start extracting the data
  PEcAn.logger::logger.info("Extracting data")

  if (run_parallel)
  {
    dat <- foreach::foreach(i=seq_along(site_info$site_id), .combine = rbind) %dopar% 
      MODISTools::mt_subset(lat = site_coords$lat[i],lon = site_coords$lon[i],
                            product = product,
                            band = band,
                            start = dates[1],
                            end = dates[length(dates)],
                            km_ab = size, km_lr = size,
                            progress = progress, site_name = as.character(site_info$site_id[i]))
  } else {
    dat <- data.frame()
    
    for (i in seq_along(site_info$site_id))
    {
      d <- MODISTools::mt_subset(lat = site_coords$lat[i],
                                lon = site_coords$lon[i],
                                product = product,
                                band = band,
                                start = dates[1],
                                end = dates[length(dates)],
                                km_ab = size, km_lr = size,
                                progress = progress)
      dat <- rbind(dat, d)
    }
  }
  
  # clean up data outputs so there isn't extra data, format classes.
  output <- as.data.frame(cbind(dat$modis_date, dat$calendar_date, dat$band, dat$tile, dat$site, dat$latitude, dat$longitude, dat$pixel, dat$value), stringsAsFactors = FALSE)
  names(output) <- c("modis_date", "calendar_date", "band", "tile", "site_id", "lat", "lon", "pixels", "data")
  
  output[ ,5:9] <- lapply(output[ ,5:9], as.numeric)
  
  # scale the data + stdev to proper units
  output$data <- output$data * (as.numeric(dat$scale))
  output$lat <- round(output$lat, 4)
  output$lon <- round(output$lon, 4)

  # remove bad values if QC filter is on
  if (QC_filter)
  {
    qc_band <- bands$band[which(grepl(var, bands$band, ignore.case = TRUE) & grepl("QC", bands$band, ignore.case = TRUE))]
    
    if (run_parallel) 
    {
      qc <- foreach::foreach(i=seq_along(site_info$site_id), .combine = rbind) %dopar% 
        MODISTools::mt_subset(lat = site_coords$lat[i],
                              lon = site_coords$lon[i],
                              product = product,
                              band = qc_band,
                              start = dates[1],
                              end = dates[length(dates)],
                              km_ab = size, km_lr = size,
                              progress = progress)
    } else {
      qc <- MODISTools::mt_subset(lat = site_coords$lat[i],
                                  lon = site_coords$lon[i],
                                  product = product,
                                  band = qc_band,
                                  start = dates[1],
                                  end = dates[length(dates)],
                                  km_ab = size, km_lr = size,
                                  progress = progress)

    }

    output$qc <- as.character(qc$value)
    
    #convert QC values and keep only okay values
    for (i in seq_len(nrow(output)))
    {
      # QC flags are stored as an 8-bit mask
      # we only care about the 3 least-significant bits
      qc_flags <- intToBits(as.integer(output$qc[i])) # NB big-endian (ones place first)
      qc_flags <- as.integer(rev(utils::head(qc_flags, 3))) # now ones place last
      output$qc[i] <- paste(qc_flags, collapse = "")
    }
    good <- which(output$qc %in% c("000", "001"))
    if (length(good) > 0)
    {
      output <- output[good, ]
    } else {
      PEcAn.logger::logger.warn("All QC values are bad. No data to output with QC filter == TRUE.")
    }
  }
  
  # unregister cores since parallel process is done
  if (run_parallel)
  {
    parallel::stopCluster(cl)
  }
  
  # break dataoutput up by site and save out chunks
  if (!(is.null(outdir)))
  {
    for (i in seq_along(site_info$site_id))
    {
      if (!(dir.exists(file.path(outdir, site_info$site_id[i]))))
      {
        dir.create(file.path(outdir, site_info$site_id[i]))
      }
        
      site <- output[which(output$site_id == site_info$site_id[i]), ]
      site$modis_date <- substr(site$modis_date, 2, length(site$modis_date))
      
      if (QC_filter)
      {
        fname <- paste(site_info$site_id[i], "/", product, "_", band, "_", start_date, "-", end_date, "_filtered.csv", sep = "")
      } else {
        fname <- paste(site_info$site_id[i], "/", product, "_", band, "_", start_date, "-", end_date, "_unfiltered.csv", sep = "")
      }
      fname <- file.path(outdir, fname)
      utils::write.csv(site, fname, row.names = FALSE)
    }
    
  }
    
    return(output)
  }
  
  ########### temporarily removed for now as python2 is being discontinued and modules are not working correctly
  # if (package_method == "reticulate"){
  #   # load in python script
  #   script.path <- file.path(system.file("extract_modis_data.py", package = "PEcAn.data.remote"))
  #   reticulate::source_python(script.path)
  #   
  #   # extract the data
  #   output <- extract_modis_data(product = product, band = band, lat = lat, lon = lon, start_date = start_date, end_date = end_date, size = size, band_qc = band_qc, band_sd = band_sd)
  #   output[ ,5:10] <- lapply(output[ ,5:10], as.numeric)
  #   output$lat <- round(output$lat, 4)
  #   output$lon <- round(output$lon, 4)
  #   
  #   if (!(is.null(outdir)))
  #   {
  #     fname <- paste(product, "_", band, "_", start_date, "_", end_date, "_", lat, "_", lon, ".csv", sep = "")
  #     fname <- file.path(outdir, fname)
  #     write.csv(output, fname)
  #   }
  #   
  #   return(output)}

}
