#
##' @title download.LandTrendr.AGB
##' @name  download.LandTrendr.AGB
##' 
##' @param outdir Where to store the output - the downloaded LandTrendr AGB raster data
##' @param target_dataset Use these argument to select which LandTrendr dataset to download. 
##' For v1 the default is "biomass" For v2 you should use "biomassfiaald"
##' @param product_dates What data product dates to download
##' @param product_version Optional. LandTrend AGB is provided with three versions, 
##' v0, v1, and v2 (latest version)
##' @param run_parallel Logical. Download and extract files in parallel?
##' @param ncores Optional. If run_parallel=TRUE how many cores to use?  If left as NULL will select max number -1
##' @param overwrite Logical. Overwrite existing files and replace with new versions
##' @param con Optional BETYdb database connection. If specified then the code will check to see if the file already exists in 
##' PEcAn before downloading, 
##' and will also create a database entry for new downloads
##' @param hostname Optional. When checking for existing files on a host you can select either a specific host using hostname
##' or the default (when NULL) is to check the localhost 
##' 
##' @importFrom PEcAn.logger logger.severe logger.info
##' @importFrom PEcAn.utils download.file
##' @importFrom PEcAn.DB dbfile.check db.query db.close
##' @importFrom parallel detectCores makeCluster
##' @importFrom RCurl getURL
##' @importFrom purrr negate %>%
##' @importFrom foreach %dopar% foreach
##'  
##' @examples
##' \dontrun{
##' outdir <- "~/scratch/abg_data/"
##' product_dates <- c(1990, 1991, 1995)  # using discontinous, or specific years
##' product_dates2 <- seq(1992, 1995, 1)  # using a date sequence for selection of years
##' product_version = "v2"
##' target_dataset = "biomassfiaald"
##' 
##' results <- PEcAn.data.remote::download.LandTrendr.AGB(outdir=outdir,
##'            target_dataset=target_dataset,
##'            product_dates = product_dates, 
##'            product_version = product_version)
##' 
##' results <- PEcAn.data.remote::download.LandTrendr.AGB(outdir=outdir, 
##'            target_dataset=target_dataset,
##'            product_dates = product_dates2, 
##'            product_version = product_version)
##' }
##' 
##' @return data.frame summarizing the results of the function call
##' 
##' @export
##' @author Shawn Serbin
##'
download.LandTrendr.AGB <- function(outdir, target_dataset = "biomass", product_dates = NULL, 
                                    product_version = "v1", run_parallel = TRUE, 
                                    ncores = NULL, overwrite = FALSE, con = NULL, 
                                    hostname = NULL) {
  
  # steps to implement:
  # check if files exist locally, also are they valid?  Check DB for file location
  # check if files exist remotely, get file size? Is that relevant as remote files are likely .zip
  # confirm all params make sense and are useful, prune any that are not

  ## before doing anything, check if the files already exists on this host
  # -- to implement.  break/return out of function if nothing to do, else below

  ## setup output folder
  if (! file.exists(outdir)) dir.create(outdir,recursive=TRUE)
  
  ## get target year range
  if (is.null(product_dates)) {
    PEcAn.logger::logger.severe("*** No products dates provided. Please provide dates to process ***")
  } else {
    target_download_years <- sort(as.vector(product_dates))
    PEcAn.logger::logger.info("Downloading dates: ")
    PEcAn.logger::logger.info(target_download_years)
  }

  ## setup parallel
  if (run_parallel) {
    if (!is.null(ncores)) {
      ncores <- ncores
    } else {
      ncores <- parallel::detectCores()-1
    }
    PEcAn.logger::logger.info(paste0("Running in parallel with: ", ncores))
  }
  
  ## setup
  PEcAn.logger::logger.info("*** Downloading LandTrendr ABG data products ***")
  URL <- "ftp://islay.ceoas.oregonstate.edu/cms"

  # setup product defaults
  target_filename_prefix <- "biomassfiaald"
  file_ext <- ".zip"
  obs_files <- paste0(target_filename_prefix,"_",target_download_years,"_median",file_ext)  # hard-coded name matching source, OK?
  err_files <- paste0(target_filename_prefix,"_",target_download_years,"_stdv",file_ext)    # hard-coded name matching source, OK?
  files_to_download <- c(obs_files,err_files)
  
  prod_obs_urls <- paste(URL,product_version,target_dataset,"median",obs_files,sep="/")
  prod_err_urls <- paste(URL,product_version,target_dataset,"stdv",err_files,sep="/")
  download_urls <- c(prod_obs_urls,prod_err_urls)
  
  # identify these are compressed files
  compressed <- TRUE

  ## download data
  # before downloading check that the remote FTP contains the desired years - a little clunky, clean up
  # if we keep this, will need to check this works with other data sources/products
  check_urls <- paste0(unique(dirname(download_urls), fromLast = TRUE),"/")
  remote_filenames <- Map(
    function(p) {
      readLines(
        curl::curl(
          p,
          handle = curl::new_handle(
            ftp_use_epsv = FALSE,
            dirlistonly = TRUE)))
    },
    check_urls)
  remote_filenames_list <- unlist(remote_filenames)
  if (sum(basename(download_urls) %in% remote_filenames_list, na.rm=T)!=length(download_urls)) { 
    `%not_in%` <- purrr::negate(`%in%`)
    missing <- which(basename(download_urls) %not_in% remote_filenames_list)
    download_urls[missing]
    PEcAn.logger::logger.severe(paste("Files missing from source: ", 
                                      download_urls[missing]))
  }
  
  # ------ new way using the database
  if (product_version == "v1") {
    median_input_id <- 2000000234
    sdev_input_id <- 2000000396
  } else if (product_version == "v2") {
    median_input_id <- 2000000395
    sdev_input_id <- 2000000397
  }
  
  ## before downloading, check if the file already exists on this host
  if (!is.null(con)) {
    if (!is.null(hostname)) {
      median_input_chk_list <- PEcAn.DB::dbfile.check("Input", median_input_id, 
                                                      hostname=hostname, con=con, return.all=T)
      med_chk <- length(median_input_chk_list)==0
      sdev_input_chk_list <- PEcAn.DB::dbfile.check("Input", sdev_input_id, 
                                                    hostname=hostname, con=con, return.all=T)
      sdev_chk <- length(sdev_input_chk_list)==0
    } else {
      median_input_chk_list <- PEcAn.DB::dbfile.check("Input", median_input_id, 
                                                      hostname=PEcAn.remote::fqdn(), con=con, return.all=T)
      med_chk <- length(median_input_chk_list)==0
      sdev_input_chk_list <- PEcAn.DB::dbfile.check("Input", median_input_id, 
                                                    hostname=PEcAn.remote::fqdn(), con=con, return.all=T)
      sdev_chk <- length(sdev_input_chk_list)==0
    }
    #if (!all(file.exists(local_files)) && !isTRUE(overwrite)) {
    #  files_to_download_final <- files_to_download[!file.exists(local_files)]
    #  download_urls_final <- download_urls[!file.exists(local_files)]
    #}
    #remote_files <- file.path(outdir,gsub(".zip", ".tif",files_to_download))
    #db_med_files <- file.path(median_input_chk_list$file_path, median_input_chk_list$file_name)
    remote_files <- file.path(gsub(".zip", ".tif",files_to_download))
    db_med_files <- suppressWarnings(file.path(median_input_chk_list$file_name))
    db_sdev_files <- suppressWarnings(file.path(sdev_input_chk_list$file_name))
    files_to_download_final <- setdiff(remote_files,c(db_med_files,db_sdev_files))

    grep(files_to_download_final,download_urls)
    download_urls
    
    if (!all(remote_files %in% db_med_files)) && !isTRUE(overwrite)) {
      files_to_download_final <- setdiff(remote_files,db_med_files)
      
      
      files_to_download_final <- remote_files[!file.exists(local_files)]
      download_urls_final <- download_urls[!file.exists(local_files)]
      
    }
    
    #!file.exists(db_med_files)
    
  } else {
    ## check for local files exist without using BETYdb
    local_files <- file.path(outdir,gsub(".zip", ".tif",files_to_download))
    if (!all(file.exists(local_files)) && !isTRUE(overwrite)) {
      files_to_download_final <- files_to_download[!file.exists(local_files)]
      download_urls_final <- download_urls[!file.exists(local_files)]
    } else {
      files_to_download_final <- files_to_download
      download_urls_final <- download_urls
    }
  } # end if/else
    
    
  #   chk <- dbfile.check(type = "Input", id = input.id, con = con)
  #   if (nrow(chk) > 0) {
  #     machines <- db.query(paste("SELECT * from machines where id in (", 
  #                                paste(chk$machine_id, sep = ","), ")"), con)
  #     if (PEcAn.remote::fqdn() %in% machines$hostname) {
  #       ## record already exists on this host
  #       return(chk$id[PEcAn.remote::fqdn() == machines$hostname])
  #     }
  #   }
  # }
  # ------ 

  
  
  
  
  # setup download
  if (length(files_to_download_final)<1) {
    PEcAn.logger::logger.info("*** Requested files already exist on this host, providing file paths ***")
  } else {
    `%dopar%` <- foreach::`%dopar%`
    PEcAn.logger::logger.info("*** Downloading AGB data ***")
    if (run_parallel) {
      cl <- parallel::makeCluster(ncores)
      doParallel::registerDoParallel(cl)
      foreach::foreach(i=1:length(files_to_download_final)) %dopar% 
        try(PEcAn.utils::download_file(download_urls_final[i], file.path(outdir, 
                                                            files_to_download_final[i])))
    } else {
      PEcAn.logger::logger.info("Caution, downloading in serial. 
                                Could take an extended period to finish") # needed?
      Map(function(u, d) PEcAn.utils::download_file(u, d), download_urls_final, file.path(outdir,
                                                                             files_to_download_final))
    }
    # let user know downloading is complete
    PEcAn.logger::logger.info("*** Downloading complete ***")
    
    if (compressed) {
      PEcAn.logger::logger.info("*** Unpacking compressed files ***")
      ## unpack files
      # check type - there is a better way to do this
      if (file_ext==".zip") {
        zip_files <- list.files(file.path(outdir), pattern = "*.zip", full.names = TRUE)
        foreach::foreach(k=1:length(zip_files)) %dopar% try(utils::unzip(file.path(zip_files[k]),
                                                                         files = NULL, list = FALSE, overwrite = TRUE,
                                                                         junkpaths = FALSE, 
                                                                         exdir = file.path(path.expand(outdir)),
                                                                         unzip = getOption("unzip"), 
                                                                         setTimes = FALSE))
        PEcAn.logger::logger.info("*** Removing compressed files ***")
        unlink(zip_files)
      }
    }
  }

  ## Prepare results - clunky, need to refine this
  downloaded_files <- file.path(outdir,gsub(".zip", ".tif",files_to_download))
  downloaded_years <- unlist(regmatches(downloaded_files, 
                                        gregexpr("\\d{4}", downloaded_files)))
  total_rows <- length(downloaded_files)
  med_rows <- length(grep(pattern = "median", downloaded_files))
  sdev_rows <- length(grep(pattern = "stdv", downloaded_files))
  out_formats <- c(rep("LandTrendr_AGB_median", times=med_rows),
                   rep("LandTrendr_AGB_stdev", times=sdev_rows))

  results <- data.frame(file = character(total_rows), 
                            host = character(total_rows), 
                            mimetype = "image/tiff", 
                            formatname = character(total_rows), 
                            startdate = character(total_rows), 
                            enddate = character(total_rows), 
                            dbfile.name = "LandTrendr", 
                            stringsAsFactors = FALSE)

  for (i in seq_len(total_rows)) { 
    results$file[i] <- downloaded_files[i]
    results$host[i] <- PEcAn.remote::fqdn()
    results$startdate[i] <- paste0(downloaded_years[i], "-01-01 00:00:00")
    results$enddate[i] <- paste0(downloaded_years[i], "-12-31 23:59:59")
    results$formatname[i] <- out_formats[i]
  }
  
  suppressWarnings(PEcAn.DB::db.close(con, showWarnings = FALSE)) # why isnt TRUE invisible? 
  #DBI::dbDisconnect(con) # or should we just do this?
  return(results)
}
#

#
##' @title extract.LandTrendr.AGB
##' @name  extract.LandTrendr.AGB
##' 
##' @param site_info A BETYdb site info dataframe containing at least each site ID, sitename, 
##' latitude, longitude, and time_zone. e.g. c(site_qry$id, site_qry$sitename, site_qry$lon,
##' site_qry$lat, site_qry$time_zone)
##' @param dataset Which LandTrendr dataset to parse, "median" or "stdv".Default: "median"
##' @param buffer Optional. operate over desired buffer area (not yet implemented)
##' @param fun Optional function to apply to buffer area.  Default - mean
##' @param data_dir  directory where input data is located. Can be NUL if con is specified
##' @param product_dates Process and extract data only from selected years. Default behavior
##' (product_dates = NULL) is to extract data from all availible years in BETYdb or data_dir
##' @param output_file Path to save LandTrendr_AGB_output.RData file containing the 
##' output extraction list (see return)
##' 
##' @importFrom sp SpatialPoints proj4string CRS spTransform
##' @importFrom raster raster crs stack extract
##' 
##' @examples
##' \dontrun{
##' 
##' # Example 1 - using BETYdb site IDs to extract data
##' # Database connection (optional)
##'
##' con <- PEcAn.DB::db.open(
##'   list(user='bety', password='bety', host='localhost',
##'   dbname='bety', driver='PostgreSQL',write=TRUE))
##' 
##' site_ID <- c(2000000023,1000025731,676,1000005149) # BETYdb site IDs
##' suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, 
##' ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})", 
##' ids = site_ID, .con = con))
##' suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
##' suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
##' data_dir <- "~/scratch/agb_data/"
##' 
##' results <- extract.LandTrendr.AGB(site_info, "median", buffer = NULL, fun = "mean", 
##' data_dir, product_dates, output_file)
##' 
##' }
##' 
##' @return list of two containing the median AGB values per pixel and the corresponding 
##' standard deviation values (uncertainties)
##' 
##' @export
##' @author Shawn Serbin, Alexey Shiklomanov
##' 
extract.LandTrendr.AGB <- function(site_info=NULL, dataset = "median", buffer = NULL, fun = "mean", 
                                   data_dir = NULL, product_dates = NULL, output_file = NULL, 
                                   ...) {

  ## get coordinates and provide spatial info - should harmonize what packages we use in all data.remote functions
  site_info <- site_info
  site_coords <- data.frame(site_info$lon, site_info$lat)
  names(site_coords) <- c("Longitude","Latitude")
  coords_latlong <- sp::SpatialPoints(site_coords)
  sp::proj4string(coords_latlong) <- sp::CRS("+init=epsg:4326")
  
  ## Subset list of years to process if requested by user
  if (!is.null(product_dates)) {
    agb_files <- list.files(file.path(data_dir), pattern = paste0("*",dataset,".tif$"), 
                            full.names = TRUE)
    availible_years <- unlist(regmatches(agb_files, 
                                         gregexpr("\\d{4}", agb_files)))
    agb_files <- agb_files[availible_years %in% product_dates]
  } else {
    agb_files <- list.files(file.path(data_dir), pattern = paste0("*",dataset,".tif$"), 
                            full.names = TRUE)
  }

  ## load gridded AGB data
  raster_data <- lapply(agb_files, raster::raster)
  
  ## reproject Lat/Long site coords to AGB Albers Equal-Area
  coords_AEA <- sp::spTransform(coords_latlong,
                                raster::crs(raster::raster(raster_data[[1]])))
  
  ## prepare product for extraction - stack requested years
  raster_data_stack <- raster::stack(raster_data)
  
  ## extract
  agb_pixel <- raster::extract(x = raster_data_stack, 
                               y = coords_AEA,
                               buffer = buffer,
                               fun = NULL,
                               df = FALSE)
  if(is.null(buffer)){
    processed_years <- unlist(regmatches(names(data.frame(agb_pixel)), 
                                         gregexpr("\\d{4}", names(data.frame(agb_pixel)))))
    agb_pixel <- data.frame(agb_pixel)
    names(agb_pixel) <- paste0("Year_",processed_years)
    agb_pixel <- data.frame(Site_ID=site_info$site_id, Site_Name=site_info$site_name, agb_pixel)
    
    ## output list
    point_list <- list()
    output_name <- paste0(dataset,"_AGB")
    point_list <- list(agb_pixel)
    names(point_list) <- output_name
  }else{
    return(agb_pixel)
  }

  ## save output to a file?
  if (!is.null(output_file)) {
    save("point_list",file = file.path(output_file,paste0('LandTrendr_',dataset,'_AGB_output.RData')))
  }
  
  ## return output list
  return(point_list)
}
###
