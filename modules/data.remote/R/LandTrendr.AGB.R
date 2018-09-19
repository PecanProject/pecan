#
##' @title download.LandTrendr.AGB
##' @name  download.LandTrendr.AGB
##' 
##' @param outdir Where to place output
##' @param product_dates What dates to download and extract
##' @param prodcut_version Optional. For some products (e.g. OSU) there are multiple versions.  
##' @param con Optional database connection. If specified then the code will check to see 
## if the file already exists in PEcAn before downloading, and will also create a database 
## entry for new downloads
##' @param run_parallel Logical. Download and extract files in parallel?
##' @param ncores Optional. If run_parallel=TRUE how many cores to use?  If left as NULL will select max number -1
##' @param generate_plots Optional. Create output diagnostic plots in outdir? --- ACTUALLY PUT THIS IN extract_ABG
##' @param overwrite Logical. Overwrite existing files and replace with new versions
##' 
##' @return data.frame summarize the results of the function call
##' 
##' @examples
##' \dontrun{
##' outdir <- "~/scratch"
##' product_dates <- c(1990,1991)
##' product_dates2 <- seq(1992,1995,1)
##' prodcut_version = "v1"
##' 
##' results <- PEcAn.data.remote::download.LandTrendr.AGB(outdir=outdir, product_dates = product_dates, 
##'            prodcut_version = prodcut_version)
##' 
##' results <- PEcAn.data.remote::download.LandTrendr.AGB(outdir=outdir, product_dates = product_dates2, 
##'            prodcut_version = prodcut_version)
##' }
##' 
##' @export
##' @author Shawn Serbin
##'
download.LandTrendr.AGB <- function(outdir, product_dates = NULL, prodcut_version = NULL, con = NULL, 
                                    run_parallel = TRUE, ncores = NULL, overwrite = FALSE) {
  
  # steps to implement:
  # determine which product the user wants - presently only 1 option but in the future this function could expand to include all ABG products
  # by using sub functions for each?  Could have a main driver section first that calls product sub functions
  # check if files exist locally, also are they valid?  Check DB for file location
  # check if files exist remotely, get file size? Is that relevant as remote files are likely .zip
  # confirm function achieves desired results. does it have everything it needs? too much? too complicated?
  # confirm all params make sense and are useful, prune any that are not
  # avoid redundancy / duplication
  # possible issue, making assumption that products will be structured as obs product / error product, may not always be the case,
  # as such will need to work on generalizing such that the download step is simple and each url is prepped beforehand
  # 
  # ...
  # add extract_ABG function below to pull out the pixel value of ABG
  
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
      ncores <- parallel::detectCores() -1
    }
    PEcAn.logger::logger.info(paste0("Running in parallel with: ", ncores))
  }
  
  ## setup
  PEcAn.logger::logger.info("*** Downloading LandTrendr ABG data products ***")
  URL <- "ftp://islay.ceoas.oregonstate.edu/cms"
  
  # put in default product version if missing - can we make this dynamic and "find" latest version on FTP?
  if (is.null(prodcut_version)) {
    prodcut_version <- "v1"
    PEcAn.logger::logger.info(paste0("No product version selected, using version: ", prodcut_version))
  }
  
  # setup product defaults
  target_dataset <- "biomassfiaald"
  file_ext <- ".zip"
  obs_files <- paste0(target_dataset,"_",target_download_years,"_median",file_ext)  # hard-coded name matching source
  err_files <- paste0(target_dataset,"_",target_download_years,"_stdv",file_ext)    # hard-coded name matching source
  files_to_download <- c(obs_files,err_files)
  local_files <- file.path(outdir,gsub(".zip", ".tif",files_to_download))
  
  prod_obs_urls <- paste(URL,prodcut_version,target_dataset,"median",obs_files,sep="/")
  prod_err_urls <- paste(URL,prodcut_version,target_dataset,"stdv",err_files,sep="/")
  download_urls <- c(prod_obs_urls,prod_err_urls)
  
  # identify these are compressed files
  compressed <- TRUE

  ## download data
  # before downloading check that the remote FTP contains the desired years - a little clunky, clean up
  # if we keep this, will need to check this works with other data sources/products
  check_urls <- paste0(unique(dirname(download_urls), fromLast = TRUE),"/")
  remote_filenames <- Map(function(p) RCurl::getURL(p, ftp.use.epsv = FALSE, 
                                                    ftplistonly = TRUE, crlf = TRUE), check_urls)
  remote_filenames_list <- strsplit(paste(as.vector(unlist(remote_filenames)), 
                                          collapse = ''), "\r*\n")[[1]]
  if (sum(basename(download_urls) %in% remote_filenames_list, na.rm=T)!=length(download_urls)) { 
    `%not_in%` <- purrr::negate(`%in%`)
    missing <- which(basename(download_urls) %not_in% remote_filenames_list)
    download_urls[missing]
    PEcAn.logger::logger.severe(paste("Files missing from source: ", download_urls[missing]))
  }
  
  ## check for local files exist - do we want to do this?  Or use DB? Or both?
  # use this to subset the files that need to be downloaded. Check file size first?
  # ok to do this in one shot or need to check file by file....think this is OK
  if (file.exists(local_files) && !isTRUE(overwrite)) {
    files_to_download_final <- files_to_download[!file.exists(local_files)]
    download_urls_final <- download_urls[!file.exists(local_files)]
  } else {
    files_to_download_final <- files_to_download
    download_urls_final <- download_urls
  }

  # setup download
  if (length(files_to_download_final)<1) {
    PEcAn.logger::logger.info("*** Requested files already exist on this host, providing file paths ***")
  } else {
    `%dopar%` <- foreach::`%dopar%`
    PEcAn.logger::logger.info("*** Downloading AGB data ***")
    if (run_parallel) {
      cl <- parallel::makeCluster(ncores)
      doParallel::registerDoParallel(cl)
      foreach::foreach(i=1:length(files_to_download_final)) %dopar% try(download.file(download_urls_final[i], 
                                                                                      file.path(outdir,
                                                                                                files_to_download_final[i])))
    } else {
      PEcAn.logger::logger.info("Caution, downloading in serial. 
                                Could take an extended period to finish") # needed?
      Map(function(u, d) download.file(u, d), download_urls_final, file.path(outdir,
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
  
  return(results)
}
#

#
##' @title extract.LandTrendr.AGB
##' @name  extract.LandTrendr.AGB
##' 
##' @param coords vector of BETYdb site IDs or a data frame containing elements 'lon' and 'lat'
##' @param buffer Optional. operate over desired buffer area [not yet implemented]
##' @param fun Optional function to apply to buffer area.  Default - mean
##' @param data_dir  directory where input data is located. Can be NUL if con is specified
##' @param con connection to PEcAn database. Can be NULL if data_dir is specified
##' @param output_file Path to save LandTrendr_AGB_output.RData file containing the output extraction list (see return)
##' @param plot_results Optional. Create a simple diagnostic plot showing results of data extraction by site/coordinate pair
##' 
##' @return list of two containing the median AGB values per pixel and the corresponding standard deviation values (uncertainties)
##' 
##' ##' @examples
##' \dontrun{
##' # Database connection (optional)
##' bety <- list(user='bety', password='bety', host='localhost',
##' dbname='bety', driver='PostgreSQL',write=TRUE)
##' con <- PEcAn.DB::db.open(bety)
##' bety$con <- con
##' 
##' # Example 1 - using BETYdb site IDs to extract data
##' data_dir <- "~/scratch/agb_data/"
##' site_ID <- c(2000000023,1000025731,676,1000005149)  # US-CZ3, US-SSH, US-WCr, US-UMd
##' results <- PEcAn.data.remote::extract.LandTrendr.AGB(coords=site_ID, data_dir = data_dir, con = con, 
##'            output_file = file.path(data_dir))
##'            
##' # Example 2 - provide arbitrary lon/lat values
##' coords <- data.frame(lon=c(-90.8,-91.8), lat=c(42.5,43.5))
##' results <- PEcAn.data.remote::extract.LandTrendr.AGB(coords=coords, data_dir = data_dir,
##'            output_file = file.path(data_dir)
##' }
##' 
##' @export
##' @author Shawn Serbin, Alexey Shiklomanov
##' 
extract.LandTrendr.AGB <- function(coords, buffer = NULL, fun = "mean", data_dir = NULL, con = NULL, 
                                   output_file = NULL, plot_results = FALSE, ...) {
  
  ## TODO - allow selection of specific years to process instead of just entire record?
  
  ## Site ID vector or long/lat data.frame?
  if (is.null(names(coords))) {
    site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})", 
                               ids = site_ID, .con = con)
    qry_results <- DBI::dbSendQuery(con,site_qry)
    qry_results <- DBI::dbFetch(qry_results)
    site_IDs <- qry_results$id
    site_names <- qry_results$sitename
    site_coords <- data.frame(cbind(qry_results$lon, qry_results$lat))
    names(site_coords) <- c("Longitude","Latitude")
  } else {
    site_IDs <- dplyr::mutate(coords, IDs = paste(lon, lat, sep=","))[[3]]
    site_coords <- coords
    names(site_coords) <- c("Longitude","Latitude")
    site_names <- rep(NA,length=length(site_IDs))
  }

  ## apply coord info
  coords_latlong <- sp::SpatialPoints(site_coords)
  sp::proj4string(coords_latlong) <- sp::CRS("+init=epsg:4326")
  
  ## load gridded AGB data
  biomass_median <- lapply(list.files(file.path(data_dir), pattern = "*median.tif$", full.names = TRUE), raster::raster)
  biomass_stdv <- lapply(list.files(file.path(data_dir), pattern = "*stdv.tif$", full.names = TRUE), raster::raster)
  
  ## reproject Lat/Long site coords to AGB Albers Equal-Area
  coords_AEA <- sp::spTransform(coords_latlong,raster::crs(raster::raster(biomass_median[[1]])))
  
  ## prepare product for extraction - stack requested years, need to make this work for 1 or 2+ years
  biomass_median_stack <- raster::stack(biomass_median)
  biomass_stdv_stack <- raster::stack(biomass_stdv)
  
  ## extract
  agb_median_pixel <- raster::extract(x = biomass_median_stack, y = coords_AEA, buffer=NULL, fun=NULL, df=FALSE)
  processed_years <- unlist(regmatches(names(data.frame(agb_median_pixel)), 
                                       gregexpr("\\d{4}", names(data.frame(agb_median_pixel)))))
  agb_median_pixel <- data.frame(agb_median_pixel)
  names(agb_median_pixel) <- paste0("Year_",processed_years)
  agb_median_pixel <- data.frame(Site_ID=site_IDs, Site_Name=site_names, agb_median_pixel)

  agb_stdv_pixel <- raster::extract(x = biomass_stdv_stack, y = coords_AEA, buffer=NULL, fun=NULL, df=FALSE)
  agb_stdv_pixel <- data.frame(agb_stdv_pixel)
  names(agb_stdv_pixel) <- paste0("Year_",processed_years)
  agb_stdv_pixel <- data.frame(Site_ID=site_IDs, Site_Name=site_names, agb_stdv_pixel)
  
  ## output list
  point_list <- list(median_AGB=list(agb_median_pixel), stdv_AGB=list(agb_stdv_pixel))
  
  ## save output to a file?
  if (!is.null(output_file)) {
    save("point_list",file = file.path(output_file,'LandTrendr_AGB_output.RData'))
  }
  
  if (plot_results) {
    print("not yet")
    
    melted_med <- reshape::melt(agb_median_pixel, id.vars = c( 'Site_ID', 'Site_Name'))
    melted_med$year <- rep(processed_years,each=length(site_IDs))
    names(melted_med)[4] <- "AGB_med"
    all <- data.frame(melted_med,AGB_sd=melted_sd$value)

    ggplot2::ggplot(all, aes(x=year, y=AGB_med, group=Site_ID)) + geom_line(size=2) + 
      facet_wrap(~Site_ID) + theme_bw() + 
      geom_errorbar(aes(ymin=AGB_med-AGB_sd, ymax=AGB_med+AGB_sd), width=.2,
                    position=position_dodge(0.05))
    
  }
  
  ## return output list
  return(point_list)
}
