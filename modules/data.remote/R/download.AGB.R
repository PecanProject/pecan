##' @title download.AGB
##' @name  download.AGB
##' 
##' @param outdir Where to place output
##' @param product_dates What dates to download and extract
##' @param temporal_resolution Optional. What temporal resolution to download. Current options: "annual"
##' @param product_source The name of the desired AGB prodcut.  Current options: "OSU"
##' @param prodcut_version Optional. For some products (e.g. OSU) there are multiple versions.  
##' @param con Optional database connection. If specified then the code will check to see 
## if the file already exists in PEcAn before downloading, and will also create a database 
## entry for new downloads
##' @param run_parallel Logical. Download and extract files in parallel?
##' @param ncores Optional. If run_parallel=TRUE how many cores to use?  If left as NULL will select max number -1
##' @param generate_plots Optional. Create output diagnostic plots in outdir? --- ACTUALLY PUT THIS IN extract_ABG
##' @param overwrite Logical. Overwrite existing files and replace with new versions
##' 
##' @importFrom 
##' 
##' @export
##' @author Shawn Serbin
##'
download.AGB <- function(outdir, product_dates = NULL, temporal_resolution = "annual", 
                         product_source = "OSU", prodcut_version = NULL, con = NULL, 
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
    # force seq or allow entering of discontinous dates?  Might need to take these explicitly instead
    target_download_years <- seq(product_dates[1],product_dates[2], 1)  # sort so that if dates are entered out of order?
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
  
  if (product_source == "OSU") {
    PEcAn.logger::logger.info("*** Downloading OSU ABG data products ***")  # need to replace with correct product name. dont use generic OSU
    URL <- "ftp://islay.ceoas.oregonstate.edu/cms"
    if (is.null(prodcut_version)) {
      prodcut_version <- "v1"
      PEcAn.logger::logger.info(paste0("No product version selected, using version: ", prodcut_version))
    }

    # setup product defaults
    target_dataset <- "biomassfiaald"
    file_ext <- ".zip"
    obs_files <- paste0(target_dataset,"_",target_download_years,"_median",file_ext)  # hard-coded name matching source
    err_files <- paste0(target_dataset,"_",target_download_years,"_stdv",file_ext)    # hard-coded name matching source
    
    prod_obs_urls <- paste(URL,prodcut_version,target_dataset,"median",obs_files,sep="/")
    prod_err_urls <- paste(URL,prodcut_version,target_dataset,"stdv",err_files,sep="/")
    
    ## set flag
    compressed <- TRUE
    
  } else {
    PEcAn.logger::logger.severe("*** Data product not yet availible ***")
  }
  
  ## download data
  `%dopar%` <- foreach::`%dopar%`
  PEcAn.logger::logger.info("*** Downloading AGB data")
  if (run_parallel) {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    
    # making assumptions here that may not hold for other data products. When adding other 
    # products will need to work on generalizing this download function
    foreach::foreach(i=1:length(prod_obs_urls)) %dopar% try(download.file(prod_obs_urls[i], 
                                                                          file.path(outdir,
                                                                                    obs_files[i])))
    foreach::foreach(j=1:length(prod_err_urls)) %dopar% try(download.file(prod_err_urls[j], 
                                                                                     file.path(outdir, 
                                                                                               err_files[j])))
  } else {
    PEcAn.logger::logger.info("Caution, downloading in serial. 
                              Could take an extended period to finish") # needed?
    Map(function(u, d) download.file(u, d), prod_obs_urls, file.path(outdir,obs_files))
    Map(function(u, d) download.file(u, d), prod_err_urls, file.path(outdir,err_files))
    
  }
  PEcAn.logger::logger.info("*** Downloading complete ***")
  
  ## making assumption here. should probably look to see if a compressed format or not...or above provide flag
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
    }
    
  }


}
