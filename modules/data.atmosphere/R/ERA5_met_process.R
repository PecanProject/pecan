#' Met Processes for ERA5 data
#'
#' @param settings a multi-settings object
#' @param in.path met input path
#' @param out.path output path
#' @param write.db if write into Bety database
#' @param write if write the settings into pecan.xml file in the outdir of settings.
#'
#' @return if write.db is True then return input IDs with physical paths; if write.db is False then return just physical paths of extracted ERA5 clim files.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
#'
ERA5_met_process <- function(settings, in.path, out.path, write.db=FALSE, write = TRUE){
  #Initialize the multicore computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  
  #getting site info
  #grab the site info from Bety DB if we can't get the site info directly from the settings object.
  if ("try-error" %in% class(try(site_info <- settings$run %>% 
                                 purrr::map('site')%>% 
                                 purrr::map(function(site.list){
                                   #conversion from string to number
                                   site.list$lat <- as.numeric(site.list$lat)
                                   site.list$lon <- as.numeric(site.list$lon)
                                   list(site.id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
                                 }) %>% 
                                 dplyr::bind_rows() %>% 
                                 as.list()))) {
    #getting site ID
    observations <- c()
    for (i in 1:length(settings)) {
      obs <- settings[[i]]$run$site$id
      observations <- c(observations,obs)
    }
    
    #query site info
    bety <- dplyr::src_postgres(dbname   = settings$database$bety$dbname,
                                host     = settings$database$bety$host,
                                user     = settings$database$bety$user,
                                password = settings$database$bety$password)
    con <- bety$con
    site_ID <- observations
    suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                                ids = site_ID, .con = con))
    suppressWarnings(qry_results <- PEcAn.DB::db.query(con = con, query = site_qry))#use PEcAn.DB instead
    site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                      lon=qry_results$lon, time_zone=qry_results$time_zone)
  }
  
  #initialize db query elements
  if(write.db){
    mimetype <- "application/x-netcdf"
    formatname <- "CF Meteorology"
    hostname <- PEcAn.remote::fqdn()
    # find mimetype, if it does not exist, it will create one
    mimetypeid <- PEcAn.DB::get.id("mimetypes", "type_string", mimetype, con, create = TRUE)
    
    # find appropriate format, create if it does not exist
    formatid <- PEcAn.DB::get.id(
      table = "formats",
      colnames = c("mimetype_id", "name"),
      values = c(mimetypeid, formatname),
      con = con,
      create = TRUE,
      dates = TRUE
    )
    
    # setup parent part of query if specified
    parent <- ""
    
    #initialize Input_IDs object when looping over each site
    Input_IDs <- list()
  }
  
  #restructure the site_info into list.
  site_info$start_date <- start_date <- rep(settings$state.data.assimilation$start.date, length(settings))
  site_info$end_date <- end_date <- rep(settings$state.data.assimilation$end.date, length(settings))
  site_info$out.path <- rep(out.path, length(settings))
  site_info$in.path <- rep(in.path, length(settings))
  site_info$model.type <- rep(settings$model$type, length(settings))
  new.site.info <- split(as.data.frame(site_info), seq(nrow(as.data.frame(site_info))))
  
  #Extract ERA5 for each site.
  PEcAn.logger::logger.info("Started extracting ERA5 data!\n")
  Clim_paths <- furrr::future_map(new.site.info, function(site){
    #check if sub-folder exists, if doesn't then create a new folder specific for each site
    site_outFolder <- paste0(site$out.path,'/', site$site.id)
    #check if folder already exists, if it does, then jump to the next loop
    if(!file.exists(site_outFolder)){
      dir.create(site_outFolder)
    }else{
      #grab physical paths of existing ERA5 files
      #need to be generalized when more models come in.
      clim.paths <- list(in.path=list.files(path=site_outFolder, pattern = '*.clim', full.names = T))
      names(clim.paths) <- site$site.id
      return(clim.paths)
    }
    
    #extract ERA5.nc files
    PEcAn.data.atmosphere::extract.nc.ERA5(slat = site$lat,
                                           slon = site$lon,
                                           in.path = site$in.path,
                                           start_date = site$start_date,
                                           end_date = site$end_date,
                                           outfolder = site_outFolder,
                                           in.prefix = 'ERA5_',
                                           newsite = as.character(site$site.id))
    
    #starting working on met2model.model function over each ensemble
    #setting up met2model function depending on model name from settings
    met2model_method <- do.call("::", list(paste0("PEcAn.", site$model.type), paste0("met2model.", site$model.type)))
    #grab the rbind.xts function
    rbind.xts <- do.call("::", list("xts", "rbind.xts"))
    #find every path associated with each ensemble member
    ens_nc <- list.files(path = site_outFolder, full.names = T)
    #loop over each ensemble member
    for (i in 1:length(ens_nc)) {
      nc_path <- ens_nc[i]
      
      #find a proper in prefix for each ensemble member
      ens_num <- strsplit(basename(nc_path),"_")[[1]][3]
      in_prefix <- paste0("ERA5.", ens_num)
      
      #preparing for the met2model.SIPNET function
      met2model_method(in.path = nc_path,
                       in.prefix = in_prefix,
                       outfolder = site_outFolder,
                       start_date = site$start_date,
                       end_date = site$end_date)
    }
    # grab physical paths of ERA5 files
    clim.paths <- list(in.path=list.files(path=site_outFolder, pattern = '*.clim', full.names = T))
    names(clim.paths) <- site$site.id
    return(clim.paths)
  }, .progress = TRUE)
  PEcAn.logger::logger.info("\nFinished!")
  
  #write the paths into settings.
  if (write) {
    #write paths into settings.
    for (i in seq_along(settings)) {
      #fill in dates related to met files.
      settings[[i]]$run$site$met.start <- 
        settings[[i]]$run$start.date <- 
        settings[[i]]$state.data.assimilation$start.date
      settings[[i]]$run$site$met.end <- 
        settings[[i]]$run$end.date <- 
        settings[[i]]$state.data.assimilation$end.date
      settings[[i]]$run$inputs$met$path <- as.list(unlist(Clim_paths[[i]])) %>% purrr::set_names(rep("path", length(Clim_paths[[i]])))
    }
    
    #write settings into xml file.
    PEcAn.logger::logger.info(paste0("Write updated pecan.xml file into: ", file.path(settings$outdir, "pecan.xml")))
    PEcAn.settings::write.settings(settings, outputfile = "pecan.xml")
  }
  
  #write into bety
  if(write.db){
    PEcAn.logger::logger.info("Write into database!")
    #loop over each site
    for (i in 1:length(site_info$site_id)) {
      #loop over each ensemble
      #initialize arrays to store input and dbfile IDs.
      dbfile_IDs <- c()
      input_IDs <- c()
      for(j in 1:length(Clim_paths[[i]])){
        #create input record for each ensemble member
        #insert into inputs table
        cmd <- paste0(
          "INSERT INTO inputs ",
          "(site_id, format_id, start_date, end_date, name) VALUES (",
          site_info$site_id[i], ", ", formatid, ", '", start_date, "', '", end_date, "','", paste0('ERA5_',site_info$site_id[i],"_",as.character(j)),
          "') RETURNING id"
        )
        # This is the id that we just registered
        inputid <- PEcAn.DB::db.query(query = cmd, con = con)
        input_IDs <- c(input_IDs, inputid)
        
        #create dbfiles associated with each ensemble ID
        dbfileid <- PEcAn.DB::dbfile.insert(
          in.path = Clim_paths[[i]][j], in.prefix = paste0("ERA5.", as.character(j)), type = "Input", id = inputid,
          con = con, reuse = TRUE, hostname = hostname
        )
        dbfile_IDs <- c(dbfile_IDs, dbfileid)
      }
      Input_IDs[[i]] <- list(input_ID=inputid$id, dbfile_IDs=dbfile_IDs, Site_ID=site_info$site_id[i], in.path=Clim_paths[[i]])
    }
    save(Input_IDs, file=paste0(out.path, '/', 'Inputs.RData'))
    return(Input_IDs)
  }else{
    save(Clim_paths, file=paste0(out.path, '/', 'Inputs.RData'))
    return(Clim_paths)
  }
}