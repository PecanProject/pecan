#' Met Processes for ERA5 data
#'
#' @param settings a multi-settings object
#' @param in.path met input path
#' @param out.path output path
#' @param Write if write into Bety database
#'
#' @return if Write is True then return input IDs with physical paths; if Write is False then return just physical paths of extracted ERA5 clim files.
#' @export
#'
#' @examples
ERA5_met_process <- function(settings, in.path, out.path, Write=FALSE){
  #getting site info
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
  
  #initialize db query elements
  if(Write){
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
  
  #initialize physical paths for each ERA5 file
  Clim_paths <- list()
  
  #initializing start and end date from settings
  start_date <- settings$state.data.assimilation$start.date
  end_date <- settings$state.data.assimilation$end.date
  
  #setting up met2model function depending on model name from settings
  met2model_method <- do.call("::", list(paste0("PEcAn.", settings$model$type), paste0("met2model.", settings$model$type)))
  
  #loop over each site
  for (i in 1:length(site_info$site_id)) {
    #check if sub-folder exists, if doesn't then create a new folder specific for each site
    site_outFolder <- paste0(out.path,'/',as.character(site_info$site_id[i]))
    
    #check if folder already exists, if it does, then jump to the next loop
    if(!file.exists(site_outFolder)){
      dir.create(site_outFolder)
    }else{
      #export info
      print(paste0("The output files for site ",as.character(site_info$site_id[i])," already exists jump to the next site"))
      
      #grab physical paths of existing ERA5 files
      #need to be generalized when more models come in.
      Clim_paths[i] <- list(in.path=list.files(path=site_outFolder, pattern = '*.clim', full.names = T))
      next
    }
    
    #extract ERA5.nc files
    PEcAn.data.atmosphere::extract.nc.ERA5(slat = site_info$lat[i],
                                           slon = site_info$lon[i],
                                           in.path = in.path,
                                           start_date = start_date,
                                           end_date = end_date,
                                           outfolder = site_outFolder,
                                           in.prefix = 'ERA5_',
                                           newsite = as.character(site_info$site_id[i]))
    
    #starting working on met2model.model function over each ensemble
    #find every path associated with each ensemble member
    ens_nc <- list.files(path = site_outFolder, full.names = T)
    
    #loop over each ensemble member
    for (j in 1:length(ens_nc)) {
      nc_path <- ens_nc[j]
      
      #find a proper in prefix for each ensemble member
      ens_num <- strsplit(basename(nc_path),"_")[[1]][3]
      in_prefix <- paste0("ERA5.", ens_num)
      
      #preparing for the met2model.SIPNET function
      met2model_method(in.path = nc_path,
                       in.prefix = in_prefix,
                       outfolder = site_outFolder,
                       start_date = start_date,
                       end_date = end_date)
    }
    # grab physical paths of ERA5 files
    Clim_paths[i] <- list(in.path=list.files(path=site_outFolder, pattern = '*.clim', full.names = T))
  }
  
  #write into bety
  if(Write){
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
