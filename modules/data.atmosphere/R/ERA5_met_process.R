#' Met Processes for ERA5 data
#'
#' @param settings a multi-settings object
#' @param in.path met input path
#' @param start_date start date
#' @param end_date end date
#' @param out.path output path
#' @param Write if write into Bety database
#'
#' @return if Write is True then return input IDs with physical paths; if Write is False then return just physical paths of extracted ERA5 clim files.
#' @export
#'
#' @examples
ERA5_met_process <- function(settings, in.path, start_date, end_date, out.path, Write=FALSE){
  #getting site info
  #getting site ID
  observations <- c()
  for (i in seq_along(1:length(settings$run))) {
    command <- paste0("settings$run$settings.",i,"$site$id")
    obs <- eval(parse(text=command))
    observations <- c(observations,obs)
  }
  
  #query site info
  dbparms = list()
  dbparms$dbname = "bety"
  dbparms$host = "128.197.168.114"
  dbparms$user = "bety"
  dbparms$password = "bety"
  
  #Connection code copied and pasted from met.process
  bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                              host     = dbparms$host, 
                              user     = dbparms$user, 
                              password = dbparms$password)
  con <- bety$con
  site_ID <- observations
  suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                              ids = site_ID, .con = con))
  suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
  suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
  site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                    lon=qry_results$lon, time_zone=qry_results$time_zone)
  
  #db query
  if(Write){
    mimetype <- "application/x-netcdf"
    formatname <- "CF Meteorology"
    hostname <- PEcAn.remote::fqdn()
    # find mimetype, if it does not exist, it will create one
    mimetypeid <- get.id("mimetypes", "type_string", mimetype, con, create = TRUE)
    
    # find appropriate format, create if it does not exist
    formatid <- get.id(
      table = "formats",
      colnames = c("mimetype_id", "name"),
      values = c(mimetypeid, formatname),
      con = con,
      create = TRUE,
      dates = TRUE
    )
    
    # setup parent part of query if specified
    parent <- ""
  }
  
  #loop over each site
  #Record all IDs for inputs and dbfiles
  if(Write){
    Input_IDs <- list()
  }
  
  #record paths of extracted .clim files if not writing into bety
  if(!Write){
    Clim_paths <- list()
  }
  
  #loop over each site
  for (i in 1:length(site_info$site_id)) {
    #check if sub-folder exists, if doesn't then create a new folder specific for each site
    site_outFolder <- paste0(out.path,'/',as.character(site_info$site_id[i]))
    
    #check if folder already exists, if it does, then jump to the next loop
    if(!file.exists(site_outFolder)){
      dir.create(site_outFolder)
    }else{
      print(paste0("The output files for site ",as.character(site_info$site_id[i])," already exists jump to the next site"))
      next
    }
    
    #Write into inputs table for each site
    if(Write){
      #insert into inputs table
      cmd <- paste0(
        "INSERT INTO inputs ",
        "(site_id, format_id, start_date, end_date, name) VALUES (",
        site_info$site_id[i], ", ", formatid, ", '", start_date, "', '", end_date, "','", paste0('ERA5_',site_info$site_id[i]),
        "') RETURNING id"
      )
      # This is the id that we just registered
      inputid <- PEcAn.DB::db.query(query = cmd, con = con)
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
    
    #starting working on met2model.SIPNET function over each ensemble
    #find every path associated with each ensemble member
    ens_nc <- list.files(path = site_outFolder, full.names = T)
    
    #finding the ensemble number
    #loop over each ensemble member
    #record dbfile IDs
    if(Write){
      dbfile_IDs <- c()
    }
    for (j in 1:length(ens_nc)) {
      nc_path <- ens_nc[j]
      
      #find a proper in prefix for each ensemble member
      ens_num <- strsplit(basename(nc_path),"_")[[1]][3]
      in_prefix <- paste0("ERA5.", ens_num)
      
      #preparing for the met2model.SIPNET function
      PEcAn.SIPNET::met2model.SIPNET(in.path = nc_path,
                                     in.prefix = in_prefix,
                                     outfolder = site_outFolder,
                                     start_date = start_date,
                                     end_date = end_date
      )
      
      #Write into dbfiles table
      if(Write){
        dbfileid <- dbfile.insert(
          in.path = nc_path, in.prefix = in_prefix, type = "Input", id = inputid,
          con = con, reuse = TRUE, hostname = hostname
        )
        dbfile_IDs <- c(dbfile_IDs, dbfileid)
      }
    }
    
    #record paths of clim files if not Write into Bety
    if(!Write){
      Clim_paths[i] <- list(in.path=list.files(path=site_outFolder, pattern = '*.clim', full.names = T))
    }

    #record input ID and dbfile IDs and extracted paths of clim files for each site
    if(Write){
      Input_IDs[[i]] <- list(input_ID=inputid$id, dbfile_IDs=dbfile_IDs, Site_ID=site_info$site_id[i], in.path=list.files(path=site_outFolder, pattern = '*.clim', full.names = T))
    }
  }
  if(Write){
    save(Input_IDs, file=paste0(out.path, '/', 'Inputs.RData'))
    return(Input_IDs)
  }else{
    save(Clim_paths, file=paste0(out.path, '/', 'Inputs.RData'))
    return(Clim_paths)
  }
}

# #test code
# #get settings
# setwd("/projectnb/dietzelab/dongchen/Multi-site/download_500_sites")
# xml_files <- paste0("Site_XMLS/",list.files("Site_XMLS"))
# 
# settings <- list()
# for (i in 1:length(xml_files)) {
#   settings[i] <- list(settings=read.settings(xml_files[i]))
# }
# 
# #convert to multi-settings
# settings <- MultiSettings(settings)
# 
# #
# in.path <- "/projectnb/dietzelab/hamzed/ERA5/Data/Ensemble"
# out.path <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/ERA5_2012_2019"
# start_date <- as.Date(paste0(2012,"-01-01"), tz="UTC")
# end_date <- as.Date(paste0(2019,"-12-31"), tz="UTC")
# Write <- T
# 
# ERA5_met_process(settings, in.path, start_date, end_date, out.path)
