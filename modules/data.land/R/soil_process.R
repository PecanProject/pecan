#' Module for managing soil texture extraction
#'
#' @param settings  PEcAn settings list
#' @param input     PEcAn input list
#' @param dbfiles   directory to write database files
#' @param overwrite overwrite previous results (boolean)
#'
#' @return path to soil file
#' @export
#'
#' @examples
soil_process <- function(settings, input, dbfiles, overwrite = FALSE,run.local=TRUE){
  
  browser()
  
  if(input$soil$source=="PalEON_soil" & is.null(input$id)){
    PEcAn.logger::logger.severe("currently soil_process requires an input ID to be specified")
    return(NULL)
  }
  
  if(is.null(input$soil$source)){
    input$soil$source <- "PalEON_soil"  ## temporarily hardcoding in the only source
                                   ## in the future this should throw an error
  }
  

  # Extract info from settings and setup
  site       <- settings$run$site
  model      <- settings$model$type
  host       <- settings$host
  dbparms    <- settings$database
  
  # set up bety connection
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  con <- bety$con
  on.exit(PEcAn.DB::db.close(con))
  # get site info
  latlon <- PEcAn.data.atmosphere::db.site.lat.lon(site$id, con = con)
  new.site <- data.frame(id = as.numeric(site$id), 
                         lat = latlon$lat, 
                         lon = latlon$lon)
  str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)
  outfolder <- file.path(dbfiles, paste0(input$soil$source, "_site_", str_ns))
  if(!dir.exists(outfolder)) dir.create(outfolder)
  #--------------------------------------------------------------------------------------------------#   
  # if we are reading from gSSURGO
  if (input$soil$source=="gSSURGO"){
    newfile<-extract_soil_gssurgo(outfolder,lat = latlon$lat,lon=latlon$lon)
    return(newfile)
  }
  
   #--------------------------------------------------------------------------------------------------# 
  # get existing input info
  source.input <- PEcAn.DB::db.query(paste0("SELECT * from Inputs where id =",input$id),con)
  if(run.local){
    source.dbfiles <- PEcAn.DB::dbfile.check("Input",input$id,con,hostname='localhost')
  }else{
    source.dbfiles <- PEcAn.DB::dbfile.check("Input",input$id,con,hostname=host$name)
  }
  source.file <- file.path(source.dbfiles$file_path,source.dbfiles$file_name)
  if(source.input$site_id == site$id){
    ## Input is alreadly local
    if(!is.null(input$path)){
      return(input$path) ## path already exists, just return
    } else { ## path doesn't exist, see if we can find the relevant dbfile
      return(source.file)    
    }
  }  ## otherwise continue to process soil
  

  # set up host information
  machine.host <- ifelse(host == "localhost" || host$name == "localhost" || run.local,
                         PEcAn.remote::fqdn(), host$name)
  machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '", machine.host, "'"), con)
  
  # retrieve model type info
  if(is.null(model)){
    modeltype_id <- db.query(paste0("SELECT modeltype_id FROM models where id = '", settings$model$id, "'"), con)[[1]]
    model <- db.query(paste0("SELECT name FROM modeltypes where id = '", modeltype_id, "'"), con)[[1]]
  }
  
  newfile <- PEcAn.data.land::extract_soil_nc(source.file,outfolder,lat = latlon$lat,lon=latlon$lon)
  
  return(newfile)
} # ic_process