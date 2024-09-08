#' Module for managing soil texture extraction
#'
#' @param settings  PEcAn settings list
#' @param input     PEcAn input list
#' @param dbfiles   directory to write database files
#' @param overwrite overwrite previous results (boolean)
#' @param run.local logical: Run only on the current machine?
#'  If FALSE, runs on `settings$host` (which might turn out to be the current machine)
#'
#' @return path to soil file
#' @export
#'
#' 
soil_process <- function(settings, input, dbfiles, overwrite = FALSE,run.local=TRUE){

  # This tries to avoid the problem of having the soil tag under input but not having source in it.
  if(is.null(input$source)){
    input$source <- "gSSURGO"  ## temporarily hardcoding in the only source
    ## in the future this should throw an error
  }else if(input$source=="PalEON_soil" && is.null(input$id)){
    PEcAn.logger::logger.severe("currently soil_process requires an input ID to be specified")
    return(NULL)
  }

  # Extract info from settings and setup
  site       <- settings$run$site
  model      <- settings$model$type
  host       <- settings$host
  dbparms    <- settings$database
  # set up bety connection
  con <- PEcAn.DB::db.open(dbparms$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  # get site info
  latlon <- PEcAn.DB::query.site(site$id, con = con)[c("lat", "lon")]
  new.site <- data.frame(id = as.numeric(site$id),
                         lat = latlon$lat,
                         lon = latlon$lon)

  str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)

  outfolder <- file.path(dbfiles, paste0(input$source, "_site_", str_ns))

  if(!dir.exists(outfolder)) dir.create(outfolder)
  #--------------------------------------------------------------------------------------------------#
  # if we are reading from gSSURGO
  if (input$source=="gSSURGO"){

    #see if there is already files generated there
    newfile <-list.files(outfolder, "*.nc$", full.names = TRUE) %>%
      as.list()
    names(newfile) <- rep("path", length(newfile))

    if(length(newfile)==0){
      radiusL <- ifelse(is.null(settings$run$input$soil$radius), 500, as.numeric(settings$run$input$soil$radius))

      newfile<-extract_soil_gssurgo(outfolder, lat = latlon$lat, lon=latlon$lon, radius = radiusL)

      # register files in DB
      for(i in 1:length(newfile)){
        in.path = paste0(dirname(newfile[i]$path), '/')
        in.prefix = stringr::str_remove(basename(newfile[i]$path), ".nc")

        PEcAn.DB::dbfile.input.insert (in.path,
                             in.prefix,
                             new.site$id,
                             startdate = NULL,
                             enddate = NULL,
                             mimetype =  "application/x-netcdf",
                             formatname = "pecan_soil_standard",
                             con = con,
                             ens=TRUE)
      }


      }

    return(newfile)
  }
  #--------------------------------------------------------------------------------------------------#
  # if we are reading  PalEON_soil
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
    modeltype_id <- PEcAn.DB::db.query(paste0("SELECT modeltype_id FROM models where id = '", settings$model$id, "'"), con)[[1]]
    model <- db.query(paste0("SELECT name FROM modeltypes where id = '", modeltype_id, "'"), con)[[1]]
  }

  newfile <- PEcAn.data.land::extract_soil_nc(source.file,outfolder,lat = latlon$lat,lon=latlon$lon)

  return(newfile)
} # ic_process
