##' @name ic_process
##' @title ic_process
##' @export
##'
##' @param settings pecan settings list
##' @param dbfiles where to write files
##' @param overwrite whether to force ic_process to proceed
##' @author Istem Fer
ic_process <- function(settings, input, dir, overwrite = FALSE){
  
  
  #--------------------------------------------------------------------------------------------------#
  # Extract info from settings and setup
  site       <- settings$run$site
  start_date <- settings$run$start.date
  end_date   <- settings$run$end.date
  model      <- settings$model$type
  host       <- settings$host
  dbparms    <- settings$database
  
  # file_path <- file.path(dir, paste0(inputinfo$source, "_site_", runinfo$site$id))
  # ### check if file_path exists, create otherwise
  # dir.create(file_path, showWarnings = F, recursive = T)
  # 
  # start_year <- lubridate::year(runinfo$start.date)
  # end_year   <- lubridate::year(runinfo$end.date)
  # startdate   <- lubridate::as_date(paste0(start_year, "-01-01"))
  # enddate <- lubridate::as_date(paste0(end_year, "-12-31"))
  # 
  # lat <- as.numeric(runinfo$site$lat)
  # lon <- as.numeric(runinfo$site$lon)
  
  # obs <- NULL
 
  # If overwrite is a plain boolean, fill in defaults for each stage
  if (!is.list(overwrite)) {
    if (overwrite) {
      overwrite <- list(getveg = TRUE,  writeveg = TRUE)
    } else {
      overwrite <- list(getveg = FALSE, writeveg = FALSE)
    }
  } else {
    if (is.null(overwrite$getveg)) {
      overwrite$getveg <- FALSE
    }
    if (is.null(overwrite$writeveg)) {
      overwrite$writeveg <- FALSE
    }
  }
  
  # set up bety connection
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  
  con <- bety$con
  on.exit(db.close(con))
  
  # set up host information
  machine.host <- ifelse(host == "localhost" || host$name == "localhost", fqdn(), host$name)
  machine <- db.query(paste0("SELECT * from machines where hostname = '", machine.host, "'"), con)
  
  
  # setup site database number, lat, lon and name and copy for format.vars if new input
  new.site <- data.frame(id = as.numeric(site$id), 
                         lat = PEcAn.data.atmosphere::db.site.lat.lon(site$id, con = con)$lat, 
                         lon = PEcAn.data.atmosphere::db.site.lat.lon(site$id, con = con)$lon)
  str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)
  
  #--------------------------------------------------------------------------------------------------#
  # Load/extract + match species module
  
  if (is.null(input$id) {
    
    raw.id <- .get.veg.module(dir = dir,
                              input_veg = input, 
                              machine = machine, 
                              start_date = start_date, end_date = end_date,
                              str_ns = str_ns, con = con, dbparms = dbparms,
                              lat = new.site$lat, lon = new.site$lon,
                              host = host, 
                              overwrite = overwrite$getveg)

    
    #############################################################################
    ### Do we want to save the intermediate step here for pss?                ###
    ### If not, veg2model.[model] function should take "obs" as an argument   ###
    ### because pss will skip the next two stage and go right into veg2model  ###
    #############################################################################
    
    # ### saving the intermediate step
    # file_name <- paste0("fia.", inputinfo$output, ".", start_year, "_", end_year, ".txt")
    # write.table(obs, file = file.path(file_path, file_name), col.names = TRUE, row.names = FALSE, sep = "\t")
    # 
  

  }
  
  #--------------------------------------------------------------------------------------------------#
  # Spp match module
  
  #--------------------------------------------------------------------------------------------------#
  # Load data
  if (input$source %in% load.sources) {
    
    source.id <- ifelse(is.null(inputinfo$source.id), 
                     logger.error("Must specify input source.id"), 
                     inputinfo$source.id)

    # query data.path from source id [input id in BETY]
    query <- paste0("SELECT * FROM dbfiles where container_id = ", source.id)
    data.path <- file.path(db.query(query, con = con)[["file_path"]], db.query(query, con = con)[["file_name"]])
      
    # query format info
    format <- query.format.vars(bety = bety, input.id = source.id)
    
    # load data
    obs <- load_data(data.path, format, site = runinfo$site)
    
    stage$loaddata <- FALSE
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Match code to species
  if (stage$sppmatch) {
    
    # decide which code format to use while matching species
    # should we retrieve it from settings or assign a code format per source type?
    # or allow both?
    if(inputinfo$source %in% c("GapMacro", "NASA_TE_FIA")){
      format.name = 'usda'
    }else if(inputinfo$source %in% c("FIA")){
      format.name = 'fia'      
    }else if(!is.null(inputinfo$match.format)){
      format.name = inputinfo$match.format
    }else{
      logger.error("Can't match code to species. No valid format found.")
    }
    
    # decide which column has the codes
    # this block may or may not be merged with the block above
    if(format.name == 'usda'){
      code.col = "species_USDA_symbol"
    }else if(format.name == 'fia'){
      code.col = "spcd"
    }else if(format.name == 'latin_name'){
      code.col = "latin_name"
    }
    
    # match code to species ID
    spp.info <- match_species_id(input_codes = obs[[code.col]], format_name = format.name, bety = bety)
    
    if(sum(is.na(spp.info$bety_species_id)) > 0){
      bad <- unique(spp.info$input_code[is.na(spp.info$bety_species_id)])
      PEcAn.utils::logger.error(paste0("Species for the following code(s) not found : ", paste(bad, collapse = ", ")))
    }
    
    # merge with data
    tmp <- spp.info[ , colnames(spp.info) != "input_code"]
    obs <- cbind(obs, tmp)
    
    ### write
    file_name <- paste0("sppmatch.", start_year, "_", end_year, ".txt")
    
    # where to read file from in veg2model
    inputinfo$path <- file.path(file_path, file_name)
    
    ### obsspppft is the combined thing, testing
    write.table(obs, file.path(file_path, file_name), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
    
  
    
    stage$sppmatch <- FALSE
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Match species to PFTs
  if (stage$pftmatch) {
    
    # can there be a stage that will skip the match species step above?
    # you can continue from above or read from previously processed data
    # anything in the DB takes precedence
    if(!is.null(inputinfo$path)){
      obs <- read.table(inputinfo$path, header = TRUE)
    }
    
    pft.info <- match_pft(obs$bety_species_id, pfts, con)
    
    ### merge with other stuff
    obs <- cbind(obs, pft.info[c("bety_pft_id", "pft")])
    
    ### write
    file_name <- paste0("pftmatch.", start_year, "_", end_year, ".txt")
    
    # where to read file from in veg2model
    inputinfo$path <- file.path(file_path, file_name)
      
    ### obsspppft is the combined thing, testing
    write.table(obs, file.path(file_path, file_name), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
    
    stage$pftmatch <- FALSE
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Write model specific IC files
  if (stage$veg2model) {
    
    ## Set model-specific functions
    do.call("library", list(paste0("PEcAn.", model)))
    veg2model.fcn <- paste("veg2model.", model, sep = "")
    if (!exists(veg2model.fcn)) {
      logger.severe(paste(veg2model.fcn, "does not exist."))
    }
    
    # call veg2model.[model]
    fcn    <- match.fun(veg2model.fcn)
    ic.id  <- fcn(obs, inputinfo = inputinfo, runinfo = runinfo, host, con, outfolder = file_path)
    
  }
  
  model.file <- db.query(paste("SELECT * from dbfiles where container_id =", ic.id), con)
  
  return(file.path(model.file$file_path, model.file$file_name))
} # ic_process