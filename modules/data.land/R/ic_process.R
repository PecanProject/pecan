##' @name ic_process
##' @title ic_process
##' @export
##'
##' @param pfts settings pft list
##' @param runinfo run from settings list
##' @param inputinfo which data source to use (FIA, GapMacro, FFT etc.) and username, 
##' this is a subset of runinfo and can be extracted from runinfo for now as this is only functional for css/pss/site, 
##' but might need this info separately when handling different IC types of IC data in the future
##' @param model model_type name
##' @param host Host info from settings file
##' @param dbparms  database settings from settings file
##' @param dir  directory to write outputs to
##' @param overwrite whether to force ic_process to proceed
##' @author Istem Fer
ic_process <- function(pfts, runinfo, inputinfo, model, host, dbparms, dir, overwrite = FALSE){
  
  mimetype    <- "text/plain"
  file_path <- file.path(dir, paste0(inputinfo$source, "_site_", runinfo$site$id))
  ### check if file_path exists, create otherwise
  dir.create(file_path, showWarnings = F, recursive = T)
  
  start_year <- lubridate::year(runinfo$start.date)
  end_year   <- lubridate::year(runinfo$end.date)
  startdate   <- lubridate::as_date(paste0(start_year, "-01-01"))
  enddate <- lubridate::as_date(paste0(end_year, "-12-31"))
  
  lat <- as.numeric(runinfo$site$lat)
  lon <- as.numeric(runinfo$site$lon)
  
  # these are not actually doing anything now, just as a reminder
  if (!is.list(overwrite)) {
    if (overwrite) {
      # Default for overwrite==TRUE is to overwrite everything but download
      overwrite <- list(download = FALSE, sppmatch = TRUE, pftmatch = TRUE,  veg2model = TRUE)
    } else {
      overwrite <- list(download = FALSE, sppmatch = FALSE, pftmatch = FALSE, veg2model = FALSE)
    }
  } else {
    if (is.null(overwrite$download)) {
      overwrite$download <- FALSE
    }
    if (is.null(overwrite$sppmatch)) {
      overwrite$sppmatch <- FALSE
    }
    if (is.null(overwrite$pftmatch)) {
      overwrite$pftmatch <- FALSE
    }
    if (is.null(overwrite$veg2model)) {
      overwrite$veg2model <- FALSE
    }
  }

  # might want to do overwrite.check
  
  # set up connection
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  
  con <- bety$con
  on.exit(db.close(con))
  
  # might want to set up host information
  
  # get source and potentially determine where to start in the process
  source <- ifelse(is.null(inputinfo$source), 
                logger.error("Must specify input source"), 
                inputinfo$source)
  
  
  # where to start IC process
  if (is.null(inputinfo$id)) {
    stage <- list(download = FALSE, loaddata = TRUE, sppmatch = TRUE, pftmatch = TRUE,  veg2model = TRUE)
    if(inputinfo$source == "FIA"){
      # other pss/site will also need not other stages, refactor here
      stage$download <- ifelse(inputinfo$output == "site", FALSE, TRUE)
      stage$loaddata <- FALSE
      stage$sppmatch <- ifelse(inputinfo$output == "css", TRUE, FALSE)
      stage$pftmatch <- ifelse(inputinfo$output == "css", TRUE, FALSE)
    }
  } else {
    
    ######
    
  }
  
  

  
  
  #--------------------------------------------------------------------------------------------------#
  # Download FIA
  if (stage$download) {
    
      
    ## connect to database
    fia.con <- db.open(dbparms$fia)
    on.exit(db.close(fia.con), add = T)
    
    obs <- download.FIA(inputinfo, lat, lon, year = start_year, con = fia.con)
    
    #############################################################################
    ### Do we want to save the intermediate step here for pss?                ###
    ### If not, veg2model.[model] function should take "obs" as an argument   ###
    ### because pss will skip the next two stage and go right into veg2model  ###
    #############################################################################
    
    # ### saving the intermediate step
    # file_name <- paste0("fia.", inputinfo$output, ".", start_year, "_", end_year, ".txt")
    # write.table(obs, file = file.path(file_path, file_name), col.names = TRUE, row.names = FALSE, sep = "\t")
    # 
    # # Insert into dbfiles
    # inputinfo$id <- dbfile.input.insert(
    #   in.path    = file_path,
    #   in.prefix  = file_name,
    #   siteid     = runinfo$site$id,
    #   startdate  = startdate,
    #   enddate    = enddate,
    #   mimetype   = mimetype,
    #   formatname = paste0("fia.", inputinfo$output),
    #   parentid   = NA,
    #   con        = con,
    #   hostname   = host,
    #   allow.conflicting.dates = TRUE
    # )$input.id
 
    stage$download <- FALSE
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Load data
  if (stage$loaddata) {
    
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
    
    # Insert into DB
    inputinfo$id <- dbfile.input.insert(
      in.path    = file_path,
      in.prefix  = file_name,
      siteid     = runinfo$site$id,
      startdate  = startdate,
      enddate    = enddate,
      mimetype   = mimetype,
      formatname = "spp.info",
      parentid   = NA,
      con        = con,
      hostname   = host,
      allow.conflicting.dates = TRUE
    )$input.id
    
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
    
    # Insert into DB
    inputinfo$id <- dbfile.input.insert(
      in.path    = file_path,
      in.prefix  = file_name,
      siteid     = runinfo$site$id,
      startdate  = startdate,
      enddate    = enddate,
      mimetype   = mimetype,
      formatname = "pft.info",
      parentid   = NA,
      con        = con,
      hostname   = host,
      allow.conflicting.dates = TRUE
    )$input.id
    
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