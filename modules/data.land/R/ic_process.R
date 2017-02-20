##' @name ic_process
##' @title ic_process
##' @export
##'
##' @param pfts settings pft list
##' @param runinfo run info from settings file
##' @param inputinfo which data source to use (FIA, GapMacro, FFT etc.) and username, 
##' this is a subset of runinfo and can be extracted from runinfo for now as this is only functional for css/pss/site, 
##' but might need this info separately when handling different IC types of IC data in the future
##' @param model model_type name
##' @param host Host info from settings file
##' @param dbparms  database settings from settings file
##' @param dir  directory to write outputs to
##' @param overwrite Whether to force met.process to proceed.
##' @author Istem Fer
ic_process <- function(pfts, runinfo, inputinfo, model, host = "localhost", dbparms, dir, overwrite = FALSE){
  
  
  year <- lubridate::year(runinfo$start.date)
  
  # If overwrite is a plain boolean, fill in defaults for each stage
  # which stages are going to be in IC Workflow?
  # download, at least for FIA data
  # sppmatch, spp code - species id match
  # pftmatch, species id - model pft match
  # veg2model, final IC product
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
      stage$download <- TRUE
    }
  } else {
    
    ######
    
  }
  
  

  
  
  #--------------------------------------------------------------------------------------------------#
  # Download FIA
  if (stage$download) {
    
    ########
    
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
    if(inputinfo$source %in% c("NASA_TE_FIA")){
      format.name = 'usda'
    }else if(!is.null(inputinfo$match.format)){
      format.name = inputinfo$match.format
    }else{
      logger.error("Can't match code to species. No valid format found.")
    }
    
    # decide which column has the codes
    if(format.name == 'usda'){
      code.col = "species_USDA_symbol"
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
    colnames(spp.info)[colnames(spp.info) == "input_code"] <- code.col
    sppmatched <- merge(obs, spp.info, by.x = code.col)
    
    ### write
    
    stage$sppmatch <- FALSE
  }
  
  #--------------------------------------------------------------------------------------------------#
  # Match species to PFTs
  if (stage$pftmatch) {
    
    pft.info <- match_pft(spp.info$bety_species_id, pfts, con)
    
    stage$pftmatch <- FALSE
    
    ### write
    
    file_name <- paste0("pftmatch_site_", runinfo$site$id, ".", inputinfo$source, ".", year, ".txt")
    
    # Insert into DB
    dbfile.input.insert(
        in.path    = dir,
        in.prefix  = file_name,
        siteid     = settings$run$site$id,
        startdate  = startdate,
        enddate    = enddate,
        mimetype   = mimetype,
        formatname = formatnames[i],
        parentid   = NA,
        con        = con,
        hostname   = settings$host$name,
        allow.conflicting.dates = TRUE
    )

    inputinfo$id <-  db.query(paste0("INSERT INTO posteriors (pft_id) VALUES (",
                                    pft.id, ") RETURNING id"), con)
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
    ic.id  <- fcn(inputinfo = inputinfo, runinfo = runinfo, outfolder = dir)
    
  }
  
  return(ic.id)
} # ic_process