##' @name ic_process
##' @title ic_process
##' @export
##'
##' @param settings pecan settings list
##' @param input Taken from settings$run$inputs. This should include id, path, and source
##' @param dir settings$database$dbfiles
##' @param overwrite Default = FALSE. whether to force ic_process to proceed
##'
##' @author Istem Fer, Hamze Dokoohaki
ic_process <- function(settings, input, dir, overwrite = FALSE){


  #--------------------------------------------------------------------------------------------------#
  # Extract info from settings and setup
  site       <- settings$run$site
  model <- list()
    model$type <- settings$model$type
    model$id <- settings$model$id
  host       <- settings$host
  dbparms    <- settings$database

  # Handle IC Workflow locally
  if(host$name != "localhost"){
    host$name <- "localhost"
    dir       <- settings$database$dbfiles
  }

  # If overwrite is a plain boolean, fill in defaults for each module
  if (!is.list(overwrite)) {
    if (overwrite) {
      overwrite <- list(getveg = TRUE,  ensveg = TRUE,  putveg = TRUE)
    } else {
      overwrite <- list(getveg = FALSE, ensveg = FALSE, putveg = FALSE)
    }
  } else {
    if (is.null(overwrite$getveg)) {
      overwrite$getveg <- FALSE
    }
    if (is.null(overwrite$ensveg)) {
      overwrite$ensveg <- FALSE
    }
    if (is.null(overwrite$putveg)) {
      overwrite$putveg <- FALSE
    }
  }

  # set up bety connection
  con <- PEcAn.DB::db.open(dbparms$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  
  #grab site lat and lon info
  latlon <- PEcAn.DB::query.site(site$id, con = con)[c("lat", "lon")] 
  # setup site database number, lat, lon and name and copy for format.vars if new input
  new.site <- data.frame(id = as.numeric(site$id),
                         lat = latlon$lat,
                         lon = latlon$lon)

  new.site$name <- settings$run$site$name


  str_ns <- paste0(new.site$id %/% 1e+09, "-", new.site$id %% 1e+09)


  outfolder <- file.path(dir, paste0(input$source, "_site_", str_ns))

  ## We need two types of start/end dates now
  ## i)  start/end of the run when we have no veg file to begin with (i.e. we'll be querying a DB)
  ## ii) start and end of the veg file if we'll load veg data from a source file, this could be different than model start/end
  # query (ii) from source id [input id in BETY]

  # this check might change depending on what other sources that requires querying its own DB we will have
  # probably something passed from settings
  if(input$source == "FIA"){

    start_date <- settings$run$start.date
    end_date   <- settings$run$end.date
  }else if (input$source=="BADM"){

    outfolder <- file.path(dir, paste0(input$source, "_site_", str_ns))
    if(!dir.exists(outfolder)) dir.create(outfolder)

    #see if there is already files generated there
    newfile <-list.files(outfolder, "*.nc$", full.names = TRUE) %>%
      as.list()
    names(newfile) <- rep("path", length(newfile))

    if (length(newfile)==0){
      newfile <- PEcAn.data.land::BADM_IC_process(settings, dir=outfolder, overwrite=FALSE)
    }

    settings$run$inputs[['poolinitcond']]$path <- newfile

    return(settings)
  }else if (input$source == "NEON_veg"){
    #For debugging purposes I am hard coding in the start and end dates, will revisit and adjust once extract_NEON_veg is working within ic_process
    start_date = as.Date(input$startdate)
    end_date = as.Date(input$enddate)
    # start_date = as.Date("2020-01-01")
    # end_date = as.Date("2021-09-01")
    #Note the start and end dates for ICs are not the same as those for the forecast runs
    #please check out NEON products DP1.10098.001 for your desired site to check data availability before setting start and end dates
  }else{

   query      <- paste0("SELECT * FROM inputs where id = ", input$id)
   input_file <- PEcAn.DB::db.query(query, con = con) 
   start_date <- input_file$start_date
   end_date   <- input_file$end_date

  }
  # set up host information
  machine.host <- ifelse(host == "localhost" || host$name == "localhost", PEcAn.remote::fqdn(), host$name)
  machine <- PEcAn.DB::db.query(paste0("SELECT * from machines where hostname = '", machine.host, "'"), con)
  
  # retrieve model type info
  if(is.null(model)){
    modeltype_id <- PEcAn.DB::db.query(paste0("SELECT modeltype_id FROM models where id = '", settings$model$id, "'"), con)[[1]]
    model <- PEcAn.DB::db.query(paste0("SELECT name FROM modeltypes where id = '", modeltype_id, "'"), con)[[1]]
  }



  getveg.id <- putveg.id <- NULL

  # this also needs to be in for-loop, n = 1 should be a special case
  # but we might still want an ensemble from a single source, so need a check accordinly
  # best pass a flag (<ensemble.source>TRUE<\ensemble.source>) if that's the case, omit the flag otherwise
  # currently downloading/reading in different ensemble members is not implemented,
  # then we'll need to pass pattern, ensemble etc to convert_input

  nsource <- ifelse(!is.null(input$ensemble), as.numeric(input$ensemble), 1)
  
#--------------------------------------------------------------------------------------------------#
  # Load/extract + match species module

  if (is.null(getveg.id) & is.null(putveg.id)) {

    getveg.id <- list()

    for(i in seq_len(nsource)){
      getveg.id[[i]] <- get_veg_module(input_veg    = input,
                                       outfolder    = outfolder,
                                       start_date   = start_date,
                                       end_date     = end_date,
                                       dbparms      = dbparms,
                                       new_site     = new.site,
                                       host         = host,
                                       machine_host = machine.host,
                                       overwrite    = overwrite$getveg)
    }

  }

#--------------------------------------------------------------------------------------------------#
  # Sampling/ensemble module
  if (!is.null(getveg.id) & !is.null(input$ensemble) & is.null(putveg.id)) {

    ctr <- 1

    ensveg.id <- list()
    for(i in seq_len(as.numeric(input$ensemble))){

      ctr <- ifelse(nsource == 1, 1, i)
      ensveg.id[[i]] <- ens_veg_module(getveg.id    = getveg.id[[ctr]],
                                       dbparms      = dbparms,
                                       input_veg    = input,
                                       outfolder    = outfolder,
                                       machine      = machine,
                                       start_date   = start_date,
                                       end_date     = end_date,
                                       n.ensemble   = i,
                                       new_site     = new.site,
                                       host         = host)
    }
    getveg.id <- ensveg.id
  }

#--------------------------------------------------------------------------------------------------#
  # Match species to PFTs + veg2model module

  if (!is.null(getveg.id) & is.null(putveg.id)) { # probably need a more sophisticated check here

    putveg.id <- list()
    for(i in seq_along(getveg.id)){
      putveg.id[[i]] <- put_veg_module(getveg.id  = getveg.id[[i]],
                                       dbparms    = dbparms,
                                       input_veg  = input,
                                       pfts       = settings$pfts,
                                       outfolder  = outfolder,
                                       n.ensemble = i,
                                       dir        = dir,
                                       machine    = machine,
                                       model      = model,
                                       start_date = start_date,
                                       end_date   = end_date,
                                       new_site   = new.site,
                                       host       = host,
                                       overwrite  = overwrite$putveg)
    }


  }

  #--------------------------------------------------------------------------------------------------#
  # Fill settings
  if (!is.null(putveg.id)) {

    # extend the inputs list for ensemble members
    #settings_inputs <- lapply(seq_along(settings$run$inputs), function(x) rep(settings$run$inputs[[x]], each = length((putveg.id))))
    settings_inputs <- settings$run$inputs

    # make sure all sublists are grouped and renamed to have unique tags, e.g.:
    # <id>
    #   <id1>...</id1>
    #        ...
    #   <idN>...</idN>
    # </id>
    # <path>
    #   <path1>...</path1>
    #          ...
    #   <pathN>...</pathN>
    # </path>
    # # settings_inputs <- lapply(seq_along(settings_inputs), function(x){
    # #   tmp.list <- settings_inputs[[x]]
    # #   sub_names <- names(settings$run$inputs[[x]])
    # #   names(settings_inputs[[x]]) <- paste0(names(settings_inputs[[x]]), rep(seq_along(putveg.id), length(settings$run$inputs$IC[[x]])))
    # #   tmp.list  <- lapply(seq_along(sub_names), function(v) return(settings_inputs[[x]][names(tmp.list) == sub_names[v]]))
    # #   names(tmp.list) <- sub_names
    # #   if(is.null(tmp.list$path)) tmp.list$path <- list()
    # #   return(tmp.list)
    # # })
    # 
    # # names(settings_inputs) <- names(settings$run$inputs)
    # 
    for(i in seq_along(putveg.id)){
      
      model_file <- PEcAn.DB::db.query(paste("SELECT * from dbfiles where container_id =", putveg.id[[i]], "and machine_id =", machine$id), con)
      
      # now that we don't have multipasses, convert_input only inserts 1st filename
      # do we want to change it in convert_inputs such that it loops over the dbfile.insert?
      path_to_settings <- file.path(model_file[["file_path"]], model_file[["file_name"]])
      settings_inputs[[input$output]][['path']][[paste0('path', i)]] <- path_to_settings

      # NOTE : THIS BIT IS SENSITIVE TO THE ORDER OF TAGS IN PECAN.XML
      # this took care of "css" only, others have the same prefix
      if(input$output == "css"){
        settings_inputs[["pss"]][['path']][[paste0('path', i)]]  <- gsub("css","pss", path_to_settings)
        settings_inputs[["site"]][['path']][[paste0('path', i)]] <- gsub("css","site", path_to_settings)

        # IF: For now IC workflow is only working for ED and it's the only case for copying to remote
        # but this copy to remote might need to go out of this if-block and change

        # Copy to remote, update DB and change paths if needed
        if (settings$host$name != "localhost") {

          folder_dir <- paste0(
            input$source, "_site_", str_ns, "/",
            input$source, "_ens", i, ".",
            lubridate::year(start_date))
          remote_dir <- file.path(settings$host$folder, folder_dir)

          # copies css
          css_file <- settings_inputs[["css"]][['path']][[paste0('path', i)]]
          remote.copy.update(
            input_id = putveg.id[[i]],
            remote_dir = remote_dir,
            local_file_path = css_file,
            host = settings$host,
            con = con)
          settings_inputs[["css"]][['path']][[paste0('path', i)]] <- file.path(
            remote_dir,
            basename(css_file))

          # pss 
          pss_file <- settings_inputs[["pss"]][['path']][[paste0('path', i)]]
          remote.copy.update(
            input_id = putveg.id[[i]],
            remote_dir = remote_dir,
            local_file_path = pss_file,
            host = settings$host,
            con = con)
          settings_inputs[["pss"]][['path']][[paste0('path', i)]] <- file.path(
            remote_dir,
            basename(pss_file))

          # site
          site_file <- settings_inputs[["site"]][['path']][[paste0('path', i)]]
          remote.copy.update(
            input_id = putveg.id[[i]],
            remote_dir = remote_dir,
            local_file_path = site_file,
            host = settings$host,
            con = con)
          settings_inputs[["site"]][['path']][[paste0('path', i)]] <- file.path(
            remote_dir,
            basename(site_file))

        }
      }
    }

    settings$run$inputs <- settings_inputs
  }

  PEcAn.logger::logger.info("Finished IC for vegetation.")

  return(settings)
} # ic_process
