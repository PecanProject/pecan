##' @export
##' @aliases do.conversions
##' @name do_conversions
##' @title do_conversions
##' @description Input conversion workflow
##' @param settings PEcAn settings list
##' @param overwrite.met,overwrite.fia,overwrite.ic logical
##'
##' @author Ryan Kelly, Rob Kooper, Betsy Cowdery, Istem Fer

do_conversions <- function(settings, overwrite.met = FALSE, overwrite.fia = FALSE, overwrite.ic = FALSE) {
  if (PEcAn.settings::is.MultiSettings(settings)) {
    return(PEcAn.settings::papply(settings, do_conversions))
  }
  
  needsave <- FALSE
  if (is.character(settings$run$inputs)) {
    settings$run$inputs <- NULL  ## check for empty set
  }
  
  dbfiles.local <- settings$database$dbfiles
  dbfiles <- ifelse(!PEcAn.remote::is.localhost(settings$host) & !is.null(settings$host$folder), settings$host$folder, dbfiles.local)
  PEcAn.logger::logger.debug("do.conversion outdir",dbfiles)
  # For each input
  for (i in seq_along(settings$run$inputs)) {
    input <- settings$run$inputs[[i]]
    if (is.null(input)) {
      next
    }
    
    input.tag <- names(settings$run$input)[i]
    PEcAn.logger::logger.info("PROCESSING: ",input.tag)
    
    
    ic.flag <- fia.flag <- FALSE
    
    if ((input.tag %in% c("css", "pss", "site")) && 
        is.null(input$path) && !is.null(input$source)) {
      if(!is.null(input$useic)){ # set <useic>TRUE</useic> if IC Workflow, leave empty if not
        ic.flag  <- TRUE
      }else if(input$source == "FIA"){
        fia.flag <- TRUE
        # possibly a warning for deprecation in the future
      }
    }
    
    # BADM IC
    if(input.tag == "poolinitcond" && is.null(input$path)){
      ic.flag  <- TRUE
    }
    
    # IC conversion : for now for ED only, hence the css/pss/site check
    # <useic>TRUE</useic>
    if (ic.flag) {
      settings <- PEcAn.data.land::ic_process(settings, input, dir = dbfiles, overwrite  = overwrite.ic)
      needsave <- TRUE
    }
    
    # keep fia.to.psscss
    if (fia.flag) {
      settings <- PEcAn.data.land::fia.to.psscss(settings, overwrite = overwrite.fia)
      needsave <- TRUE
    }
    

    
    # soil extraction
    if(input.tag == "soil" && is.null(input$path)){
      settings$run$inputs[[i]]$path <- PEcAn.data.land::soil_process(settings, input, dbfiles.local, overwrite=FALSE)
      needsave <- TRUE
      ## NOTES: at the moment only processing soil locally. Need to think about how to generalize this
      ## because many models will read PEcAn standard in write.configs and write out into settings
      ## which is done locally in rundir and then rsync'ed to remote
      ## rather than having a model-format soils file that is processed remotely
    }

    # Phenology data extraction
    if(input.tag == "leaf_phenology" && is.null(input$path)){
      #settings$run$inputs[[i]]$path <- PEcAn.data.remote::extract_phenology_MODIS(site_info,start_date,end_date,outdir,run_parallel = TRUE,ncores = NULL)
      needsave <- TRUE
    }

    # met conversion
    
    if (input.tag == "met") {
      name <- "MET Process"
      if ( (PEcAn.utils::status.check(name) == 0)) { ## previously is.null(input$path) && 
        PEcAn.logger::logger.info("calling met.process: ",settings$run$inputs[[i]][['path']])
        settings$run$inputs[[i]] <- 
          PEcAn.data.atmosphere::met.process(
            site       = settings$run$site, 
            input_met  = settings$run$inputs$met,
            start_date = settings$run$start.date,
            end_date   = settings$run$end.date,
            model      = settings$model$type,
            host       = settings$host,
            dbparms    = settings$database$bety, 
            dir        = dbfiles,
            spin       = settings$spin,
            overwrite  = overwrite.met)
        PEcAn.logger::logger.debug("updated met path: ",settings$run$inputs[[i]][['path']])
        needsave <- TRUE
      }
    }
  }
  if (needsave) {
    XML::saveXML(PEcAn.settings::listToXml(settings, "pecan"), file = file.path(settings$outdir, "pecan.METProcess.xml"))
  } else if (file.exists(file.path(settings$outdir, "pecan.METProcess.xml"))) {
    settings <- PEcAn.settings::read.settings(file.path(settings$outdir, "pecan.METProcess.xml"))
  }
  return(settings)
}
