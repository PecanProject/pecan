##' @export
##' @name do.conversions
##' @title do.conversions
##' @description Input conversion workflow
##' @author Ryan Kelly, Rob Kooper, Betsy Cowdery, Istem Fer
##' @export
do.conversions <- function(settings, overwrite.met = FALSE, overwrite.fia = FALSE, overwrite.ic = FALSE) {
  if (PEcAn.settings::is.MultiSettings(settings)) {
    return(PEcAn.settings::papply(settings, do.conversions))
  }
  
  needsave <- FALSE
  if (is.character(settings$run$inputs)) {
    settings$run$inputs <- NULL  ## check for empty set
  }
  
  dbfiles <- ifelse(!is.localhost(settings$host) & !is.null(settings$host$folder), settings$host$folder, settings$database$dbfiles)
  
  for (i in seq_along(settings$run$inputs)) {
    input <- settings$run$inputs[[i]]
    if (is.null(input)) {
      next
    }
    
    input.tag <- names(settings$run$input)[i]
    
    if ((input.tag %in% c("css", "pss", "site")) && 
        is.null(input$path) && !is.null(input$source)) {
      if(!is.null(input$useic)){ # set <useic>TRUE</useic> if IC Workflow, leave empty if not
        ic.flag  <- input$useic
        fia.flag <- FALSE
      }else if(input$source == "FIA"){
        ic.flag  <- FALSE
        fia.flag <- TRUE
        # possibly a warning for deprecation in the future
      }
    }else{
      ic.flag <- fia.flag <- FALSE
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
    
    # met conversion
    if (input.tag == "met") {
      name <- ifelse(is.null(settings$browndog), "MET Process", "BrownDog")
      if (is.null(input$path) && (status.check(name) == 0)) {
        settings$run$inputs[[i]][['path']] <- 
          PEcAn.data.atmosphere::met.process(
            site       = settings$run$site, 
            input_met  = settings$run$inputs$met,
            start_date = settings$run$start.date,
            end_date   = settings$run$end.date,
            model      = settings$model$type,
            host       = settings$host,
            dbparms    = settings$database$bety, 
            dir        = dbfiles,
            browndog   = settings$browndog,
            overwrite  = overwrite.met)
        
        needsave <- TRUE
      }
    }
  }
  if (needsave) {
    saveXML(listToXml(settings, "pecan"), file = file.path(settings$outdir, "pecan.METProcess.xml"))
  } else if (file.exists(file.path(settings$outdir, "pecan.METProcess.xml"))) {
    settings <- PEcAn.settings::read.settings(file.path(settings$outdir, "pecan.METProcess.xml"))
  }
  return(settings)
}