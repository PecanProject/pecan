##' @export
##' @name do.conversions
##' @title do.conversions
##' @description Input conversion workflow
##' @author Ryan Kelly, Rob Kooper, Betsy Cowdery
do.conversions <- function(settings) {
  if(is.MultiSettings(settings)) {
    return(papply(settings, do.conversions))
  }
  
  needsave <- FALSE
  if(is.character(settings$run$inputs)) settings$run$inputs <- NULL  ## check for empty set
  for(i in seq_along(settings$run$inputs)) {
    input <- settings$run$inputs[[i]]
    if (is.null(input)) next
    
    input.tag <- names(settings$run$input)[i]
    
    # fia database
    if ((input['input'] == 'fia') && (status.check("FIA2ED") == 0)) {
      status.start("FIA2ED")
      fia.to.psscss(settings)
      status.end()
      needsave <- TRUE
    }
    
    # met conversion
    if(input.tag == 'met') {
      name <- ifelse(is.null(settings$browndog), "MET Process", "BrownDog")
      if (is.null(input$path) && (status.check(name) == 0)) {
        status.start(name)
        result <- PEcAn.data.atmosphere::met.process(
          site       = settings$run$site, 
          input_met  = settings$run$inputs$met,
          start_date = settings$run$start.date,
          end_date   = settings$run$end.date,
          model      = settings$model$type,
          host       = settings$host,
          dbparms    = settings$database$bety, 
          dir        = settings$database$dbfiles,
          browndog   = settings$browndog)
        settings$run$inputs[[i]][['path']] <- result
        status.end()
        needsave <- TRUE
      }
    }
  }
  if (needsave) {
    saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.METProcess.xml'))  
  } else if (file.exists(file.path(settings$outdir, 'pecan.METProcess.xml'))) {
    settings <- read.settings(file.path(settings$outdir, 'pecan.METProcess.xml'))
  }
  return(settings)
}