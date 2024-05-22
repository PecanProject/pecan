library(dplyr)
library("PEcAn.all")
library("RCurl")

#' Post a settings file for running a Meta-Analysis
#' @param req Send pecan.xml in bodyas xml filetype
#' @return A list of post.distns.MA.R
#' @author Nihar Sanda
#* @post /run
submitWorkflow <- function(req, res){
  if(req$HTTP_CONTENT_TYPE == "application/xml") {
    # read req$bosy as xml
    settingsXml <- XML::xmlParseString(stringr::str_replace(req$body, "<?.*?>\n", ""))
    
    ## convert the xml to a list
    settings <- XML::xmlToList(settingsXml)
    settings <- as.Settings(settings)
    settings <- expandMultiSettings(settings)
    
    # Update/fix/check settings.
    # Will only run the first time it's called, unless force=TRUE
    settings <-
      PEcAn.settings::prepare.settings(settings, force = FALSE)
    
    # Changing update to TRUE
    settings$meta.analysis$update <- TRUE
    
    # Write pecan.CHECKED.xml
    PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
    
    # Do conversions
    settings <- PEcAn.workflow::do_conversions(settings)
    settings <- PEcAn.workflow::runModule.get.trait.data(settings)
    
    # initiating variables needed for running meta analysis
    pfts <- settings$pfts
    iterations <- settings$meta.analysis$iter
    random <- settings$meta.analysis$random.effects$on
    use_ghs <- settings$meta.analysis$random.effects$use_ghs
    threshold <- settings$meta.analysis$threshold
    dbfiles <- settings$database$dbfiles
    database <- settings$database$bety
  
    # running meta analysis
    run.meta.analysis(pfts, iterations, random, threshold, 
                      dbfiles, database, use_ghs)
    
    #PEcAn.MA::runModule.run.meta.analysis(settings = ma_settings)
    
    if(dir.exists(settings$pfts$pft$outdir)){ 
     filepath <- paste0(settings$pfts$pft$outdir, "/post.distns.Rdata")
     e <- new.env(parent = emptyenv())
     load(filepath, envir = e)
     objs <- ls(envir = e, all.names = TRUE)
     for(obj in objs) {
       data <- get(obj, envir =e)
     }
     #csv_file <- paste0(settings$pfts$pft$outdir, '/post.distns.csv')
     #plumber::include_file(csv_file, res)
     return(list(status = "Meta Analysis ran successfully", data=data))
    }
  }
  else{
    res$status <- 415
    return(paste("Unsupported request content type:", req$HTTP_CONTENT_TYPE))
  }
}