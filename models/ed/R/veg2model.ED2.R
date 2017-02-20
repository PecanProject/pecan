#' Writes ED specific IC files
#'
#' @param in.path
#' @param outfolder
#' @param start_date
#' @param end_date
#' @return results dataframe
#'  We need the in.path, the out folder, the start and endates and the lat/long. All of these are in setting$run. 
veg2model.ED2 <- function(inputinfo, runinfo, outfolder, overwrite = FALSE){
  
  # Build results dataframe for convert.input
  results <- data.frame(file = c(ic_files), 
                        host = c(fqdn()), 
                        mimetype = c("text/plain"), 
                        formatname = c("ED2.cohort", "ED2.patch", "ED2.site"), 
                        startdate = as_date(paste0(year, "-01-01")), 
                        enddate = as_date(paste0(year, "-12-31")), 
                        dbfile.name = c(“pss.file”, “css.file”, “site.file”) 
                        stringsAsFactors = FALSE)
  
  # Read templates of IC files or placeholders
  
  
  # Convert PFT names to ED2 Numbers
  
  data(pftmapping)
  for (pft.i in settings$pfts) {
    pft.number <- NULL
    pft.number <- pft.i$constants$num
    if (is.null(pft.number)) {
      pft.number <- pftmapping$ED[which(pftmapping == pft.i$name)]
    }
    if (is.null(pft.number)) {
      logger.severe(paste0("Couldn't find an ED2 PFT number for ", pft.i$name))
    }
    pfts$pft[pfts$pft == pft.i$name] <- pft.number
  }
  
  # Loop over years
  # Format IC files
  
  
  
  return(invisible(results))  
}
