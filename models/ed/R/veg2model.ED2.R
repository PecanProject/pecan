#' Writes ED specific IC files
#'
#' @param in.path
#' @param outfolder
#' @param start_date
#' @param end_date
#' @return results dataframe
#' @author Istem Fer
#'  We need the in.path, the out folder, the start and endates and the lat/long. All of these are in setting$run. 
veg2model.ED2 <- function(inputinfo, runinfo, outfolder, overwrite = FALSE){
  
  start_year <- lubridate::year(runinfo$start.date)
  end_year   <- lubridate::year(runinfo$end.date)
  startdate   <- lubridate::as_date(paste0(start_year, "-01-01"))
  enddate <- lubridate::as_date(paste0(end_year, "-12-31"))
  lat <- as.numeric(runinfo$site$lat)
  lon <- as.numeric(runinfo$site$lon)
  
  formatnames <- c("ED2.cohort", "ED2.patch", "ED2.site")
  dbfilenames <- c("pss.file", "css.file", "site.file")
  
  if(inputinfo$source == "FIA"){
    prefix.psscss <- inputinfo$prefix.psscss
    prefix.site   <- inputinfo$prefix.site
  }else{
    prefix.psscss <- paste("siteid", runinfo$site$id, inputinfo$source, start_year, end_year,
                           get.ed.file.latlon.text(lat, lon, site.style = FALSE), sep = ".")
                         
    prefix.site <- paste("siteid", runinfo$site$id, inputinfo$source, start_year, end_year,
                       get.ed.file.latlon.text(lat, lon, site.style = TRUE), sep = ".")

  }
  

  # all the file names

  pss.file.local <- file.path(out.dir.local, paste0(prefix.psscss, ".pss"))
  css.file.local <- file.path(out.dir.local, paste0(prefix.psscss, ".css"))
  site.file.local <- file.path(out.dir.local, paste0(prefix.site, ".site"))
  
  file_name <- paste0("pftmatch.", start_year, "_", end_year, ".txt")
  ic_files <- 
  # Build results dataframe for convert.input
  results <- data.frame(file = c(ic_files), 
                        host = c(fqdn()), 
                        mimetype = c("text/plain"), 
                        formatname = formatnames, 
                        startdate = as_date(paste0(year, "-01-01")), 
                        enddate = as_date(paste0(year, "-12-31")), 
                        dbfile.name = dbfilenames, 
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

get.ed.file.latlon.text <- function(lat, lon, site.style = FALSE, ed.res = 1) {
  if (site.style) {
    lat <- ifelse(lat >= 0, ed.res * floor(lat / ed.res) + 0.5 * ed.res, -ed.res * floor(-lat / ed.res) - 0.5 * ed.res)
    lon <- ifelse(lon >= 0, ed.res * floor(lon / ed.res) + 0.5 * ed.res, -ed.res * floor(-lon / ed.res) - 0.5 * ed.res)
    return(paste0("lat", round(lat, 1), "lon", round(lon, 1)))
  } else {
    return(paste0("lat", round(lat, 4), "lon", round(lon, 4)))
  }
} # get.ed.file.latlon.text
