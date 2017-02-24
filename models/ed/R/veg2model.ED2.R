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
  pss.file.local  <- file.path(outfolder, paste0(prefix.psscss, ".pss"))
  css.file.local  <- file.path(outfolder, paste0(prefix.psscss, ".css"))
  site.file.local <- file.path(outfolder, paste0(prefix.site, ".site"))
  
  filenames <- c(pss.file.local, css.file.local, site.file.local)

  # Build results dataframe for convert.input
  results <- data.frame(file = filenames, 
                        host = c(fqdn()), 
                        mimetype = c("text/plain"), 
                        formatname = formatnames, 
                        startdate = as_date(paste0(year, "-01-01")), 
                        enddate = as_date(paste0(year, "-12-31")), 
                        dbfile.name = dbfilenames, 
                        stringsAsFactors = FALSE)
  
  # get data that was processed in the upstream
  obs <- read.table(inputinfo$path, header = TRUE, sep = "\t")
  
  ##################
  ##              ##
  ##     SITE     ##
  ##              ##
  ##################
  # Obviously, this is just a placeholder for now...
  site <- c(
    "nsite 1 file_format 1", 
    "sitenum area TCI elev slope aspect soil",
    "1 1.0 -7 100.0 0.0 0.0 3"
  )
  
  # if source == "FIA" we still want some more checks
  #  # --- Consistency tests between PFTs and FIA
  
  # Read templates of IC files or placeholders
  
  # Loop over years
  # Format IC files
  
  
  # Locally write files
  write.table(pss, pss.file.local, quote = FALSE, row.names = FALSE)
  write.table(css, css.file.local, quote = FALSE, row.names = FALSE)
  
  site.file.con <- file(site.file.local)
  writeLines(site, site.file.con)
  close(site.file.con)
  
  
  
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
