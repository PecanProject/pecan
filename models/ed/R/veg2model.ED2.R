#' Writes ED specific IC files
#'
#' @param in.path
#' @param outfolder
#' @param start_date
#' @param end_date
#' @return results dataframe
#' @author Istem Fer
#'  We need the in.path, the out folder, the start and endates and the lat/long. All of these are in setting$run. 
veg2model.ED2 <- function(inputinfo, runinfo, outfolder, con, overwrite = FALSE){
  
  start_year <- lubridate::year(runinfo$start.date)
  end_year   <- lubridate::year(runinfo$end.date)
  startdate   <- lubridate::as_date(paste0(start_year, "-01-01"))
  enddate     <- lubridate::as_date(paste0(end_year, "-12-31"))
  lat <- as.numeric(runinfo$site$lat)
  lon <- as.numeric(runinfo$site$lon)
  
  # handle file details
  if(inputinfo$output == "site"){
    
    formatname <- "ED2.site"
    dbfilename <- "site.file"
    fileprefix <- paste("siteid", runinfo$site$id, inputinfo$source, start_year, end_year,
                        get.ed.file.latlon.text(lat, lon, site.style = TRUE), sep = ".")
    filename   <- paste0(fileprefix, ".site")
    
  }else if(inputinfo$output == "pss"){
    
    formatname <- "ED2.patch"
    dbfilename <- "pss.file"
    # IF : previously there was also "gridres" in the file name after FIA download
    # but that was a hardcoded value. If we softcode and want it in the filenames we should pass it here
    fileprefix <- paste("siteid", runinfo$site$id, inputinfo$source, start_year, end_year,
                        get.ed.file.latlon.text(lat, lon, site.style = FALSE), sep = ".")
    filename   <- paste0(fileprefix, ".pss")

  }else if(inputinfo$output == "css"){
    
    formatname <- "ED2.cohort"
    dbfilename <- "css.file"
    fileprefix <- paste("siteid", runinfo$site$id, inputinfo$source, start_year, end_year,
                        get.ed.file.latlon.text(lat, lon, site.style = FALSE), sep = ".")
    filename   <- paste0(fileprefix, ".css")
      
  }
  
  localfile  <- file.path(outfolder, filename)

  # Build results dataframe for convert.input, not used currently but maybe later
  results <- data.frame(file = filename, 
                        host = c(fqdn()), 
                        mimetype = c("text/plain"), 
                        formatname = formatname, 
                        startdate, 
                        enddate, 
                        dbfile.name = dbfilename, 
                        stringsAsFactors = FALSE)
  
  # get data that was processed in the upstream unless it's site file
  if(inputinfo$output == "site"){
    
    # hardcoded per fia2ED implemention
    site <- c(
      "nsite 1 file_format 1", 
      "sitenum area TCI elev slope aspect soil",
      "1 1.0 -7 100.0 0.0 0.0 3"
    )
    
    site.file.con <- file(localfile)
    writeLines(site, localfile)
    close(site.file.con)

    
  }else{

    obs <- read.table(inputinfo$path, header = TRUE, sep = "\t")
    
    # if source == "FIA" we still want some more checks
    #  # --- Consistency tests between PFTs and FIA
    
    # Read templates of IC files or placeholders
    
    # Loop over years
    # Format IC files
    
    
    # Locally write files
    write.table(pss, pss.file.local, quote = FALSE, row.names = FALSE)
    write.table(css, css.file.local, quote = FALSE, row.names = FALSE)
    
    
    
  }


  # Insert into DB
  ic.id <- dbfile.input.insert(
    in.path    = outfolder,
    in.prefix  = filename,
    siteid     = runinfo$site$id,
    startdate  = startdate,
    enddate    = enddate,
    mimetype   = "text/plain",
    formatname = formatname,
    parentid   = NA,
    con        = con,
    hostname   = fqdn(),
    allow.conflicting.dates = TRUE
  )$input.id
  
  ### if things were modularized and convert input was used:
  # return(invisible(results))  
  return(ic.id)
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
