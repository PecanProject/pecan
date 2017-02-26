#' Writes ED specific IC files
#'
#' @param obs data either passed from upstram or NULL
#' @param inputinfo
#' @param runinfo run from settings
#' @param outfolder where to write files
#' @param host 
#' @param con bety$con
#' @param overwrite
#' @return ic.id initial conditions file input id
#' @author Istem Fer
#'  We need the in.path, the out folder, the start and endates and the lat/long. All of these are in setting$run. 
veg2model.ED2 <- function(obs = NULL, inputinfo, runinfo, outfolder, host, con, overwrite = FALSE){
  
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
    
    n.patch    <- NULL
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

  # Build results dataframe for convert.input, not used currently
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

    
  }else if(inputinfo$output == "pss"){
    
    if(!is.null(inputinfo$id)){

      # if id is in inputinfo it probably should take precedence 
      # but this is not going to be used yet
      ########
      
      
    }else if(is.null(obs)){ # if obs is NOT NULL (e.g. FIA case), we can skip this and continue
      
      # other cases for now,
      # create pss file from scratch by using values passed from settings or using some defaults  
      
      # a series of checks to get pss info from settings if it exists
      ### assuming one patch for now, otherwise these lines might change ###
      time    <- ifelse(!is.null(inputinfo$time), inputinfo$time, start_year-1)
      n.patch   <- ifelse(!is.null(inputinfo$patch), inputinfo$patch, 1)
      trk     <- ifelse(!is.null(inputinfo$trk), inputinfo$trk, 1)
      age     <- ifelse(!is.null(inputinfo$age), inputinfo$age, 100)
      
      obs <- data.frame(time = time, patch = n.patch, trk = trk, age = age)
      
      PEcAn.utils::logger.info(paste0("Values used in the patch file - time:", 
                                      obs$time, ", patch:", obs$patch, ", trk:", 
                                      obs$trk, ", age:", obs$age))
     
    }
    
    # this check might be redundant, direct nrows(obs) assignment could work 
    # but leaving it for now
    n.patch   <- ifelse(is.null(n.patch), nrow(obs), n.patch)
      
    ## fill missing data w/ defaults
    obs$site  <- 1
    obs$area  <- 1 / n.patch
    obs$water <- 0
    
    # Reorder columns
    obs <- obs[, c("site", "time", "patch", "trk", "age", "area", "water")]
    
    # Add soil data
    soil            <- c(1, 5, 5, 0.01, 0, 1, 1)  #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)\t
    soil.dat        <- as.data.frame(matrix(soil, n.patch, 7, byrow = TRUE))
    names(soil.dat) <- c("fsc", "stsc", "stsl", "ssc", "psc", "msn", "fsn")
    obs             <- cbind(obs, soil.dat)
    
    # Locally write files
    write.table(obs, localfile, quote = FALSE, col.names = TRUE, row.names = FALSE)

    
  }else if(inputinfo$output == "css"){
    
    if(!is.null(inputinfo$path)){
      obs <- read.table(inputinfo$path, header = TRUE)
    }else if(is.null(obs)){
      logger.severe("No css data found.")
    }
    
    if(inputinfo$source == "FIA"){ # we still need one more check from pss
      
      pss <- read.table(runinfo$inputs$pss$path, header = TRUE)
      
      # Remove rows that don't map to any patch
      obs <- obs[which(obs$patch %in% pss$patch), ]
      if (nrow(obs) == 0) {
        logger.severe("No trees map to previously selected patches.")
      } else {
        logger.debug(paste0(nrow(obs), " trees that map to previously selected patches."))
      }
    }
    
    if(is.null(obs$n)){
      # is this how n is calculated?
      obs$n <- ifelse(!is.null(inputinfo$area), nrow(obs)/inputinfo$area, 0.001)
    }
    
    if(inputinfo$source == "GapMacro"){
      inv.years <- as.numeric(unique(obs$year))
      # suitable years
      av.years <- inv.years[inv.years < start_year]
      if(length(av.years) == 0){
        logger.severe("No available years found in the data.")
      }
      obs$time <- max(av.years)
      # filter out other years
      obs <- obs[obs$year == obs$time, ]
      colnames(obs)[colnames(obs) == "DBH"] <- "dbh"
      
      obs$patch  <- 1
      obs$cohort <- 1:nrow(obs)
      obs$hite   <- 0
    
    }

    # Convert PFT names to ED2 Numbers
    data(pftmapping)
    obs$pft.number <- NA
    for (p in seq_along(obs$pft)) {
      obs$pft.number[p] <- pftmapping$ED[pftmapping$PEcAn == as.character(obs$pft[p])]
      if (is.null(obs$pft.number[p])) {
        logger.severe(paste0("Couldn't find an ED2 PFT number for ", as.character(obs$pft[p])))
      }
    }
  
    
    # --- Continue work formatting css 
    n.cohort                      <- nrow(obs)
    obs$time[is.na(obs$time)]     <- 1
    obs$cohort[is.na(obs$cohort)] <- 1:sum(is.na(obs$cohort))
    obs$dbh[is.na(obs$dbh)]       <- 1  # assign nominal small dbh to missing
    density.median                <- median(obs$n[which(obs$n > 0)])
    obs$n[is.na(obs$n) | obs$n == 0]    <- density.median
    obs$hite <- obs$bdead <- obs$balive <- obs$lai <- 0
    
    # pft.number col needs to be pft 
    obs <- obs[ , colnames(obs) != "pft"]
    colnames(obs)[colnames(obs) == "pft.number"] <- "pft"
    
    obs <- obs[, c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "lai")]
    
    # Locally write files
    write.table(obs, localfile, quote = FALSE, col.names = TRUE, row.names = FALSE)
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
    hostname   = host,
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
