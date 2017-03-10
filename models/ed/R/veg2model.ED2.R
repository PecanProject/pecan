#' Writes ED specific IC files
#'
#' @param outfolder where to write files
#' @param overwrite
#' @return results data frame
#' @export
#' @author Istem Fer
veg2model.ED2 <- function(outfolder, start_date, end_date, 
                          lat, lon, site_id, source,
                          veg_info, overwrite = FALSE, ...){
  

  #--------------------------------------------------------------------------------------------------#
  # Handle file names
  
  start_year  <- lubridate::year(start_date)
  end_year    <- lubridate::year(end_date)
  formatnames <- c("ED2.cohort", "ED2.patch", "ED2.site")
  dbfilenames <- c("css.file", "pss.file", "site.file")
  
  file.prefix <- paste("siteid", site_id, source, start_year, end_year,
                      get.ed.file.latlon.text(lat, lon, site.style = FALSE), sep = ".")
  #site.prefix  <- paste("siteid", site_id, source, start_year, end_year,
  #                       get.ed.file.latlon.text(lat, lon, site.style = TRUE), sep = ".")
  
  filenames   <- c(paste0(file.prefix, ".css"),
                   paste0(file.prefix, ".pss"),
                   paste0(file.prefix, ".site"))
  
  filenames_full  <- file.path(outfolder, filenames)

  
  #--------------------------------------------------------------------------------------------------#
  # Prepare pss
  
  # get data that was processed in the upstream 
  # this check should be unnecessary once we handle pss data in the upstream
  if(source == "FIA"){
    
    # this might change depending on how I process data in put.veg  
    pss <- veg_info[[1]]
    
  } else{
    
    # other cases for now,
    # create pss file from scratch by using values passed from using defaults 
    # if you don't want to pass these from settings make sure you pass them to convert.inputs in put.veg.module
    # wait until we solve metadata problem?
    
    ### assuming one patch for now, otherwise these lines might change ###
    time    <- start_year-1
    n.patch <- 1
    trk     <- 1
    age     <- 100
    
    pss <- data.frame(time = time, patch = n.patch, trk = trk, age = age)
    
    PEcAn.utils::logger.info(paste0("Values used in the patch file - time:", 
                                    pss$time, ", patch:", pss$patch, ", trk:", 
                                    pss$trk, ", age:", pss$age))
    
  }
  
  n.patch   <- nrow(pss)
  
  ## fill missing data w/ defaults
  pss$site  <- 1
  pss$area  <- 1 / n.patch
  pss$water <- 0
  
  # Reorder columns
  pss <- pss[, c("site", "time", "patch", "trk", "age", "area", "water")]
  
  # Add soil data
  soil            <- c(1, 5, 5, 0.01, 0, 1, 1)  #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)\t
  soil.dat        <- as.data.frame(matrix(soil, n.patch, 7, byrow = TRUE))
  names(soil.dat) <- c("fsc", "stsc", "stsl", "ssc", "psc", "msn", "fsn")
  pss             <- cbind(pss, soil.dat)

  #--------------------------------------------------------------------------------------------------#
  # Prepare css
  
  # this might change depending on how I process data in put.veg  
  css <- veg_info[[2]]
    

  if(is.null(css$patch)){
    css$patch  <- 1
  }

  # Remove rows that don't map to any patch
  css <- css[which(css$patch %in% pss$patch), ]
  if (nrow(css) == 0) {
    logger.severe("No trees map to previously selected patches.")
  } else {
    logger.debug(paste0(nrow(css), " trees that map to previously selected patches."))
  }

    
  if(is.null(css$n)){ 
    # will get back to giving sensical values
    css$n <- 0.001
  }
  
  if(is.null(css$cohort)){ 
    # will get back to giving sensical values
    css$cohort <- 1:nrow(css)
  }
  
  inv.years <- as.numeric(unique(css$year))
  # suitable years
  av.years <- inv.years[inv.years < start_year]
  if(length(av.years) == 0){
    logger.severe("No available years found in the data.")
  }
  css$time <- max(av.years)
  # filter out other years
  css <- css[css$year == css$time, ]
  
  if("DBH" %in% colnames(css)){
    colnames(css)[colnames(css) == "DBH"] <- "dbh"
  }
  
  
  # Convert PFT names to ED2 Numbers
  data(pftmapping, package = "PEcAn.ED2")
  css$pft.number <- NA
  for (p in seq_along(css$pft)) {
    css$pft.number[p] <- pftmapping$ED[pftmapping$PEcAn == as.character(css$pft[p])]
    if (is.null(css$pft.number[p])) {
      logger.severe(paste0("Couldn't find an ED2 PFT number for ", as.character(css$pft[p])))
    }
  }
  
    
  # --- Continue work formatting css 
  n.cohort                      <- nrow(css)
  css$time[is.na(css$time)]     <- 1
  css$cohort[is.na(css$cohort)] <- 1:sum(is.na(css$cohort))
  css$dbh[is.na(css$dbh)]       <- 1  # assign nominal small dbh to missing
  density.median                <- median(css$n[which(css$n > 0)])
  css$n[is.na(css$n) | css$n == 0]    <- density.median
  css$hite <- css$bdead <- css$balive <- css$lai <- 0
    
  # pft.number col needs to be pft 
  css <- css[ , colnames(css) != "pft"]
  colnames(css)[colnames(css) == "pft.number"] <- "pft"
    
  css <- css[, c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "lai")]

  #--------------------------------------------------------------------------------------------------#
  # Write files
  
  # css
  write.table(css, filenames_full[1], quote = FALSE, row.names = FALSE)
  
  # pss
  write.table(pss, filenames_full[2], quote = FALSE, row.names = FALSE)
  
  # site
  # hardcoded per fia2ED implemention
  site <- c(
    "nsite 1 file_format 1", 
    "sitenum area TCI elev slope aspect soil",
    "1 1.0 -7 100.0 0.0 0.0 3"
  )
  
  site.file.con <- file(filenames_full[3])
  writeLines(site, filenames_full[3])
  close(site.file.con)
  
  # Build results dataframe for convert.input
  results <- data.frame(file = filenames_full, 
                        host = c(fqdn()), 
                        mimetype = "text/plain", 
                        formatname = formatnames, 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = filenames, 
                        stringsAsFactors = FALSE)
  
  ### return for convert.inputs
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
