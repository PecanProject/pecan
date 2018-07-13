#' Writes ED specific IC files
#'
#' @param outfolder where to write files
#' @return filenames
#' @export
#' @author Istem Fer
veg2model.ED2 <- function(outfolder, veg_info, start_date, new_site, source){
  

  lat       <- as.numeric(as.character(new_site$lat))
  lon       <- as.numeric(as.character(new_site$lon))

  #--------------------------------------------------------------------------------------------------#
  # Handle file names
  
  start_year  <- lubridate::year(start_date)
  formatnames <- c("ED2.cohort", "ED2.patch", "ED2.site")
  dbfilenames <- c("css.file", "pss.file", "site.file")
  
  file.prefix <- paste(source, start_year,
                      get.ed.file.latlon.text(lat, lon, site.style = FALSE), sep = ".")

  
  filenames   <- c(paste0(file.prefix, ".css"),
                   paste0(file.prefix, ".pss"),
                   paste0(file.prefix, ".site"))
  
  filenames_full  <- file.path(outfolder, filenames)

  
  #--------------------------------------------------------------------------------------------------#
  # Prepare pss
  
  # veg_info[[1]] either has 
  # i)   full pss info (FIA case)
  # ii)  info passed from settings
  # iii) no info
  pss <- as.data.frame(veg_info[[1]], stringsAsFactors = FALSE)
  
  # for FIA these steps are unnecessary, it already has the pss info
  if(source != "FIA"){
    if(!is.null(pss$time)){
      time <- pss$time
    }else{
      time <- start_year
    }
    if(!is.null(pss$n.patch)){
      n.patch <- pss$n.patch
    }else{
      n.patch <- 1
    }
    if(!is.null(pss$trk)){
      trk <- pss$trk
    }else{
      trk <- 1
    }
    if(!is.null(pss$age)){
      age <- pss$age
    }else{
      age <- 100
    }
    
    pss <- data.frame(time = time, patch = n.patch, trk = trk, age = age)
    
    PEcAn.logger::logger.info(paste0("Values used in the patch file - time:", 
                                    pss$time, ", patch:", pss$patch, ", trk:", 
                                    pss$trk, ", age:", pss$age))
    
    # TODO : soils can also be here, passed from settings
  }
  
  n.patch   <- nrow(pss)
  
  ## fill missing data w/ defaults
  pss$site  <- 1
  pss$area  <- 1 / n.patch
  pss$water <- 0
  
  # Reorder columns
  pss <- pss[, c("site", "time", "patch", "trk", "age", "area", "water")]
  
  # Add soil data
  soil            <- c(1, 5, 5, 0.01, 0, 1, 1)  #soil C & N pools (biogeochem) defaults (fsc,stsc,stsl,ssc,psc,msn,fsn)
  soil.dat        <- as.data.frame(matrix(soil, n.patch, 7, byrow = TRUE))
  names(soil.dat) <- c("fsc", "stsc", "stsl", "ssc", "psc", "msn", "fsn")
  pss             <- cbind(pss, soil.dat)

  #--------------------------------------------------------------------------------------------------#
  # Prepare css
  
  obs <- veg_info[[2]]
  
  # remove NA rows for unmatched PFTs, this should mean dead trees only
  css <- obs[!is.na(obs$pft), ]
  
  # might further need removing dead trees by mortality status
  # css <- remove_dead_trees()

  if(is.null(css$patch)){
    css$patch  <- 1
  }

  # Remove rows that don't map to any patch
  css <- css[which(css$patch %in% pss$patch), ]
  if (nrow(css) == 0) {
    PEcAn.logger::logger.severe("No trees map to previously selected patches.")
  } else {
    PEcAn.logger::logger.debug(paste0(nrow(css), " trees that map to selected patches."))
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
  av.years <- inv.years[inv.years <= start_year]
  if(length(av.years) == 0){
    PEcAn.logger::logger.severe("No available years found in the data.")
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
      PEcAn.logger::logger.severe(paste0("Couldn't find an ED2 PFT number for ", as.character(css$pft[p])))
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
  
  # convert.input inserts only 1 file anyway
  return(list(filepath = filenames_full[1], filename = filenames[1], 
              mimetype = "text/plain", formatname = "ED2.cohort"))

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
