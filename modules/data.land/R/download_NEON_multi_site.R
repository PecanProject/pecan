#' Title generalized for download NEON files of multiSites. and automatically detect the most recent available NEON data and if data is problematic, try next year.
#'
#' @param settings a multisite settings object, should work with a single site setting. 
#' @param main_store_dir the preferred path to store the downloaded NEON files. the log file and VEG_INFO should also be stored in that path
#'
#' @return VEG_INFO: a list object containing veg_info, site IDs of each site.
#' @export
#'
#' @examples main_store_dir <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/IC/NEON_downloads"
#'           settings <- read.settings("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/pecan.xml")
#'           vege_info <- Download_multiSites_NEON(settings, main_store_dir)

Download_multiSites_NEON <- function(settings, main_store_dir){
  #get NEON sites table
  neonsites <- neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
  
  #output
  VEG_INFO <- list()
  
  #log file
  log_txt <- c()
  
  #starting FOR LOOP
  for (i in 1:length(settings)) {
    #match lon and lat
    tempLat <- as.numeric(settings[[i]]$run$site$lat)
    tempLon <- as.numeric(settings[[i]]$run$site$lon)
    
    betyneondist <- swfscMisc::distance(lat1 = tempLat, lon1 = tempLon, lat2 = neonsites$siteLatitude, lon2 = neonsites$siteLongitude)
    index <- which(betyneondist == min(betyneondist))
    tempNEON <- neonsites[index,]
    
    #grab data info from NEON
    dataProducts <- tempNEON$dataProducts[[1]]
    
    #create subfolder
    store_dir <- paste0(main_store_dir,"/",settings[[i]]$run$site$id)
    dir.create(store_dir)
    
    #filter on date
    if(length(which(dataProducts$dataProductCode=="DP1.10098.001")) == 0){
      print(paste0("no data available for site: ", tempNEON$siteCode, ". jump to the next site!"))
      log_txt <- c(log_txt, paste0("no data available for site: ", tempNEON$siteCode, ". jump to the next site!"))
      next
    }
    NEON_dates <- lubridate::date(paste0(dataProducts$availableMonths[[which(dataProducts$dataProductCode=="DP1.10098.001")]], "-01"))
    closeYear <- unique(lubridate::year(NEON_dates)[which(abs(lubridate::year(NEON_dates)-lubridate::year(settings$state.data.assimilation$start.date)) == min(abs(lubridate::year(NEON_dates)-lubridate::year(settings$state.data.assimilation$start.date))))])
    target_date <- lubridate::date(paste0(closeYear,"-07-15"))
    daysDiff <- abs(lubridate::days(lubridate::date(NEON_dates)-lubridate::date(target_date))@day)
    
    #search within 1 month
    start_date <- ymd(as.Date(NEON_dates[which(daysDiff == min(daysDiff))]))
    end_date <- start_date %m+% months(1)
    
    #download
    neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = tempNEON$siteCode, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
    
    #read
    apparentindividual <- neonstore::neon_read(table = "apparentindividual", product = "DP1.10098.001", site = tempNEON$siteCode, start_date = start_date, end_date = end_date, dir = store_dir)
    
    #re-download if data is problematic
    while (is.null(apparentindividual)) {
      print(paste0("current year: ", closeYear, " is problematic. Try next year"))
      closeYear <- closeYear + 1
      target_date <- lubridate::date(paste0(closeYear,"-07-15"))
      daysDiff <- abs(lubridate::days(lubridate::date(NEON_dates)-lubridate::date(target_date))@day)
      
      #search within 1 month
      start_date <- ymd(as.Date(NEON_dates[which(daysDiff == min(daysDiff))]))
      end_date <- start_date %m+% months(1)
      
      #download
      neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = tempNEON$siteCode, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
      apparentindividual <- neonstore::neon_read(table = "apparentindividual", product = "DP1.10098.001", site = tempNEON$siteCode, start_date = start_date, end_date = end_date, dir = store_dir)
    }
    #
    print(paste0("find good year: ", closeYear, ", for site: ", tempNEON$siteCode))
    log_txt <- c(log_txt, paste0("find good year: ", closeYear, ", for site: ", tempNEON$siteCode))
    
    mappingandtagging <- neonstore::neon_read(table = "mappingandtagging", product = "DP1.10098.001", site = tempNEON$siteCode, start_date = start_date, end_date = end_date, dir = store_dir)
    joined.veg <- dplyr::left_join(mappingandtagging, apparentindividual, by = "individualID")
    
    #Filter joined.veg for required information: DBH, tree height, and species
    filter.veg <- dplyr::select(joined.veg, .data$siteID.x, .data$plotID.x, .data$subplotID, .data$taxonID, .data$scientificName, .data$taxonRank, .data$date.y, .data$stemDiameter, .data$height)
    
    #Filter for most recent record
    filter.date <- dplyr::filter(filter.veg, .data$date.y >= start_date)
    
    #Create year column
    filter.date$year <- format(as.Date(filter.date$date.y, format="%d/%m/%Y"),"%Y")
    
    #Rename NEON column names to match pecan functions
    colnames(filter.date) <- c("site_name", "plot", "Subplot", "species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height", "year")
    
    #Create veg_info object as a list
    veg_info <- list()
    
    #Set filter.date as veg_info[[2]]
    veg_info[[2]] <- filter.date
    
    #Set plot size as veg_info[[1]]
    veg_info[[1]] <- list(area = 400)
    
    #set site ID
    veg_info[[3]] <- settings[[i]]$run$site$id
    
    #write into output
    VEG_INFO[[i]] <- veg_info
  }
  fileConn <- file("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/IC/NEON_downloads/log.txt")
  writeLines(log_txt, fileConn)
  close(fileConn)
  save(VEG_INFO, file = paste0(main_store_dir,"/VEG_INFO.Rdata"))
  return(VEG_INFO)
}
