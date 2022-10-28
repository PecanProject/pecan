#' extract_NEON_veg
#' @title extract_NEON_veg
#' @name extract_NEON_veg
#'
#' @param lon site longitude, passed from ic_process
#' @param lat site latitude, passed from ic_process
#' @param start_date "YYYY-MM-DD", used to download NEON datasets for desired time period
#' @param end_date "YYYY_MM_DD", used to download NEON datasets for desired time period
#' @param store_dir location where you want to store downloaded NEON files
#' @param neonsites prepared datasets table from NEON using neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
#' @param ... Additional parameters
#' 
#' 
#' @return veg_info object to be passed to extract_veg within ic_process
#' @author Alexis Helgeson and Michael Dietze
#'
#' @export
#' @importFrom rlang .data
#' @examples 
#' start_date = as.Date("2020-01-01") 
#' end_date = as.Date("2021-09-01")
extract_NEON_veg <- function(lon, lat, start_date, end_date, store_dir, neonsites = NULL, ...){
  
  #Function to grab the first measurements for each plot between start and end date.
  Grab_First_Measurements_of_Each_Plot <- function(temp_data){
    Plot_Year <- paste0(temp_data$plot, temp_data$year)
    unique_Year <- sort(unique(temp_data$year))
    unique_Plot <- sort(unique(temp_data$plot))
    Ind <- rep(NA, length(Plot_Year))
    
    for (j in 1:length(unique_Plot)) {
      for (k in 1:length(unique_Year)) {
        if(length(which(Plot_Year == paste0(unique_Plot[j], unique_Year[k])))>0){
          Ind[which(Plot_Year == paste0(unique_Plot[j], unique_Year[k]))] <- 1
          break
        }
      }
    }
    temp_data <- cbind(temp_data, Ind)
    if(sum(is.na(temp_data$Ind))==0){
      temp_data <- temp_data
    }else{
      temp_data <- temp_data[-which(is.na(temp_data$Ind)),]
    }
    temp_data
  }
  
  #Find sitename from lon and lat params using distance
  if(is.null(neonsites)){
    neonsites <- neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
  }
  neonsites <- dplyr::select(neonsites, "siteCode", "siteLatitude", "siteLongitude") #select for relevant columns
  betyneondist <- swfscMisc::distance(lat1 = lat, lon1 = lon, lat2 = neonsites$siteLatitude, lon2 = neonsites$siteLongitude)
  mindist <- min(betyneondist)
  distloc <- match(mindist, betyneondist)
  lat <- neonsites$siteLatitude[distloc]
  lon <- neonsites$siteLongitude[distloc]
  site <- dplyr::filter(neonsites, .data$siteLatitude == lat & .data$siteLongitude == lon)
  sitename = site$siteCode
  #Load in NEON datasets
  neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
  apparentindividual <- neonstore::neon_read(table = "apparentindividual", product = "DP1.10098.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
  if(is.null(apparentindividual)){
    filter.date <- NA
  }else{
    mappingandtagging <- neonstore::neon_read(table = "mappingandtagging", product = "DP1.10098.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
    joined.veg <- dplyr::left_join(mappingandtagging, apparentindividual, by = "individualID")
    #Filter joined.veg for required information: DBH, tree height, and species
    filter.veg <- dplyr::select(joined.veg, "siteID.x", "plotID.x", "subplotID", "taxonID", "scientificName", "taxonRank", "date.y", "stemDiameter", "height")
    #Filter for most recent record
    filter.date <- dplyr::filter(filter.veg, .data$date.y >= start_date)
    filter.date <- filter.date[which(!is.na(filter.date$subplotID), !is.na(filter.date$stemDiameter)),]
    #Create year column
    filter.date$year <- format(as.Date(filter.date$date.y, format="%d/%m/%Y"),"%Y")
    #Rename NEON column names to match pecan functions
    colnames(filter.date) <- c("site_name", "plot", "Subplot", "species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height", "year")
    filter.date <- Grab_First_Measurements_of_Each_Plot(filter.date)
  }
   
  #herb AGB
  neonstore::neon_download("DP1.10023.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
  massdata <- neonstore::neon_read(table = "massdata", product = "DP1.10023.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
  if(is.null(massdata)){
    filter.herb <- NA
  }else{
    perbout <- neonstore::neon_read(table = "perbout", product = "DP1.10023.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
    joined.herb <- dplyr::left_join(massdata, perbout, by = "sampleID")
    filter.herb <- dplyr::select(joined.herb, "siteID.y", "plotID.x", "subplotID", "plotType.x", "clipArea", "dryMass", "collectDate.y")
    #Create year column
    filter.herb$year <- format(as.Date(filter.herb$collectDate.y, format="%Y-%m-%d"),"%Y")
    #Rename NEON column names to match pecan functions
    colnames(filter.herb) <- c("site_name", "plot", "Subplot", "plotType", "clipArea", "dryMass", "date", "year")
    filter.herb <- Grab_First_Measurements_of_Each_Plot(filter.herb)
  }
  
  # #species info
  # neonstore::neon_download("DP1.10058.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
  # div_1m2 <- neonstore::neon_read(table = "div_1m2", product = "DP1.10058.001", site = sitename, start_date = start_date, end_date = end_date, dir = store_dir)
  # 
  # #check if species info is available for herb plots
  # herb.plot <- unique(filter.herb$plotID.x)
  # check.species <- herb.plot %in% filter.species$plotID
  # colnames(filter.herb)[2] <- "plotID"
  # if (TRUE %in% check.species) {
  #   #add species info to filter.herb if it exists
  #   filter.herb <- dplyr::left_join(filter.herb, filter.species, by = "plotID")
  # }else{
  #   PEcAn.logger::logger.info(paste0("No herbacious species info available for ", sitename))
  # }
  # 
  # #remove NAs from species column only, next step species matching does not like NAs
  # filter.herb <- filter.herb[!is.na(filter.herb$scientificName),]
  # filter.date <- filter.date[!is.na(filter.date$scientificName),]
  
  # #soil carbon
  neonstore::neon_download("DP1.00096.001", dir = store_dir, table = NA, site = sitename, start_date = as.Date("2012-01-01"), end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
  perbulksample <- neonstore::neon_read(table = "perbulksample", product = "DP1.00096.001", site = sitename, start_date = as.Date("2012-01-01"), end_date = end_date, dir = store_dir)
  if(is.null(perbulksample)){
    print("no soil carbon data found!")
    joined.soil <- NA
  }else{
    #filter for regular type of bulk density measurements
    perbulksample <- perbulksample[perbulksample$bulkDensSampleType=="Regular",]
    
    #remove duplicated and NA measurements for bulk density
    bulkDensBottomDepth <- perbulksample$bulkDensBottomDepth
    bulkDensExclCoarseFrag <- perbulksample$bulkDensExclCoarseFrag
    Ind <- (!duplicated(bulkDensBottomDepth))&(!is.na(bulkDensExclCoarseFrag))
    bulkDensBottomDepth <- bulkDensBottomDepth[Ind]
    bulkDensExclCoarseFrag <- bulkDensExclCoarseFrag[Ind]
    
    #calculate bulk density (need to do: more precise depth matching)
    bulkDensity <- mean(bulkDensExclCoarseFrag[which(bulkDensBottomDepth <= 30)])
    
    #if there is no bulk density measurements bellow 30cm.
    if(is.na(bulkDensity)){
      joined.soil <- NA
    }else{
      #download periodic data and join tables
      #so far we use end date of the the year 2021 
      #because the "sls_soilCoreCollection" table will fail when we use more recent end date for some sites.
      neonstore::neon_download("DP1.10086.001", dir = store_dir, table = NA, site = sitename, start_date = as.Date("2012-01-01"), end_date = as.Date("2021-01-01"), type = "basic",api = "https://data.neonscience.org/api/v0")
      sls_soilChemistry <- neonstore::neon_read(table = "sls_soilChemistry", product = "DP1.10086.001", site = sitename, start_date = as.Date("2012-01-01"), end_date = as.Date("2021-01-01"), dir = store_dir)
      sls_soilCoreCollection <- neonstore::neon_read(table = "sls_soilCoreCollection", product = "DP1.10086.001", site = sitename, start_date = as.Date("2012-01-01"), end_date = as.Date("2021-01-01"), dir = store_dir)
      
      if(is.null(sls_soilChemistry) | is.null(sls_soilCoreCollection)){
        print("no soil carbon data found!")
        joined.soil <- NA
      }else{
        joined.soil <- dplyr::left_join(sls_soilChemistry, sls_soilCoreCollection, by = "sampleID")
        
        #select columns
        joined.soil <- dplyr::select(joined.soil, .data$siteID.x, .data$plotID.x, .data$plotType.x, .data$organicCPercent, .data$collectDate.x, .data$sampleTopDepth, .data$sampleBottomDepth)
        joined.soil$year <- lubridate::year(joined.soil$collectDate.x)
        colnames(joined.soil) <- c("site_name", "plot", "plotType", "organicCPercent", "date", "top", "bottom", "year")
        
        joined.soil <- Grab_First_Measurements_of_Each_Plot(joined.soil)
        
        #remove NA values for organicCPercent data
        joined.soil <- joined.soil[which(!is.na(joined.soil$organicCPercent)),]
        
        #calculate soil carbon
        joined.soil$bulkDensity <- bulkDensity
        
        #convert from g/cm2 to g/m2, note that we have to divide by 100 because of percentage
        joined.soil$SoilCarbon <- (joined.soil$organicCPercent * joined.soil$bulkDensity)*30*100 
      }
    }
  }
  
  #Create veg_info object as a list
  veg_info <- list()
  #Set filter.date as veg_info[[2]]
  veg_info[[2]] <- filter.date
  #Set plot size as veg_info[[1]]
  veg_info[[1]] <- filter.herb
  veg_info[[3]] <- joined.soil
  
  return(veg_info)
}
