#' Prepare SMAP Soil Moisture (SMP) data for the SDA workflow.
#'
#' @param Site_Info Bety list of site info including site_id, lon, and lat.
#' @param Start_Date Start date of SDA workflow.
#' @param End_Date End date of SDA workflow.
#' @param Time_Step A list containing time step and number of time step, which allows time step to be any years or days.
#' @param OutDir Where the final CSV file, and the CSV file from GEE are stored.
#' @param Search_Window search window for locate available SMP values.
#' @param Export_CSV Decide if we want to export the CSV file.
#' @param Update_CSV Decide if we want to update current CSV file given an updated SMAP_gee.csv file
#'
#' @return A data frame containing SMAP smp and sd for each site and each time step.
#' @export
#'
#' @examples
#' @author Dongchen Zhang
SMAP_prep <- function(Site_Info, Start_Date, End_Date, Time_Step = list(unit="year", num=1), 
                      OutDir, Search_Window = 30, Export_CSV = TRUE, Update_CSV = FALSE){
  
  #note that, the SMAP_gee.csv file comes from Google Earth Engine (GEE) directly.
  #Code for generating this file can be found through this link: 
  #https://code.earthengine.google.com/ecbeb770e576d8ef72f72f5f12da3496
  #Feel free to contact Dongchen Zhang (zhangdc@bu.edu) who wrote this code.
  #The SMAP.csv file will be generated the first time when you use this code.
  #for the next time, it will save you lot of time if you can provide the SMAP.csv directly.
  
  #check if SMAP.csv exists.
  if(!file.exists(file.path(OutDir, "SMAP.csv")) | as.logical(Update_CSV)){
    if(!file.exists(file.path(OutDir, "SMAP_gee.csv"))){
      PEcAn.logger::logger.info("Please Provide SMAP dir that contains at least the SMAP_gee.csv file!")
      return(0)
    }else{
      SMAP_gee <- read.csv(file.path(OutDir, "SMAP_gee.csv"))
      
      #prepare CSV from Current SMAP_gee file
      SMAP_CSV <- matrix(NA, 0, 6) %>% `colnames<-`(c("date", "site_id", "lat", "lon", "smp", "sd"))
      
      for (i in 2 : dim(SMAP_gee)[1]) {
        String <-  strsplit(gsub(",", "", gsub("\\[|\\]", "", SMAP_gee[i,2])), " ")[[1]]
        date <- as.Date(strsplit(String[1], "_")[[1]][5], "%Y%m%d")
        lon <- as.numeric(String[2])
        lat <- as.numeric(String[3])
        smp <- as.numeric(String[5])
        sd <- 0.04 #From Daniel
        
        #Match current lon/lat with site_info
        Distance <- swfscMisc::distance(lat1 = lat, lon1 = lon, lat2 = Site_Info$lat, lon2 = Site_Info$lon)
        distloc <- match(min(Distance), Distance)
        site_id <- Site_Info$site_id[distloc]
        
        SMAP_CSV <- rbind(SMAP_CSV, tibble(date, site_id, lat, lon, smp, sd))#in date, id, lat, lon, smp, sd
      }
      if(as.logical((Export_CSV))){
        write.csv(SMAP_CSV, file = file.path(OutDir, "SMAP.csv"), row.names = F)
      }
    }
  }else{
    #TODO: When current SMAP.csv need to be updated
    SMAP_CSV <- read.csv(file.path(OutDir, "SMAP.csv"))
    Current_years <- sort(unique(lubridate::year(SMAP_CSV$date)))
    Required_years <- lubridate::year(Start_Date):lubridate::year(End_Date)
    Required_years <- Required_years[which(Required_years>=2015)] #SMAP data only available after 2015.
    if(sum(!Required_years%in%Current_years)){
      PEcAn.logger::logger.info("The existing SMAP.csv file doesn't contain data between start and end date!")
      PEcAn.logger::logger.info("Please update the SMAP_gee.csv file to include the data that are missing! And then flag Update_CSV as TRUE to proceed!")
      return(0)
    }
  }
  
  #calculate time points given start, end date, and time step.
  if(Time_Step$unit == "year"){
    years <- seq(0, (lubridate::year(End_Date) - lubridate::year(Start_Date)), as.numeric(Time_Step$num))#how many years between start and end date
    time_points <- as.Date(Start_Date) %m+% years(years)
  }else if(Time_Step$unit == "day"){
    days <- seq(0, (lubridate::yday(End_Date) - lubridate::yday(Start_Date)), as.numeric(Time_Step$num))#how many days between start and end date
    time_points <- as.Date(Start_Date) %m+% days(days)
  }
  
  #initialize SMAP_Output
  SMAP_Output <- matrix(NA, length(Site_Info$site_id), 2*length(time_points)+1) %>% 
    `colnames<-`(c("site_id", paste0(time_points, "_SMP"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
  SMAP_Output$site_id <- Site_Info$site_id
  
  #Calculate SMAP for each time step and site.
  #loop over time and site
  for (t in time_points) {
    t <- as.Date(t)#otherwise the t will be number instead of date.
    for (id in Site_Info$site_id) {
      site_SMP <- SMAP_CSV[which(SMAP_CSV$site_id == id),]
      diff_days <- abs(lubridate::days(lubridate::date(site_SMP$date)-lubridate::date(t))@day)
      if(sum(diff_days <= Search_Window)){#data found
        SMAP_Output[which(SMAP_Output$site_id==id), paste0(t, "_SMP")] <- site_SMP$smp[which.min(diff_days)]
        SMAP_Output[which(SMAP_Output$site_id==id), paste0(t, "_SD")] <- site_SMP$sd[which.min(diff_days)]
      }
    }
  }
  SMAP_Output
}




