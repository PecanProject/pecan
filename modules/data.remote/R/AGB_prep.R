#' Prepare Landtrendr AGB data for the SDA workflow.
#'
#' @param Site_Info Bety list of site info including site_id, lon, and lat.
#' @param Start_Date Start date of SDA workflow.
#' @param End_Date End date of SDA workflow.
#' @param AGB_dir Where the Landtrendr AGB data can be accessed.
#' @param OutDir Where the final CSV file will be stored.
#' @param Export_CSV Decide if we want to export the CSV file.
#' @param Allow_download If data is missing, should we download the missing data?
#'
#' @return A data frame containing AGB median and sd for each site and each time step.
#' @export if Export_CSV is TRUE, then it will export an CSV file
#' containing extracted AGB data associated with each site and each time step.
#'
#' @examples
#' @author Dongchen Zhang
AGB_prep <- function(Site_Info, Start_Date, End_Date, 
                     AGB_dir = "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/AGB", 
                     OutDir = NULL, Export_CSV = TRUE, Allow_download = FALSE){
  #if we export CSV but didn't provide any path
  if(Export_CSV && is.null(OutDir)){
    PEcAn.logger::logger.info("If you want to export CSV file, please ensure input the Outdir!")
    return(0)
  }
  
  time_points <- lubridate::year(Start_Date):lubridate::year(End_Date)
  time_points <- time_points[which(time_points<2018)] #filter out any time points that are larger than 2017
  
  #check if we have all AGB data downloaded, if not, download them
  if(Allow_download){
    AGB_median_years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", list.files(AGB_dir, pattern = "*median.tif")))
    missing_years_median <- time_points[which(!time_points%in%AGB_median_years)] #for landtrendr AGB data, we only have data before 2018.
    
    #starting downloading
    if(length(missing_years_median)>0){
      if(getOption('timeout') < 3600) options(timeout=3600)#enable 1h download time
      PEcAn.data.remote::download.LandTrendr.AGB(outdir = AGB_dir, product_dates = missing_years_median, run_parallel = FALSE)
    }
  }
  
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if(file.exists(file.path(OutDir, "AGB.csv"))){
    Previous_CSV <- as.data.frame(read.csv(file.path(OutDir, "AGB.csv")))
    AGB_Output <- matrix(NA, length(Site_Info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_AGB"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, agb, sd, target time point.
    AGB_Output$site_id <- Site_Info$site_id
    
    #Calculate LAI for each time step and site.
    #loop over time and site
    for (t in time_points) {
      for (id in Site_Info$site_id) {
        site_AGB <- Previous_CSV[which(Previous_CSV$site_id == id),]
        if(length(site_AGB$agb[which(site_AGB$date == t)])==1){
          AGB_Output[which(AGB_Output$site_id==id), paste0(t, "_AGB")] <- site_AGB$agb[which(site_AGB$date == t)]
          AGB_Output[which(AGB_Output$site_id==id), paste0(t, "_SD")] <- site_AGB$sd[which(site_AGB$date == t)]
        }
      }
    }
  }else{#we don't have any previous downloaded CSV file.
    AGB_Output <- matrix(NA, length(Site_Info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_AGB"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, AGB, std, target time point.
    AGB_Output$site_id <- Site_Info$site_id
  }
  
  #only Site that has NA for any time points need to be downloaded.
  new_Site_Info <- Site_Info %>% purrr::map(function(x)x[!complete.cases(AGB_Output)])
  
  #if we have any site missing previously
  if(length(new_Site_Info$site_id) != 0){
    #extracting AGB data
    med_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(new_Site_Info, "median", buffer = NULL, fun = "mean", 
                                                              AGB_dir, product_dates=lubridate::year(Start_Date):lubridate::year(End_Date))[[1]] %>% dplyr::select(-2) %>%
                                                              `colnames<-`(c("site_id", paste0(time_points, "_AGB")))
    sdev_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(new_Site_Info, "stdv", buffer = NULL, fun = "mean", 
                                                               AGB_dir, product_dates=lubridate::year(Start_Date):lubridate::year(End_Date))[[1]]%>% dplyr::select(-c(1:2)) %>%
                                                              `colnames<-`(c(paste0(time_points, "_SD")))
    
    #Handle data
    AGB_Output <- cbind(med_agb_data, sdev_agb_data)
    
    #prepare CSV from AGB_Output
    Current_CSV <- matrix(NA, 0, 6) %>% `colnames<-`(c("date", "site_id", "lat", "lon", "agb", "sd"))
    for (id in AGB_Output$site_id) {
      site_AGB <- AGB_Output[which(AGB_Output$site_id==id),]
      for (String in paste0(time_points, "_AGB")) {
        Current_CSV <- rbind(Current_CSV, 
                             c(strsplit(String, "_")[[1]][1], 
                               id, 
                               new_Site_Info$lat[which(new_Site_Info$site_id==id)], 
                               new_Site_Info$lon[which(new_Site_Info$site_id==id)], 
                               site_AGB[String],
                               site_AGB[gsub("AGB", "SD", String)]))#in date, id, lat, lon, agb, sd
        
      }
    }

    #Compare with existing CSV file. (We name the CSV file as LAI.csv)
    if(Export_CSV){
      if(exists("Previous_CSV")){#we already read the csv file previously.
        Current_CSV <- rbind(Previous_CSV, Current_CSV)
        Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$date)),]#using site_id and date to remove duplicated records.
        write.csv(Current_CSV, file = file.path(OutDir, "AGB.csv"), row.names = FALSE)
      }else{
        write.csv(Current_CSV, file = file.path(OutDir, "AGB.csv"), row.names = FALSE)
      }
    }
  }
  AGB_Output
}