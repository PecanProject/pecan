#' Prepare Landtrendr AGB data for the SDA workflow.
#'
#' @param Site_Info Bety list of site info including site_id, lon, and lat.
#' @param Start_Date Start date of SDA workflow.
#' @param End_Date End date of SDA workflow.
#' @param timestep A list containing time step and number of time step, which allows time step to be any years or days.
#' @param AGB_input_dir Where the Landtrendr AGB data can be accessed.
#' @param outdir Where the final CSV file will be stored.
#' @param Export_CSV Decide if we want to export the CSV file.
#' @param Allow_download If data is missing, should we download the missing data?
#' @param buffer buffer area to calculate the min var of AGB data.
#' @param skip_buffer flag to skip calculating min var based on buffer area.
#'
#' @return A data frame containing AGB median and sd for each site and each time step.
#' @export
#'
#' @examples
#' @author Dongchen Zhang
Landtrendr_AGB_prep <- function(Site_Info, Start_Date, End_Date, timestep = list(unit="year", num=1), 
                     AGB_input_dir = "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/AGB", 
                     outdir = NULL, Export_CSV = TRUE, Allow_download = FALSE, buffer = NULL, skip_buffer = TRUE){
  #export special operator
  `%>%` <- magrittr::`%>%` 
  `%m+%` <- as.function(lubridate::`%m+%`)
  
  #if we export CSV but didn't provide any path
  if(as.logical(Export_CSV) && is.null(outdir)){
    PEcAn.logger::logger.info("If you want to export CSV file, please ensure input the outdir!")
    return(0)
  }
  
  #calculate time points given start, end date, and time step.
  if(timestep$unit == "year"){
    years <- seq(0, (lubridate::year(End_Date) - lubridate::year(Start_Date)), as.numeric(timestep$num))#how many years between start and end date
    time_points <- as.Date(Start_Date) %m+% lubridate::years(years)
  }else if(timestep$unit == "day"){
    days <- seq(0, (lubridate::yday(End_Date) - lubridate::yday(Start_Date)), as.numeric(timestep$num))#how many days between start and end date
    time_points <- as.Date(Start_Date) %m+% lubridate::days(days)
  }else{
    PEcAn.logger::logger.error("The Landtrendr_AGB_prep function only supports year or day as timestep units!")
    return(0)
  }
  
  time_points <- time_points[which(lubridate::year(time_points)<2018)] #filter out any time points that are larger than 2017
  
  #check if we have all AGB data downloaded, if not, download them
  if(as.logical(Allow_download)){
    AGB_median_years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", list.files(AGB_input_dir, pattern = "*median.tif")))
    missing_years_median <- lubridate::year((time_points[which(!lubridate::year(time_points)%in%AGB_median_years)])) #for landtrendr AGB data, we only have data before 2018.
    
    #starting downloading
    if(length(missing_years_median)>0){
      if(getOption('timeout') < 3600) options(timeout=3600)#enable 1h download time
      PEcAn.data.remote::download.LandTrendr.AGB(outdir = AGB_input_dir, product_dates = missing_years_median, run_parallel = FALSE)
    }
  }
  
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if(file.exists(file.path(outdir, "AGB.csv")) && length(buffer)==0 && skip_buffer){
    Previous_CSV <- as.data.frame(utils::read.csv(file.path(outdir, "AGB.csv")))
    AGB_Output <- matrix(NA, length(Site_Info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, agb, sd, target time point.
    AGB_Output$site_id <- Site_Info$site_id
    
    #Calculate LAI for each time step and site.
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]
      for (id in Site_Info$site_id) {
        site_AGB <- Previous_CSV[which(Previous_CSV$site_id == id),]
        if(length(site_AGB$agb[which(site_AGB$date == lubridate::year(t))])==1){
          AGB_Output[which(AGB_Output$site_id==id), paste0(t, "_AbvGrndWood")] <- site_AGB$agb[which(site_AGB$date == lubridate::year(t))]
          AGB_Output[which(AGB_Output$site_id==id), paste0(t, "_SD")] <- site_AGB$sd[which(site_AGB$date == lubridate::year(t))]
        }
      }
    }
  }else{#we don't have any previous downloaded CSV file.
    AGB_Output <- matrix(NA, length(Site_Info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, AGB, std, target time point.
    AGB_Output$site_id <- Site_Info$site_id
  }
  
  #only Site that has NA for any time points need to be downloaded.
  new_Site_Info <- Site_Info %>% purrr::map(function(x)x[!stats::complete.cases(AGB_Output)])
  
  #if we have any site missing previously
  if(length(new_Site_Info$site_id) != 0){
    if(is.null(buffer)){
      #extracting AGB data
      med_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(new_Site_Info, "median", buffer = buffer, fun = "mean", 
                                                                AGB_input_dir, product_dates=lubridate::year(Start_Date):lubridate::year(End_Date))[[1]] %>% dplyr::select(-2) %>%
        `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood")))
      sdev_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(new_Site_Info, "stdv", buffer = buffer, fun = "mean", 
                                                                 AGB_input_dir, product_dates=lubridate::year(Start_Date):lubridate::year(End_Date))[[1]]%>% dplyr::select(-c(1:2)) %>%
        `colnames<-`(c(paste0(time_points, "_SD")))
      #Handle data
      AGB_Output <- cbind(med_agb_data, sdev_agb_data)
    }else{#buffer is not empty
      #extracting AGB data
      med <- PEcAn.data.remote::extract.LandTrendr.AGB(new_Site_Info, "median", buffer = buffer, fun = "mean", 
                                                                AGB_input_dir, product_dates=lubridate::year(Start_Date):lubridate::year(End_Date))
      sdev <- PEcAn.data.remote::extract.LandTrendr.AGB(new_Site_Info, "stdv", buffer = buffer, fun = "mean", 
                                                                 AGB_input_dir, product_dates=lubridate::year(Start_Date):lubridate::year(End_Date))
      sdev_agb_data <- med_agb_data <- c()
      for (i in seq_along(new_Site_Info$site_id)) {
        temp_var <- rowSums(sdev[[i]])
        min_var_Ind <- which.min(temp_var)
        
        sdev_agb_data <- rbind(sdev_agb_data, sdev[[i]][min_var_Ind,])
        med_agb_data <- rbind(med_agb_data, med[[i]][min_var_Ind,])
      }
      colnames(med_agb_data) <- paste0(time_points, "_AbvGrndWood")
      colnames(sdev_agb_data) <- paste0(time_points, "_SD")
      #Handle data
      AGB_Output <- cbind(med_agb_data, sdev_agb_data) %>% as.data.frame
      AGB_Output$site_id <- Site_Info$site_id
    }
    
    #prepare CSV from AGB_Output
    Current_CSV <- matrix(NA, 0, 6) %>% `colnames<-`(c("date", "site_id", "lat", "lon", "agb", "sd"))
    for (id in AGB_Output$site_id) {
      site_AGB <- AGB_Output[which(AGB_Output$site_id==id),]
      for (i in seq_along(time_points)) {
        date <- lubridate::year(time_points[i])
        site_id <- id
        lon <- new_Site_Info$lon[which(new_Site_Info$site_id==id)]
        lat <- new_Site_Info$lat[which(new_Site_Info$site_id==id)]
        agb <- site_AGB[paste0(time_points[i], "_AbvGrndWood")] %>% purrr::set_names("agb")
        sd <- site_AGB[paste0(time_points[i], "_SD")] %>% purrr::set_names("sd")
        Current_CSV <- rbind(Current_CSV, tibble::tibble(date, site_id, lat, lon, agb, sd))#in date, id, lat, lon, agb, sd
      }
    }

    #Compare with existing CSV file. (We name the CSV file as LAI.csv)
    if(Export_CSV){
      if(exists("Previous_CSV")){#we already read the csv file previously.
        Current_CSV <- rbind(Previous_CSV, Current_CSV)
        Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$date)),]#using site_id and date to remove duplicated records.
        utils::write.csv(Current_CSV, file = file.path(outdir, "AGB.csv"), row.names = FALSE)
      }else{
        utils::write.csv(Current_CSV, file = file.path(outdir, "AGB.csv"), row.names = FALSE)
      }
    }
  }
  list(AGB_Output = AGB_Output, time_points = time_points, var = "AbvGrndWood")
}
