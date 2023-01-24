#' Prepare Soilgrids SoilC data for the SDA workflow.
#'
#' @param Site_Info Bety list of site info including site_id, lon, and lat.
#' @param Start_Date Start date of SDA workflow.
#' @param End_Date End date of SDA workflow.
#' @param timestep A list containing time step and number of time step, which allows time step to be any years or days.
#' @param outdir Where the final CSV file will be stored.
#'
#' @return A data frame containing AGB median and sd for each site and each time step.
#' @export
#'
#' @examples
#' @author Dongchen Zhang
Soilgrids_SoilC_prep <- function(Site_Info, Start_Date, End_Date, timestep = list(unit="year", num=1), 
                           outdir = NULL){
  #export special operator
  `%>%` <- magrittr::`%>%` 
  `%m+%` <- as.function(lubridate::`%m+%`)
  
  #if we export CSV but didn't provide any path
  if(is.null(outdir)){
    PEcAn.logger::logger.info("Please provide the input dir!")
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
    PEcAn.logger::logger.error("The Soilgrids_SoilC_prep function only supports year or day as timestep units!")
  }
  
  #if we have previous extracted soilgrids csv file.
  if(file.exists(file.path(outdir, "soilgrids_soilC_data.csv"))){
    Previous_CSV <- as.data.frame(utils::read.csv(file.path(outdir, "soilgrids_soilC_data.csv")))
    SoilC_Output <- matrix(NA, length(Site_Info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_SoilC"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, agb, sd, target time point.
    SoilC_Output$site_id <- Site_Info$site_id
    
    #loop over time and site
    for (i in 1:length(time_points)) {
      t <- time_points[i]
      for (id in Site_Info$site_id) {
        site_SoilC <- Previous_CSV[which(Previous_CSV$Site_ID == id),]
        SoilC_Output[which(SoilC_Output$site_id==id), paste0(t, "_SoilC")] <- site_SoilC$Total_soilC_0.200cm
        SoilC_Output[which(SoilC_Output$site_id==id), paste0(t, "_SD")] <- site_SoilC$Std_soilC_0.200cm
      }
    }
  }else{
    PEcAn.logger::logger.info("The soilgrids prep function needs the SoilC.CSV file to be prepared before hand!")
    return(0)
  }
  list(SoilC_Output = SoilC_Output, time_points = time_points, var = "SoilC")
}