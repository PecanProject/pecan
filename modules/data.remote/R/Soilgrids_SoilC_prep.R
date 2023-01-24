#' Prepare Soilgrids SoilC data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param start_date Start date of SDA workflow.
#' @param end_date End date of SDA workflow.
#' @param time_points A vector contains each time point within the start and end date.
#' @param outdir Where the final CSV file will be stored.
#'
#' @return A data frame containing AGB median and sd for each site and each time step.
#' @export
#'
#' @examples
#' @author Dongchen Zhang
Soilgrids_SoilC_prep <- function(site_info, start_date, end_date, time_points, 
                           outdir = NULL){
  #export special operator
  `%>%` <- magrittr::`%>%` 
  `%m+%` <- as.function(lubridate::`%m+%`)
  
  #if we export CSV but didn't provide any path
  if(is.null(outdir)){
    PEcAn.logger::logger.info("Please provide the input dir!")
    return(0)
  }
  
  #if we have previous extracted soilgrids csv file.
  if(file.exists(file.path(outdir, "soilgrids_soilC_data.csv"))){
    Previous_CSV <- as.data.frame(utils::read.csv(file.path(outdir, "soilgrids_soilC_data.csv")))
    SoilC_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_TotSoilCarb"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, agb, sd, target time point.
    SoilC_Output$site_id <- site_info$site_id
    
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]
      for (id in site_info$site_id) {
        site_SoilC <- Previous_CSV[which(Previous_CSV$Site_ID == id),]
        SoilC_Output[which(SoilC_Output$site_id==id), paste0(t, "_TotSoilCarb")] <- site_SoilC$Total_soilC_0.200cm
        SoilC_Output[which(SoilC_Output$site_id==id), paste0(t, "_SD")] <- site_SoilC$Std_soilC_0.200cm
      }
    }
  }else{
    PEcAn.logger::logger.info("The soilgrids prep function needs the SoilC.CSV file to be prepared before hand!")
    return(0)
  }
  list(SoilC_Output = SoilC_Output, time_points = time_points, var = "SoilC")
}