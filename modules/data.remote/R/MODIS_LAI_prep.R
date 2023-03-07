#' Prepare MODIS LAI data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param start_date Start date of SDA workflow.
#' @param end_date End date of SDA workflow.
#' @param time_points A vector contains each time point within the start and end date.
#' @param NCore Number of CPU to be used for LAI extraction.
#' @param run_parallel Bool variable decide if you want to proceed parallelly.
#' @param outdir Where the final CSV file will be stored.
#' @param search_window search window for locate available LAI values.
#' @param export_csv Decide if we want to export the CSV file.
#'
#' @return A data frame containing LAI and sd for each site and each time step.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
MODIS_LAI_prep <- function(site_info, start_date, end_date, time_points, 
                     NCore = NULL, run_parallel = FALSE, outdir = NULL, search_window = 30, export_csv = FALSE){
  #if we export CSV but didn't provide any path
  if(as.logical(export_csv) && is.null(outdir)){
    PEcAn.logger::logger.info("If you want to export CSV file, please ensure input the outdir!")
    return(0)
  }
  
  #collect available dates for all sites.
  time_exist <- c()
  for (i in seq_along(site_info$site_id)) {
    time_exist <- c(time_exist, MODISTools::mt_dates("MOD15A2H", site_info$lat[i], site_info$lon[i])["calendar_date"])
  }
  PEcAn.logger::logger.info("MODIS LAI Dates Matches Completed!")
  time_exist <- sort(unique(do.call("c", time_exist)))
  
  new_time_points <- as.Date(c())
  for (i in seq_along(time_points)) {
    if(sum((abs(lubridate::days(lubridate::date(time_points[i]))@day-
                lubridate::days(lubridate::date(time_exist))@day)<=search_window))>0){
      new_time_points <- c(new_time_points, time_points[i])
    }
  }
  time_points <- new_time_points
  
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if(file.exists(file.path(outdir, "LAI.csv"))){
    Previous_CSV <- utils::read.csv(file.path(outdir, "LAI.csv"))
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
    
    #Calculate LAI for each time step and site.
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      for (id in site_info$site_id) {
        site_LAI <- Previous_CSV[which(Previous_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(sum(diff_days <= as.numeric(search_window))){#data found
          IND <- which((diff_days <= as.numeric(search_window)))
          IND1 <- which(site_LAI$lai[IND] == max(site_LAI$lai[IND]))[1]
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_LAI")] <- max(site_LAI$lai[IND[IND1]])
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_SD")] <- site_LAI$sd[IND[IND1]]
        }
      }
    }
  }else{#we don't have any previous downloaded CSV file.
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
  }
  #only Site that has NA for any time points need to be downloaded.
  new_site_info <- site_info %>% purrr::map(function(x)x[!stats::complete.cases(LAI_Output)])
  
  #if we have any site missing previously
  #TODO: only download data for specific date when we have missing data.
  if(length(new_site_info$site_id) != 0){
    #converting from date to YEAR-DOY(example: 2012-01-01 to 2012001)
    start_YEARDOY <- paste0(lubridate::year(start_date),sprintf("%03d", 1))#using 1 and 365 DOY to avoid any possible missing data.
    end_YEARDOY <- paste0(lubridate::year(end_date),sprintf("%03d", 365))
    
    #download LAI data and LAI std
    lai_data <- PEcAn.data.remote::call_MODIS(outdir = NULL, 
                                              var = "LAI", 
                                              site_info = new_site_info, 
                                              product_dates = c(start_YEARDOY, end_YEARDOY),
                                              run_parallel = as.logical(run_parallel), 
                                              ncores = NCore, 
                                              product = "MOD15A2H", 
                                              band = "Lai_500m",
                                              package_method = "MODISTools", 
                                              QC_filter = TRUE, 
                                              progress = FALSE)
    
    lai_sd <- PEcAn.data.remote::call_MODIS(outdir = NULL, 
                                            var = "LAI", 
                                            site_info = new_site_info, 
                                            product_dates = c(start_YEARDOY, end_YEARDOY),
                                            run_parallel = as.logical(run_parallel), 
                                            ncores = NCore, 
                                            product = "MOD15A2H", 
                                            band = "LaiStdDev_500m",
                                            package_method = "MODISTools", 
                                            QC_filter = TRUE, progress = FALSE)
    
    #combine data together and pick what we want
    LAI <- cbind(lai_data %>% as.data.frame %>% dplyr::select("calendar_date", "site_id", "lat", "lon", "data", "qc"), lai_sd$data) %>% 
      `colnames<-`(c("date", "site_id", "lat", "lon", "lai", "qc", "sd"))
    
    #QC filter
    LAI <- LAI[-which(LAI$qc=="001"),] %>% dplyr::select(-6)#remove qc band
    
    #Compare with existing CSV file. (We name the CSV file as LAI.csv)
    if(as.logical(export_csv)){
      if(exists("Previous_CSV")){#we already read the csv file previously.
        Current_CSV <- rbind(Previous_CSV, LAI)
        Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$date)),]#using site_id and date to remove duplicated records.
        utils::write.csv(Current_CSV, file = file.path(outdir, "LAI.csv"), row.names = FALSE)
      }else{
        Current_CSV <- LAI
        utils::write.csv(Current_CSV, file = file.path(outdir, "LAI.csv"), row.names = FALSE)
      }
    }
    
    #Calculate LAI for each time step and site.
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      for (id in new_site_info$site_id) {
        site_LAI <- Current_CSV[which(Current_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(sum(diff_days <= as.numeric(search_window))){#data found
          IND <- which((diff_days <= as.numeric(search_window)))
          IND1 <- which(site_LAI$lai[IND] == max(site_LAI$lai[IND]))[1]
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_LAI")] <- max(site_LAI$lai[IND[IND1]])
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_SD")] <- site_LAI$sd[IND[IND1]]
        }
      }
    }
  }
  PEcAn.logger::logger.info("MODIS LAI Prep Completed!")
  list(LAI_Output = LAI_Output, time_points = time_points, var = "LAI")
}
