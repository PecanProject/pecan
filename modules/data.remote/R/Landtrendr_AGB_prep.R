#' Prepare Landtrendr AGB data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, site_name, lon, and lat.
#' @param start_date Start date of SDA workflow.
#' @param end_date End date of SDA workflow.
#' @param time_points A vector contains each time point within the start and end date.
#' @param AGB_indir Where the Landtrendr AGB data can be accessed.
#' @param outdir Where the final CSV file will be stored.
#' @param export_csv Decide if we want to export the CSV file.
#' @param allow_download If data is missing, should we download the missing data?
#' @param buffer buffer area to calculate the min var of AGB data.
#' @param skip_buffer flag to skip calculating min var based on buffer area.
#'
#' @return A data frame containing AGB median and sd for each site and each time step.
#' @export
#'
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
Landtrendr_AGB_prep <- function(site_info, start_date, end_date, time_points, 
                                AGB_indir, outdir = NULL, export_csv = TRUE, 
                                allow_download = FALSE, buffer = NULL, skip_buffer = TRUE){
  #Initialize the multicore computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  #if we export CSV but didn't provide any path
  if(as.logical(export_csv) && is.null(outdir)){
    PEcAn.logger::logger.info("If you want to export CSV file, please ensure input the outdir!")
    return(0)
  }
  #Landtrendr AGB doesn't provide data after 2017.
  time_points <- time_points[which(lubridate::year(time_points) < 2018)]
  #check the integrity of AGB files.
  AGB_median_years <- as.numeric(gsub(".*?([0-9]+).*", "\\1", list.files(AGB_indir, pattern = "*median.tif")))
  missing_years_median <- lubridate::year((time_points[which(!lubridate::year(time_points)%in%AGB_median_years)])) #for landtrendr AGB data, we only have data before 2018.
  #starting downloading
  if(length(missing_years_median)>0){
    if(as.logical(allow_download)){
      if(getOption('timeout') < 3600) options(timeout=3600)#enable 1h download time
      PEcAn.data.remote::download.LandTrendr.AGB(outdir = AGB_indir, product_dates = missing_years_median, run_parallel = FALSE)
    }else{
      #files are missing, and we don't allow download
      PEcAn.logger::logger.info("Partial AGB files are missing, please set the allow_download as TRUE to download them automatically!")
      return(0)
    }
  }
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if(!is.null(outdir)){
    if(file.exists(file.path(outdir, "AGB.csv")) && length(buffer)==0 && as.logical(skip_buffer)){
      PEcAn.logger::logger.info("Extracting previous AGB file!")
      Previous_CSV <- as.data.frame(utils::read.csv(file.path(outdir, "AGB.csv")))
      AGB_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
        `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, agb, sd, target time point.
      AGB_Output$site_id <- site_info$site_id
      #Calculate AGB for each time step and site.
      #loop over time and site
      AGB.list <- time_points %>% furrr::future_map(function(t){
        out.t <- data.frame()
        for (id in site_info$site_id) {
          site_AGB <- Previous_CSV[which(Previous_CSV$site_id == id),]
          if(length(site_AGB$agb[which(site_AGB$date == lubridate::year(t))])==1){
            out.t <- rbind(out.t, list(mean = site_AGB$agb[which(site_AGB$date == lubridate::year(t))], 
                                       sd = site_AGB$sd[which(site_AGB$date == lubridate::year(t))]))
          } else {
            out.t <- rbind(out.t, list(mean = NA, sd = NA))
          }
        }
        out.t %>% purrr::set_names(c(paste0(t, "_AbvGrndWood"), paste0(t, "_SD")))
      }, .progress = T)
      for (i in seq_along(time_points)) {
        t <- time_points[i]#otherwise the t will be number instead of date.
        AGB_Output[, paste0(t, "_AbvGrndWood")] <- AGB.list[[i]][,paste0(t, "_AbvGrndWood")]
        AGB_Output[, paste0(t, "_SD")] <- AGB.list[[i]][,paste0(t, "_SD")]
      }
    }
  }
  #only Site that has NA for any time points need to be downloaded.
  if(!exists("AGB_Output")){
    AGB_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, AGB, std, target time point.
    AGB_Output$site_id <- site_info$site_id
  }
  AGB_Output_temp <- AGB_Output
  new_site_info <- site_info %>% purrr::map(function(x)x[!stats::complete.cases(AGB_Output)])
  #if we have any site missing previously
  if(length(new_site_info$site_id) != 0){
    if(is.null(buffer) | as.logical(skip_buffer)){
      #prepare lists for future::map parallelization.
      new_site_info$AGB_indir <- rep(AGB_indir, length(new_site_info$site_id))
      new_site_info$start_date <- rep(start_date, length(new_site_info$site_id))
      new_site_info$end_date <- rep(end_date, length(new_site_info$site_id))
      l <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info))))
      
      #extracting AGB data
      AGB_Output <- l %>% furrr::future_map(function(ll) {
        time_points <- seq(as.Date(ll$start_date), as.Date(ll$end_date), "1 year")
        #Landtrendr AGB doesn't provide data after 2017.
        time_points <- time_points[which(lubridate::year(time_points) < 2018)]
        product_dates <- lubridate::year(time_points)
        site_info <- list(site_id = ll$site_id,
                          lat = ll$lat,
                          lon = ll$lon,
                          site_name = NA)
        
        med_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(site_info = site_info, 
                                                                  dataset = "median", 
                                                                  fun = "mean", 
                                                                  data_dir = ll$AGB_indir, 
                                                                  product_dates = product_dates)[[1]] %>% dplyr::select(-2) %>%
          `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood")))
        sdev_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(site_info = site_info, 
                                                                   dataset = "stdv", 
                                                                   fun = "mean", 
                                                                   data_dir = ll$AGB_indir, 
                                                                   product_dates = product_dates)[[1]] %>% dplyr::select(-c(1:2)) %>%
          `colnames<-`(c(paste0(time_points, "_SD")))
        cbind(med_agb_data, sdev_agb_data)
      }, .progress = T) %>% dplyr::bind_rows()
    }else{#buffer is not empty
      #extracting AGB data
      med <- PEcAn.data.remote::extract.LandTrendr.AGB(site_info = new_site_info, 
                                                       dataset = "median", 
                                                       buffer = buffer, 
                                                       fun = "mean", 
                                                       data_dir = AGB_indir, 
                                                       product_dates = lubridate::year(time_points))[[1]] %>% dplyr::select(-2) %>%
        `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood")))
      sdev <- PEcAn.data.remote::extract.LandTrendr.AGB(site_info = new_site_info, 
                                                        dataset = "stdv", 
                                                        buffer = buffer, 
                                                        fun = "mean", 
                                                        data_dir = AGB_indir, 
                                                        product_dates = lubridate::year(time_points))[[1]] %>% dplyr::select(-c(1:2)) %>%
        `colnames<-`(c(paste0(time_points, "_SD")))
      sdev_agb_data <- med_agb_data <- c()
      #searching for the min variance.
      for (i in seq_along(new_site_info$site_id)) {
        temp_var <- rowSums(sdev[[i]])
        min_var_Ind <- which.min(temp_var)
        
        sdev_agb_data <- rbind(sdev_agb_data, sdev[[i]][min_var_Ind,])
        med_agb_data <- rbind(med_agb_data, med[[i]][min_var_Ind,])
      }
      #Handle data
      AGB_Output <- cbind(med$site_id, med_agb_data, sdev_agb_data) %>% 
        as.data.frame%>%
        `colnames<-`(c("site_id", paste0(time_points, "_AbvGrndWood"), paste0(time_points, "_SD")))
    }
    #prepare CSV from AGB_Output
    Current_CSV <- matrix(NA, 0, 6) %>% 
      `colnames<-`(c("date", "site_id", "lat", "lon", "agb", "sd"))
    for (id in AGB_Output$site_id) {
      site_AGB <- unlist(AGB_Output[which(AGB_Output$site_id==id),])
      for (i in seq_along(time_points)) {
        date <- lubridate::year(time_points[i])
        site_id <- id
        lon <- new_site_info$lon[which(new_site_info$site_id==id)]
        lat <- new_site_info$lat[which(new_site_info$site_id==id)]
        agb <- site_AGB[paste0(time_points[i], "_AbvGrndWood")] %>% purrr::set_names("agb")
        sd <- site_AGB[paste0(time_points[i], "_SD")] %>% purrr::set_names("sd")
        Current_CSV <- rbind(Current_CSV, tibble::tibble(date, site_id, lat, lon, agb, sd))#in date, id, lat, lon, agb, sd
      }
    }
    #Compare with existing CSV file. (We name the CSV file as AGB.csv)
    if(export_csv){
      if(exists("Previous_CSV")){#we already read the csv file previously.
        Current_CSV <- rbind(Previous_CSV, Current_CSV)
        Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$date)),]#using site_id and date to remove duplicated records.
        utils::write.csv(Current_CSV, file = file.path(outdir, "AGB.csv"), row.names = FALSE)
      }else{
        utils::write.csv(Current_CSV, file = file.path(outdir, "AGB.csv"), row.names = FALSE)
      }
    }
    #write current csv into AGB_Output data frame.
    #recreate the AGB_Output object
    AGB_Output <- AGB_Output_temp
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      for (id in site_info$site_id) {
        site_AGB <- Current_CSV[which(Current_CSV$site_id == id),]
        AGB_Output[which(AGB_Output$site_id==id), paste0(t, "_AbvGrndWood")] <- as.numeric(site_AGB[which(site_AGB$date == lubridate::year(t)), "agb"])
        AGB_Output[which(AGB_Output$site_id==id), paste0(t, "_SD")] <- as.numeric(site_AGB[which(site_AGB$date == lubridate::year(t)), "sd"])
      }
    }
  }
  PEcAn.logger::logger.info("Landtrendr AGB Prep Completed!")
  list(AGB_Output = AGB_Output, time_points = time_points, var = "AbvGrndWood")
}