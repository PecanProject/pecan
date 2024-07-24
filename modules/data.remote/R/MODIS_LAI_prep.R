#' Prepare MODIS LAI data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param time_points A vector contains each time point within the start and end date.
#' @param outdir Where the final CSV file will be stored.
#' @param search_window search window for locate available LAI values.
#' @param export_csv Decide if we want to export the CSV file.
#' @param sd_threshold Threshold for filtering out any estimations with unrealistic high standard error, default is 20. The QC check will be skipped if it's set as NULL.
#'
#' @return A data frame containing LAI and sd for each site and each time step.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
MODIS_LAI_prep <- function(site_info, time_points, outdir = NULL, search_window = 30, export_csv = FALSE, sd_threshold = 20){
  #initialize future parallel computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore, workers = 10)
  } else {
    future::plan(future::multisession, workers = 10) #10 is the maximum number of requests permitted for the MODIS server.
  }
  #if we export CSV but didn't provide any path
  if(as.logical(export_csv) && is.null(outdir)){
    PEcAn.logger::logger.info("If you want to export CSV file, please ensure input the outdir!")
    return(0)
  }
  #convert time points into paired start and end dates.
  start.end.dates <- data.frame()
  for (i in seq_along(time_points)) {
    start.end.dates <- rbind(start.end.dates,
                             list(start_date = as.character(time_points[i] - lubridate::days(search_window)),
                                  end_date = as.character(time_points[i] + lubridate::days(search_window))))
    
  }
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if(file.exists(file.path(outdir, "LAI.csv"))){
    PEcAn.logger::logger.info("Extracting previous LAI file!")
    Previous_CSV <- utils::read.csv(file.path(outdir, "LAI.csv"))
    if (!is.null(sd_threshold)) {
      PEcAn.logger::logger.info("filtering out records with high standard errors!")
      ind.rm <- which(Previous_CSV$sd >= sd_threshold)
      if (length(ind.rm) > 0) {
        Previous_CSV <- Previous_CSV[-ind.rm,]
      }
    }
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
    #Calculate LAI for each time step and site.
    #loop over time and site
    LAI.list <- time_points %>% furrr::future_map(function(t){
      out.t <- data.frame()
      for (id in site_info$site_id) {
        site_LAI <- Previous_CSV[which(Previous_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(any(diff_days <= search_window)){#data found
          out.t <- rbind(out.t, list(mean = site_LAI$lai[which.min(diff_days)], sd = site_LAI$sd[which.min(diff_days)]))
        } else {
          out.t <- rbind(out.t, list(mean = NA, sd = NA))
        }
      }
      out.t %>% purrr::set_names(c(paste0(t, "_LAI"), paste0(t, "_SD")))
    }, .progress = T)
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      LAI_Output[, paste0(t, "_LAI")] <- LAI.list[[i]][,paste0(t, "_LAI")]
      LAI_Output[, paste0(t, "_SD")] <- LAI.list[[i]][,paste0(t, "_SD")]
    }
  }else{#we don't have any previous downloaded CSV file.
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
  }
  #only Site that has NA for any time points need to be downloaded.
  new_site_info <- site_info %>% purrr::map(function(x)x[!stats::complete.cases(LAI_Output)])
  #TODO: only download data for specific date when we have missing data.
  if(length(new_site_info$site_id) != 0){
    product <- "MCD15A3H"
    PEcAn.logger::logger.info("Extracting LAI mean products!")
    lai_mean <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
      furrr::future_map(function(s){
        split(as.data.frame(start.end.dates), seq(nrow(as.data.frame(start.end.dates)))) %>% 
          purrr::map(function(dates){
            if (! "try-error" %in% class(try(mean <- MODISTools::mt_subset(product = product,
                                                                           lat = s$lat,
                                                                           lon = s$lon,
                                                                           band = "Lai_500m",
                                                                           start = dates$start_date,
                                                                           end = dates$end_date,
                                                                           progress = FALSE)))) {
              return(list(mean = mean$value, date = mean$calendar_date))
            } else {
              return(NA)
            }
          }) %>% dplyr::bind_rows()
      }, .progress = T)
    PEcAn.logger::logger.info("Extracting LAI std products!")
    lai_std <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
      furrr::future_map(function(s){
        split(as.data.frame(start.end.dates), seq(nrow(as.data.frame(start.end.dates)))) %>% 
          purrr::map(function(dates){
            if (! "try-error" %in% class(try(std <- MODISTools::mt_subset(product = product,
                                                                          lat = s$lat,
                                                                          lon = s$lon,
                                                                          band = "LaiStdDev_500m",
                                                                          start = dates$start_date,
                                                                          end = dates$end_date,
                                                                          progress = FALSE)))) {
              return(std$value)
            } else {
              return(NA)
            }
          }) %>% unlist %>% purrr::set_names(NULL)
      }, .progress = T)
    PEcAn.logger::logger.info("Extracting LAI qc products!")
    lai_qc <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
      furrr::future_map(function(s){
        split(as.data.frame(start.end.dates), seq(nrow(as.data.frame(start.end.dates)))) %>% 
          purrr::map(function(dates){
            if (! "try-error" %in% class(try(qc <- MODISTools::mt_subset(product = product,
                                                                         lat = s$lat,
                                                                         lon = s$lon,
                                                                         band = "FparLai_QC",
                                                                         start = dates$start_date,
                                                                         end = dates$end_date,
                                                                         progress = FALSE)))) {
              qc$value %>% purrr::map(function(v){
                qc_flag <- intToBits(as.integer(v)) # NB big-endian (ones place first)
                qc_flag <- as.integer(rev(utils::head(qc_flag, 3))) # now ones place last
                paste(qc_flag, collapse = "")
              })
            } else {
              return(NA)
            }
          }) %>% unlist %>% purrr::set_names(NULL)
      }, .progress = T)
    # LAI <- data.frame(matrix(NA, 0, 6)) %>% `colnames<-`(c("date", "site_id", "lat", "lon", "lai", "sd"))
    LAI <- data.frame()
    for (i in seq_along(lai_std)) {
      for (j in seq_along(lai_std[[i]])) {
        # skip pixels with NA observation.
        if (is.na(lai_std[[i]][j])) {
          next
        }
        # skip bad pixels based on qc band.
        if (! lai_qc[[i]][j] %in% c("000", "001")) {
          next
        }
        if (!is.null(sd_threshold)) {
          if (lai_std[[i]][j] >= sd_threshold) {
            next
          }
        }
        LAI <- rbind(LAI, list(date = lai_mean[[i]]$date[j],
                               site_id = new_site_info$site_id[i],
                               lat = new_site_info$lat[i],
                               lon = new_site_info$lon[i],
                               lai = lai_mean[[i]]$mean[j]*0.1,
                               sd = lai_std[[i]][j]*0.1))
      }
    }
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
    } else {
      Current_CSV <- LAI
    }
    #Calculate LAI for each time step and site.
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      for (id in new_site_info$site_id) {
        site_LAI <- Current_CSV[which(Current_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(any(diff_days <= as.numeric(search_window))){#data found
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_LAI")] <- site_LAI$lai[which.min(diff_days)]
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_SD")] <- site_LAI$sd[which.min(diff_days)]
        }
      }
    }
  }
  PEcAn.logger::logger.info("MODIS LAI Prep Completed!")
  list(LAI_Output = LAI_Output, time_points = time_points, var = "LAI")
}