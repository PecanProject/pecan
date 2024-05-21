#' Prepare MODIS land cover data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param time_points A vector contains each time point within the start and end date.
#' @param outdir Where the final CSV file will be stored.
#' @param qc.filter values that will pass the QC check. the check will be skipped if it's NULL.
#'
#' @return A data frame containing MODIS land cover types for each site and each time step.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
#' @details This function enables the feature of grabbing pre-extracted MODIS LC CSV files such that any site that 
#' has records will be skipped (See Line 33). In more detail, we will be loading the previous `LC.csv` file, which
#' contains previous extracted land cover records and trying to match that with current requests (location, time).
#' Any requests that fail the match will be regarded as new extractions and combine with the previous `LC.csv` file.
MODIS_LC_prep <- function(site_info, time_points, outdir = NULL, qc.filter = c("000", "001")){
  #initialize future parallel computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore, workers = 10)
  } else {
    future::plan(future::multisession, workers = 10) #10 is the maximum number of requests permitted for the MODIS server.
  }
  #if we export CSV but didn't provide any path
  if(is.null(outdir)){
    PEcAn.logger::logger.info("If you want to export CSV file, please ensure input the outdir!")
    return(0)
  }
  #convert from dates into years.
  years <- lubridate::year(time_points)
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if (!is.null(outdir)) {
    if(file.exists(file.path(outdir, "LC.csv"))){
      PEcAn.logger::logger.info("Extracting previous MODIS Land Cover file!")
      Previous_CSV <- utils::read.csv(file.path(outdir, "LC.csv"))
      LC_Output <- matrix(NA, length(site_info$site_id), length(years)+1) %>% 
        `colnames<-`(c("site_id", paste0(years, "_LC"))) %>% 
        as.data.frame()#we need: site_id, LC, target time point.
      #fill in the site ids.
      LC_Output$site_id <- site_info$site_id
      #Calculate LAI for each time step and site.
      #loop over time and site
      LC.list <- years %>% furrr::future_map(function(t){
        out.t <- c()
        for (id in site_info$site_id) {
          site_LC <- Previous_CSV[which(Previous_CSV$site_id == id),]
          out.t <- c(out.t, site_LC$LC)
        }
        out.t %>% purrr::set_names(c(paste0(t, "_LC")))
      }, .progress = T)
      for (i in seq_along(years)) {
        t <- years[i]#otherwise the t will be number instead of date.
        LC_Output[, paste0(t, "_LC")] <- LC.list[[i]][,paste0(t, "_LC")]
      }
    }
  } else {
    #we don't have any previous downloaded CSV file.
    LC_Output <- matrix(NA, length(site_info$site_id), length(years)+1) %>% 
      `colnames<-`(c("site_id", paste0(years, "_LC"))) %>% 
      as.data.frame()#we need: site_id, LC, target time point.
    LC_Output$site_id <- site_info$site_id
  }
  #only Site that has NA for any time points need to be downloaded.
  new_site_info <- site_info %>% purrr::map(function(x)x[!stats::complete.cases(LC_Output)])
  #start extraction.
  if(length(new_site_info$site_id) != 0){
    #grab the product and band names.
    product <- "MCD12Q1"
    band <- "LC_Type1"
    LC.types <- list("1"="Evergreen Needleleaf Forests",
                     "2"="Evergreen Broadleaf Forests",
                     "3"="Deciduous Needleleaf Forests",
                     "4"="Deciduous Broadleaf Forests",
                     "5"="Mixed Forests",
                     "6"="Closed Shrublands",
                     "7"="Open Shrublands",
                     "8"="Woody Savannas",
                     "9"="Savannas",
                     "10"="Grasslands",
                     "11"="Permanent Wetlands",
                     "12"="Croplands",
                     "13"="Urban and Built-up Lands",
                     "14"="Cropland/Natural Vegetation Mosaics",
                     "15"="Permanent Snow and Ice",
                     "16"="Barren",
                     "17"="Water Bodies")
    #extracting LC types from MODISTools.
    PEcAn.logger::logger.info("Extracting MODIS Land Cover products!")
    lc.list <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
      furrr::future_map(function(s){
        years %>% 
          purrr::map(function(y){
            if (! "try-error" %in% class(try(value <- MODISTools::mt_subset(product = product,
                                                                            lat = s$lat,
                                                                            lon = s$lon,
                                                                            band = band,
                                                                            start = paste0(y, "-01-01"),
                                                                            end = paste0(y, "-01-01"),
                                                                            progress = FALSE)))) {
              return(list(value = value$value, date = value$calendar_date))
            } else {
              return(NA)
            }
          }) %>% dplyr::bind_rows()
      }, .progress = T)
    #extracting QC values.
    if (!is.null(qc.filter)) {
      PEcAn.logger::logger.info("Extracting Land Cover QC products!")
      qc.list <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
        furrr::future_map(function(s){
          years %>% 
            purrr::map(function(y){
              if (! "try-error" %in% class(try(qc <- MODISTools::mt_subset(product = product,
                                                                           lat = s$lat,
                                                                           lon = s$lon,
                                                                           band = "QC",
                                                                           start = paste0(y, "-01-01"),
                                                                           end = paste0(y, "-01-01"),
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
    }
    LC <- data.frame()
    for (i in seq_along(lc.list)) {
      for (j in seq_along(lc.list[[i]]$value)) {
        # skip pixels with NA observation.
        if (is.na(lc.list[[i]]$value[j])) {
          next
        }
        if (!is.null(qc.filter)) {
          # skip bad pixels based on qc band.
          if (! qc.list[[i]][j] %in% qc.filter) {
            next
          }
        }
        LC <- rbind(LC, list(date = lc.list[[i]]$date[j],
                             site_id = new_site_info$site_id[i],
                             lat = new_site_info$lat[i],
                             lon = new_site_info$lon[i],
                             lc = LC.types[[lc.list[[i]]$value[j]]]))
      }
    }
    #Compare with existing CSV file. (We name the CSV file as LC.csv)
    if(!is.null(outdir)){
      if(exists("Previous_CSV")){#we already read the csv file previously.
        Current_CSV <- rbind(Previous_CSV, LC)
        Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$date)),]#using site_id and date to remove duplicated records.
        utils::write.csv(Current_CSV, file = file.path(outdir, "LC.csv"), row.names = FALSE)
      }else{
        Current_CSV <- LC
        utils::write.csv(Current_CSV, file = file.path(outdir, "LC.csv"), row.names = FALSE)
      }
    } else {
      Current_CSV <- LC
    }
    #Fill LC for each time step and site.
    #loop over time and site
    for (i in seq_along(years)) {
      t <- years[i]
      for (id in new_site_info$site_id) {
        if (!any(Current_CSV$site_id == id)) {
          next
        }
        site_LC <- Current_CSV[which(Current_CSV$site_id == id),]
        LC_Output[which(LC_Output$site_id==id), paste0(t, "_LC")] <- site_LC$lc[which(site_LC$date==paste0(t, "-01-01"))]
      }
    }
  }
  PEcAn.logger::logger.info("MODIS Land Cover Prep Completed!")
  LC_Output
}