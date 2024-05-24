#' Extract CDS soil moisture data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param time.points A vector contains each time point within the start and end date.
#' @param in.path physical paths to where the unziped soil moisture files are downloaded.
#' @param out.path Where the final CSV file will be stored.
#' @param allow.download Flag determine if we want to automatic download files if they are not available.
#' @param search_window time length (days) for locate available soil moisture values.
#'
#' @return A data frame containing soil moisture and sd for each site and each time step.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom dplyr %>%
extract_SM_CDS <- function(site_info,
                           time.points, 
                           in.path, 
                           out.path = NULL, 
                           allow.download = TRUE,
                           search_window = 10){
  #find downloaded files and the corresponding dates.
  ncfiles <- base::list.files(in.path, full.names = T)
  available.dates <- ncfiles %>% purrr::map(function(file){
    base::strsplit(x = file, split = ".", fixed = T)[[1]][2]
  }) %>% unlist %>% as.Date
  #loop over time points.
  results <- data.frame()
  for (i in seq_along(time.points)) {
    #generate dates within search window.
    dates <- c(seq(lubridate::date(time.points[i]), by = "1 day", length.out = search_window))
    dates.ind.download <- which(!dates%in%available.dates)
    dates.ind.exist <- which(dates%in%available.dates)
    #if there is no nc file downloaded or within the search window.
    if (length(dates.ind.download) > 0) {
      PEcAn.logger::logger.info(paste("The nc files for the following dates were not found.", 
                                      paste(dates[dates.ind.download], collapse = ", ")))
      #try download if allowed.
      if (allow.download) {
        PEcAn.logger::logger.info("Try download from cds server.")
        ncfile <- c(ncfiles[dates.ind.exist], PEcAn.data.land::download.SM_CDS(in.path, dates[dates.ind.download])) %>% sort()
      } else {
        PEcAn.logger::logger.info("The download is not enabled, skip to the next time point.")
        next
      }
    } else {
      ncfile <- ncfiles[dates.ind.exist] %>% sort()
    }
    #read nc files.
    PEcAn.logger::logger.info(paste("Extracting nc files for date", time.points[i]))
    sm <- ncfile %>% furrr::future_map(function(nc){
      list(sm = raster::brick(nc, varname = "sm") %>% 
             raster::extract(sp::SpatialPoints(cbind(site_info$lon, site_info$lat)),
                             method = 'simple') %>% as.vector(),
           sm_uncertainty = raster::brick(nc, varname = "sm_uncertainty") %>% 
             raster::extract(sp::SpatialPoints(cbind(site_info$lon, site_info$lat)),
                             method = 'simple') %>% as.vector())
    }, .progress = T) %>% dplyr::bind_cols() %>% as.data.frame()
    sm.mean <- sm[,seq(1, length(ncfile)*2, 2)] %>% `colnames<-`(rep("sm.mean", length(seq(1, length(ncfile)*2, 2))))
    sm.uncertainty <- sm[,seq(2, length(ncfile)*2, 2)] %>% `colnames<-`(rep("sm.uncertainty", ncol(sm.mean)))
    #fill in results data frame.
    for (j in seq_along(site_info$site_id)) {
      nonNA.ind <- which.min(is.na(sm.mean[j,]))
      results <- rbind(results, list(date = as.character(time.points[i]),
                                     site.id = site_info$site_id[j],
                                     lon = site_info$lon[j],
                                     lat = site_info$lat[j],
                                     sm.mean = sm.mean[j, nonNA.ind],
                                     sm.uncertainty = sm.uncertainty[j, nonNA.ind]))
    }
  }
  #tweak results.
  #if sm mean is NA, we remove the entire row.
  # results <- results[!is.na(results$sm.mean),]
  #if sm uncertainty is NA, we set the uncertainty as the maximum uncertainty across sites.
  results$sm.uncertainty[which(is.na(results$sm.uncertainty))] <- max(results$sm.uncertainty, na.rm = T)
  #write into csv file.
  if (!is.null(out.path)) {
    utils::write.csv(results, file = file.path(out.path, "sm.csv"))
  }
  results
}