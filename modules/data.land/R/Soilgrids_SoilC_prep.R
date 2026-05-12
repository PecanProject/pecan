#' Prepare Soilgrids SoilC data for the SDA workflow.
#'
#' @param site_info Bety list of site info including site_id, lon, and lat.
#' @param start_date Start date of SDA workflow.
#' @param end_date End date of SDA workflow.
#' @param time_points A vector contains each time point within the start and end date.
#' @param outdir Where the final CSV file will be stored.
#' @param export_csv Decide if we want to export the CSV file.
#'
#' @return A data frame containing AGB median and sd for each site and each time step.
#' @export
#'
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
Soilgrids_SoilC_prep <- function(site_info, start_date, end_date, time_points,
                                 outdir = NULL, export_csv = FALSE) {
  
  if (as.logical(export_csv) & is.null(outdir)) {
    PEcAn.logger::logger.info(
      "If you want to export CSV file, please ensure input the outdir!"
    )
    return(0)
  }
  
  soilc_file <- if (!is.null(outdir)) {
    file.path(outdir, "soilgrids_soilC_data.csv")
  } else {
    NA_character_
  }
  
  if (!is.null(outdir) && !file.exists(soilc_file)) {
    
    if (as.logical(export_csv)) {
      Previous_CSV <- PEcAn.data.land::soilgrids_soilC_extract(site_info, outdir)
    } else {
      Previous_CSV <- PEcAn.data.land::soilgrids_soilC_extract(site_info)
    }
    
  } else if (!is.null(outdir) && file.exists(soilc_file)) {
    
    Previous_CSV <- as.data.frame(utils::read.csv(soilc_file))
    
  } else {
    
    Previous_CSV <- PEcAn.data.land::soilgrids_soilC_extract(site_info)
  }
  
  if (!is.null(outdir) && file.exists(soilc_file)) {
    Previous_CSV <- as.data.frame(utils::read.csv(soilc_file))
  }
  
  SoilC_Output <- matrix(
    NA,
    length(site_info$site_id),
    2 * length(time_points) + 1
  ) %>%
    `colnames<-`(
      c(
        "site_id",
        paste0(time_points, "_TotSoilCarb"),
        paste0(time_points, "_SD")
      )
    ) %>%
    as.data.frame()
  
  SoilC_Output$site_id <- site_info$site_id
  
  for (i in seq_along(time_points)) {
    t <- time_points[i]
    
    for (id in site_info$site_id) {
      site_SoilC <- Previous_CSV[
        which(as.character(Previous_CSV$Site_ID) == as.character(id)),
      ]
      
      if (nrow(site_SoilC) == 0) {
        next
      }
      
      SoilC_Output[
        which(as.character(SoilC_Output$site_id) == as.character(id)),
        paste0(t, "_TotSoilCarb")
      ] <- site_SoilC$Total_soilC_0.200cm
      
      SoilC_Output[
        which(as.character(SoilC_Output$site_id) == as.character(id)),
        paste0(t, "_SD")
      ] <- site_SoilC$Std_soilC_0.200cm
    }
  }
  
  PEcAn.logger::logger.info("Soilgrids SoilC Prep Completed!")
  
  list(
    SoilC_Output = SoilC_Output,
    time_points  = time_points,
    var          = "TotSoilCarb"
  )
}