#' Prepare MODIS LAI data for the SDA workflow.
#'
#' @param site_info list: Bety list of site info including site_id, lon, and lat.
#' @param time_points character: a vector containing each time point within the start and end date.
#' @param csv.outdir character: where the final CSV file (`MODIS_LAI.csv`) will be stored.
#' @param download.outdir character: where the MODIS LAI image files will be stored.
#' @param search_window numeric: search window to locate available LAI values. Default is 30.
#' @param export_csv logical: decide if we want to export the CSV file. Default is FALSE.
#' @param sd_threshold numeric or character: for filtering out estimations with unrealistically high standard error. Default is 20. Set to NULL to skip QC check.
#' @param skip_download logical: determine if we want to use an existing `MODIS_LAI.csv` file and skip the download step. Default is FALSE.
#' @param boundary numeric vector or list: the upper and lower quantiles for filtering out noisy LAI values (e.g., `c(0.05, 0.95)` or `list(0.05, 0.95)`). Default is NULL.
#'
#' @return A data frame containing MODIS LAI and sd for each site and each time step.
#' @export
#'
#' @author Dongchen Zhang
MODIS_LAI_prep <- function(site_info,
                           time_points,
                           csv.outdir,
                           download.outdir,
                           search_window = 30,
                           export_csv = FALSE,
                           sd_threshold = 20,
                           skip_download = FALSE,
                           boundary = NULL) {
  LAI_prep_helper(
    site_info = site_info,
    time_points = time_points,
    csv.outdir = csv.outdir,
    download.outdir = download.outdir,
    sensor = "MODIS",
    search_window = search_window,
    export_csv = export_csv,
    sd_threshold = sd_threshold,
    skip_download = skip_download,
    boundary = boundary
  )
}