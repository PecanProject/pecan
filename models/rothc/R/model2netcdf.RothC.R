#' Convert RothC output to PEcAn-formatted netCDF
#'
#' @param outdir Location of model output
#' @param sitelat Latitude of the site
#' @param sitelon Longitude of the site
#' @param start_date Start time of the simulation
#' @param end_date End time of the simulation
#' @export
#' @importFrom rlang .data .env
#'
#' @author Chris Black
model2netcdf.RothC <- function(outdir, sitelat, sitelon, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  var_map <- rothc_varname_map |>
    dplyr::filter(!is.na(.data$pecan_name)) |>
    dplyr::left_join(
      PEcAn.utils::standard_vars,
      by = c("pecan_name" = "Variable.Name")
    ) |>
    dplyr::rename("pecan_unit" = "Units", "pecan_longname" = "Long.name")

  # used below to rename variables after unit conversion
  var_rename_list <- var_map$rothc_name |>
    stats::setNames(var_map$pecan_name)


  # Year, Month, C_Inp_t_C_ha, OA_Inp_t_C_ha, TEMP_C, RM_TMP, RAIN_mm,
  # PEVAP_mm, SMD_mm, RM_Moist, PC, RM_PC, DPM_t_C_ha, RPM_t_C_ha, Bio_t_C_ha,
  # Hum_t_C_ha, IOM_t_C_ha, SOC_t_C_ha, CO2_t_C_ha
  res <- utils::read.csv(file.path(outdir, "month_results.out")) |>
    # Remove spinup lines. TODO handle better:
    # - are there always just two of them?
    # - is first year of data always used as spinup?
    # - is temperature always null for these and no others?
    dplyr::filter(.data$Year > 1) |>
    # Convert time formats
    dplyr::mutate(
      midpoint_date =
        ISOdate(year = .data$Year, month = .data$Month, day = 15, tz = "UTC"),
      month_began = .data$midpoint_date |>
        lubridate::rollback(preserve_hms = FALSE, roll_to_first = TRUE),
      month_ended = .data$midpoint_date |>
        lubridate::rollforward(preserve_hms = FALSE, roll_to_first = FALSE),
      time = difftime(.data$midpoint_date, .env$start_date, units = "days")
    )

  # ensure all requested dates are present and then subset,
  # padding start/end to full months
  stopifnot(
    start_date >= min(res$month_began),
    end_date <= max(res$month_ended)
  )
  res <- res |>
    dplyr::filter(
      .data$month_began >= lubridate::rollback(start_date, roll_to_first = TRUE),
      .data$month_ended <= lubridate::rollforward(end_date, roll_to_first = FALSE)
    ) |>
    # Convert output columns to PEcAn std units/names
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(var_map$rothc_name),
        \(x) convert_to_pecan_units(x, varmap = var_map)
      )
    ) |>
    dplyr::rename(dplyr::all_of(var_rename_list))


  # Loop over years of output writing netCDFs
  # TODO this is consistent with PEcAn practice for other models,
  # but feels a bit silly with monthly output.
  # Consider writing as one multi-year file instead?
  for (yrdat in split(res, ~Year)) {
    yr <- yrdat$Year[[1]]
    # time_bounds <- array(data = NA, dim = c(nrow(yrdat), 2))
    # time_bounds[, 1] <- yrdat$month_start_date |>
    #   difftime(start_date, units = "days")
    # time_bounds[, 2] <- yrdat$month_end_date |>
    #   difftime(start_date, units = "days")
    nc_dims <- list(
      lon = PEcAn.utils::to_ncdim("lon", sitelon),
      lat = PEcAn.utils::to_ncdim("lat", sitelat),
      time = ncdf4::ncdim_def(
        name = "time",
        longname = "time",
        units = paste0("days since ", start_date, " 00:00:00"),
        vals = as.integer(yrdat$time),
        calendar = "standard",
        unlim = TRUE
      )
      # time_bounds = PEcAn.utils::to_ncdim("time_bounds", time_bounds) # TODO should this be "time_bnds"?
    )
    nc_vars <- lapply(var_map$pecan_name, PEcAn.utils::to_ncvar, nc_dims) |>
      stats::setNames(var_map$pecan_name)
    # nc_var$time_bounds <- ??? #TODO

    nc <- ncdf4::nc_create(
      file.path(outdir, paste0(yr, ".nc")),
      nc_vars
    )
    # ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec=NA) # TODO need this line?
    for (vn in var_map$pecan_name) {
      ncdf4::ncvar_put(nc, nc_vars[[vn]], yrdat[[vn]])
    }
    ncdf4::nc_close(nc)
  }
}



# Helper to change column units by lookup in var map
# NB `cur_column()` only works inside `dplyr::across()` --
# it returns the column name `x` has in the enclosing dataframe.
convert_to_pecan_units <- function(x, varmap) {
  var_row <- varmap[varmap$rothc_name == dplyr::cur_column(), ]
  stopifnot(nrow(var_row) == 1)

  PEcAn.utils::ud_convert(x, var_row$rothc_unit, var_row$pecan_unit)
}
