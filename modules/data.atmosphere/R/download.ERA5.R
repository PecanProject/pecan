#' Download ERA 5 data
#'
#' Link to [full data documentation](https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation).
#'
#' Under the hood, this function uses the Python `cdsapi` module,
#' which can be installed via `pip` (`pip install --user cdsapi`). The
#' module is accessed via the `reticulate` package.
#'
#' Using the CDS API requires you to create a free account at
#' https://cds.climate.copernicus.eu. Once you have done that, you
#' will need to configure the CDS API on your local machine by
#' creating a `${HOME}/.cdsapi` file, as described
#' [here](https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key).
#'
#' @param outfolder Directory where results should be written
#' @param product_types Character vector of product types, or `"all"`.
#'   Must be one or more of: `"reanalysis"`, `"ensemble members"`,
#'   `"ensemble mean"`, `"ensemble spread"`
#' @param reticulate_python Path to Python binary for `reticulate`
#'   (passed to [reticulate::use_python()]). If `NULL` (default), use
#'   the system default.
#' @param start_date,end_date Range of years to retrieve. Format is
#'   `YYYY-MM-DD`.
#' @param lat.in,lon.in Site coordinates, decimal degrees (numeric)
#' @param overwrite Logical. If `FALSE` (default), skip any files with
#'   the same target name (i.e. same variable) that already exist in
#'   `outfolder`. If `TRUE`, silently overwrite existing files.
#' @param ... Currently unused. Allows soaking up additional arguments
#'   to other methods.
#' @return Character vector of file names containing raw, downloaded
#'   data (invisibly)
#' @author Alexey Shiklomanov
#' @md 
    # ^ tells Roxygen to interpret this fn's doc block as Markdown
#' @export
#' @examples
#' \dontrun{
#' files <- download.ERA5(
#'   "ERA5_output",
#'   start_date = "2010-01-01",
#'   end_date = "2010-02-01",
#'   lat.in = 45.5594,
#'   lon.in = -84.6738,
#'   product_types = "all"
#' )
#' }
download.ERA5.old <- function(outfolder, start_date, end_date, lat.in, lon.in,
                          product_types = "all",
                          overwrite = FALSE,
                          reticulate_python = NULL,
                          ...) {
  PEcAn.logger::logger.warn(
    "This function is an incomplete prototype! Use with caution!"
  )

  PEcAn.utils::need_packages("reticulate")
  
  if (!is.null(reticulate_python)) {
    reticulate::use_python(reticulate_python)
  }

  tryCatch({
    cdsapi <- reticulate::import("cdsapi")
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "Failed to load `cdsapi` Python library. ",
      "Please make sure it is installed to a location accessible to `reticulate`.",
      "You should be able to install it with the following command: ",
      "`pip install --user cdsapi`.",
      "The following error was thrown by `reticulate::import(\"cdsapi\")`: ",
      conditionMessage(e)
    )
  })
  
  
  if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")))
    PEcAn.logger::logger.severe(
      "Please create a `${HOME}/.cdsapirc` file as described here:",
      "https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key ."
    )
  

  tryCatch({
    cclient <- cdsapi$Client()
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "The following error was thrown by `cdsapi$Client()`: ",
      conditionMessage(e)
    )
  })

  all_products <- c("reanalysis", "ensemble_members",
                    "ensemble mean", "ensemble_spread")
  
  if (product_types == "all") {
    product_types <- all_products
  }
  
  if (any(!product_types %in% all_products)) {
    bad_products <- setdiff(product_types, all_products)
    PEcAn.logger::logger.severe(sprintf(
      "Invalid product types %s. Products must be one of the following: %s",
      paste0("`", bad_products, "`", collapse = ", "),
      paste0("`", all_products, "`", collapse = ", ")
    ))
  }

  # Full data documentation:
  # https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation
  variables <- tibble::tribble(
    ~cf_name, ~units, ~api_name, ~ncdf_name,
    "air_temperature", "Kelvin", "2m_temperature", "t2m",
    "air_pressure", "Pa", "surface_pressure", NA_character_,
    NA_character_, "Kelvin", "2m_dewpoint_temperature", NA_character_,
    "precipitation_flux", "kg/m2/s", "total_precipitation", NA_character_,
    "eastward_wind", "m/s", "10m_u_component_of_wind", NA_character_,
    "northward_wind", "m/s", "10m_v_component_of_wind", NA_character_,
    "surface_downwelling_shortwave_flux_in_air", "W/m2", "surface_solar_radiation_downwards", NA_character_,
    "surface_downwelling_longwave_flux_in_air", "W/m2", "surface_thermal_radiation_downwards", NA_character_
  )
  nvar <- nrow(variables)

  # Spatial subset must be a bounding box (N, W, S, E). This sets the
  # bounding box to a single point -- the closest coordinate at the
  # 0.25 x 0.25 resolution of the product.
  area <- rep(round(c(lat.in, lon.in) * 4) / 4, 2)

  files <- character()
  dir.create(outfolder, showWarnings = FALSE)

  
  # First, download all the files
  for (i in seq_len(nvar)) {
    var <- variables[["api_name"]][[i]]
    PEcAn.logger::logger.debug(glue::glue(
      "Downloading variable {i} of {nvar} ({var})."
    ))
    fname <- file.path(outfolder, paste("era5", var, "nc", sep = "."))
    if (file.exists(fname) && !overwrite) {
      PEcAn.logger::logger.warn(glue::glue(
        "File `{fname}` already exists, and `overwrite` is FALSE. ",
        "Skipping to next variable."
      ))
      next
    }
    do_next <- tryCatch({
      cclient$retrieve(
        "reanalysis-era5-single-levels",
        list(
          variable = var,
          product_type = 'ensemble_members',
          date = paste(start_date, end_date, sep = "/"),
          time = "00/to/23/by/1",
          area = area,
          grid = c(0.25, 0.25),
          format = "netcdf"
        ),
        fname
      )
      FALSE
    }, error = function(e) {
      PEcAn.logger::logger.warn(
        glue::glue(
          "Failed to download variable `{var}`. ",
          "Skipping to next variable. ",
          "Error message was:\n",
          conditionMessage(e)
        )
      )
      TRUE
    })
    
    if (isTRUE(do_next)) next
    files <- c(files, fname)
  }

  # TODO: Return `data.frame`, like the other methods.
  invisible(files)

  ## # Then, post-process each file
  ## for (i in seq_len(files)) {
  ##   nc <- ncdf4::nc_open(files[i])
  ##   shortname <- names(nc[["var"]])
  ##   var_sub <- variables[variables[["ncdf_name"]] == shortname, ]
  ##   ncvar <- ncdf4::ncvar_def(var_sub[["cf_name"]], dim = ...)
  ##   # TODO: Check units, and perform conversions where necessary
  ##   # TODO: Convert to PEcAn standard format
  ## }

  # TODO: Figure out how do download ensembles.

  # NOTE: Dew point temperature has to be converted to specific
  # humidity. Can do something like the following:
  # dewpoint <- ncdf4::ncvar_get(nc, "d2m") # Check units!
  # airtemp <- ncdf4::ncvar_get(nc, "t2m")
  # pressure <- ncdf4::ncvar_get(nc, "")  # Make sure this is in Pa
  # rh <- get.rh(airtemp, dewpoint) / 100 # Check units!
  # qair <- rh2qair(rh, airtemp, pressure) # Pressure in Pa
}
