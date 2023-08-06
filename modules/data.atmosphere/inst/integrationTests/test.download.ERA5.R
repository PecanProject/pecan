library(testthat)

tmpdir <- tempfile(pattern = "era5Data")
dir.create(tmpdir)
on.exit(teardown(unlink(tmpdir, recursive = TRUE)))

outfolder <- tmpdir
# outfolder  <- "./era5Data"
start_date <- "2010-01-01"
end_date   <- "2010-02-01"
lat.in     <- 45.5594
lon.in     <- -84.6738

product_types <- "all"
overwrite <- FALSE
reticulate_python <- NULL

PEcAn.utils:::need_packages("reticulate")

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


# check for the existence of the cdsapirc file
if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")))
PEcAn.logger::logger.severe(
  "Please create a `${HOME}/.cdsapirc` file as described here:",
  "https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key ."
)

# initialize the cdsapi client
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

# Check that all product types are valid
if (any(!product_types %in% all_products)) {
  bad_products <- setdiff(product_types, all_products)
  PEcAn.logger::logger.severe(sprintf(
    "Invalid product types %s. Products must be one of the following: %s",
    paste0("`", bad_products, "`", collapse = ", "),
    paste0("`", all_products, "`", collapse = ", ")
  ))
}


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

test_that("Downloaded files exist in the specified directory", {
  for (i in files) {
    expect_true(file.exists(i))
  }
})

