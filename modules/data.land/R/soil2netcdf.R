#' Save soil texture & parameters in PEcAn standard netCDF CF
#'
#' A table of standard names and units can be displayed by running
#'  soil.units() without any arguements
#'
#' soil_params is called internally to estimate additional soil physical
#' parameters from sand/silt/clay & bulk density. Will not overwrite any
#' provided values
#'
#' Need to expand to alternatively take soil_type (texture class) as an input
#'
#' On output, soil_type named class is converted to a number because netCDF is a
#' pain for storing strings. Conversion back can be done by
#'  load(system.file ("data/soil_class.RData",package = "PEcAn.data.land"))
#'  and then soil.name[soil_n]
#'
#' @param soil.data List of soil variables in standard names & units. Minimum is
#'  soil_depth and two of [sand, silt, clay]. Bulk density encouraged.
#' @param new.file filename (including path) for output
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{ 
#' soil.data <- list(
#'   fraction_of_sand_in_soil = c(0.3,0.4,0.5),
#'   fraction_of_clay_in_soil = c(0.3,0.3,0.3),
#'   soil_depth = c(0.2,0.5,1.0)
#' )
#'
#' soil2netcdf(soil.data,"soil.nc")
#' }
soil2netcdf <- function(soil.data, new.file) {
  if (any(lengths(soil.data) != length(soil.data[[1]]))) {
    PEcAn.logger::logger.warn(
      "Soil2netcdf: input lengths differ.",
      "Will make them equal by recycling shorter variables.",
      "If this is not expected, please  fix inputs and rerun."
    )
  }
  soil.data <- as.data.frame(soil.data)


  ## convert soil type to parameters via look-up-table / equations
  mysoil <- PEcAn.data.land::soil_params(
    sand = soil.data$fraction_of_sand_in_soil,
    silt = soil.data$fraction_of_silt_in_soil,
    clay = soil.data$fraction_of_clay_in_soil,
    bulk = soil.data$soil_bulk_density,
    soil_type = soil.data$soil_type
  )

  ## Merge in new variables
  for (n in seq_along(mysoil)) {
    if (!(names(mysoil)[n] %in% names(soil.data))) {
      soil.data[[names(mysoil)[n]]] <- mysoil[[n]]
    }
  }

  ## convert soil_type to number
  soil.data$soil_type <- soil.data$soil_n
  soil.data$soil_n <- NULL

  ## create depth dimension
  depth <- ncdf4::ncdim_def(name = "depth",
                            units = "meters",
                            vals = soil.data$soil_depth,
                            create_dimvar = TRUE)
  soil.data$soil_depth <- NULL ## deleting so don't ALSO write as a variable

  ## Drop empty/missing data
  good_vars <- sapply(soil.data, \(x) !all(is.na(x)))
  if (sum(good_vars) < 1) {
    PEcAn.logger::logger.error("All variables missing")
    return()
  }

  ## create netCDF variables
  def_var <- function(vn) {
    ncdf4::ncvar_def(name = vn,
                     units = soil.units(vn),
                     dim = depth)
  }
  ncvar <- sapply(names(soil.data), def_var, simplify = FALSE)

  ## create new file
  nc <- ncdf4::nc_create(new.file, vars = ncvar)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  ## add data
  for (vn in names(soil.data)) {
    ncdf4::ncvar_put(nc, ncvar[[vn]], soil.data[[vn]])
  }
}
