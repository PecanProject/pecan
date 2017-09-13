#' Save soil texture & parameters in PEcAn standard netCDF CF
#'
#' @param soil.data List of soil variables in standard names & units. Minimum is two of [sand, silt, clay]. Bulk density encouraged.
#' @param out.file 
#'
#' @return none
#' @export
#'
#' @details 
#' 
#' A table of standard names and units can be displayed by running soil.units() without any arguements
#' 
#' soil_params is called internally to estimate additional soil physical parameters from sand/silt/clay & bulk density. Will not overwrite any provided values
#'
#' Need to expand to alternatively take soil_type (texture class) as an input
#' 
#' On output, soil_type named class is converted to a number because netCDF is a pain for storing strings.
#' Conversion back can be done by load(system.file("data/soil_class.RData",package = "PEcAn.data.land")) and then soil.name[soil_n]
#'
#' @examples
soil2netcdf <- function(soil.data,out.file){
  ## convert soil type to parameters via look-up-table / equations
  mysoil <- PEcAn.data.land::soil_params(sand=soil.data$volume_fraction_of_sand_in_soil,
                                         silt=soil.data$volume_fraction_of_silt_in_soil,
                                         clay=soil.data$volume_fraction_of_clay_in_soil,
                                         bulk=soil.data$soil_bulk_density)
  
  ## Merge in new variables
  for(n in seq_along(mysoil)){
    if(!(names(mysoil)[n] %in% names(soil.data))){
      soil.data[[names(mysoil)[n]]] <- mysoil[[n]]
    }
  }
  
  ## convert soil_type to number
  soil.data$soil_type <- soil.data$soil_n
  soil.data$soil_n <- NULL
  
  ## open new file
  prefix <- tools::file_path_sans_ext(basename(in.file))
  new.file <- file.path(outdir,paste0(prefix,".nc"))
  
  ## create netCDF variables
  ncvar <- list()
  for(n in seq_along(soil.data)){
    varname <- names(soil.data)[n]
    if(length(soil.data[[n]])>1){
      ## if vector, save by depth
      ncvar[[n]] <- ncdf4::ncvar_def(name = varname, 
                                     units = soil.units(varname), 
                                     dim = depth)
    }else {
      ## else save as scalar
      ncvar[[n]] <- ncdf4::ncvar_def(name = varname, 
                                     units = soil.units(varname), 
                                     dim=list())
    }
  }
  
  ## create new file
  nc <- ncdf4::nc_create(new.file, vars = ncvar)
  
  ## add data
  for (i in seq_along(ncvar)) {
    ncdf4::ncvar_put(nc, ncvar[[i]], soil.data[[i]]) 
  }
  
  ncdf4::nc_close(nc)
  
}