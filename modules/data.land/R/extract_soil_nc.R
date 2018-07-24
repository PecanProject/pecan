#' Extract soil data
#'
#' @param outdir 
#' @param lat 
#' @param lon 
#'
#' @return
#' @export
#'
#' @examples
#' outdir  <- "~/paleon/envTest"
#' lat     <- 40
#' lon     <- -80
#' \dontrun{
#'    PEcAn.data.land::extract_soil_gssurgo(outdir,lat,lon)
#' }
#' @author Hamze Dokoohaki
#' 
extract_soil_gssurgo<-function(outdir,lat,lon){
  #reading the mapunit based on latitude and longitude of the site
  #the output is a gml file which need to be downloaded and read as a spatial file but I don't do that.
  #I just read the file as a text and parse it out and try to find the mukey==mapunitkey
  RCurl::getURL(paste0("https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMWGS84Geographic.wfs?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=MapunitPoly&FILTER=<Filter><DWithin><PropertyName>Geometry</PropertyName><gml:Point>%20<gml:coordinates>",
                lon,",",lat,"</gml:coordinates></gml:Point><Distance%20units=%27m%27>0</Distance>%20</DWithin></Filter>"), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)->xmll
  regexpr('<ms:mukey>',xmll)->startp
  regexpr('</ms:mukey>',xmll)->stopp
  #if you found the mapunit key
  if (startp!=-1 & stopp!=-1)
    gSSURGO.Query(substr(xmll,startp%>%as.numeric+10,stopp%>%as.numeric-1))->soilprop
    
    #Filter based on the most abundant component in that mapunit key 
    soilprop%>%
      filter(comppct==max(soilprop$comppct,na.rm=T),depth>0)%>%
      arrange(depth)%>%
      select(-comppct,-cokey,-mukey,-AWC)->soilprop.new
    # rename it to however pecan likes it
    names(soilprop.new)<-c("fraction_of_gravel_in_soil","soil_cec","fraction_of_sand_in_soil","fraction_of_silt_in_soil",
                           "fraction_of_clay_in_soil","soilC","soil_ph","soil_depth","soil_bulk_density")
    
    #unit colnversion
    soilprop.new$soil_bulk_density<-udunits2::ud.convert(soilprop.new$soil_bulk_density,"g cm-3","kg m-3")
    #converting it to list
    names(soilprop.new)%>%
      purrr::map(function(var){
        soilprop.new[,var]
      })%>%setNames(names(soilprop.new))->soil.data.gssurgo
    
    # calc new filename
    prefix <- "gSSURGO_soil"
    new.file <- file.path(outdir,paste0(prefix,".nc"))
    
    #sending it to the func where some new params will be added and then it will be written down as nc file.
    soil2netcdf(soil.data.gssurgo,new.file)
    
    return(new.file)
}






#' Extract soil data
#'
#' @param in.file 
#' @param outdir 
#' @param lat 
#' @param lon 
#'
#' @return
#' @export
#'
#' @examples
#' in.file <- "~/paleon/env_paleon/soil/paleon_soil.nc"
#' outdir  <- "~/paleon/envTest"
#' lat     <- 40
#' lon     <- -80
#' \dontrun{
#'    PEcAn.data.land::extract_soil_nc(in.file,outdir,lat,lon)
#' }
extract_soil_nc <- function(in.file,outdir,lat,lon){
  
  ## open soils
  nc <- ncdf4::nc_open(in.file)
  
  ## extract lat/lon
  dims <- names(nc$dim)
  lat.dim <- dims[grep("^lat",dims)]
  lon.dim <- dims[grep("^lon",dims)]
  soil.lat <- ncdf4::ncvar_get(nc, lat.dim)
  soil.lon <- ncdf4::ncvar_get(nc, lon.dim)
  
  ## check in range
  dlat <- abs(median(diff(soil.lat)))
  dlon <- abs(median(diff(soil.lon)))
  if(lat < (min(soil.lat)-dlat) | lat > (max(soil.lat)+dlat)){
    PEcAn.logger::logger.error("site lat out of bounds",lat,range(soil.lat))
  }
  if(lon < (min(soil.lon)-dlon) | lon > (max(soil.lon)+dlon)){
    PEcAn.logger::logger.error("site lon out of bounds",lon,range(soil.lon))
  }
  if(dims[1] == lat.dim){
    soil.row <- which.min(abs(lat-soil.lat))
    soil.col <- which.min(abs(lon-soil.lon))
  } else if(dims[1] == lon.dim){
    soil.col <- which.min(abs(lat-soil.lat))
    soil.row <- which.min(abs(lon-soil.lon))
  } else {
    PEcAn.logger::logger.error("could not determine lat/lon dimension order:: ",dims)
  }
  
  ## extract raw soil data
  soil.data <- list()
  soil.vars <- names(nc$var)
  for(i in seq_along(soil.vars)){
    if(length(dims) == 2){
      soil.data[[soil.vars[i]]] <- ncdf4::ncvar_get(nc,soil.vars[i])[soil.row,soil.col]
    } else {
      ## assuming there's a 3rd dim of soil depth profile
      soil.data[[soil.vars[i]]] <- ncdf4::ncvar_get(nc,soil.vars[i])[soil.row,soil.col,]
    }
  }
  ncdf4::nc_close(nc)
  
  ## PalEON / MSTMIP / UNASM hack
  # t_ variables are topsoil layer (0– 30 cm) and
  # s_ variables are subsoil layer (30–100 cm)
  depth <- ncdf4::ncdim_def(name = "depth", units = "meters", vals = c(0.3,1.0), create_dimvar = TRUE)  
  dvars <- soil.vars[grep("t_",soil.vars,fixed=TRUE)]
  for(i in seq_along(dvars)){
    svar <- sub("t_","s_",dvars[i])
    soil.data[[dvars[i]]] <- c(soil.data[[dvars[i]]],soil.data[[svar]]) ## combine different depths
    soil.data[[svar]] <- NULL  ## drop old variable
    names(soil.data)[which(names(soil.data) == dvars[i])] <- sub("t_","",dvars[i]) ## rename original
  }
  
  
  ## name/unit conversions 
  soil.data$sand   <- soil.data$sand/100
  soil.data$silt   <- soil.data$silt/100
  soil.data$clay   <- soil.data$clay/100
  soil.data$oc     <- soil.data$oc/100
  soil.data$gravel <- soil.data$gravel/100
  soil.data$ref_bulk <- udunits2::ud.convert(soil.data$ref_bulk,"g cm-3","kg m-3")
  names(soil.data)[which(names(soil.data) == "clay")] <- "fraction_of_clay_in_soil"
  names(soil.data)[which(names(soil.data) == "sand")] <- "fraction_of_sand_in_soil"
  names(soil.data)[which(names(soil.data) == "silt")] <- "fraction_of_silt_in_soil"
  names(soil.data)[which(names(soil.data) == "gravel")] <- "fraction_of_gravel_in_soil"
  names(soil.data)[which(names(soil.data) == "ref_bulk")] <- "soil_bulk_density"
  names(soil.data)[which(names(soil.data) == "ph")]   <- "soil_ph"
  names(soil.data)[which(names(soil.data) == "cec")]  <- "soil_cec" ## units = meq/100g
  names(soil.data)[which(names(soil.data) == "oc")]   <- "soilC"  ## this is currently the BETY name, would like to change and make units SI
  
  ## calc new filename
  prefix <- tools::file_path_sans_ext(basename(in.file))
  new.file <- file.path(outdir,paste0(prefix,".nc"))
  
  ## Calculate soil parameters and export to netcdf
  soil2netcdf(soil.data,new.file)
    
  return(new.file)
  
}


#' Get standard units for a soil variable
#'
#' @param varname 
#'
#' @return
#' @export
#'
#' @examples
#' soil.units("soil_albedo")
soil.units <- function(varname = NA){
  variables <- as.data.frame(matrix(c("soil_depth","m",
                                      "soil_cec","meq/100g",
                                      "fraction_of_clay_in_soil","1",
                                      "fraction_of_sand_in_soil","1",
                                      "fraction_of_silt_in_soil","1",
                                      "fraction_of_gravel_in_soil","1",
                                      "volume_fraction_of_water_in_soil_at_saturation","m3 m-3",
                                      "volume_fraction_of_water_in_soil_at_field_capacity","m3 m-3",
                                      "volume_fraction_of_condensed_water_in_dry_soil","m3 m-3",
                                      "volume_fraction_of_condensed_water_in_soil_at_wilting_point","m3 m-3",
                                      "soilC","percent",
                                      "soil_ph","1",
                                      "soil_bulk_density","kg m-3",
                                      "soil_type","string",
                                      "soil_hydraulic_b","1",
                                      "soil_water_potential_at_saturation","m",
                                      "soil_hydraulic_conductivity_at_saturation","m s-1",
                                      "thcond0","W m-1 K-1",
                                      "thcond1","W m-1 K-1",
                                      "thcond2","1",
                                      "thcond3","1",
                                      "soil_thermal_conductivity","W m-1 K-1", 
                                      "soil_thermal_conductivity_at_saturation","W m-1 K-1", 
                                      "soil_thermal_capacity","J kg-1 K-1",
                                      "soil_albedo","1"
                                      ),
                                    ncol=2,byrow = TRUE))
  colnames(variables) <- c('var','unit')
  
  unit = which(variables$var == varname)
  
  if(length(unit) == 0){
    if(is.na(varname)){
      return(variables)
    } else {
      return(NA)
    }
  }else{
    unit = as.character(variables$unit[unit])
    return(unit)
  }
  
}
