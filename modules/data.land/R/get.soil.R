##' Get Soil
##'
##' @title get.soil 
##' @param lat 
##' @param lon 
##' @param soil.nc netCDFe file with soil data 
##' @return usda soil class 
##' @export
##' @author David LeBauer
get.soil <- function(lat, lon, soil.nc = soil.nc){
    ## Lat and Lon
    Lat <- ncvar_get(soil.nc, "latitude")
    Lon <- ncvar_get(soil.nc, "longitude")

    lati <- which.min(abs(Lat - lat))
    loni <- which.min(abs(Lon - lon))

    ## topsoil
    usda_class <- ncvar_get(soil.nc, "t_usda_tex",
                            start = c(loni, lati),
                            count = c(1,1))
    ref_depth <- ud.convert(ncvar_get(soil.nc, "ref_depth",
                           start = c(loni, lati),
                           count = c(1, 1)), "cm", "m")
    return(list(usda_class = usda_class, ref_depth = ref_depth))
}
