#' Sensor spectral response functions

sensor.list <- c("identity", "aviris.ng", "aviris.classic",
                 "hyperion", "chris.proba", "landsat5", "landsat7",
                 "landsat8", "modis", "viirs", "avhrr")

sensor.proper <- c("ASD Field Spec", "AVIRIS NG", "AVIRIS Classic",
                   "Hyperion", "CHRIS-Proba", "Landsat 5", "Landsat 7",
                   "Landsat 8", "MODIS", "VIIRS", "AVHRR")
names(sensor.proper) <- sensor.list

#' @name spectral.response
#' @title Convolution of spectra to sensor RSR
#' @param spec Full (1 nm) spectrum (vector)
#' @param sensor Sensor name (string). See sensor.list
spectral.response <- function(spec, sensor){
    sensor <- tolower(sensor)
    stopifnot(sensor %in% sensor.list)
    if(sensor == "identity") return(spec)
    rsr <- sensor.rsr[[sensor]]
    m <- spec[rsr[,"index"]] * rsr[,-1]
    out.spec <- .colSums(m, nrow(m), ncol(m))
    return(out.spec)
}
