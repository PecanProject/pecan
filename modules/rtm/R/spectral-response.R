#' Sensor spectral response functions

sensor.list <- c("identity", "aviris.ng", "aviris.classic",
                 "hyperion", "chris.proba", "landsat5", "landsat7",
                 "landsat8", "modis", "viirs", "avhrr")

sensor.proper <- c("ASD Field Spec", "AVIRIS NG", "AVIRIS Classic",
                   "Hyperion", "CHRIS-Proba", "Landsat 5", "Landsat 7",
                   "Landsat 8", "MODIS", "VIIRS", "AVHRR")
names(sensor.proper) <- sensor.list

spectral.response <- function(spec, sensor){
    sensor <- tolower(sensor)
    stopifnot(sensor %in% sensor.list)
    if(sensor == "identity") return(spec)
    data(sensor.rsr)
    rsr <- sensor.rsr[[sensor]]
    out.spec <- colSums(spec[rsr[,"index"]] * rsr[,-1])
    return(out.spec)
}
