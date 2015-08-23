#' Sensor spectral response functions

#' @name spec.function
#' @title Generate spectrum based on spectral response
#' @param spec Spectrum (1 nm, 400:2500 nm) with resolution matching PROSPECT.
#' @param avg Vector of average band widths, as reported in FWHM data.
#' @param rng Vector of full-width half maximum widths (if func is "fwhm") or 
#' bandwidths (if func is "bell"), as reported in sensor data sheets.
#' @param func String denoting type of transformation function. "fwhm" assumes 
#' a Gaussian distribution and reported Full Width Half Maximum values. "bell" 
#' uses a simplified, quartic bell-shaped function (see
#' www.brockmann-consult.de/beam/chris-box/theory-details.html).
spec.function <- function(spec, avg, rng, func="fwhm") {
    if(func == "fwhm"){
        qnorm25 <- 0.6744898    # qnorm(0.25); for converting fwhm to SD
        sigma <- rng / 2 / qnorm25
        qnorm99 <- 2.326348     # qnorm(0.99); threshold for filter
        tfunc <- function(wl, avg, sigma) dnorm(wl, avg, sigma)
        bound.upper <- ceiling(avg + sigma * qnorm99) - 399
        bound.lower <- floor(avg - sigma * qnorm99) - 399
        par2 <- sigma
    } else if(func == "bell"){
        bound.upper <- ceiling(avg + rng) - 399
        bound.lower <- floor(avg - rng) - 399
        par2 <- rng
        tfunc <- function(wl, avg, bandwidth) (1 + abs(2*(wl - avg)/bandwidth)^4)^4
    } else stop("Unrecognized transformation function")
    bound.sub <- as.logical((bound.upper <= 2101) * (bound.lower >= 1))
    avg.sub <- avg[bound.sub] - 399
    par2.sub <- par2[bound.sub]
    bound.list <- mapply(seq, bound.lower[bound.sub], bound.upper[bound.sub])
    coef.list <- mapply(tfunc, bound.list, avg.sub, par2.sub)
    spec.list <- lapply(bound.list, function(x) spec[x])
    out.list <- mapply(function(a,b) sum(a*b), coef.list, spec.list)
    out.spec <- unlist(out.list)
    return(out.spec)
    #pnorm25.75 <- 0.6914625 # pnorm(0.75) - pnorm(0.25); for upscaling output
}

custom.rsr <- function(spec, rsr){
    inds.rsr <- as.logical((rsr[,"Wavelength"] >= 400) *
                           (rsr[,"Wavelength"] <= 2500))
    rsr.sub <- rsr[inds.rsr,]
    inds.spec <- rsr.sub[,"Wavelength"] - 399
    out.spec <- colSums(spec[inds.spec] * rsr.sub[,-1])
    return(out.spec)
}

sensor.list <- c("aviris.ng", "aviris.classic", "hyperion",
                 "hyspiri", "chris.proba", "landsat5", "landsat7", 
                 "landsat8", "modis", "viirs", "avhrr")

spectral.response <- function(spec, sensor){
    sensor <- tolower(sensor)
    stopifnot(sensor %in% sensor.list)
    if (sensor == "aviris.ng"){
        avg <- seq(385, 2505, by=5)
        fwhm <- rep(5.5, length(avg))
        out.spec <- spec.response(spec, avg, fwhm, func="fwhm")
    } else if (sensor == "aviris.classic"){
        data("fwhm.aviris.classic")
        out.spec <- with(fwhm.aviris.classic,
                         spec.response(spec, avg, fwhm, func="fwhm"))
    } else if (sensor == "hyperion"){
        data("fwhm.hyperion")
        out.spec <- with(hyperion.fwhm, 
                         spec.response(spec, avg, fwhm, func="fwhm"))
    } else if (sensor == "hyspiri"){
        #' HyspIRI (10nm, 380 to 2510)
        #' Source: HyspIRI Comprehensive Development Report
        avg <- seq(385, 2505, by=10)
        rng <- rep(10, length(avg))
        out.spec <- spec.response(spec, avg, rng, func="bell")
    } else if (sensor == "chris.proba") {
#' CHRIS PROBA
#'   Info: 63 bands (36 m spatial resolution); 1.3nm at 410, 12nm at 1050
        data("bandwidth.chrisproba")
        out.spec <- with(bandwidth.chrisproba,
                         spec.response(spec, Mid, Max-Min, func="bell"))
    } else if (sensor == "modis") {
#' MODIS (TERRA; detector-averaged)
        data("rsr.modis")
        out.spec <- custom.rsr(spec, rsr.modis)
    } else if (sensor == "landsat5") {
#' Landsat 5 TM
        data("rsr.landsat")
        out.spec <- custom.rsr(spec, rsr.landsat5)
    } else if (sensor == "landsat7") {
#' Landsat 7 ETM+
        data("rsr.landsat")
        out.spec <- custom.rsr(spec, rsr.landsat7)
    } else if (sensor == "landsat8") {
#' Landsat 8 OLI
        data("rsr.landsat")
        out.spec <- custom.rsr(spec, rsr.landsat8)
    } else if (sensor == "avhrr") {
#' AVHRR (306; on NOAA-18)
        data("rsr.avhrr")
        out.spec <- custom.rsr(spec, rsr.avhrr)
    } else if (sensor == "viirs") {
#' VIIRS
        data("rsr.viirs")
        out.spec <- custom.rsr(spec, rsr.viirs)
    }
    return(out.spec)
}
