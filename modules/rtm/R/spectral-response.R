#' Sensor spectral response functions

#' @name spec.response
#' @title Generate spectrum based on spectral response
#' @param spec Spectrum (1 nm, 400:2500 nm) with resolution matching PROSPECT.
#' @param avg Vector of average band widths, as reported in FWHM data.
#' @param rng Vector of full-width half maximum widths (if func is "fwhm") or 
#' bandwidths (if func is "bell"), as reported in sensor data sheets.
#' @param func String denoting type of transformation function. "fwhm" assumes 
#' a Gaussian distribution and reported Full Width Half Maximum values. "bell" 
#' uses a simplified, quartic bell-shaped function (see
#' www.brockmann-consult.de/beam/chris-box/theory-details.html).
spec.response <- function(spec, avg, rng, func="fwhm") {
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


#' Next generation AVIRIS (~5 nm)
sr.aviris5 <- function(spec) {}

#' First generation AVIRIS (~10 nm)
sr.aviris10 <- function(spec) {}

#' Hyperion
sr.hyperion <- function(spec){
    data("fwhm.hyperion")
    out.spec <- with(hyperion.fwhm, 
                     spec.response(spec, avg, fwhm, func="fwhm"))
    return(out.spec)
}

#' HyspIRI (10nm, 380 to 2510)
#' Source: HyspIRI Comprehensive Development Report
sr.hyspiri <- function(spec) {
    avg <- seq(385, 2505, by=10)
    rng <- rep(10, length(avg))

#' CHRIS Proba

#' MODIS

#' Landsat

#' AVHRR
