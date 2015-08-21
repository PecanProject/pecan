# Generate spectral response functions for different sensors

#' @name rsr.from.fwhm
#' @title Generate relative spectral response (RSR) matrix based on FWHM data
#' @param avg Vector of average band widths, as reported in FWHM data.
#' @param rng Vector of full-width half maximum widths (if func is "fwhm") or 
#' bandwidths (if func is "bell"), as reported in sensor data sheets.
#' @param func String denoting type of transformation function. "fwhm" assumes 
#' a Gaussian distribution and reported Full Width Half Maximum values. "bell" 
#' uses a simplified, quartic bell-shaped function (see
#' www.brockmann-consult.de/beam/chris-box/theory-details.html).
rsr.from.fwhm <- function(avg, rng, func="fwhm") {
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
    nbands <- length(bound.list)
    nspec <- 2101
    rsr <- matrix(0, nspec, nbands+1)
    rownames(rsr) <- 400:2500
    colnames(rsr) <- c("index", sprintf("B%d", 1:nbands))
    rsr[,"index"] <- as.numeric(rownames(rsr)) - 399
    for(b in 1:nbands){
        inds <- bound.list[[b]]
        rsr[inds,b+1] <- coef.list[[b]]
    }
    return(rsr) 
}

# Trim RSR matrix to wavelength range of interest. 
trim.rsr <- function(rsr, wl.min=400, wl.max=2500){
    inds.rsr <- as.logical((rsr[,"Wavelength"] >= wl.min) *
                           (rsr[,"Wavelength"] <= wl.max))
    rsr.sub <- rsr[inds.rsr,]
    rownames(rsr.sub) <- rsr.sub[, "Wavelength"]
    colnames(rsr.sub)[1] <- "index"
    rsr.sub[,"index"] <- rsr.sub[,"index"] - 399
    return(rsr.sub)
}

# Generate RSR matrices for all sensors and return as list
generate.rsr.all <- function(){
    data(raw.sensor.data)
    rsr.aviris.ng <- with(fwhm.aviris.ng, rsr.from.fwhm(Wavelength, fwhm, func="fwhm"))
    rsr.aviris.classic <- with(fwhm.aviris.classic, rsr.from.fwhm(avg, fwhm, func="fwhm"))
    rsr.hyperion <- with(fwhm.hyperion, rsr.from.fwhm(avg, rng, fun="fwhm"))
    rsr.chris.proba <- with(bandwidth.chrisproba, rsr.from.fwhm(Mid, Max-Min, func="bell"))
    rsr.landsat5 <- trim.rsr(rsr.landsat5)
    rsr.landsat7 <- trim.rsr(rsr.landsat7)
    rsr.landsat8 <- trim.rsr(rsr.landsat8)
    rsr.modis <- trim.rsr(rsr.modis)
    rsr.viirs <- trim.rsr(rsr.viirs)
    rsr.avhrr <- trim.rsr(rsr.avhrr)
    rsr.list <- list(aviris.ng = rsr.aviris.ng, 
                     aviris.classic = rsr.aviris.classic,
                     chris.proba = rsr.chris.proba,
                     landsat5 = rsr.landsat5,
                     landsat7 = rsr.landsat7,
                     landsat8 = rsr.landsat8,
                     modis = rsr.modis, 
                     viirs = rsr.viirs, 
                     avhrr = rsr.avhrr)
    return(rsr.list)
}

