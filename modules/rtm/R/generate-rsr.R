# Generate spectral response functions for different sensors
#' @name rsr.from.fwhm
#' @title Generate relative spectral response (RSR) matrix based on FWHM data
#' @param wavelength Vector of average band widths, as reported in FWHM data.
#' @param fwhm Vector of full-width half maximum (FWHM) bandwidths, as reported 
#' in FWHM data.
rsr.from.fwhm <- function(avg, rng) {
    sigma <- rng / 2 / abs(qnorm(0.25))
    rsr <- t(sapply(400:2500, dnorm, avg, sigma))
    rownames(rsr) <- 400:2500
    rsr <- cbind(400:2500-399, rsr)
    colnames(rsr) <- c("index", sprintf("B%s", 1:length(avg)))
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
    rsr.aviris.ng <- with(fwhm.aviris.ng, rsr.from.fwhm(Wavelength, fwhm))
    rsr.aviris.classic <- with(fwhm.aviris.classic, rsr.from.fwhm(avg, fwhm))
    rsr.hyperion <- with(fwhm.hyperion, rsr.from.fwhm(avg, rng))
    rsr.chris.proba <- with(bandwidth.chrisproba, rsr.from.fwhm(Mid, Max-Min))
    rsr.landsat5 <- trim.rsr(rsr.landsat5)
    rsr.landsat7 <- trim.rsr(rsr.landsat7)
    rsr.landsat8 <- trim.rsr(rsr.landsat8)
    rsr.modis <- trim.rsr(rsr.modis)
    rsr.viirs <- trim.rsr(rsr.viirs)
    rsr.avhrr <- trim.rsr(rsr.avhrr)
    rsr.list <- list(aviris.ng = rsr.aviris.ng, 
                     aviris.classic = rsr.aviris.classic,
                     hyperion = rsr.hyperion,
                     chris.proba = rsr.chris.proba,
                     landsat5 = rsr.landsat5,
                     landsat7 = rsr.landsat7,
                     landsat8 = rsr.landsat8,
                     modis = rsr.modis, 
                     viirs = rsr.viirs, 
                     avhrr = rsr.avhrr)
    return(rsr.list)
}

