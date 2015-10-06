#' @name rsr.from.fwhm
#' @title Generate relative spectral response (RSR) matrix based on FWHM data
#' @param wavelength Vector of average band widths, as reported in FWHM data.
#' @param fwhm Vector of full-width half maximum (FWHM) bandwidths, as reported 
#' in FWHM data.
rsr.from.fwhm <- function(wavelength, fwhm) {
    sigma <- fwhm / 2 / abs(qnorm(0.25))
    rsr <- t(sapply(400:2500, dnorm, wavelength, sigma))
    rownames(rsr) <- 400:2500
    rsr <- cbind(400:2500-399, rsr)
    colnames(rsr) <- c("index", sprintf("B%s", 1:length(fwhm)))
    colsums <- colSums(rsr)
    lt1 <- which(colsums < 0.99)
    rsr.sub <- rsr[, -lt1]
    return(rsr.sub)
}

#' @name trim.rsr
#' @title Trim RSR matrix to wavelength limits
#' @param rsr RSR matrix
#' @param wl.min Minimum wavelength (inclusive, default = 400)
#' @param wl.max Maximum wavelength (inclusive, default = 2500)
trim.rsr <- function(rsr, wl.min=400, wl.max=2500){
    inds.rsr <- as.logical((rsr[,"Wavelength"] >= wl.min) *
                           (rsr[,"Wavelength"] <= wl.max))
    rsr.sub <- rsr[inds.rsr,]
    rownames(rsr.sub) <- rsr.sub[, "Wavelength"]
    colnames(rsr.sub)[1] <- "index"
    rsr.sub[,"index"] <- rsr.sub[,"index"] - 399
    return(rsr.sub)
}

#' @name generate.rsr.all
#' @title Generate RSR matrices for all sensors and return as list
#' @description Only needs to be called when updating these funcitons with new 
#' data
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
    sensor.rsr <- list(aviris.ng = rsr.aviris.ng, 
                       aviris.classic = rsr.aviris.classic,
                       hyperion = rsr.hyperion,
                       chris.proba = rsr.chris.proba,
                       landsat5 = rsr.landsat5,
                       landsat7 = rsr.landsat7,
                       landsat8 = rsr.landsat8,
                       modis = rsr.modis, 
                       viirs = rsr.viirs, 
                       avhrr = rsr.avhrr)
    return(sensor.rsr)
}

#' @name read.rsr.folder
#' @title Read and process RSR data from directory
#' @param dir.path Directory containing RSR data
#' @param type Type of sensor. Options are: landsat, avhrr
read.rsr.folder <- function(dir.path, type){
    type <- tolower(type)
    flist <- list.files(dir.path)
    nbands <- length(flist)
    bandnames <- gsub("(.*)[.]csv", "\\1", flist)
    band.list <- list()
    for(i in 1:nbands){
        fpath <- file.path(dir.path, flist[i])
        raw.input <- read.csv(fpath)
        if(type == "landsat"){
            out.dat <- raw.input[,1:2]
        } else if (type == "avhrr"){
            raw.input$Wavelength <- raw.input$Wavelength.._m. * 1000
            out.dat <- raw.input[, c("Wavelength", "Relative.SRF")]
        }
        colnames(out.dat) <- c("Wavelength", bandnames[i])
        band.list[[i]] <- out.dat
    }
    band.full <- band.list[[1]]
    for(i in 2:nbands){
        band.full <- merge(band.full, band.list[[i]], by="Wavelength", all=TRUE)
    }
    band.full[is.na(band.full)] <- 0
    return(as.matrix(band.full))
}
