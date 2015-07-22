#' Functions for generating spectral response matrices
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
