### Process SED files

read.spec <- function(path, output=FALSE, write.path="data/SE_spectra/"){
        rawfile <- readLines(path)
        specstart <- grep("Data:", rawfile) + 2
        header <- rawfile[1:specstart-1]
        rawspec <- rawfile[specstart:length(rawfile)]
        speclist <- strsplit(rawspec, "\\t")
        speclist2 <- lapply(speclist, as.numeric)
        specdat <- do.call(rbind, speclist2)
        out <- data.frame(Wavelength = specdat[,1], Reflectance = specdat[,4])
        fname <- header[grep("File Name:", header)]
        fname <- gsub("^.*NASA_HyspIRI\\\\", "", fname)
        fname <- gsub("[\\]", "__", fname)
        fname <- gsub("sed", "csv", fname)
        fname <- gsub("PSM-3500_1336023", "PSM", fname)
        fname <- gsub("([0-9]{4})_([A-Za-z]+)_([0-9]{2})", "\\1\\2\\3", fname)
        fname <- paste(write.path, "/", fname, sep='')
        if(output){
                return(list(spectra=out, header=header))
        } else {
                write.csv(out, file=fname)
        }
}