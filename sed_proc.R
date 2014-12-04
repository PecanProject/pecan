### Process SED files

read.spec <- function(path, output=FALSE){
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
  if(output){
  return(list(spectra=out, header=header))
  } else {
    write.csv(out, file=fname)
}
}

# testpath1 <- "data/testspec/SJJR/June2013/06092013_SE/SE_Files/SJJR_BLOAK1_CHEML1M_LC_R_00017.sed"
# testpath2 <- "data/testspec/SJJR/June2013/06092013_SE/SE_Files/SJJR_BLOAK1_CHEML1M_LC_R_00018.sed"
# testpath3 <- "data/testspec/SJJR/June2013/06092013_SE/SE_Files/SJJR_BLOAK1_CHEML1M_LC_R_00019.sed"
# 
# z1 <- read.spec(testpath1)
# z2 <- read.spec(testpath2)
# z3 <- read.spec(testpath3)