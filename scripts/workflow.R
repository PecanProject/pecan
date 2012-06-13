#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
#--------------------------------------------------------------------------------------------------#


#---------------- Load PEcAn settings file. -------------------------------------------------------#
default.settings <- Sys.getenv(x = c("SETTINGS","USER","HOME"))   # Import default location
args <- commandArgs(trailingOnly = TRUE)                          # Import command argument

if (is.na(args[1])==TRUE){
  pecan.settings.file <- default.settings[1]
  print(paste("---- PEcAn settings file: ",pecan.settings.file, sep = ""))
  print(" ")
} else {
  pecan.settings.file <- args[1]
  print(paste("---- PEcAn settings file: ",pecan.settings.file, sep = ""))
  print(" ")
}

# Open and read in settings file for PEcAn run.
settings = read.settings(pecan.settings.file)
#--------------------------------------------------------------------------------------------------#


#---------------- Run PEcAn workflow. -------------------------------------------------------------#
get.trait.data()        # Query the trait database for data and priors

run.meta.analysis()     # Run the PEcAn meta.analysis

#write.model.config()   # calls model specific write.configs e.g. write.config.ed.R

#run.sa()
#--------------------------------------------------------------------------------------------------#
