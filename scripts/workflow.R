#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
#--------------------------------------------------------------------------------------------------#


#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings()
#--------------------------------------------------------------------------------------------------#


#---------------- Run PEcAn workflow. -------------------------------------------------------------#
get.trait.data()        # Query the trait database for data and priors

run.meta.analysis()     # Run the PEcAn meta.analysis

# write.model.config()   # calls model specific write.configs e.g. write.config.ed.R

# run.sensitivity.analysis()
#--------------------------------------------------------------------------------------------------#
