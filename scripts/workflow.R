#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
#--------------------------------------------------------------------------------------------------#

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings()
#--------------------------------------------------------------------------------------------------#


#---------------- Run PEcAn workflow. -------------------------------------------------------------#
get.trait.data()        	      # Query the trait database for data and priors

run.meta.analysis()     	      # Run the PEcAn meta.analysis

run.write.configs("ED2")        # Calls model specific write.configs e.g. write.config.ed.R

clear.scratch(settings)         # Clear any old model output in ebi-cluster scratch/$USER on worker nodes

start.model.runs("ED2")         # Start ecosystem model runs

get.model.output("ED2")         # Get results of model runs

#analyze.ensemble()
# run.sensitivity.analysis()
#transform.trait.data()
#variance.decomposition()
#data.assimilation()

#--------------------------------------------------------------------------------------------------#
