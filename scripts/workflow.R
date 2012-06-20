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

run.write.configs("ED2")        # calls model specific write.configs e.g. write.config.ed.R

clear.scratch(settings)         # clear any old model output in scratch/$USER on worker nodes

start.model.runs("ED2")         # start ecosystem model runs

#start.runs()
#get.output()
#analyze.ensemble()
# run.sensitivity.analysis()
#transform.trait.data()
#variance.decomposition()
#data.assimilation()

#--------------------------------------------------------------------------------------------------#
