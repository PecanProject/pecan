#---------------- Load libraries. -----------------------------------------------------------------#
.libPaths("~/lib/R")
require(PEcAn.all)
#--------------------------------------------------------------------------------------------------#

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("~/inputs/sylvania.SIPNET.xml")
#--------------------------------------------------------------------------------------------------#

plot.data <- read.plot("~/inputs/plot/Sylvania_Plot_Sam.csv")
inc.dataV <- read.velmex("~/inputs/plot/velmex/")
inc.dataT <- Read_Tuscon("~/inputs/plot/tuscon/")

fd <- fuse_plot_treering(plot.data,inc.dataV,inc.dataT)

diametergrow(fd$diameters,fd$increments,fd$survival)

plot2AGB(unit.conv=0.02)

## run ensemble
# Query the trait database for data and priors
#settings$pfts <- get.trait.data(settings$pfts, settings$run$dbfiles, settings$database$bety, settings$meta.analysis$update)

# Run the PEcAn meta.analysis
#run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database$bety)

# Calls model specific write.configs e.g. write.config.ed.R
run.write.configs(settings, settings$database$bety$write)

# Start ecosystem model runs
start.model.runs(settings, settings$database$bety$write)

# Get results of model runs
get.results(settings)

sda.particle(settings$model$name)

sda.enkf(settings$model$name)

#--------------------------------------------------------------------------------------------------#
