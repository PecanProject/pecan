#---------------- Load libraries. -----------------------------------------------------------------#
.libPaths("~/lib/R")
require(PEcAn.all)
#--------------------------------------------------------------------------------------------------#

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("~/inputs/sylvania.SIPNET.xml")
#--------------------------------------------------------------------------------------------------#
model = ifelse("model" %in% names(settings),settings$model$name,"ED2")

plot.data <- read.plot("~/inputs/SamTreeRing/FullPlotData.csv")
inc.data <- read.velmex("~/inputs/SamTreeRing/")

fd <- fuse_plot_treering(plot.data,inc.data)

diametergrow(fd$diameters,fd$increments,fd$survival)

plot2AGB(unit.conv=0.02)

## run ensemble
#get.trait.data()          # Query the trait database for data and priors
#run.meta.analysis()     	# Run the PEcAn meta.analysis
run.write.configs(model)        # Calls model specific write.configs e.g. write.config.ed.R
start.model.runs(model)         # Start ecosystem model runs
#get.model.output(model)         # Get results of model runs

sda.particle(model)


#--------------------------------------------------------------------------------------------------#
