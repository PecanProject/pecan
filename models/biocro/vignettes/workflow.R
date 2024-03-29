
## @knitr , echo=FALSE,warning=FALSE
library(PEcAn.all)


## @knitr , echo=FALSE,warning=FALSE
settings <- read.settings(system.file("extdata/pecan.biocro.xml", package = "PEcAn.BIOCRO"))

### limit scope for initial testing
settings$sensitivity.analysis$quantiles <- list(sigma = 0.5)
model <- settings$model$type




## @knitr , echo=FALSE,warning=FALSE,cache=TRUE

# Query the trait database for data and priors
settings$pfts <- get.trait.data(settings$pfts, settings$model$type, settings$database$dbfiles, 
  settings$database$bety, settings$meta.analysis$update)

# Run the PEcAn meta.analysis
run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$meta.analysis$random.effects$on, 
  settings$meta.analysis$threshold, settings$database$dbfiles, settings$database$bety)

## @knitr , echo=FALSE,warning=FALSE,cache=TRUE
run.write.configs(model)  # Calls model specific write.configs e.g. write.config.ed.R
## load met data
PEcAn.workflow::start_model_runs(model)  # Start ecosystem model runs
read.outputs(settings$model$type, settings)
# read.outputs(model, settings) #, variables = 'StemBiom')

get.results(settings)  # Get results of model runs

# run.sensitivity.analysis() # Run sensitivity analysis and variance
# decomposition on model output

# run.ensemble.analysis() # Run ensemble analysis on model output.  OPTIONAL:
# run.ensemble.analysis(plot.timeseries=TRUE) to get an esemble time-series
# output for the target variables set in the PEcAn.xml file

### PEcAn workflow run complete
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#


