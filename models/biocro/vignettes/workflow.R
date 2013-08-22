
## @knitr , echo=FALSE,warning=FALSE
require(PEcAn.all)


## @knitr , echo=FALSE,warning=FALSE
settings <- read.settings(system.file("extdata/pecan.biocro.xml",
                                      package = "PEcAn.BIOCRO"))

### limit scope for initial testing
settings$sensitivity.analysis$quantiles <- list(sigma = 0.5)
model <- settings$model$name




## @knitr , echo=FALSE,warning=FALSE,cache=TRUE

# Query the trait database for data and priors
settings$pfts <- get.trait.data(settings$pfts, settings$run$dbfiles, settings$database, settings$meta.analysis$update)

# Run the PEcAn meta.analysis
run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database)

## @knitr , echo=FALSE,warning=FALSE,cache=TRUE
run.write.configs(model)        # Calls model specific write.configs e.g. write.config.ed.R
## load met data
start.model.runs(model)         # Start ecosystem model runs
read.outputs(settings$model$name, settings)
#read.outputs(model, settings)#, variables = "StemBiom")

get.model.output(model = model, settings = settings)         # Get results of model runs

#run.sensitivity.analysis()      # Run sensitivity analysis and variance decomposition on model output

#run.ensemble.analysis()		      # Run ensemble analysis on model output. 
                                # OPTIONAL: run.ensemble.analysis(plot.timeseries=TRUE) to get an esemble 
                                # time-series output for the target variables set in the PEcAn.xml file

### PEcAn workflow run complete
print("---------- PEcAn Workflow Complete ----------")
#--------------------------------------------------------------------------------------------------#


