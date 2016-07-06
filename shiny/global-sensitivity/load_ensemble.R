#' @param workflow_dir PEcAn workflow directory
#' @param settings PEcAn settings list
#' @param variable Variable names to read, as a character vector
#' @param quiet If TRUE, don't show status messages from `read.ensemble.output`
load_ensemble <- function(workflow_dir, settings, variable, quiet=TRUE){
    library(PEcAn.all)
    library(ncdf4)
    
    # Load the model output
    ## ANS -- NOTE: There may be a faster/better way to do this using built-in PEcAn functions
    ## ANS -- or...should these be automatically stored somewhere?
    
    if(quiet) sink("/dev/null")
    ensemble.output.raw <- read.ensemble.output(ensemble.size = NULL,
                                                pecandir = workflow_dir,
                                                outdir = settings$modeloutdir,
                                                start.year = as.numeric(settings$ensemble$start.year),
                                                end.year = as.numeric(settings$ensemble$end.year),
                                                variable = variable)
    if(quiet) sink()
    ensemble.output <- data.frame(do.call(rbind, ensemble.output.raw))
    
    ## NOTE: read.ensemble.output only returns the mean value at each timestep.
    ## If we want other statistics, they need to be either hard-coded (loop with read.output),
    ## or read.ensemble.output needs to be modified.
    
    # Load parameter values
    load(file.path(workflow_dir, "samples.Rdata"))
    ## "samples.RData" contains the following:
    ##    ensemble.samples -- For each PFT, data.frame of sampled parameter values. Not linked to run IDs, but presumably in same order
    ##    pft.names -- Names of each PFT
    ##    runs.samples -- Run IDs, not paired with anything, but probably in same order as samples
    ##    sa.samples -- Sensitivity analysis samples? Here it's blank
    ##    trait.names -- Names of parameters (traits) sampled; list by PFT.
    ##    trait.samples -- Samples from meta-analysis? 5004 samples per trait.
    
    ensemble.output$runid <- runs.samples$ensemble$id
    ensemble.samples.cbind <- do.call(cbind, ensemble.samples[pft.names])
    ensemble.output.full <- cbind(ensemble.output, ensemble.samples.cbind)
    
    return(ensemble.output.full)
}

