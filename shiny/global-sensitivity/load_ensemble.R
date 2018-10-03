#' @param workflow_dir PEcAn workflow directory
#' @param settings PEcAn settings list
#' @param variable Variable names to read, as a character vector
#' @param quiet If TRUE, don't show status messages from `read.ensemble.output`
load_ensemble <- function(workflow_dir, settings, variable){
    library(PEcAn.all)

    # Load the model output
    ## ANS -- NOTE: There may be a faster/better way to do this using built-in PEcAn functions
    ## ANS -- or...should these be automatically stored somewhere?
    
    message("Workflow dir: ", workflow_dir)
    message("Model outdir: ", settings$modeloutdir)
    message("Start year: ", settings$ensemble$start.year)
    message("End year: ", settings$ensemble$end.year)
    message("Read ensemble output...")
    
    # Read samples file
    samples.file <- file.path(workflow_dir, "samples.Rdata")
    if (file.exists(samples.file)) {
      load(samples.file)
      ens.run.ids <- runs.samples$ensemble
    } else {
      stop(samples.file, "not found required by read.ensemble.output")
    }

    ensemble.output.raw <- list()
    for (row in rownames(ens.run.ids)) {
      run.id <- ens.run.ids[row, "id"]
      PEcAn.logger::logger.info("reading ensemble output from run id: ", run.id)
      ensemble.output.raw[[row]] <- sapply(read.output(run.id, 
                                               file.path(settings$modeloutdir, run.id),
                                               as.numeric(settings$ensemble$start.year),
                                               as.numeric(settings$ensemble$end.year),
                                               variable), mean, na.rm = TRUE)
    }

    message("Rbind ensemble output...")
    ensemble.output <- data.frame(do.call(rbind, ensemble.output.raw))
    
    ## NOTE: read.ensemble.output only returns the mean value at each timestep.
    ## If we want other statistics, they need to be either hard-coded (loop with read.output),
    ## or read.ensemble.output needs to be modified.
    
    # Load parameter values
    message("Load parameter values...")
    load(file.path(workflow_dir, "samples.Rdata"))

    ## "samples.RData" contains the following:
    ##    ensemble.samples -- For each PFT, data.frame of sampled parameter values. Not linked to run IDs, but presumably in same order
    ##    pft.names -- Names of each PFT
    ##    runs.samples -- Run IDs, not paired with anything, but probably in same order as samples
    ##    sa.samples -- Sensitivity analysis samples? Here it's blank
    ##    trait.names -- Names of parameters (traits) sampled; list by PFT.
    ##    trait.samples -- Samples from meta-analysis? 5004 samples per trait.
    
    message("Get run samples...")
    ensemble.output$runid <- runs.samples$ensemble$id

    message('Cbind ensemble samples...')
    ensemble.samples.cbind <- do.call(cbind, ensemble.samples[pft.names])

    message('Cbind ensemble output and samples...')
    ensemble.output.full <- cbind(ensemble.output, ensemble.samples.cbind)
    
    return(ensemble.output.full)
}

