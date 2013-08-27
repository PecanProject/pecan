## See README in tests/ folder for details
require("PEcAn.all")
require(parallel)

#debugonce(run.meta.analysis)
options(warn = 1, keep.source = TRUE, error =
  quote({
    db.print.connections()
    cat("Environment:\n", file=stderr());

    # TODO: setup option for dumping to a file (?)
    # Set `to.file` argument to write this to a file for post-mortem debugging
    dump.frames();  # writes to last.dump

    #
    # Debugging in R
    #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/index.shtml
    #
    # Post-mortem debugging
    #   http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/pmd.shtml
    #
    # Relation functions:
    #   dump.frames
    #   recover
    # >>limitedLabels  (formatting of the dump with source/line numbers)
    #   sys.frame (and associated)
    #   traceback
    #   geterrmessage
    #
    # Output based on the debugger function definition.

    n <- length(last.dump)
    calls <- names(last.dump)
    cat(paste("  ", 1L:n, ": ", calls, sep = ""), sep = "\n", file=stderr())
    cat("\n", file=stderr())

    if (!interactive()) {
      q()
  }
  }))

# check settings
settings <- read.settings('pecan.xml')

# get traits of pfts
settings$pfts <- get.trait.data(settings$pfts, settings$run$dbfiles, settings$database, settings$meta.analysis$update)
saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 'pecan.xml'))

# run meta-analysis
if('meta.analysis' %in% names(settings)) {
    run.meta.analysis(settings$pfts, settings$meta.analysis$iter, settings$run$dbfiles, settings$database)
}

# write configurations
if (!file.exists(file.path(settings$rundir, "runs.txt"))) {
    run.write.configs(settings$model$name, settings$bety$write)
} else {
    logger.info("Already wrote configuraiton files")    
}

# run model
if (!file.exists(file.path(settings$rundir, "runs.txt"))) {
    logger.severe("No ensemble or sensitivity analysis specified in pecan.xml, work is done.")
} else {
    start.model.runs(settings$model$name, settings$bety$write)
}

# get outputs
convert.outputs(settings$model$name, settings)
get.model.output(settings$model$name, settings)

# ensemble analysis
if (!file.exists(file.path(settings$outdir,"ensemble.ts.pdf"))) {
    run.ensemble.analysis(TRUE)    
} else {
    logger.info("Already executed run.ensemble.analysis()")
}

# sensitivity analysis
if (!file.exists(file.path(settings$outdir, "sensitivity.results.Rdata"))) {
    run.sensitivity.analysis()
} else {
    logger.info("Already executed run.sensitivity.analysis()")    
}

# send email if configured
if (!is.null(settings$email)) {
    sendmail(settings$email$from, settings$email$to,
             paste0("Workflow has finished executing at ", date()),
             paste0("You can find the results on ", Sys.info()[['nodename']], " in ", normalizePath(settings$outdir)))
}

db.print.connections()

