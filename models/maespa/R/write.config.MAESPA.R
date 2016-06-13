# ------------------------------------------------------------------------------- Copyright (c) 2012
# University of Illinois, NCSA.  All rights reserved. This program and the accompanying materials are
# made available under the terms of the University of Illinois/NCSA Open Source License which
# accompanies this distribution, and is available at http://opensource.ncsa.illinois.edu/license.html
# -------------------------------------------------------------------------------

## -------------------------------------------------------------------------------------------------#
##' Writes a config file for Maespa
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.MAESPA
##' @title Write MAESPA configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for MAESPA for given run
##' @export
##' @author Tony Gardella
## -------------------------------------------------------------------------------------------------#
write.config.MAESPA <- function(defaults, trait.values, settings, run.id) {
    
    
    # find out where to write run/ouput
    rundir <- file.path(settings$run$host$rundir, as.character(run.id))
    outdir <- file.path(settings$run$host$outdir, as.character(run.id))
    if (is.null(settings$run$host$qsub) && (settings$run$host$name == "localhost")) {
        rundir <- file.path(settings$rundir, as.character(run.id))
        outdir <- file.path(settings$modeloutdir, as.character(run.id))
    }
    # ----------------------------------------------------------------------- create launch script (which
    # will create symlink)
    if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
        jobsh <- readLines(con = settings$run$jobtemplate, n = -1)
    } else {
        jobsh <- readLines(con = system.file("template.job", package = "PEcAn.MAESPA"), n = -1)
        
    }
    
    # create host specific settings
    hostspecific <- ""
    if (!is.null(settings$model$job.sh)) {
        hostspecific <- paste(hostspecific, sep = "\n", paste(settings$model$job.sh, collapse = "\n"))
    }
    if (!is.null(settings$run$host$job.sh)) {
        hostspecific <- paste(hostspecific, sep = "\n", paste(settings$run$host$job.sh, collapse = "\n"))
    }
    # ------------------------------------------------------------------------------------ Begin writing
    # input files
    
    ## Change Start/end Dates to dy/mo/yr
    
    start_date <- format(strptime(settings$run$start.date, format = "%Y/%m/%d"), "%d/%m/%y")
    end_date <- format(strptime(settings$run$end.date, format = "%Y/%m/%d"), "%d/%m/%y")
    
    ### Load trait values
    param.file.name = paste(rundir, "/", "Maespa_params.", run.id, ".Rdata", sep = "")
    maespa.params = save(trait.values, file = param.file.name)
    load(param.file.name)
    
    params = data.frame(trait.values)
    colnames <- c(names(trait.values[[1]]))
    colnames(params) <- colnames
    
    vcmax <- as.numeric(params["Vcmax"])
    jmax <- as.numeric(params["Jmax"])
    
    
    ## Set default plot as 30x30 meter plot with 100 trees
    xmax <- 30
    ymax <- 30
    notrees <- 100
    stem_density <- (xmax * ymax)/notrees
    
    ### Confile.dat
    confile.path <- system.file("confile.dat", package = "PEcAn.MAESPA")
    confile <- readLines(confile.path)
    confile.run.path <- file.path(settings$rundir, run.id, "confile.dat")
    writeLines(confile, con = confile.run.path)
    
    replacePAR(confile.run.path, "itermax", "model", newval = 100, noquotes = TRUE)
    replacePAR(confile.run.path, "itargets", "treescon", newval = 153, noquotes = TRUE)
    replacePAR(confile.run.path, "startdate", "dates", newval = start_date)
    replacePAR(confile.run.path, "enddate", "dates", newval = end_date)
    
    ### Str.dat USING DEFAULT EXAMPLE VERSION RIGHT NOW AS IS
    strfile.path <- system.file("str.dat", package = "PEcAn.MAESPA")
    strfile <- readLines(strfile.path)
    strfile.run.path <- file.path(settings$rundir, run.id, "str.dat")
    writeLines(strfile, con = strfile.run.path)
    
    ### Phy.dat
    phyfile.path <- system.file("phy.dat", package = "PEcAn.MAESPA")
    phyfile <- readLines(phyfile.path)
    phyfile.run.path <- file.path(settings$rundir, run.id, "phy.dat")
    writeLines(phyfile, con = phyfile.run.path)
    
    replacePAR(phyfile.run.path, "values", "vcmax", newval = vcmax)
    replacePAR(phyfile.run.path, "dates", "vcmax", newval = start_date)
    replacePAR(phyfile.run.path, "values", "jmax", newval = jmax)
    replacePAR(phyfile.run.path, "dates", "jmax", newval = start_date)
    
    ### Trees.dat
    treesfile.path <- system.file("trees.dat", package = "PEcAn.MAESPA")
    treesfile <- readLines(treesfile.path)
    treesfile.run.path <- file.path(settings$rundir, run.id, "trees.dat")
    writeLines(treesfile, con = treesfile.run.path)
    
    replacePAR(treesfile.run.path, "xmax", "plot", newval = xmax, noquotes = TRUE)
    replacePAR(treesfile.run.path, "ymax", "plot", newval = ymax, noquotes = TRUE)
    replacePAR(treesfile.run.path, "notrees", "plot", newval = notrees, noquotes = TRUE)
    
    ## watpar.dat
    watparsfile.path <- system.file("watpars.dat", package = "PEcAn.MAESPA")
    watparsfile <- readLines(watparsfile.path)
    watparsfile.run.path <- file.path(settings$rundir, run.id, "watpars.dat")
    writeLines(watparsfile, con = watparsfile.run.path)
    
    # MET FILE path
    metfile <- settings$run$input$met$path
    
    # -----------------------------------------------------------------------------------------------
    # create job.sh
    jobsh <- gsub("@HOSTSPECIFIC@", hostspecific, jobsh)
    
    jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
    jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
    jobsh <- gsub("@SITE_MET@", metfile, jobsh)
    jobsh <- gsub("@STEM_DENS@", stem_density, jobsh)
    
    jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
    jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
    
    jobsh <- gsub("@OUTDIR@", outdir, jobsh)
    jobsh <- gsub("@RUNDIR@", rundir, jobsh)
    
    
    jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
    
    writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
    Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
    
    
}  ### END SCRIPT 