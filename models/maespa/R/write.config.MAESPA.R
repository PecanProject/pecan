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
##-------------------------------------------------------------------------------------------------#
write.config.MAESPA <- function(defaults, trait.values, settings, run.id) {

  if(!require("Maeswrap")){
    logger.severe("The Maeswrap package is not installed. 
                  Please consult PEcAn documentation for install notes:
                  https://pecanproject.github.io/pecan-documentation/master/pecan-models.html#maespa")
  }
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, as.character(run.id))
  outdir <- file.path(settings$host$outdir, as.character(run.id))
  if (is.null(settings$host$qsub) && (settings$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.MAESPA"), n = -1)
    
  }
  
  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # ------------------------------------------------------------------------------------ Begin
  # writing input Maespa files
  
  ## Change Start/end Dates to dy/mo/yr
  start_date <- format(strptime(settings$run$start.date, format = "%Y/%m/%d"), "%d/%m/%y")
  end_date <- format(strptime(settings$run$end.date, format = "%Y/%m/%d"), "%d/%m/%y")
  
  ### Load trait values
  param.file.name <- paste(rundir, "/", "Maespa_params.", run.id, ".Rdata", sep = "")
  maespa.params <- save(trait.values, file = param.file.name)
  load(param.file.name)
  
  params <- data.frame(trait.values)
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
  confile.path     <- system.file("confile.dat", package = "PEcAn.MAESPA")
  confile          <- readLines(confile.path)
  confile.run.path <- file.path(settings$rundir, run.id, "confile.dat")
  writeLines(confile, con = confile.run.path)
  
  Maeswrap::replacePAR(confile.run.path, "itermax", "model", newval = 100, noquotes = TRUE)
  Maeswrap::replacePAR(confile.run.path, "itargets", "treescon", newval = 153, noquotes = TRUE)
  Maeswrap::replacePAR(confile.run.path, "startdate", "dates", newval = start_date)
  Maeswrap::replacePAR(confile.run.path, "enddate", "dates", newval = end_date)
  
  ### str.dat USING DEFAULT EXAMPLE VERSION RIGHT NOW AS IS
  strfile.path     <- system.file("str.dat", package = "PEcAn.MAESPA")
  strfile          <- readLines(strfile.path)
  strfile.run.path <- file.path(settings$rundir, run.id, "str.dat")
  writeLines(strfile, con = strfile.run.path)
  
  ### phy.dat
  phyfile.path     <- system.file("phy.dat", package = "PEcAn.MAESPA")
  phyfile          <- readLines(phyfile.path)
  phyfile.run.path <- file.path(settings$rundir, run.id, "phy.dat")
  writeLines(phyfile, con = phyfile.run.path)
  
  Maeswrap::replacePAR(phyfile.run.path, "values", "vcmax", newval = vcmax)
  Maeswrap::replacePAR(phyfile.run.path, "dates", "vcmax", newval = start_date)
  Maeswrap::replacePAR(phyfile.run.path, "values", "jmax", newval = jmax)
  Maeswrap::replacePAR(phyfile.run.path, "dates", "jmax", newval = start_date)
  
  ### trees.dat
  treesfile.path <- system.file("trees.dat", package = "PEcAn.MAESPA")
  treesfile <- readLines(treesfile.path)
  treesfile.run.path <- file.path(settings$rundir, run.id, "trees.dat")
  writeLines(treesfile, con = treesfile.run.path)
  
  Maeswrap::replacePAR(treesfile.run.path, "xmax", "plot", newval = xmax, noquotes = TRUE)
  Maeswrap::replacePAR(treesfile.run.path, "ymax", "plot", newval = ymax, noquotes = TRUE)
  Maeswrap::replacePAR(treesfile.run.path, "notrees", "plot", newval = notrees, noquotes = TRUE)
  
  ## watpar.dat
  watparsfile.path <- system.file("watpars.dat", package = "PEcAn.MAESPA")
  watparsfile <- readLines(watparsfile.path)
  watparsfile.run.path <- file.path(settings$rundir, run.id, "watpars.dat")
  writeLines(watparsfile, con = watparsfile.run.path)
  
  # MET FILE
  metdat <- settings$run$input$met$path
  
  #---------------------------------------------------------------------------------------------
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", metdat, jobsh)
  jobsh <- gsub("@STEM_DENS@", stem_density, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
} # write.config.MAESPA
