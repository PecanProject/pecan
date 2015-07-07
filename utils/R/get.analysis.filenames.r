##' Generate ensemble filenames
##' 
##' @name ensemble.filename
##' @title Generate ensemble filenames
##' 
##' @return a filename
##' @export
##'
##' Generally uses values in settings, but can be overwritten for manual uses
##' @author Ryan Kelly
ensemble.filename <- function(settings, 
                              prefix = "ensemble.samples", suffix = "Rdata", 
                              all.var.yr = TRUE,
                              ensemble.id = settings$ensemble$ensemble.id,
                              variable    = settings$ensemble$variable,
                              start.year  = settings$ensemble$start.year,
                              end.year    = settings$ensemble$end.year) {

  if(is.null(ensemble.id)) ensemble.id <- "NOENSEMBLEID" # Not supposed to happen...
  
  ensemble.dir <- file.path(settings$outdir, "ensemble", ensemble.id)
  
  dir.create(ensemble.dir, showWarnings=FALSE, recursive=TRUE)
  
  if(all.var.yr) {
    # All variables and years will be included; omit those from filename
    ensemble.file <- file.path(ensemble.dir, 
      paste(prefix, suffix, sep='.'))
  } else {
    ensemble.file <- file.path(ensemble.dir, 
      paste(prefix, variable, start.year, end.year, suffix, sep='.'))
  }
  
  return(ensemble.file)
}


##' Generate sensitivity analysis filenames
##' 
##' @name sensitivity analysis.filename
##' @title Generate sensitivity analysis filenames
##' 
##' @return a filename
##' @export
##'
##' Generally uses values in settings, but can be overwritten for manual uses
##' @author Ryan Kelly
sensitivity.filename <- function(settings, 
                              prefix = "sensitivity.samples", suffix = "Rdata", 
                              all.var.yr = TRUE,
                              pft        = NULL,
                              ensemble.id = settings$sensitivity.analysis$ensemble.id,
                              variable    = settings$sensitivity.analysis$variable,
                              start.year  = settings$sensitivity.analysis$start.year,
                              end.year    = settings$sensitivity.analysis$end.year) {

  if(is.null(ensemble.id)) ensemble.id <- "NOENSEMBLEID" # Not supposed to happen...

  if(is.null(pft)) {
    # Goes in main output directory. 
    sensitivity.dir <- file.path(settings$outdir, "sensitivity", ensemble.id)
  } else {
    ind <- which(sapply(settings$pfts, function(x) x$name) == pft)
    sensitivity.dir <- file.path(settings$pfts[[ind]]$outdir, "sensitivity", ensemble.id)
  }
  
  dir.create(sensitivity.dir, showWarnings=FALSE, recursive=TRUE)
  
  if(all.var.yr) {
    # All variables and years will be included; omit those from filename
    sensitivity.file <- file.path(sensitivity.dir, 
      paste(prefix, suffix, sep='.'))
  } else {
    sensitivity.file <- file.path(sensitivity.dir, 
      paste(prefix, variable, start.year, end.year, suffix, sep='.'))
  }
  
  return(sensitivity.file)
}
