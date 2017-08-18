##' Generate ensemble filenames
##' 
##' @name ensemble.filename
##' @title Generate ensemble filenames
##' 
##' @return a filename
##' @export
##'
##' @details Generally uses values in settings, but can be overwritten for manual uses
##' @author Ryan Kelly
ensemble.filename <- function(settings, prefix = "ensemble.samples", suffix = "Rdata", 
                              all.var.yr = TRUE, ensemble.id = settings$ensemble$ensemble.id, 
                              variable = settings$ensemble$variable, 
                              start.year = settings$ensemble$start.year, 
                              end.year = settings$ensemble$end.year) {
  
  if (is.null(ensemble.id) || is.na(ensemble.id)) {
    # This shouldn't generally arise, as run.write.configs() appends ensemble.id to
    # settings. However,it will come up if running run.write.configs(..., write=F),
    # because then no ensemble ID is created in the database. A simple workflow will
    # still work in that case, but provenance will be lost if multiple ensembles are
    # run.
    ensemble.id <- "NOENSEMBLEID"
  }
  
  ensemble.dir <- settings$outdir
  
  dir.create(ensemble.dir, showWarnings = FALSE, recursive = TRUE)
  
  if (all.var.yr) {
    # All variables and years will be included; omit those from filename
    ensemble.file <- file.path(ensemble.dir, paste(prefix, ensemble.id, suffix, sep = "."))
  } else {
    ensemble.file <- file.path(ensemble.dir, paste(prefix, ensemble.id, variable, 
                                                   start.year, end.year, suffix, sep = "."))
  }
  
  return(ensemble.file)
} # ensemble.filename


##' Generate sensitivity analysis filenames
##' 
##' @name sensitivity.filename
##' @title Generate sensitivity analysis filenames
##' 
##' @return a filename
##' @export
##'
##' @details  Generally uses values in settings, but can be overwritten for manual uses
##' @author Ryan Kelly
sensitivity.filename <- function(settings, 
                              prefix = "sensitivity.samples", suffix = "Rdata", 
                              all.var.yr = TRUE,
                              pft        = NULL,
                              ensemble.id = settings$sensitivity.analysis$ensemble.id,
                              variable    = settings$sensitivity.analysis$variable,
                              start.year  = settings$sensitivity.analysis$start.year,
                              end.year    = settings$sensitivity.analysis$end.year) {
  
  if(is.null(ensemble.id) || is.na(ensemble.id)) {
    # This shouldn't generally arise, as run.write.configs() appends ensemble.id to settings. However,it will come up if running run.write.configs(..., write=F), because then no ensemble ID is created in the database. A simple workflow will still work in that case, but provenance will be lost if multiple ensembles are run.
    ensemble.id <- "NOENSEMBLEID"
  }
  ## for other variables, these are just included in the filename so just need to
  ## make sure they don't crash
  if (is.null(variable)) {
    variable <- "NA"
  }
  if (is.null(start.year)) {
    start.year <- "NA"
  }
  if (is.null(end.year)) {
    end.year <- "NA"
  }
  
  if (is.null(pft)) {
    # Goes in main output directory.
    sensitivity.dir <- settings$outdir
  } else {
    ind <- which(sapply(settings$pfts, function(x) x$name) == pft)
    if (length(ind) == 0) {
      ## no match
      PEcAn.logger::logger.warn("sensitivity.filename: unmatched PFT = ", pft, " not among ", 
                  sapply(settings$pfts, function(x) x$name))
      sensitivity.dir <- file.path(settings$outdir, "pfts", pft)
    } else {
      if (length(ind) > 1) {
        ## multiple matches
        PEcAn.logger::logger.warn("sensitivity.filename: multiple matchs of PFT = ", pft, 
                    " among ", sapply(settings$pfts, function(x) x$name), " USING")
        ind <- ind[1]
      }
      if (is.null(settings$pfts[[ind]]$outdir) | is.na(settings$pfts[[ind]]$outdir)) {
        ## no outdir
        settings$pfts[[ind]]$outdir <- file.path(settings$outdir, "pfts", pft)
      }
      sensitivity.dir <- settings$pfts[[ind]]$outdir
    }
  }
  
  dir.create(sensitivity.dir, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(sensitivity.dir)) {
    PEcAn.logger::logger.error("sensitivity.filename: could not create directory, please check permissions ", 
                 sensitivity.dir, " will try ", settings$outdir)
    if (dir.exists(settings$outdir)) {
      sensitivity.dir <- settings$outdir
    } else {
      PEcAn.logger::logger.error("sensitivity.filename: no OUTDIR ", settings$outdir)
    }
  }
  
  if (all.var.yr) {
    # All variables and years will be included; omit those from filename
    sensitivity.file <- file.path(sensitivity.dir,
                                  paste(prefix, ensemble.id, suffix, sep = "."))
  } else {
    sensitivity.file <- file.path(sensitivity.dir, 
                                  paste(prefix, ensemble.id, variable, start.year, end.year, suffix, sep = "."))
  }
  
  return(sensitivity.file)
} # sensitivity.filename
