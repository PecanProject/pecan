##' Generate ensemble filenames
##'
##' Generates a vector of filenames to be used for PEcAn ensemble output files.
##' All paths start from directory `settings$outdir`,
##' which will be created if it does not exist.
##'
##' Typically used by passing only a settings object,
##'   but all values can be overridden for manual use.
##'
##' If only a single variable or a subset of years are needed,
##'   the generated filename will identify these in the form
##    `prefix.ensemble_id.variable.startyear.endyear.suffix`
##' If all vars and years are included, set `all.yr.var` to TRUE
##'   to get a filename of the form `prefix.ensemble_id.suffix`.
##' All elements are recycled vectorwise.
##' @param settings list of PEcAn settings.
##' @param prefix string to appear at the beginning of the filename
##' @param suffix file extension: string to appear at the end of the filename
##' @param all.var.yr logical: does ensemble include all vars and years?
##'   If FALSE, filename will include years and vars
##' @param ensemble.id ensemble ID(s)
##' @param variable variable(s) included in the ensemble.
##' @param start.year,end.year first and last year simulated.
##'
##' @return a vector of filenames, each in the form
##'   `[settings$outdir]/[prefix].[ensemble.ID].[variable].[start.year].[end.year][suffix]`.
##' @export
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
##' @inheritParams ensemble.filename
##' @param pft name of PFT used for analysis. If NULL, assumes all
##'   PFTs in run are used and does not add them to the filename
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
      if (is.null(settings$pfts[[ind]]$outdir) || is.na(settings$pfts[[ind]]$outdir)) {
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
