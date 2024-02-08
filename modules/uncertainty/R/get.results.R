#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' Reads model output and runs sensitivity and ensemble analyses
##'
##' Output is placed in model output directory (settings$outdir).
##' @export
##' @param settings list, read from settings file (xml) using \code{\link{read.settings}}
##' @param sa.ensemble.id,ens.ensemble.id ensemble IDs for the sensitivity
##'   analysis and ensemble analysis.
##'   If not provided, they are first looked up from `settings`,
##'   then if not found they are not used and the most recent set of results
##'   is read from \code{samples.Rdata} in directory \code{settings$outdir}
##' @param variable variables to retrieve, as vector of names or expressions
##' @param start.year,end.year first and last years to retrieve
##' @author David LeBauer, Shawn Serbin, Mike Dietze, Ryan Kelly
get.results <- function(settings, sa.ensemble.id = NULL, ens.ensemble.id = NULL, 
                        variable = NULL, start.year = NULL, end.year = NULL) {
  
  outdir <- settings$outdir
  
  sensitivity.output <- list()
  if ("sensitivity.analysis" %in% names(settings)) {
    ### Load PEcAn sa info
    # Can specify ensemble ids manually. If not, look in settings. 
    # if no ensemble ids in settings look in samples.Rdata, 
    # which for backwards compatibility still contains the sample info for (the most recent)
    # sensitivity and ensemble analysis combined.
    if (!is.null(sa.ensemble.id)) {
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata", 
                                    ensemble.id = sa.ensemble.id,
                                    all.var.yr = TRUE, 
                                    pft = NULL)
    } else if (!is.null(settings$sensitivity.analysis$ensemble.id)) {
      sa.ensemble.id <- settings$sensitivity.analysis$ensemble.id
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata", 
                                    ensemble.id = sa.ensemble.id,
                                    all.var.yr = TRUE, 
                                    pft = NULL)
    } else {
      fname <- file.path(outdir, "samples.Rdata")
      sa.ensemble.id <- NULL
    }
    
    if (!file.exists(fname)) {
      PEcAn.logger::logger.severe("No sensitivity analysis samples file found!")
    }
    samples <- new.env()
    load(fname, envir = samples)
    
    # For backwards compatibility, define some variables if not just loaded
    if (is.null(samples$pft.names)) {
      samples$pft.names <- names(samples$trait.samples)
    }
    if (is.null(samples$trait.names)) {
      samples$trait.names <- lapply(samples$trait.samples, names)
    }
    if (is.null(samples$sa.run.ids)) {
      samples$sa.run.ids <- samples$runs.samples$sa
    }
    
    # Set variable and years. Use args first, then settings, then defaults/error
    start.year.sa <- start.year
    if (is.null(start.year.sa)) {
      start.year.sa <- settings$sensitivity.analysis$start.year
    }
    if (is.null(start.year.sa)) {
      start.year.sa <- NA
    }
    
    end.year.sa <- end.year
    if (is.null(end.year.sa)) {
      end.year.sa <- settings$sensitivity.analysis$end.year
    }
    if (is.null(end.year.sa)) {
      end.year.sa <- NA
    }
    
    variables.sa <- variable
    if (is.null(variables.sa)) {
      if ("variable" %in% names(settings$sensitivity.analysis)) {
        variables.sa <- settings$sensitivity.analysis[names(settings$sensitivity.analysis) == "variable"]
      } else {
        PEcAn.logger::logger.severe("no variable defined for sensitivity analysis")
      }
    }
    
    # Only handling one variable at a time for now
    if (length(variables.sa) >= 1) {
      for(variable.sa in variables.sa){
        PEcAn.logger::logger.warn(paste0("Currently performing sensitivity analysis on variable ", 
                                         variable.sa, ")"))
        
        # if an expression is provided, convert.expr returns names of the variables accordingly
        # if a derivation is not requested it returns the variable name as is
        variables <- PEcAn.utils::convert.expr(unlist(variable.sa))
        variable.sa <- variables$variable.eqn
        variable.fn <- variables$variable.drv
        
        for(pft.name in samples$pft.names){
          quantiles <- rownames(samples$sa.samples[[pft.name]])
          traits <- samples$trait.names[[pft.name]]
          
          # when there is variable-per pft in the outputs, check for the tag for deciding SA per pft
          per.pft <- ifelse(!is.null(settings$sensitivity.analysis$perpft), 
                            as.logical(settings$sensitivity.analysis$perpft), FALSE)
          sensitivity.output[[pft.name]] <- read.sa.output(
            traits = traits,
            quantiles = quantiles,
            pecandir = outdir,
            outdir = settings$modeloutdir,
            pft.name = pft.name,
            start.year = start.year.sa,
            end.year = end.year.sa,
            variable = variable.sa,
            sa.run.ids = samples$sa.run.ids,
            per.pft = per.pft)
        }
        
        # Save sensitivity output
        
        fname <- sensitivity.filename(settings, "sensitivity.output", "Rdata", 
                                      all.var.yr = FALSE, 
                                      pft = NULL, 
                                      ensemble.id = sa.ensemble.id, 
                                      variable = variable.fn, 
                                      start.year = start.year.sa, 
                                      end.year = end.year.sa)
        save(sensitivity.output, file = fname)
      }
    }
  }
  
  ensemble.output <- list()
  if ("ensemble" %in% names(settings)) {
    ### Load PEcAn ensemble info Can specify ensemble ids manually. If not, look in
    ### settings. If none there, just look in samples.Rdata, which for backwards
    ### compatibility still contains the sample info for (the most recent) sensitivity
    ### and ensemble analysis combined.
    if (!is.null(ens.ensemble.id)) {
      fname <- ensemble.filename(settings, "ensemble.samples", "Rdata", 
                                 ensemble.id = ens.ensemble.id, 
                                 all.var.yr = TRUE)
    } else if (!is.null(settings$ensemble$ensemble.id)) {
      ens.ensemble.id <- settings$ensemble$ensemble.id
      fname <- ensemble.filename(settings, "ensemble.samples", "Rdata",
                                 ensemble.id = ens.ensemble.id, 
                                 all.var.yr = TRUE)
    } else {
      fname <- file.path(outdir, "samples.Rdata")
    }
    if (!file.exists(fname)) {
      PEcAn.logger::logger.severe("No ensemble samples file found!")
    }
    ens <- new.env()
    load(fname, envir = ens)
    
    # For backwards compatibility, define some variables if not just loaded
    if (is.null(ens$pft.names)) {
      ens$pft.names <- names(ens$trait.samples)
    }
    if (is.null(ens$trait.names)) {
      ens$trait.names <- lapply(ens$trait.samples, names)
    }
    if (is.null(ens$ens.run.ids)) {
      ens$ens.run.ids <- ens$runs.samples$ens
    }
    
    # Set variable and years. Use args first, then settings, then defaults/error
    start.year.ens <- start.year
    if (is.null(start.year.ens)) {
      start.year.ens <- settings$ensemble$start.year
    }
    if (is.null(start.year.ens)) {
      start.year.ens <- NA
    }
    
    end.year.ens <- end.year
    if (is.null(end.year.ens)) {
      end.year.ens <- settings$ensemble$end.year
    }
    if (is.null(end.year.ens)) {
      end.year.ens <- NA
    }
    
    variables.ens <- variable
    if (is.null(variables.ens)) {
      if ("variable" %in% names(settings$ensemble)) {
        nc_var <- which(names(settings$ensemble) == "variable")
        for (i in seq_along(nc_var)) {
          variables.ens[i] <- settings$ensemble[[nc_var[i]]]
        }
      }
    }
    
    if (is.null(variables.ens)) 
      PEcAn.logger::logger.severe("No variables for ensemble analysis!")
    
    # Only handling one variable at a time for now
    if (length(variables.ens) >= 1) {
      for(variable.ens in variables.ens){
        PEcAn.logger::logger.warn(paste0("Currently performing ensemble analysis on variable ", 
                                         variable.ens, ")"))
        
        # if an expression is provided, convert.expr returns names of the variables accordingly
        # if a derivation is not requested it returns the variable name as is
        variables <- PEcAn.utils::convert.expr(variable.ens)
        variable.ens <- variables$variable.eqn
        variable.fn <- variables$variable.drv
        
        ensemble.output <- PEcAn.uncertainty::read.ensemble.output(
          settings$ensemble$size,
          pecandir = outdir, 
          outdir = settings$modeloutdir,
          start.year = start.year.ens, 
          end.year = end.year.ens, 
          variable = variable.ens,
          ens.run.ids = ens$ens.run.ids
        )
        
        # Save ensemble output
        fname <- ensemble.filename(settings, "ensemble.output", "Rdata", 
                                   all.var.yr = FALSE, 
                                   ensemble.id = ens.ensemble.id, 
                                   variable = variable.fn, 
                                   start.year = start.year.ens, 
                                   end.year = end.year.ens)
        save(ensemble.output, file = fname)
      }
    }
  }
} # get.results

#' Apply get.results to each of a list of settings
#'
#' @param settings a PEcAn \code{Settings} or \code{MultiSettings} object
#' @seealso get.results
#' @export
runModule.get.results <- function(settings) {
  if (PEcAn.settings::is.MultiSettings(settings)) {
    return(PEcAn.settings::papply(settings, runModule.get.results))
  } else if (PEcAn.settings::is.Settings(settings)) {
    return(get.results(settings))
  } else {
    stop("runModule.get.results only works with Settings or MultiSettings")
  }
} # runModule.get.results
