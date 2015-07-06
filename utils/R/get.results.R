#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Reads model output and runs sensitivity and ensemble analyses
##'
##' Output is placed in model output directory (settings$modeloutdir).
##' @name get.results
##' @title Generate model output for PEcAn analyses
##' @export
##' @param settings list, read from settings file (xml) using \code{\link{read.settings}}
##' @author David LeBauer, Shawn Serbin, Mike Dietze, Ryan Kelly
get.results <- function(settings, sa.ensemble.id=NULL, ens.ensemble.id=NULL,
                        variable=NULL, start.year=NULL, end.year=NULL) {

if(FALSE) { sa.ensemble.id=NULL; ens.ensemble.id=NULL; variable=NULL; start.year=NULL; end.year=NULL}

  outdir <- settings$outdir
  
  sensitivity.output <- list()   
  if('sensitivity.analysis' %in% names(settings)) {
    ### Load PEcAn sa info
    # Can specify ensemble ids manually. If not, look in settings. If none there, just look in samples.Rdata, which for backwards compatibility still contains the sample info for (the most recent)  sensitivity and ensemble analysis combined.
    if(!is.null(sa.ensemble.id)) {
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata", 
                 ensemble.id=sa.ensemble.id, all.var.yr=TRUE, pft=NULL)
    } else if(!is.null(settings$sensitivity.analysis$ensemble.id)) {
      sa.ensemble.id <- settings$sensitivity.analysis$ensemble.id
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata", 
                 ensemble.id=sa.ensemble.id, all.var.yr=TRUE, pft=NULL)
    } else {
      fname <- file.path(outdir, 'samples.Rdata')
      sa.ensemble.id <- NULL
    }
    if(!file.exists(fname)) logger.severe("No sensitivity analysis samples file found!")
    load(fname)

    # For backwards compatibility, define some variables if not just loaded
    if(!exists("pft.names"))    pft.names <- names(trait.samples)
    if(!exists("trait.names"))  trait.names <- lapply(trait.samples, names)
    if(!exists("sa.run.ids"))   sa.run.ids <- runs.samples$sa
  
    # Set variable and years. Use args first, then settings, then defaults/error
    start.year.sa <- start.year
    if(is.null(start.year.sa)) start.year.sa <- settings$sensitivity.analysis$start.year
    if(is.null(start.year.sa)) start.year.sa <- NA

    end.year.sa <- end.year
    if(is.null(end.year.sa)) end.year.sa <- settings$sensitivity.analysis$end.year
    if(is.null(end.year.sa)) end.year.sa <- NA

    variable.sa <- variable
    if(is.null(variable.sa)) {
      if("variable" %in% names(settings$sensitivity.analysis)){
        variable.sa = settings$sensitivity.analysis[names(settings$sensitivity.analysis) == "variable"]
      }
    }
    if(is.null(variable.sa)) logger.sever("No variables for sensitivity analysis!")
    
    # Only handling one variable at a time for now
    if(length(variable.sa) > 1) {
      variable.sa <- variable.sa[1]
      logger.warn(paste0("Currently performs sensitivity analysis on only one variable at a time. Using first (", variable.sa, ")"))
    }

    for(pft.name in pft.names){
      quantiles <- rownames(sa.samples[[pft.name]])    
      traits <- trait.names[[pft.name]]
      
      sensitivity.output[[pft.name]] <- read.sa.output(traits = traits,
                                                       quantiles = quantiles,
                                                       pecandir = outdir,
                                                       outdir = settings$modeloutdir, 
                                                       pft.name = pft.name,
                                                       start.year = start.year.sa,
                                                       end.year = end.year.sa,
                                                       variable = variable.sa,
                                                       sa.run.ids = sa.run.ids)
    }

    # Save sensitivity output
    fname <- sensitivity.filename(
      settings, "sensitivity.output", "Rdata", all.var.yr=FALSE, pft=NULL,
      ensemble.id=sa.ensemble.id, variable=variable.sa, start.year=start.year.sa, end.year=end.year.sa)
    save(sensitivity.output, file = fname)
    
  }
  
  ensemble.output    <- list()
  if('ensemble' %in% names(settings)) {
    ### Load PEcAn ensemble info
    # Can specify ensemble ids manually. If not, look in settings. If none there, just look in samples.Rdata, which for backwards compatibility still contains the sample info for (the most recent)  sensitivity and ensemble analysis combined.
    if(!is.null(ens.ensemble.id)) {
      fname <- ensemble.filename(settings, "ensemble.samples", "Rdata", 
                 ensemble.id=ens.ensemble.id, all.var.yr=TRUE)
    } else if(!is.null(settings$ensemble$ensemble.id)) {
      ens.ensemble.id <- settings$ensemble$ensemble.id
      fname <- ensemble.filename(settings, "ensemble.samples", "Rdata", 
                 ensemble.id=ens.ensemble.id, all.var.yr=TRUE)
    } else {
      fname <- file.path(outdir, 'samples.Rdata')
    }
    if(!file.exists(fname)) logger.severe("No ensemble samples file found!")
    load(fname)
    
    # For backwards compatibility, define some variables if not just loaded
    if(!exists("pft.names"))    pft.names <- names(trait.samples)
    if(!exists("trait.names"))  trait.names <- lapply(trait.samples, names)
    if(!exists("ens.run.ids"))   ens.run.ids <- runs.samples$ens

    # Set variable and years. Use args first, then settings, then defaults/error
    start.year.ens <- start.year
    if(is.null(start.year.ens)) start.year.ens <- settings$ensemble$start.year
    if(is.null(start.year.ens)) start.year.ens <- NA

    end.year.ens <- end.year
    if(is.null(end.year.ens)) end.year.ens <- settings$ensemble$end.year
    if(is.null(end.year.ens)) end.year.ens <- NA

    variable.ens <- variable
    if(is.null(variable.ens)) {
      if("variable" %in% names(settings$ensemble)){
        var <- which(names(settings$ensemble) == 'variable')
        for(i in 1:length(var)){
          variable.ens[i] = settings$ensemble[[var[i]]]
        }
      }
    }

    if(is.null(variable.ens)) logger.sever("No variables for ensemble analysis!")
  
    # Only handling one variable at a time for now
    if(length(variable.ens) > 1) {
      variable.ens <- variable.ens[1]
      logger.warn(paste0("Currently performs ensemble analysis on only one variable at a time. Using first (", variable.ens, ")"))
    }

    ensemble.output <- read.ensemble.output(settings$ensemble$size,
                                            pecandir    = outdir,
                                            outdir      = settings$modeloutdir, 
                                            start.year  = start.year.ens,
                                            end.year    = end.year.ens,
                                            variable    = variable.ens,
                                            ens.run.ids = ens.run.ids)


    # Save ensemble output
    fname <- ensemble.filename(settings, "ensemble.output", "Rdata", all.var.yr=FALSE,
      ensemble.id=ens.ensemble.id, variable=variable.ens, 
      start.year=start.year.ens, end.year=end.year.ens)
    save(ensemble.output, file = fname)
  }

}
#==================================================================================================#

