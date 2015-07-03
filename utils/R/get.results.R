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
##' @author David LeBauer, Shawn Serbin, Mike Dietze
get.results <- function(settings, sa.ensemble.id=NULL, ens.ensemble.id=NULL,
                        variable=NULL, start.year=NULL, end.year=NULL) {
  if(FALSE) {
    sa.ensemble.id <- ens.ensemble.id <- variable <- start.year <- end.year <- NULL
  }
  outdir <- settings$outdir
  
  sensitivity.output <- list()   
  if('sensitivity.analysis' %in% names(settings)) {
    ### Load PEcAn sa info
    # Can specify ensemble ids manually. If not, look in settings. If none there, just look in samples.Rdata, which for backwards compatibility still contains the sample info for (the most recent)  sensitivity and ensemble analysis combined.
    if(!is.null(sa.ensemble.id)) {
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata", 
                 ensemble.id=sa.ensemble.id, all.var.yr=TRUE, pft=NULL)
    } else if(!is.null(settings$sensitivity.analysis$ensemble.id)) {
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata", 
                 ensemble.id=settings$sensitivity.analysis$ensemble.id, all.var.yr=TRUE, pft=NULL)
    } else {
      fname <- file.path(outdir, 'samples.Rdata')
    }
    load(fname)

    # For backwards compatibility, define some variables if not just loaded
    if(!exists("pft.names"))    pft.names <- names(trait.samples)
    if(!exists("trait.names"))  trait.names <- lapply(trait.samples, names)
    if(!exists("sa.run.ids"))   sa.run.ids <- runs.samples$sa
  
    # Set variable and years. Use args first, then settings, then defaults
    # *** Note, in the unusual case where sensitivity and analysis settings for these variables are
    # different, ensemble settings won't get used below (because all are being set to non-null here) 
    if(is.null(start.year)) {
      start.year <- ifelse(is.null(settings$sensitivity.analysis$start.year), NA, 
                           settings$sensitivity.analysis$start.year)
    }
    if(is.null(end.year)) {
      end.year   <- ifelse(is.null(settings$sensitivity.analysis$end.year), NA, 
                           settings$sensitivity.analysis$end.year)
    }
    
    if(is.null(variable)) {
      if("variable" %in% names(settings$sensitivity.analysis)){
        variable = settings$sensitivity.analysis[
                   names(settings$sensitivity.analysis) == "variable"]
      }
    }
    
    # Only handling one variable at a time for now
    if(length(variable) > 1) {
      variable <- variable[1]
      logger.warn(paste0("Currently performs sensitivity analysis on only one variable at a time. Using first (", variable, ")"))
    }

    for(pft.name in pft.names){
      quantiles <- rownames(sa.samples[[pft.name]])    
      traits <- trait.names[[pft.name]]
      
      sensitivity.output[[pft.name]] <- read.sa.output(traits = traits,
                                                       quantiles = quantiles,
                                                       pecandir = outdir,
                                                       outdir = settings$modeloutdir, 
                                                       pft.name = pft.name,
                                                       start.year = start.year,
                                                       end.year = end.year,
                                                       variable = variable,
                                                       sa.run.ids = sa.run.ids)
    }

    # Save sensitivity output
    fname <- sensitivity.filename(
      settings, "sensitivity.output", "Rdata", all.var.yr=FALSE, pft=pft.name)
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
      fname <- ensemble.filename(settings, "ensemble.samples", "Rdata", 
                 ensemble.id=settings$ensemble$ensemble.id, all.var.yr=TRUE)
    } else {
      fname <- file.path(outdir, 'samples.Rdata')
    }
    load(fname)
    
    # For backwards compatibility, define some variables if not just loaded
    if(!exists("pft.names"))    pft.names <- names(trait.samples)
    if(!exists("trait.names"))  trait.names <- lapply(trait.samples, names)
    if(!exists("ens.run.ids"))   ens.run.ids <- runs.samples$ens
  
    # Set variable and years. Use args first, then settings, then defaults
    if(is.null(start.year)) {
      start.year <- ifelse(is.null(settings$ensemble$start.year), NA, settings$ensemble$start.year)
    }
    if(is.null(end.year)) {
      end.year   <- ifelse(is.null(settings$sensemble$end.year), NA, settings$ensemble$end.year)
    }
    
    if(is.null(variable)) {
      if("variable" %in% names(settings$ensemble)){
        var <- which(names(settings$ensemble) == 'variable')
        for(i in 1:length(var)){
          variable[i] = settings$ensemble[[var[i]]]
        }
      }
    }

    # Only handling one variable at a time for now
    if(length(variable) > 1) {
      variable <- variable[1]
      logger.warn(paste0("Currently performs ensemble analysis on only one variable at a time. Using first (", variable, ")"))
    }

    ensemble.output <- read.ensemble.output(settings$ensemble$size,
                                            pecandir = outdir,
                                            outdir = settings$modeloutdir, 
                                            start.year=start.year,
                                            end.year=end.year,
                                            variable=variable,
                                            ens.run.ids = ens.run.ids)


    # Save ensemble output
    fname <- ensemble.filename(settings, "ensemble.output", "Rdata", all.var.yr=FALSE)
    save(ensemble.output, file = fname)
  }

}
#==================================================================================================#

