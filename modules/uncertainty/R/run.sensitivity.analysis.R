#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' run sensitivity.analysis
##'
##' @name run.sensitivity.analysis
##' @title run sensitivity.analysis
##' @return nothing, saves \code{sensitivity.results} as sensitivity.results.Rdata,
##' sensitivity plots as sensitivityanalysis.pdf, and variance decomposition 'popsicle plot'
##' as variancedecomposition.pdf a side effect (OPTIONAL)
##'
##' @param plot logical. Option to generate sensitivity analysis and variance
##' decomposition plots (plot=TRUE) or to turn these plots off (plot=FALSE).
##'
##' @export
##' @author David LeBauer, Shawn Serbin, Ryan Kelly
##'
run.sensitivity.analysis <- function(settings,plot=TRUE, ensemble.id=NULL, variable=NULL, start.year=NULL, end.year=NULL, ...){
  if ('sensitivity.analysis' %in% names(settings)) {
    # Set variable and years. Use args first, then settings, then defaults/error
    if(is.null(start.year)) {
      start.year <- settings$sensitivity.analysis$start.year
    }
    if(is.null(end.year)) {
      end.year <- settings$sensitivity.analysis$end.year
    }
    if(is.null(start.year) | is.null(end.year)) {
      PEcAn.logger::logger.severe("No years given for sensitivity analysis!")
    }

    if(is.null(variable)) {
      variable = settings$sensitivity.analysis$variable
    }
    if(is.null(variable)) {
      PEcAn.logger::logger.severe("No variables for ensemble analysis!")
    }

    # Only handling one variable at a time for now
    if(length(variable) > 1) {
      variable <- variable[1]
      PEcAn.logger::logger.warn(paste0("Currently performs ensemble analysis on only one variable at a time. Using first (", variable, ")"))
    }

    ### Load samples
    # Have to load samples.Rdata for the traits. But can overwrite the run ids if a sensitivity analysis ensemble id provided. samples.Rdata always has only the most recent ensembles for both ensemble and sensitivity runs.
    fname <- file.path(settings$outdir, 'samples.Rdata')
    if(!file.exists(fname)) PEcAn.logger::logger.severe("No samples.Rdata file found!")
    load(fname)

    # Can specify ensemble ids manually. If not, look in settings. If none there, will use the most recent, which was loaded with samples.Rdata
    if(!is.null(ensemble.id)) {
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata",
                                    ensemble.id=ensemble.id, all.var.yr=TRUE)
    } else if(!is.null(settings$sensitivity.analysis$ensemble.id)) {
      ensemble.id <- settings$sensitivity.analysis$ensemble.id
      fname <- sensitivity.filename(settings, "sensitivity.samples", "Rdata",
                                    ensemble.id=ensemble.id, all.var.yr=TRUE)
    } else {
      ensemble.id <- NULL
    }
    if(file.exists(fname)) load(fname)

    # For backwards compatibility, define some variables if not just loaded
    if(!exists("pft.names"))    pft.names <- names(trait.samples)
    if(!exists("trait.names"))  trait.names <- lapply(trait.samples, names)
    if(!exists("sa.run.ids"))   sa.run.ids <- runs.samples$sa

    ### Load parsed model results
    variables <- convert.expr(variable)
    variable.fn <- variables$variable.drv

    fname <- sensitivity.filename(
      settings, "sensitivity.output", "Rdata", all.var.yr = FALSE,
      ensemble.id = ensemble.id, variable = variable.fn,
      start.year = start.year, end.year = end.year)
    load(fname)

    ### Generate SA output and diagnostic plots
    sensitivity.results <- list()
    for(pft in settings$pfts){
      traits <- trait.names[[pft$name]]
      quantiles.str <- rownames(sa.samples[[pft$name]])
      quantiles.str <- quantiles.str[which(quantiles.str != '50')]
      quantiles <- as.numeric(quantiles.str)/100

      C.units <- grepl('^Celsius$', trait.lookup(traits)$units, ignore.case = TRUE)
      if(any(C.units)){
        for(x in which(C.units)) {
          trait.samples[[pft$name]][[x]] <- udunits2::ud.convert(trait.samples[[pft$name]][[x]], "degC", "K")
        }
      }

      ## only perform sensitivity analysis on traits where no more than 2 results are missing
      good.saruns <- sapply(sensitivity.output[[pft$name]], function(x) sum(is.na(x)) <=2)
      if(!all(good.saruns)) { # if any bad saruns, reduce list of traits and print warning
        bad.saruns <- !good.saruns
        warning(paste('missing >2 runs for', vecpaste(traits[bad.saruns]),
                      '\n sensitivity analysis or variance decomposition will be performed on these trait(s)',
                      '\n it is likely that the runs did not complete, this should be fixed !!!!!!'))
      }

      ### Gather SA results
      sensitivity.results[[pft$name]] <- sensitivity.analysis(
        trait.samples = trait.samples[[pft$name]][traits],
        sa.samples = sa.samples[[pft$name]][ ,traits, drop=FALSE],
        sa.output = sensitivity.output[[pft$name]][ ,traits, drop=FALSE],
        outdir = pft$outdir)

      ### Send diagnostic output to the console
      print(sensitivity.results[[pft$name]]$variance.decomposition.output)
      print(sensitivity.output[[pft$name]])

      ### Plotting - Optional
      if(plot){
        fname <- sensitivity.filename(
          settings, "sensitivity.analysis", "pdf",
          all.var.yr=FALSE, pft=pft$name, ensemble.id=ensemble.id, variable=variable.fn,
          start.year=start.year, end.year=end.year)

        ### Generate SA diagnostic plots
        sensitivity.plots <- plot_sensitivities(
          sensitivity.results[[pft$name]]$sensitivity.output, linesize = 1, dotsize = 3)

        pdf(fname, height = 12, width = 9)
        ## arrange plots  http://stackoverflow.com/q/10706753/199217
        ncol <- floor(sqrt(length(sensitivity.plots)))
        print(do.call("grid.arrange", c(sensitivity.plots, ncol=ncol)))
        print(sensitivity.plots) # old method.  depreciated.
        dev.off()

        ### Generate VD diagnostic plots
        vd.plots <- plot_variance_decomposition(sensitivity.results[[pft$name]]$variance.decomposition.output)
        #variance.scale = log, variance.prefix='Log')
        fname <- sensitivity.filename(settings, "variance.decomposition", "pdf",
                                      all.var.yr=FALSE, pft=pft$name, ensemble.id=ensemble.id, variable=variable.fn,
                                      start.year=start.year, end.year=end.year)

        pdf(fname, width = 11, height = 8)
        do.call(grid.arrange, c(vd.plots, ncol = 4))
        dev.off()
      }

    }  ## end if sensitivity analysis

    fname <- sensitivity.filename(settings, "sensitivity.results", "Rdata",
                                  all.var.yr=FALSE, pft=NULL, ensemble.id=ensemble.id, variable=variable.fn,
                                  start.year=start.year, end.year=end.year)

    save(sensitivity.results, file = fname)
  }
}
#==================================================================================================#

##' @export
runModule.run.sensitivity.analysis <- function(settings, ...) {
  if(is.MultiSettings(settings)) {
    return(papply(settings, runModule.run.sensitivity.analysis, ...))
  } else if (is.Settings(settings)) {
    run.sensitivity.analysis(settings, ...)
  } else {
    stop("runModule.run.sensitivity.analysis only works with Settings or MultiSettings")
  }
}
