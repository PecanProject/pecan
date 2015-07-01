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
##' @author David LeBauer, Shawn Serbin
##'
run.sensitivity.analysis <- function(plot=TRUE){
  if(!exists("settings")){
      logger.severe("no settings file found")
  }
  
  if ('sensitivity.analysis' %in% names(settings)) {
    
    variables = settings$sensitivity.analysis$variable
    
    ### Load parsed model results
    load(file.path(settings$outdir, paste('sensitivity',settings$sensitivity.analysis$ensemble.id,'Rdata', sep='.')))
    load(file.path(settings$outdir, 'samples.Rdata'))

    ### Generate SA output and diagnostic plots
    sensitivity.results <- list()
    for(pft in settings$pfts){
        traits <- names(trait.samples[[pft$name]])
        quantiles.str <- rownames(sa.samples[[pft$name]])
        quantiles.str <- quantiles.str[which(quantiles.str != '50')]
        quantiles <- as.numeric(quantiles.str)/100
        ## ensemble.output <- read.ensemble.output(settings$ensemble$size, settings$outdir, pft.name=pft$name)

        C.units <- grepl('^Celsius$', trait.lookup(traits)$units, ignore.case = TRUE)
        if(any(C.units)){
          for(x in which(C.units)) trait.samples[[pft$name]][[x]] <- trait.samples[[pft$name]][[x]] + 273.15
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
        start.year <- ifelse(is.null(settings$ensemble$start.year), NA, settings$ensemble$start.year)
        end.year   <- ifelse(is.null(settings$ensemble$end.year), NA, settings$ensemble$end.year)
        ftime = ifelse(is.na(start.year),"",
                       ifelse(end.year==start.year,paste0(".",start.year),
                              paste0(".",start.year,"-",end.year)))
        if(plot){

          fname = paste0("sensitivity.analysis.",variables[1],ftime,".pdf")
          
          ### Generate SA diagnostic plots
          sensitivity.plots <- plot.sensitivities(sensitivity.results[[pft$name]]$sensitivity.output,
                                                  linesize = 1,
                                                  dotsize = 3)
          pdf(file.path(pft$outdir, fname), height = 12, width = 9)
          ## arrange plots  http://stackoverflow.com/q/10706753/199217
          ncol <- floor(sqrt(length(sensitivity.plots)))
          print(do.call("grid.arrange", c(sensitivity.plots, ncol=ncol)))
          print(sensitivity.plots) # old method.  depreciated.
          dev.off()
          
          ### Generate VD diagnostic plots
          vd.plots <- plot.variance.decomposition(sensitivity.results[[pft$name]]$variance.decomposition.output)
          #variance.scale = log, variance.prefix='Log')
          fname = paste0("variancedecomposition.",variables[1],ftime,".pdf")
          pdf(file.path(pft$outdir,fname), width = 11, height = 8)
          do.call(grid.arrange, c(vd.plots, ncol = 4))
          dev.off()
        }

    }  ## end if sensitivity analysis

    fname = paste0("sensitivity.results.",variables[1],ftime,".Rdata")
    save(sensitivity.results, file = file.path(settings$outdir,fname))
  }
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################
