#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##'
##' @name pecan.ma.summary
##' @title Generate summary statitics and diagnostics for PEcAn meta.analysis
##' @param mcmc.object JAGS mcmc output from \code{\link{pecan.ma}}
##' @param pft plant functional type
##' @param outdir output directory
##' @param threshold Gelman-Rubin convergence threshold;
##' default = 1.2 following Bolker 2008 Ecological Models and Data in R
##' @param ggmcmc plot ggmcmc plots (this is slow)? default is FALSE
##' @export
##'
##' @examples
##' \dontrun{
##' summary <- pecan.ma.summary(trait.mcmc,settings$pfts$pft,settings$outdir,settings$meta.analysis$threshold)
##' }
##' @author David LeBauer, Shawn Serbin
pecan.ma.summary <- function(mcmc.object, pft, outdir, threshold = 1.2, ggmcmc = FALSE){
  if(!is.null(settings$meta.analysis$threshold)) {
    threshold = settings$meta.analysis$threshold
  }
  fail = FALSE
  sink(file = file.path(outdir,'meta-analysis.log'), append = TRUE, split = TRUE)
  for (trait in names(mcmc.object)){
    
    ## new diagnostic plots. If preferred we can replace use of base below
    if(ggmcmc){
      if("ggmcmc" %in% rownames(installed.packages())){
        if(is.mcmc.list(mcmc.object[[trait]])){
          require(ggmcmc)
          theme_set(theme_bw())
          ggmcmc(ggs(mcmc.object[[trait]]), file.path(outdir, paste0("gg.ma.summaryplots.", trait, ".pdf")))        
        }
      }      
    }
    
    ## reordering maparms so that beta.o etc not sent to end
    .maparms <- names(mcmc.object[[trait]][1,][1][[1]])
    .parms <- c('beta.o', 'thetaSD', 'trtSD', 'ySD')
    maparms <- .maparms[ c(which(.maparms %in% .parms), which(!.maparms %in% .parms))]

    ## plots for mcmc diagnosis
    pdf(file.path(outdir, paste('ma.summaryplots.', trait, '.pdf', sep = '')))
    
    for (i in maparms) {
      plot(mcmc.object[[trait]][,i], trace = FALSE, density = TRUE,
           main = paste('summary plots of',i ,'for', pft, trait))
      box(lwd=2)
      plot(mcmc.object[[trait]][,i],density = FALSE)
      box(lwd=2)
      autocorr.plot(mcmc.object[[trait]][,i][1], xlim = c(1, 50))
      box(lwd=2)
    }
    xyplot(mcmc.object[[trait]])
    densityplot(mcmc.object[[trait]])
    acfplot(mcmc.object[[trait]])
    dev.off()
 
    ## G-R diagnostics to ensure convergence    
    gd <- gelman.diag(mcmc.object[[trait]])
    mpsrf <- round(gd$mpsrf, digits = 3)
    not.converged <- data.frame()
    if(mpsrf < threshold){
      logger.info(paste ("JAGS model converged for", pft, trait,
                    "\nGD MPSRF = ",mpsrf,"\n", sep=" "))
    } else {
      not.converged <- rbind(not.converged, data.frame(pft = pft, trait = trait, mpsrf = mpsrf))
      logger.info( paste ("JAGS model did not converge for", pft, trait,
                   "\nGD MPSRF = ",mpsrf,"\n", sep=" ") )
      fail = TRUE
    }    
  }
  if(fail){
    logger.warn('JAGS model failed to converge for one or more pft.')
    for (i in 1:nrow(not.converged)){
      with(not.converged[i,],
           logger.info(paste(pft, trait, "MPSRF = ", mpsrf))
      )
    }
  }
  sink()
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.    					
####################################################################################################
