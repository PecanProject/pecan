#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------##
##' Run meta analysis
##' 
##' @name run.meta.analysis
##'
##' @title Invoke PEcAn meta.analysis
##' @return nothing, as side effect saves \code{trait.mcmc} created by
##' \code{\link{pecan.ma}} and post.distns created by
##' \code{\link{approx.posterior(trait.mcmc, ...)}}  to trait.mcmc.Rdata \
##' and post.distns.Rdata, respectively
##' @export
##' @author Shawn Serbin, David LeBauer
run.meta.analysis <- function() {
  if('meta.analysis' %in% names(settings)) {
    
    ### Get info on how many iterations should be run within the meta.analysis
    ma.iter   = as.numeric(settings$meta.analysis$iter)
    
    ### Warn user if no iterations are specified.  Could also change to throw warning but then use a default number.
    #if(length(ma.iter)==0) stop("**** WARNING: PEcAn meta.analysis requested without specifying the number of iterations****")
    
    ## Warn user if no MA iterations are specified.  Set default to 10e6
    if(length(ma.iter)==0) {
      print("----------------------------------------------------------------------------------------------")
      print("**** WARNING: PEcAn meta.analysis requested without specifying the number of iterations ****")
      print("**** Using default number of 1,00,000.  This will take a very long time. ****")
      print("**** Please set the number of iterations for future PEcAn runs ****")
      ma.iter=10000
      print("----------------------------------------------------------------------------------------------")
    }
    ###
    
    ### Identify PFTs in the input settings.xml file
    num.pfts <- length(settings$pfts)
    pft.names=as.list(rep(NA,num.pfts))
    for (i in 1:num.pfts){
      pft.names[i] <- settings$pfts[i]$pft$name
      
      ### If no PFT(s) are specified insert NULL to warn user 
      if(length(pft.names)==0) pft.names[1]="NULL" 
      ###
      
    } ### End of for loop to extract pft names
    

    print("-------------------------------------------------------------------")
    print("Selected PFT(s) for meta.analysis: ")
    print(pft.names)
    print("-------------------------------------------------------------------")
    
    ### Check that PFT(s) were specified in settings.xml file.  Otherwise stop and report error.
    if(length(settings$pfts) == 0){
      stop("**** WARNING: No PFT(s) specified in the PEcAn XML settings file ****......Please check.")
    }
    
    ## loop over pfts
    dbcon <- db.open(settings$database)
    for(pft in settings$pfts) {
      # create path where to store files
      pathname <- file.path(settings$run$dbfiles, "inputs", pft$name, paste0(gsub("/", "-", settings$run$start.date), "-", gsub("/", "-", settings$run$end.date)))
      dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

      if ((settings$meta.analysis$update == 'AUTO') || !as.logical(settings$meta.analysis$update)) {
        mcmcinfo <- dbfile.check(settings$run$site$id, settings$run$start.date, settings$run$end.date, 'application/x-RData', 'trait.mcmc', dbcon)
        postinfo <- dbfile.check(settings$run$site$id, settings$run$start.date, settings$run$end.date, 'application/x-RData', 'post.distns', dbcon)
        if ((nrow(mcmcinfo)>0) && (nrow(postinfo)>0)) {
          if(nrow(mcmcinfo)>1) {
            mcmcinfo <- mcmcinfo[1,]
          }
          if(nrow(postinfo)>1) {
            postinfo <- postinfo[1,]
          }
          logger.info("Reusing existing trait.mcmc", mcmcinfo[['id']], "and post.distns", postinfo[['id']], "data")
          file.copy(file.path(mcmcinfo[['file_path']], mcmcinfo[['file_name']]), file.path(pft$outdir, 'trait.mcmc.Rdata'))
          file.copy(file.path(postinfo[['file_path']], postinfo[['file_name']]), file.path(pft$outdir, 'post.distns.Rdata'))
          next
        }
      }
      
      print(" ")
      print("-------------------------------------------------------------------")
      print(paste(" Running meta.analysis for PFT: ",pft$name,sep="") )
      print("-------------------------------------------------------------------")
      print(" ")
    
      ## Load trait data for PFT
      load(file.path(pft$outdir, 'trait.data.Rdata'))
      load(file.path(pft$outdir, 'prior.distns.Rdata'))

      if(length(trait.data) == 0){
        logger.info("no trait data for PFT", pft$name, 
                    "\n so no meta-analysis will be performed")
      } else if (length(trait.data) > 0) {
         logger.info("Trait data loaded for PFT: ", pft$name)
        

        ## Convert data to format expected by pecan.ma
        
        jagged.data <- jagify(trait.data)
        ma.data = jagged.data
        for (i in 1:length(jagged.data)){
          ma.data[[i]] <- rename.jags.columns(jagged.data[[i]])
        }
        trait.data <- ma.data
        ## Check that data is consistent with prior
        for(trait in names(trait.data)){
          data.median    <- median(trait.data[[trait]]$Y)
          prior          <- prior.distns[trait, ]
          p.data         <- p.point.in.prior(point = data.median, prior = prior)
          if(p.data <= 0.9995 & p.data >= 0.0005){
            if (p.data <= 0.975 & p.data >= 0.025) {
              message("OK! ", trait, " data and prior are consistent:\n")
            } else {
              warning("CHECK THIS: ", trait, " data and prior are inconsistent:\n")
            }
          } else if (p.data > 0.9995 | p.data < 0.0005) {
            stop("NOT OK! ", trait," data and prior are probably not the same:\n")
          }
          message(trait, " P[X<x] = ", p.data)
        }
        
        ## Average trait data
        trait.average <- sapply(trait.data,
                                function(x){mean(x$Y, na.rm = TRUE)})
        
        ## Set gamma distribution prior
        prior.variances = as.data.frame(rep(1, nrow(prior.distns)))
        row.names(prior.variances) = row.names(prior.distns)
        prior.variances[names(trait.average), ] = 0.001 * trait.average^2 
        prior.variances["seedling_mortality", 1] = 1.0
        taupriors <- list(tauA = 0.01,
                          tauB = apply(prior.variances, 1, function(x) min(0.01, x)))
        
        ### Run the meta-analysis
        trait.mcmc  <- pecan.ma(trait.data, prior.distns, taupriors, j.iter = ma.iter, 
                                settings, outdir = pft$outdir)
        ### Check that meta-analysis posteriors are consistent with priors
        for(trait in names(trait.mcmc)){
          post.median    <- median(as.matrix(trait.mcmc[[trait]][,'beta.o']))
          prior          <- prior.distns[trait, ]
          p.ma.post      <- p.point.in.prior(point = post.median, prior = prior)
          ## if inside 95%CI, ok.
          if(p.ma.post <= 0.9995 & p.ma.post >= 0.0005){
            if (p.ma.post <= 0.975 & p.ma.post >= 0.025) {
              message("OK! ", trait, " posterior and prior are consistent:\n")
            } else {
              warning("CHECK THIS: ", trait, " posterior and prior are inconsistent:\n")
            }
          } else if (p.ma.post > 0.9995 | p.ma.post < 0.0005) {
            stop("NOT OK! ", trait," posterior and prior are probably not the same:\n")
          }
          message(trait, " P[X<x] = ", p.ma.post)
        }
        
        ### Add some space between console info
        print(" ")
        print(" ")
        
        ### Generate summaries and diagnostics
        pecan.ma.summary(trait.mcmc, pft$name, pft$outdir)
        
        ### Save the meta.analysis output
        save(trait.mcmc, file = file.path(pft$outdir, 'trait.mcmc.Rdata'))
        
        post.distns <- approx.posterior(trait.mcmc, prior.distns, trait.data, pft$outdir)
        save(post.distns, file = file.path(pft$outdir, 'post.distns.Rdata'))

        ### save and store in database
        filename <- tempfile(pattern="trait.mcmc.", tmpdir=pathname, fileext=".Rdata")
        save(trait.mcmc, file=filename)
        dbfile.insert(filename, settings$run$site$id, settings$run$start.date, settings$run$end.date, 'application/x-RData', 'trait.mcmc', dbcon)

        filename <- tempfile(pattern="post.distns.", tmpdir=pathname, fileext=".Rdata")
        save(post.distns, file=filename)
        dbfile.insert(filename, settings$run$site$id, settings$run$start.date, settings$run$end.date, 'application/x-RData', 'post.distns', dbcon)

      } ### End meta.analysis for loop
    }
    db.close(dbcon)
  } else {
    
    print('PEcAn settings file does not call for a trait meta-analysis')
    
  } ### End if/else
  
} ### End of function: run.meta.analysis.R
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' compare point to prior distribution
##'
##' used to compare data to prior, meta analysis posterior to prior
##' @title find quantile of point within prior distribution
##' @param point 
##' @param prior list of distn, parama, paramb
##' @return result of p<distn>(point, parama, paramb)
##' @export
##' @author David LeBauer
p.point.in.prior <- function(point, prior){
  prior.median <- do.call(paste('q', prior$distn, sep = ""),
                          list(0.5, prior$parama, prior$paramb))
  p.point <- do.call(paste('p', prior$distn, sep = ""),
                     list(point, prior$parama, prior$paramb))
  return(p.point)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
