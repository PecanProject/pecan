#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
run.meta.analysis.pft <- function(pft, iterations, random = TRUE, threshold = 1.2, dbfiles, dbcon) {
    require(coda)
  # check to see if get.trait was executed
  if (!file.exists(file.path(pft$outdir, 'trait.data.Rdata')) || !file.exists(file.path(pft$outdir, 'prior.distns.Rdata'))) {
    logger.severe("Could not find output from get.trait for", pft$name)
    return(NA)
  }

  # check to see if run.meta.analysis can be skipped
  if (file.exists(file.path(pft$outdir, 'trait.mcmc.Rdata')) && 
      file.exists(file.path(pft$outdir, 'post.distns.Rdata')) &&
      settings$meta.analysis$update != TRUE ) {
    logger.info("Assuming get.trait copied results already")
    return(pft)
  }

  # make sure there is a posteriorid
  if (is.null(pft$posteriorid)) {
    logger.severe("Make sure to pass in pft list from get.trait. Missing posteriorid for", pft$name)
    return(NA)
  }

  # get list of existing files so they get ignored saving
  old.files <- list.files(path=pft$outdir)

  logger.info("-------------------------------------------------------------------")
  logger.info(" Running meta.analysis for PFT:", pft$name)
  logger.info("-------------------------------------------------------------------")
  
  ## Load trait data for PFT
  load(file.path(pft$outdir, 'trait.data.Rdata'))
  load(file.path(pft$outdir, 'prior.distns.Rdata'))

  if(length(trait.data) == 0) {
    logger.info("no trait data for PFT", pft$name, "\n so no meta-analysis will be performed")
    return(NA)
  }

  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ## Convert data to format expected by pecan.ma
  jagged.data <- lapply(trait.data, jagify)

  ## Check that data is consistent with prior
  for(trait in names(jagged.data)){
    data.median    <- median(jagged.data[[trait]]$Y)
    prior          <- prior.distns[trait, ]
    p.data         <- p.point.in.prior(point = data.median, prior = prior)
    if(p.data <= 0.9995 & p.data >= 0.0005){
      if (p.data <= 0.975 & p.data >= 0.025) {
        logger.info("OK! ", trait, " data and prior are consistent:")
      } else {
        logger.warn("CHECK THIS: ", trait, " data and prior are inconsistent:")
      }
    } else if (p.data > 0.9995 | p.data < 0.0005) {
      logger.debug("NOT OK! ", trait," data and prior are probably not the same:")
      return(NA)
    }
    logger.info(trait, "P[X<x] =", p.data)
  }
  
  ## Average trait data
  trait.average <- sapply(jagged.data,
                          function(x){mean(x$Y, na.rm = TRUE)})
  
  ## Set gamma distribution prior
  prior.variances = as.data.frame(rep(1, nrow(prior.distns)))
  row.names(prior.variances) = row.names(prior.distns)
  prior.variances[names(trait.average), ] = 0.001 * trait.average^2 
  prior.variances["seedling_mortality", 1] = 1.0
  taupriors <- list(tauA = 0.01,
                    tauB = apply(prior.variances, 1, function(x) min(0.01, x)))
  
  ### Run the meta-analysis
  trait.mcmc  <- pecan.ma(jagged.data, prior.distns, taupriors, j.iter = iterations, 
                          outdir = pft$outdir, random = random)
  ### Check that meta-analysis posteriors are consistent with priors
  for(trait in names(trait.mcmc)){
    post.median    <- median(as.matrix(trait.mcmc[[trait]][,'beta.o']))
    prior          <- prior.distns[trait, ]
    p.ma.post      <- p.point.in.prior(point = post.median, prior = prior)
    ## if inside 95%CI, ok.
    if(p.ma.post <= 0.9995 & p.ma.post >= 0.0005){
      if (p.ma.post <= 0.975 & p.ma.post >= 0.025) {
        logger.info("OK! ", trait, " posterior and prior are consistent:")
      } else {
        logger.warn("CHECK THIS: ", trait, " posterior and prior are inconsistent:")
      }
    } else if (p.ma.post > 0.9995 | p.ma.post < 0.0005) {
      logger.severe("NOT OK! ", trait," posterior and prior are probably not the same:")
      return(NA)
    }
    logger.info(trait, "P[X<x] =", p.ma.post)
  }
   
  ### Generate summaries and diagnostics
  pecan.ma.summary(trait.mcmc, pft$name, pft$outdir, threshold)
  
  ### Save the meta.analysis output
  save(trait.mcmc, file = file.path(pft$outdir, 'trait.mcmc.Rdata'))
  
  post.distns <- approx.posterior(trait.mcmc, prior.distns, jagged.data, pft$outdir)
  save(post.distns, file = file.path(pft$outdir, 'post.distns.MA.Rdata'))
  
  # Symlink to post.distns.Rdata (no "MA" identifier)
  if(file.exists(file.path(pft$outdir, 'post.distns.Rdata'))) {
    file.remove(file.path(pft$outdir, 'post.distns.Rdata'))
  }
  file.symlink(file.path(pft$outdir, 'post.distns.MA.Rdata'), file.path(pft$outdir, 'post.distns.Rdata'))

  ### save and store in database all results except those that were there already
  for(file in list.files(path=pft$outdir)) {
    # Skip file if it was there already, or if it's a symlink (like the post.distns.Rdata link above)
    if (file %in% old.files || nchar(Sys.readlink(file.path(pft$outdir, file))) > 0) {
      next
    }
    filename <- file.path(pathname, file)
    file.copy(file.path(pft$outdir, file), filename)
    dbfile.insert(pathname,file, 'Posterior', pft$posteriorid, dbcon)
  }
}

##--------------------------------------------------------------------------------------------------##
##' Run meta analysis
##' 
##' @name run.meta.analysis
##'
##' @title Invoke PEcAn meta.analysis
##' This will use the following items from setings:
##' - settings$pfts
##' - settings$database$bety
##' - settings$run$dbfiles
##' - settings$meta.analysis$update
##' @param pfts the list of pfts to get traits for
##' @param iterations the number of iterations for the mcmc analysis
##' @param random should random effects be used?
##' @param dbfiles location where previous results are found
##' @param database database connection parameters
##' @return nothing, as side effect saves \code{trait.mcmc} created by
##' \code{\link{pecan.ma}} and post.distns created by
##' \code{\link{approx.posterior(trait.mcmc, ...)}}  to trait.mcmc.Rdata \
##' and post.distns.Rdata, respectively
##' @export
##' @author Shawn Serbin, David LeBauer
run.meta.analysis <- function(pfts, iterations, random = TRUE, threshold = 1.2, dbfiles, database) {
  # process all pfts
  dbcon <- db.open(database)
  result <- lapply(pfts, run.meta.analysis.pft, iterations, random, threshold, dbfiles, dbcon)
  db.close(dbcon)

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
##' @export p.point.in.prior
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
