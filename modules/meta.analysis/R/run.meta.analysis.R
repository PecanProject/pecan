#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

run.meta.analysis.pft <- function(pft, iterations, random = TRUE, threshold = 1.2, dbfiles, dbcon, use_ghs = TRUE, update = FALSE) {
  # check to see if get.trait was executed
  if (!file.exists(file.path(pft$outdir, "trait.data.Rdata")) || 
      !file.exists(file.path(pft$outdir, "prior.distns.Rdata"))) {
    PEcAn.logger::logger.severe("Could not find output from get.trait for", pft$name)
    return(NA)
  }
  
  # check to see if run.meta.analysis can be skipped
  if (file.exists(file.path(pft$outdir, "trait.mcmc.Rdata")) && 
      file.exists(file.path(pft$outdir, "post.distns.Rdata")) && 
      update != TRUE) {
    PEcAn.logger::logger.info("Assuming get.trait copied results already")
    return(pft)
  }
  
  # make sure there is a posteriorid
  if (is.null(pft$posteriorid)) {
    PEcAn.logger::logger.severe("Make sure to pass in pft list from get.trait. Missing posteriorid for", pft$name)
    return(NA)
  }
  
  # make sure random and use_ghs is logical, and threshold is numeric
  # when someone re-reads xml and continues from meta.analysis these can cause bugs (especially the threshold bug is very subtle)
  random    <- as.logical(random)
  use_ghs   <- as.logical(use_ghs)
  threshold <- as.numeric(threshold)
  
  # get list of existing files so they get ignored saving
  old.files <- list.files(path = pft$outdir)
  
  PEcAn.logger::logger.info("-------------------------------------------------------------------")
  PEcAn.logger::logger.info(" Running meta.analysis for PFT:", pft$name)
  PEcAn.logger::logger.info("-------------------------------------------------------------------")
  
  ## Load trait data for PFT
  trait_env <- new.env()
  load(file.path(pft$outdir, "trait.data.Rdata"), envir = trait_env)
  prior_env <- new.env()
  load(file.path(pft$outdir, "prior.distns.Rdata"), envir = prior_env)
  
  if (length(trait_env$trait.data) == 0) {
    PEcAn.logger::logger.info("no trait data for PFT", pft$name, "\n so no meta-analysis will be performed")
    return(NA)
  }
  
  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)
  
  ## Convert data to format expected by pecan.ma
  jagged.data <- lapply(trait_env$trait.data, PEcAn.MA::jagify, use_ghs = use_ghs)
  
  ## Save the jagged.data object, replaces previous madata.Rdata object
  ## First 6 columns are equivalent and direct inputs into the meta-analysis
  save(jagged.data, file = file.path(pft$outdir, "jagged.data.Rdata"))
  
  if(!use_ghs){
    # check if any data left after excluding greenhouse
    all_trait_check <- sapply(jagged.data, nrow)
    if(any(all_trait_check == 0)){
      nodat <- which(all_trait_check == 0)
      jagged.data[nodat] <- NULL
      PEcAn.logger::logger.info("No more data left after excluding greenhouse data for the following traits:", paste(names(all_trait_check)[nodat], collapse = ", "))
    }
  }

  check_consistent <- function(data.median, prior, trait, msg_var,
                               perr = 5e-04, pwarn = 0.025) {

    p.data <- p.point.in.prior(point = data.median, prior = prior)

    if (p.data <= 1 - perr & p.data >= perr) {
      if (p.data <= 1 - pwarn & p.data >= pwarn) {
        PEcAn.logger::logger.info("OK! ", trait, " ", msg_var, " and prior are consistent:")
      } else {
        PEcAn.logger::logger.warn("CHECK THIS: ", trait, " ", msg_var, " and prior are inconsistent:")
      }
    } else {
      PEcAn.logger::logger.debug("NOT OK! ", trait, " ", msg_var, " and prior are probably not the same:")
      return(NA)
    }
    PEcAn.logger::logger.info(trait, "P[X<x] =", p.data)
    return(1)
  }

  
  ## Check that data is consistent with prior
  for (trait in names(jagged.data)) {
    data.median <- stats::median(jagged.data[[trait]][ , 'Y'])
    prior       <- prior_env$prior.distns[trait, ]
    check       <- check_consistent(data.median, prior, trait, "data")
    if (is.na(check)) {
      return(NA)
    }
  }
  
  ## Average trait data
  trait.average <- sapply(jagged.data, function(x) mean(x$Y, na.rm = TRUE) )
  
  ## Set gamma distribution prior
  tau_value <- 0.01
  prior.variances <- as.data.frame(rep(1, nrow(prior_env$prior.distns)))
  row.names(prior.variances) <- row.names(prior_env$prior.distns)
  prior.variances[names(trait.average), ] <- 0.001 * trait.average ^ 2
  prior.variances["seedling_mortality", 1] <- 1
  taupriors <- list(tauA = tau_value, tauB = apply(prior.variances, 1, function(x) min(tau_value, x)))
  
  ### Run the meta-analysis
  trait.mcmc <- pecan.ma(jagged.data,
                         prior_env$prior.distns,
                         taupriors, 
                         j.iter = iterations, 
                         outdir = pft$outdir, 
                         random = random)
  
  ### Check that meta-analysis posteriors are consistent with priors
  for (trait in names(trait.mcmc)) {
    post.median <- stats::median(as.matrix(trait.mcmc[[trait]][, "beta.o"]))
    prior       <- prior_env$prior.distns[trait, ]
    check <- check_consistent(post.median, prior, trait, "data")
    if (is.na(check)) {
      return(NA)
    }
  }
  
  ### Generate summaries and diagnostics, discard samples if trait failed to converge
  trait.mcmc <- pecan.ma.summary(trait.mcmc, pft$name, pft$outdir, threshold)
  
  ### Save the meta.analysis output
  save(trait.mcmc, file = file.path(pft$outdir, "trait.mcmc.Rdata"))
  
  post.distns <- approx.posterior(trait.mcmc, prior_env$prior.distns, jagged.data, pft$outdir)
  dist_MA_path <- file.path(pft$outdir, "post.distns.MA.Rdata")
  save(post.distns, file = dist_MA_path)

  dist_path <- file.path(pft$outdir, "post.distns.Rdata")
  
  # Symlink to post.distns.Rdata (no 'MA' identifier)
  if (file.exists(dist_path)) {
    file.remove(dist_path)
  }
  file.symlink(dist_MA_path, dist_path)
  
  ### save and store in database all results except those that were there already
  for (file in list.files(path = pft$outdir)) {
    # Skip file if it was there already, or if it's a symlink (like the post.distns.Rdata link above)
    if (file %in% old.files || nchar(Sys.readlink(file.path(pft$outdir, file))) > 0) {
      next
    }
    filename <- file.path(pathname, file)
    file.copy(file.path(pft$outdir, file), filename)
    PEcAn.DB::dbfile.insert(pathname, file, "Posterior", pft$posteriorid, dbcon)
  }
} # run.meta.analysis.pft

##--------------------------------------------------------------------------------------------------##
##' Run meta analysis
##'
##' This will use the following items from settings:
##' - settings$pfts
##' - settings$database$bety
##' - settings$database$dbfiles
##' - settings$meta.analysis$update
##' @param pfts the list of pfts to get traits for
##' @param iterations the number of iterations for the mcmc analysis
##' @param random should random effects be used?
##' @param use_ghs do not exclude greenhouse data if TRUE
##' @param dbfiles location where previous results are found
##' @param database database connection parameters
##' @param update logical: Rerun the meta-analysis if result files already exist?
##' @param threshold Gelman-Rubin convergence diagnostic, passed on to
##'   \code{\link{pecan.ma.summary}}
##' @return nothing, as side effect saves \code{trait.mcmc} created by
##' \code{\link{pecan.ma}} and post.distns created by
##' \code{\link{approx.posterior}(trait.mcmc, ...)}  to trait.mcmc.Rdata
##'   and post.distns.Rdata, respectively
##' @export
##' @author Shawn Serbin, David LeBauer
run.meta.analysis <- function(pfts, iterations, random = TRUE, threshold = 1.2, dbfiles, database, use_ghs = TRUE , update = FALSE) {
  # process all pfts
  dbcon <- PEcAn.DB::db.open(database)
  on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)

  result <- lapply(pfts, run.meta.analysis.pft, iterations = iterations, random = random, 
                   threshold = threshold, dbfiles = dbfiles, dbcon = dbcon, use_ghs = use_ghs, update = update)
} # run.meta.analysis.R
## ==================================================================================================#
#' Run meta-analysis on all PFTs in a (list of) PEcAn settings
#'
##' @param settings a PEcAn settings or MultiSettings object
##' @return list of PFTs, invisibly;
##'  saves MA results to `settings$pft$outdir` as a side effect
##' @export
runModule.run.meta.analysis <- function(settings) {
  if (PEcAn.settings::is.MultiSettings(settings)) {
    pfts <- list()
    pft.names <- character(0)
    for (i in seq_along(settings)) {
      pfts.i      <- settings[[i]]$pfts
      pft.names.i <- sapply(pfts.i, function(x) x$name)
      ind         <- which(pft.names.i %in% setdiff(pft.names.i, pft.names))
      pfts        <- c(pfts, pfts.i[ind])
      pft.names   <- sapply(pfts, function(x) x$name)
    }
    
    PEcAn.logger::logger.info(paste0("Running meta-analysis on all PFTs listed by any Settings object in the list: ", 
                       paste(pft.names, collapse = ", ")))
    
    iterations <- settings$meta.analysis$iter
    random     <- settings$meta.analysis$random.effects$on
    use_ghs    <- settings$meta.analysis$random.effects$use_ghs
    threshold  <- settings$meta.analysis$threshold
    dbfiles    <- settings$database$dbfiles
    database   <- settings$database$bety
    run.meta.analysis(pfts, iterations, random, threshold, dbfiles, database, use_ghs)
  } else if (PEcAn.settings::is.Settings(settings)) {
    pfts       <- settings$pfts
    iterations <- settings$meta.analysis$iter
    random     <- settings$meta.analysis$random.effects$on
    use_ghs    <- settings$meta.analysis$random.effects$use_ghs
    threshold  <- settings$meta.analysis$threshold
    dbfiles    <- settings$database$dbfiles
    database   <- settings$database$bety
    run.meta.analysis(pfts, iterations, random, threshold, dbfiles, database, use_ghs, update = settings$meta.analysis$update)
  } else {
    stop("runModule.run.meta.analysis only works with Settings or MultiSettings")
  }
} # runModule.run.meta.analysis

##--------------------------------------------------------------------------------------------------#
##' compare point to prior distribution
##'
##' used to compare data to prior, meta analysis posterior to prior
##' @title find quantile of point within prior distribution
##' @param point quantile of given prior to return
##' @param prior list of distn, parama, paramb
##' @return result of `p<distn>(point, parama, paramb)`
##' @author David LeBauer
p.point.in.prior <- function(point, prior) {
  out <- do.call(paste0("p", prior$distn), 
                 list(point, prior$parama, prior$paramb))
  return(out)
} # p.point.in.prior
