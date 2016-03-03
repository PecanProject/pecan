
##' Convert priors / MCMC samples to chains that can be sampled for model parameters 
##' 
##' @name get.parameter.samples
##' @title Sample from priors or posteriors
##' @param pfts the pfts node of the list of pecan settings
##' @export
##'
##' @author David LeBauer, Shawn Serbin
### Identify PFTs in the input settings.xml file
get.parameter.samples <- function(pfts = settings$pfts, posterior.files=rep(NA, length(settings$pfts)), ens.sample.method="uniform"){
  require(coda)
  require(PEcAn.priors)
  num.pfts <- length(settings$pfts)
  pft.names <- list()
  outdirs <- list()
  
  for (i.pft in seq_along(pfts)){
    pft.names[i.pft] <- settings$pfts[i.pft]$pft$name
    
    ### If no PFT(s) are specified insert NULL to warn user 
    if(length(pft.names)==0) pft.names[1] <- "NULL" 
    
    ### Get output directory info
    outdirs[i.pft] <- settings$pfts[i.pft]$pft$outdir
    
  } ### End of for loop to extract pft names
  
  logger.info("Selected PFT(s): ", pft.names)
  
  ## Generate empty list arrays for output.
  trait.samples <- sa.samples <- ensemble.samples <- env.samples <- runs.samples <- list()
  
  ## Load PFT priors and posteriors
  for (i in seq_along(pft.names)){
    ## Load posteriors
    if(!is.na(posterior.files[i])) {
      # Load specified file
      load(file.path(outdirs[i], posterior.files[i]))
      if(!exists('prior.distns') & exists('post.distns')) {
        prior.distns <- post.distns
      }
    } else {
      # Default to most recent posterior in the workflow, or the prior if there is none
      fname = file.path(outdirs[i], 'post.distns.Rdata')
      if(file.exists(fname)){
        load(fname)
        prior.distns = post.distns
      } else {
        load(file.path(outdirs[i], 'prior.distns.Rdata'))
      }
    }
    
    ### Load trait mcmc data (if exists)
    if("trait.mcmc.Rdata" %in% dir(unlist(outdirs))) {
      ma.results <- TRUE
      load(file.path(outdirs[i], 'trait.mcmc.Rdata'))
    }
    
    pft.name <- unlist(pft.names[i])
    
    ### When no ma for a trait, sample from  prior
    ### Trim all chains to shortest mcmc chain, else 20000 samples
    priors <- rownames(prior.distns)
    if(exists('trait.mcmc')) {
      ma.traits <- names(trait.mcmc)
      samples.num <- min(sapply(trait.mcmc, function(x) nrow(as.matrix(x))))
      
      ## report which traits use MA results, which use priors 
      if(length(ma.traits) > 0){
        logger.info("PFT",  pft.names[i], "has MCMC samples for:\n", paste0(ma.traits, collapse = "\n "))        
      }
      if(!all(priors %in% ma.traits)){
        logger.info("PFT", pft.names[i], "will use prior distributions for:\n", paste0(priors[!priors %in% ma.traits], collapse = "\n "))        
      }
    } else {
      ma.traits <- NULL
      samples.num <- 20000
      logger.info("No MCMC results for PFT",  pft.names[i])  
      logger.info("PFT", pft.names[i], "will use prior distributions for", priors )
    }
    
    
    logger.info("using ", samples.num, "samples per trait")
    for (prior in priors) {
      if (prior %in% ma.traits) {
        samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
      } else {
        samples <- get.sample(prior.distns[prior,], samples.num)
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
    
  } ### End for loop
  if("sensitivity.analysis" %in% names(settings)){
    
    ### Get info on the quantiles to be run in the sensitivity analysis (if requested)
    quantiles <- get.quantiles(settings$sensitivity.analysis$quantiles)
    ### Get info on the years to run the sensitivity analysis (if requested)
    sa.years <- data.frame(sa.start = settings$sensitivity.analysis$start.year,
                           sa.end = settings$sensitivity.analysis$end.year)
    
    logger.info("\n Selected Quantiles: ", vecpaste(round(quantiles, 3)))
    
    ### Generate list of sample quantiles for SA run
    sa.samples <-  get.sa.sample.list(pft       = trait.samples, 
                                      env       = env.samples, 
                                      quantiles = quantiles)
  }
  if("ensemble" %in% names(settings)){
    if(settings$ensemble$size == 1) {
      ## run at median if only one run in ensemble
      ensemble.samples <- get.sa.sample.list(pft = trait.samples,
                                             env = env.samples,
                                             quantiles = 0.5)
    } else if (settings$ensemble$size > 1) {
      
      ## subset the trait.samples to ensemble size using Halton sequence 
      ensemble.samples <- get.ensemble.samples(settings$ensemble$size, 
                                               trait.samples, env.samples, ens.sample.method)
    }
  }
  
  save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples,
       file = file.path(settings$outdir, 'samples.Rdata'))

} 