
##' Convert priors / MCMC samples to chains that can be sampled for model parameters 
##' 
##' @param settings PEcAn settings object
##' @param posterior.files list of filenames to read from
##' @param ens.sample.method one of "halton", "sobol", "torus", "lhc", "uniform"
##' @export
##'
##' @author David LeBauer, Shawn Serbin, Istem Fer
### Identify PFTs in the input settings.xml file
get.parameter.samples <- function(settings, 
                                  posterior.files = rep(NA, length(settings$pfts)), 
                                  ens.sample.method = "uniform") {
  pfts      <- settings$pfts
  num.pfts  <- length(settings$pfts)
  pft.names <- list()
  outdirs   <- list()
  ## Open database connection
  con <- try(PEcAn.DB::db.open(settings$database$bety))
  on.exit(try(PEcAn.DB::db.close(con), silent = TRUE), add = TRUE)
  
  # If we fail to connect to DB then we set to NULL
  if (inherits(con, "try-error"))  {
    con <- NULL
    PEcAn.logger::logger.warn("We were not able to successfully establish a connection with Bety ")
  }
  
  for (i.pft in seq_along(pfts)) {
    pft.names[i.pft] <- settings$pfts[[i.pft]]$name
    
    ### If no PFT(s) are specified insert NULL to warn user
    if (length(pft.names) == 0) {
      pft.names[1] <- "NULL"
    }
    
    ### Get output directory info
    if(!is.null(settings$pfts[[i.pft]]$outdir)){
      outdirs[i.pft] <- settings$pfts[[i.pft]]$outdir
    } else { 
      outdirs[i.pft] <- unique(PEcAn.DB::dbfile.check(type = "Posterior",container.id = settings$pfts[[i.pft]]$posteriorid,con=con)$file_path)
    }
    
  }  ### End of for loop to extract pft names
  
  PEcAn.logger::logger.info("Selected PFT(s): ", pft.names)
  
  ## Generate empty list arrays for output.
  trait.samples <- sa.samples <- ensemble.samples <- env.samples <- runs.samples <- param.names <- list()
  
  # flag determining whether samples are independent (e.g. when params fitted individually)
  independent <- TRUE
  
  ## Load PFT priors and posteriors
  for (i in seq_along(pft.names)) {
    
    distns = new.env()

    ## Load posteriors
    if (!is.na(posterior.files[i])) {
      # Load specified file
      load(posterior.files[i], envir = distns)
      if (is.null(distns$prior.distns) & !is.null(distns$post.distns)) {
        distns$prior.distns <- distns$post.distns
      }
    } else {
      # Default to most recent posterior in the workflow, or the prior if there is none
      fname <- file.path(outdirs[i], "post.distns.Rdata")
      if (file.exists(fname)) {
        load(fname, envir = distns)
        distns$prior.distns <- distns$post.distns
      } else {
        load(file.path(outdirs[i], "prior.distns.Rdata"), envir = distns)
      }
    }
    
    ### Load trait mcmc data (if exists, either from MA or PDA)
    if (!is.null(settings$pfts[[i]]$posteriorid) && !inherits(con, "try-error")) {# first check if there are any files associated with posterior ids
      files <- PEcAn.DB::dbfile.check("Posterior",
                                      settings$pfts[[i]]$posteriorid, 
                                      con, settings$host$name, return.all = TRUE)
      tid <-  grep("trait.mcmc.*Rdata", files$file_name)
      if (length(tid) > 0) {
        trait.mcmc.file <- file.path(files$file_path[tid], files$file_name[tid])
        ma.results <- TRUE
        load(trait.mcmc.file, envir = distns)


        # PDA samples are fitted together, to preserve correlations downstream let workflow know they should go together
        if(grepl("mcmc.pda", trait.mcmc.file)) independent <- FALSE 
        # NOTE: Global MA samples will also be together, right?
        
        
      }else{
        PEcAn.logger::logger.info("No trait.mcmc file is associated with this posterior ID.")
        ma.results <- FALSE
      }
    }else if ("trait.mcmc.Rdata" %in% dir(unlist(outdirs[i]))) {
      PEcAn.logger::logger.info("Defaulting to trait.mcmc file in the pft directory.")
      ma.results <- TRUE
      load(file.path(outdirs[i], "trait.mcmc.Rdata"), envir = distns)
    } else {
      ma.results <- FALSE
    }
    
    pft.name <- unlist(pft.names[i])
    
    ### When no ma for a trait, sample from prior
    ### Trim all chains to shortest mcmc chain, else 20000 samples
    if(!is.null(distns$prior.distns)){
      priors <- rownames(distns$prior.distns)
    } else {
      priors <- NULL
    }  
    if (!is.null(distns$trait.mcmc)) {
      param.names[[i]] <- names(distns$trait.mcmc)
      names(param.names)[i] <- pft.name
      
      samples.num <- min(sapply(distns$trait.mcmc, function(x) nrow(as.matrix(x))))
      
      ## report which traits use MA results, which use priors
      if (length(param.names[[i]]) > 0) {
        PEcAn.logger::logger.info("PFT", pft.names[i], "has MCMC samples for:\n",
                                  paste0(param.names[[i]], collapse = "\n "))
      }
      if (!all(priors %in% param.names[[i]])) {
        PEcAn.logger::logger.info("PFT", pft.names[i], "will use prior distributions for:\n", 
                                  paste0(priors[!priors %in% param.names[[i]]], collapse = "\n "))
      }
    } else {
      param.names[[i]] <- list()
      samples.num <- 20000
      PEcAn.logger::logger.info("No MCMC results for PFT", pft.names[i])
      PEcAn.logger::logger.info("PFT", pft.names[i], "will use prior distributions for", 
                                priors)
    }
    if(is.null(priors)) priors = param.names[[i]]
    
    PEcAn.logger::logger.info("using ", samples.num, "samples per trait")
    if (ens.sample.method == "halton") {
      q_samples <- randtoolbox::halton(n = samples.num, dim = length(priors))
    } else if (ens.sample.method == "sobol") {
      q_samples <- randtoolbox::sobol(n = samples.num, dim = length(priors), scrambling = 3)
    } else if (ens.sample.method == "torus") {
      q_samples <- randtoolbox::torus(n = samples.num, dim = length(priors))
    } else if (ens.sample.method == "lhc") {
      q_samples <- PEcAn.emulator::lhc(t(matrix(0:1, ncol = length(priors), nrow = 2)), samples.num)
    } else if (ens.sample.method == "uniform") {
      q_samples <- matrix(stats::runif(samples.num * length(priors)),
                               samples.num, 
                               length(priors))
    } else {
      PEcAn.logger::logger.info("Method ", ens.sample.method, " has not been implemented yet, using uniform random sampling")
      # uniform random
      q_samples <- matrix(stats::runif(samples.num * length(priors)),
                          samples.num, 
                          length(priors))
    }
    for (prior in priors) {
      if (prior %in% param.names[[i]]) {
        samples <- distns$trait.mcmc[[prior]] %>%
          purrr::map(~ .x[,'beta.o']) %>%
          unlist() %>%
          as.matrix()
      } else {
        samples <- PEcAn.priors::get.sample(distns$prior.distns[prior, ], samples.num, q_samples[ , priors==prior])
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
  }  ### End for loop
  
  # if samples are independent, set param.names to NULL
  # this is important for downstream, when param.names is not NULL MCMC will be sampled accordingly
  if(independent){
    param.names <- NULL
  }
  
  if ("sensitivity.analysis" %in% names(settings)) {
    
    ### Get info on the quantiles to be run in the sensitivity analysis (if requested)
    quantiles <- PEcAn.utils::get.quantiles(settings$sensitivity.analysis$quantiles)
    ### Get info on the years to run the sensitivity analysis (if requested)
    sa.years <- data.frame(sa.start = settings$sensitivity.analysis$start.year, 
                           sa.end = settings$sensitivity.analysis$end.year)
    
    PEcAn.logger::logger.info("\n Selected Quantiles: ", PEcAn.utils::vecpaste(round(quantiles, 3)))
    
    ### Generate list of sample quantiles for SA run
    sa.samples <- PEcAn.utils::get.sa.sample.list(pft = trait.samples, env = env.samples, 
                                     quantiles = quantiles)
  }
  if ("ensemble" %in% names(settings)) {
    if (settings$ensemble$size == 1) {
      ## run at median if only one run in ensemble
      ensemble.samples <- PEcAn.utils::get.sa.sample.list(pft = trait.samples, env = env.samples, 
                                             quantiles = 0.5)
      #if it's not there it's one probably
      if (is.null(settings$ensemble$size)) settings$ensemble$size<-1
    } else if (settings$ensemble$size > 1) {
      
      ## subset the trait.samples to ensemble size using Halton sequence
      ensemble.samples <- get.ensemble.samples(settings$ensemble$size, trait.samples, 
                                               env.samples, ens.sample.method, param.names)
    }
  }
  
  save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples, 
       file = file.path(settings$outdir, "samples.Rdata"))
} # get.parameter.samples
