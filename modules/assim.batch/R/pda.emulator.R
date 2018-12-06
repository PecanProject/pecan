##' Paramater Data Assimilation using emulator
##'
##' @title Paramater Data Assimilation using emulator
##' @param settings = a pecan settings list
##' @param external.data = list of inputs
##' @param external.priors = list or priors
##'
##' @return nothing. Diagnostic plots, MCMC samples, and posterior distributions
##'  are saved as files and db records.
##'
##' @author Mike Dietze
##' @author Ryan Kelly, Istem Fer
##' @export
pda.emulator <- function(settings, external.data = NULL, external.priors = NULL,
                         params.id = NULL, param.names = NULL, prior.id = NULL, 
                         chain = NULL, iter = NULL, adapt = NULL, adj.min = NULL, 
                         ar.target = NULL, jvar = NULL, n.knot = NULL) {
  
  ## this bit of code is useful for defining the variables passed to this function if you are
  ## debugging
  if (FALSE) {
    external.data <- external.priors <- NULL
    params.id <- param.names <- prior.id <- chain <- iter <- NULL
    n.knot <- adapt <- adj.min <- ar.target <- jvar <- NULL
  }
  
  # handle extention flags
  # is this an extension run
  extension.check <- is.null(settings$assim.batch$extension) 
  
  if (extension.check) {
    # not an extension run
    run.normal <- TRUE
    run.round <- FALSE
    run.longer <- FALSE
  } else if (!extension.check & settings$assim.batch$extension == "round") {
    # 'round' extension
    run.normal <- FALSE
    run.round <- TRUE
    run.longer <- FALSE
  } else if (!extension.check & settings$assim.batch$extension == "longer") {
    # 'longer' extension
    run.normal <- FALSE
    run.round <- FALSE
    run.longer <- TRUE
  }
  
  ## -------------------------------------- Setup ------------------------------------- 
  ## Handle settings
  settings <- pda.settings(
    settings=settings, params.id=params.id, param.names=param.names, 
    prior.id=prior.id, chain=chain, iter=iter, adapt=adapt, 
    adj.min=adj.min, ar.target=ar.target, jvar=jvar, n.knot=n.knot, run.round)
  
  ## will be used to check if multiplicative Gaussian is requested
  any.mgauss <- sapply(settings$assim.batch$inputs, `[[`, "likelihood")
  isbias <- which(unlist(any.mgauss) == "multipGauss")
  
  ## check if scaling factors are gonna be used
  any.scaling <- sapply(settings$assim.batch$param.names, `[[`, "scaling")
  sf <- unique(unlist(any.scaling))
  
  # used in rounds only
  pass2bias <- NULL
  
  ## Open database connection
  if (settings$database$bety$write) {
    con <- try(db.open(settings$database$bety), silent = TRUE)
    if (is(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(db.close(con))
    }
  } else {
    con <- NULL
  }
  
  bety <- src_postgres(dbname = settings$database$bety$dbname, 
                       host = settings$database$bety$host, 
                       user = settings$database$bety$user, 
                       password = settings$database$bety$password)
  
  ## Load priors
  if(is.null(external.priors)){
    temp        <- pda.load.priors(settings, bety$con, run.normal)
    prior.list  <- temp$prior
    settings    <- temp$settings
  }else{
    prior.list  <- external.priors
  }
  pname       <- lapply(prior.list, rownames)
  n.param.all <- sapply(prior.list, nrow)
  
  
  if(is.null(external.data)){
    inputs <- load.pda.data(settings, bety)
  }else{
    inputs <- external.data
  }
  
  n.input     <- length(inputs)
  
  
  ## Set model-specific functions
  do.call("library", list(paste0("PEcAn.", settings$model$type)))
  my.write.config <- paste("write.config.", settings$model$type, sep = "")
  if (!exists(my.write.config)) {
    PEcAn.logger::logger.severe(paste(my.write.config, 
                                      "does not exist. Please make sure that the PEcAn interface is loaded for", 
                                      settings$model$type))
  }
  
  ## Select parameters to constrain
  all_pft_names <- sapply(settings$pfts, `[[`, "name")
  prior.ind <- prior.ind.orig <- vector("list", length(settings$pfts)) 
  names(prior.ind) <- names(prior.ind.orig) <- all_pft_names
  for(i in seq_along(settings$pfts)){
    pft.name <- settings$pfts[[i]]$name
    if(pft.name %in% names(settings$assim.batch$param.names)){
      prior.ind[[i]]      <- which(pname[[i]] %in% settings$assim.batch$param.names[[pft.name]])
      prior.ind.orig[[i]] <- which(pname[[i]] %in% settings$assim.batch$param.names[[pft.name]] |
                                     pname[[i]] %in% any.scaling[[pft.name]])
    }
  }
  
  n.param <- sapply(prior.ind, length)
  n.param.orig <- sapply(prior.ind.orig, length)
  
  ## Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  
  ## Create an ensemble id
  settings$assim.batch$ensemble.id <- pda.create.ensemble(settings, con, workflow.id)
  
  ## history restart
  pda.restart.file <- file.path(settings$outdir,paste0("history.pda",
                                                       settings$assim.batch$ensemble.id, ".Rdata"))
  current.step <- "START" 
  
  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)
  
  ## ------------------------------------ Emulator ------------------------------------ 
  # if we are going to throw scaling factor(s) instead of parameters
  # 1. append scaling factor priors to prior.list
  # 2. use the same probs for all pft params to be scaled
  if(!is.null(sf)){
    sf.ind <- length(prior.list) + 1
    sf.list <- pda.generate.sf(settings$assim.batch$n.knot, sf, prior.list)
    probs.sf <- sf.list$probs
    prior.list <- sf.list$priors
  }else {
    probs.sf <- NULL
  }
  
  ## Set prior distribution functions (d___, q___, r___, and multivariate versions)
  prior.fn <- lapply(prior.list, pda.define.prior.fn)
  
  
  ## Propose parameter knots (X) for emulator design
  knots.list <- lapply(seq_along(settings$pfts), 
                       function(x) pda.generate.knots(settings$assim.batch$n.knot, sf, probs.sf,
                                                      n.param.all[x], 
                                                      prior.ind.orig[[x]], 
                                                      prior.fn[[x]], 
                                                      pname[[x]]))
  names(knots.list) <- sapply(settings$pfts,"[[",'name')
  
  knots.params <- lapply(knots.list, `[[`, "params")
  # don't need anymore
  # knots.probs <- lapply(knots.list, `[[`, "probs")
  
  current.step <- "GENERATE KNOTS"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
  ## Run this block if this is a "round" extension
  if (run.round) {
    
    ## Propose a percentage (if not specified 90%) of the new parameter knots from the posterior of the previous run
    knot.par        <- ifelse(!is.null(settings$assim.batch$knot.par),
                              as.numeric(settings$assim.batch$knot.par),
                              0.9)
    
    n.post.knots    <- floor(knot.par * settings$assim.batch$n.knot)
    
    # trim down, as a placeholder
    knots.params.temp <- lapply(knots.params, function(x) x[1:n.post.knots, ])
    
    if(!is.null(sf)){
      load(settings$assim.batch$sf.samp)
    }else{
      sf.samp <- NULL
    }
    sampled_knots <- sample_MCMC(settings$assim.batch$mcmc.path, n.param.orig, prior.ind.orig, 
                                     n.post.knots, knots.params.temp,
                                     prior.list, prior.fn, sf, sf.samp)

    knots.params.temp <- sampled_knots$knots.params.temp
    probs.round.sf    <- sampled_knots$sf_knots
    pass2bias         <- sampled_knots$pass2bias

    # mixture of knots
    mix.knots <- sample(settings$assim.batch$n.knot, (settings$assim.batch$n.knot - n.post.knots))
    for (i in seq_along(settings$pfts)) {
      knots.list[[i]]$params <- rbind(knots.params[[i]][mix.knots, ],
                                      knots.params.temp[[i]])
      names(knots.list)[i] <- settings$pfts[[i]]['name']
    }
    
    if(!is.null(sf)){
      probs.sf <- rbind(probs.sf[mix.knots, ], probs.round.sf)
    }
    
    knots.params <- lapply(knots.list, `[[`, "params")
    
    current.step <- "Generate Knots: round-if block"
    save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  } # end round-if block
  
  
  ## Run this block if this is normal run or a "round" extension
  if(run.normal | run.round){
    
    ## Set up runs and write run configs for all proposed knots
    run.ids <- pda.init.run(settings, con, my.write.config, workflow.id, knots.params, 
                            n = settings$assim.batch$n.knot, 
                            run.names = paste0(settings$assim.batch$ensemble.id, ".knot.",
                                               1:settings$assim.batch$n.knot))   
    current.step <- "pda.init.run"
    save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
    
    ## start model runs
    PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
    
    ## Retrieve model outputs and error statistics
    model.out <- list()
    pda.errors <- list()
    
    
    ## read model outputs    
    for (i in seq_len(settings$assim.batch$n.knot)) {
      align.return <- pda.get.model.output(settings, run.ids[i], bety, inputs)
      model.out[[i]] <- align.return$model.out
      if(all(!is.na(model.out[[i]]))){
        inputs <- align.return$inputs
      }
    }
    
    
    current.step <- "pda.get.model.output"
    save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
    
    # efficient sample size calculation
    inputs <- pda.neff.calc(inputs)
    
    # handle bias parameters if multiplicative Gaussian is listed in the likelihoods
    if(any(unlist(any.mgauss) == "multipGauss")) {
      bias.list  <- return.bias(settings, isbias, model.out, inputs, prior.list, run.round, pass2bias)
      bias.terms <- bias.list$bias.params
      prior.list <- bias.list$prior.list.bias
      nbias      <- bias.list$nbias
      prior.fn   <- lapply(prior.list, pda.define.prior.fn)
    } else {
      bias.terms <- NULL
    }
    
    
    for (i in seq_len(settings$assim.batch$n.knot)) {
      if(!is.null(bias.terms)){
        all.bias <- lapply(bias.terms, function(n) n[i,])
        all.bias <- do.call("rbind", all.bias)
      } else {
        all.bias <- NULL
      }
      ## calculate error statistics and save in the DB      
      pda.errors[[i]] <- pda.calc.error(settings, con, model_out = model.out[[i]], run.id = run.ids[i], inputs, bias.terms = all.bias)
    } 
    
  } # end if-block
  current.step <- "pda.calc.error"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
  init.list <- list()
  jmp.list <- list()
  
  prior.all <- do.call("rbind", prior.list)
  length.pars <- 0
  prior.ind.list <- prior.ind.list.ns <- list()
  # now I need to go through all parameters for each pft, but leave out the ones that scaling factor is requested
  for(p in seq_along(settings$assim.batch$param.names)){
    param.names <- settings$assim.batch$param.names[[p]]
    prior.ind.list[[p]] <- length.pars + which(pname[[p]] %in% unlist(param.names) &
                                                 !(pname[[p]] %in% sf))
    prior.ind.list.ns[[p]] <- length.pars + which(pname[[p]] %in% unlist(param.names))
    length.pars <- length.pars + length(pname[[p]])
  }
  prior.ind.all    <- unlist(prior.ind.list)
  prior.ind.all.ns <- unlist(prior.ind.list.ns)
  # if no scaling is requested prior.ind.all == prior.ind.all.ns
  # keep this ind.all w/o bias until extracting prob values below
  
  if (run.normal | run.round) {
    
    # retrieve n
    n.of.obs <- sapply(inputs,`[[`, "n") 
    names(n.of.obs) <- sapply(model.out[[1]],names)
    
    
    # UPDATE: Use mlegp package, I can now draw from parameter space
    knots.params.all <- do.call("cbind", knots.params)
    X <- knots.params.all[, prior.ind.all, drop = FALSE]
    
    if(!is.null(sf)){
      X <- cbind(X, probs.sf)
    }
    
    # retrieve SS
    error.statistics <- list()
    SS.list <- list()
    bc <- 1
    
    # what percentage of runs is allowed to fail?
    if(!is.null(settings$assim.batch$allow.fail)){
      allow.fail <- as.numeric(settings$assim.batch$allow.fail)
    } else {
      allow.fail <- 0.5
    }
    # what is it in number of runs?
    no.of.allowed <- floor(settings$assim.batch$n.knot * allow.fail)
    
    for(inputi in seq_len(n.input)){
      error.statistics[[inputi]] <- sapply(pda.errors,`[[`, inputi)
      
     if(unlist(any.mgauss)[inputi] == "multipGauss") {
        
        # if yes, then we need to include bias term in the emulator
        bias.params <- bias.terms
        biases <- c(t(bias.params[[bc]]))
        bc <- bc + 1
        
        # replicate model parameter set per bias parameter
        rep.rows <- rep(1:nrow(X), each = nbias)
        X.rep <- X[rep.rows,]
        Xnew <- cbind(X.rep, biases)
        colnames(Xnew) <- c(colnames(X.rep), paste0("bias.", names(n.of.obs)[inputi]))
        SS.list[[inputi]] <- cbind(Xnew, c(error.statistics[[inputi]]))
        
      } else {
        SS.list[[inputi]] <- cbind(X, error.statistics[[inputi]])
      } # if-block
      
      # check failed runs and remove them if you'll have a reasonable amount of param sets after removal
      # how many runs failed?
      no.of.failed <- sum(is.na(SS.list[[inputi]][, ncol(SS.list[[inputi]])]))
      
      # check if you're left with enough sets
      if(no.of.failed < no.of.allowed & (settings$assim.batch$n.knot - no.of.failed) > 1){
        SS.list[[inputi]] <- SS.list[[inputi]][!rowSums(is.na(SS.list[[inputi]])), ]
        if( no.of.failed  > 0){
          PEcAn.logger::logger.info(paste0(no.of.failed, " runs failed. Emulator for ", names(n.of.obs)[inputi], " will be built with ", settings$assim.batch$n.knot - no.of.failed, " knots."))
        } 
      } else{
        PEcAn.logger::logger.error(paste0("Too many runs failed, not enough parameter set to build emulator for ", names(n.of.obs)[inputi], "."))
      }
      
    } # for-loop
    
    if (run.round) {
      # check if this is another 'round' of emulator 
      
      # load original knots
      load(settings$assim.batch$ss.path)
      # add on
      SS <- lapply(seq_along(SS), function(iss) rbind(SS.list[[iss]], SS[[iss]]))
      
    } else {
      SS <- SS.list
    }
    
    PEcAn.logger::logger.info(paste0("Using 'mlegp' package for Gaussian Process Model fitting."))
    
    ## Generate emulator on SS, return a list ##
    
    # start the clock
    ptm.start <- proc.time()
    
    # prepare for parallelization
    dcores <- parallel::detectCores() - 1
    ncores <- min(max(dcores, 1), length(SS))
    
    cl <- parallel::makeCluster(ncores, type="FORK")
    
    ## Parallel fit for GPs
    GPmodel <- parallel::parLapply(cl, SS, function(x) mlegp::mlegp(X = x[, -ncol(x), drop = FALSE], Z = x[, ncol(x), drop = FALSE], nugget = 0, nugget.known = 1, verbose = 0))
    # GPmodel <- lapply(SS, function(x) mlegp::mlegp(X = x[, -ncol(x), drop = FALSE], Z = x[, ncol(x), drop = FALSE], nugget = 0, nugget.known = 1, verbose = 0))
    
    
    parallel::stopCluster(cl)
    
    # Stop the clock
    ptm.finish <- proc.time() - ptm.start
    PEcAn.logger::logger.info(paste0("GP fitting took ", paste0(round(ptm.finish[3])), " seconds."))
    
    
    gp <- GPmodel
    
  } else { # is this a "longer" type of extension run
    
    load(settings$assim.batch$emulator.path)  # load previously built emulator(s) to run a longer mcmc
    load(settings$assim.batch$ss.path)
    load(settings$assim.batch$resume.path)
    
    n.of.obs <- resume.list[[1]]$n.of.obs
    
    if(any(unlist(any.mgauss) == "multipGauss")){
      load(settings$assim.batch$bias.path) # load prior.list with bias term from previous run
      prior.all <- do.call("rbind", prior.list)
    }
    
    
    for (c in seq_len(settings$assim.batch$chain)) {
      init.list[[c]] <- resume.list[[c]]$prev.samp[nrow(resume.list[[c]]$prev.samp), ]
      jmp.list[[c]] <- resume.list[[c]]$jump
    }
  }
  
  # add indice and increase n.param for scaling factor
  if(!is.null(sf)){
    prior.ind.all <- c(prior.ind.all, 
                       ((length.pars + 1): (length.pars + length(sf))))
    n.param       <- c(n.param, length(sf))
    length.pars   <- length.pars + length(sf)
  }
  
  # add indice and increase n.param for bias
  if(any(unlist(any.mgauss) == "multipGauss")){
    prior.ind.all <- c(prior.ind.all, 
                       ((length.pars + 1) : (length.pars + length(isbias))))
    prior.ind.all.ns <- c(prior.ind.all.ns, 
                          ((length.pars + 1) : (length.pars + length(isbias))))
    n.param <- c(n.param, length(isbias))
    n.param.orig <- c(n.param.orig, length(isbias))
    length.pars   <- length.pars + length(isbias)
  }
  
  
  ## Set up prior functions accordingly
  prior.fn.all <- pda.define.prior.fn(prior.all)
  
  # define range to make sure mcmc.GP doesn't propose new values outside
  
  # NOTE: this will need to change when there is more than one bias parameter
  # but then, there are other things that needs to change in the emulator workflow
  # such as the way proposed parameters are used in estimation in get_ss function
  # so punting this development until it is needed
  rng <-  t(apply(SS[[isbias]][,-ncol(SS[[isbias]])], 2, range))
  
  if (run.normal | run.round) {
    
    resume.list <- list()
    
    # start from knots
    indx <- sample(seq_len(settings$assim.batch$n.knot), settings$assim.batch$chain)
    
    for (c in seq_len(settings$assim.batch$chain)) {
      jmp.list[[c]] <- sapply(prior.fn.all$qprior, 
                              function(x) 0.1 * diff(eval(x, list(p = c(0.05, 0.95)))))[prior.ind.all]
      jmp.list[[c]] <- sqrt(jmp.list[[c]])
      
      init.list[[c]] <- as.list(SS[[isbias]][indx[c], -ncol(SS[[isbias]])])
      resume.list[[c]] <- NA
    }
  }
  

  if (!is.null(settings$assim.batch$mix)) {
    mix <- settings$assim.batch$mix
  } else if (sum(n.param) > 1) {
    mix <- "joint"
  } else {
    mix <- "each"
  }
  
  # get hyper parameters if any
  hyper.pars <- return_hyperpars(settings$assim.batch, inputs)
  
  PEcAn.logger::logger.info(paste0("Starting emulator MCMC. Please wait."))
  
  current.step <- "pre-MCMC"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
  # start the clock
  ptm.start <- proc.time()
  
  # prepare for parallelization
  dcores <- parallel::detectCores() - 1
  ncores <- min(max(dcores, 1), settings$assim.batch$chain)
  
  PEcAn.logger::logger.setOutputFile(file.path(settings$outdir, "pda.log"))
  
  cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(settings$outdir, "pda.log"))
  
  ## Sample posterior from emulator
  mcmc.out <- parallel::parLapply(cl, 1:settings$assim.batch$chain, function(chain) {
    mcmc.GP(gp          = gp, ## Emulator(s)
            x0          = init.list[[chain]],     ## Initial conditions
            nmcmc       = settings$assim.batch$iter,       ## Number of reps
            rng         = rng,       ## range
            format      = "lin",      ## "lin"ear vs "log" of LogLikelihood 
            mix         = mix,     ## Jump "each" dimension independently or update them "joint"ly
            jmp0        = jmp.list[[chain]],  ## Initial jump size
            ar.target   = settings$assim.batch$jump$ar.target,   ## Target acceptance rate
            priors      = prior.fn.all$dprior[prior.ind.all], ## priors
            settings    = settings,
            run.block   = (run.normal | run.round),  
            n.of.obs    = n.of.obs,
            llik.fn     = llik.fn,
            hyper.pars  = hyper.pars,
            resume.list = resume.list[[chain]]
    )
  })
  
  parallel::stopCluster(cl)
  
  # Stop the clock
  ptm.finish <- proc.time() - ptm.start
  PEcAn.logger::logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(settings$assim.batch$iter), " iterations."))
  
  current.step <- "post-MCMC"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
  mcmc.samp.list <- sf.samp.list <- list()
  
  for (c in seq_len(settings$assim.batch$chain)) {
    
    m <- matrix(NA, nrow =  nrow(mcmc.out[[c]]$mcmc.samp), ncol = length(prior.ind.all.ns))
    
    if(!is.null(sf)){
      sfm <- matrix(NA, nrow =  nrow(mcmc.out[[c]]$mcmc.samp), ncol = length(sf))
      # give colnames but the order can change, we'll overwrite anyway
      colnames(sfm) <- paste0(sf, "_SF")
    }
    ## Set the prior functions back to work with actual parameter range
    
    prior.all <- do.call("rbind", prior.list)
    prior.fn.all <- pda.define.prior.fn(prior.all)
    
    # retrieve rownames separately to get rid of var_name* structures
    prior.all.rownames <- unlist(sapply(prior.list, rownames))
    
    sc <- 1
    for (i in seq_along(prior.ind.all.ns)) {
      sf.check <- prior.all.rownames[prior.ind.all.ns][i]
      idx <- grep(sf.check, rownames(prior.all)[prior.ind.all])
      if(any(grepl(sf.check, sf))){
        
        m[, i] <- eval(prior.fn.all$qprior[prior.ind.all.ns][[i]],
                       list(p = mcmc.out[[c]]$mcmc.samp[, idx]))
        if(sc <= length(sf)){
          sfm[, sc] <- mcmc.out[[c]]$mcmc.samp[, idx]
          colnames(sfm)[sc] <- paste0(sf.check, "_SF")
          sc <- sc + 1
        }
        
      }else{
        m[, i] <- mcmc.out[[c]]$mcmc.samp[, idx]
      }
    }
    
    colnames(m) <- prior.all.rownames[prior.ind.all.ns]
    mcmc.samp.list[[c]] <- m
    
    if(!is.null(sf)){
      sf.samp.list[[c]] <- sfm
    }
    
    resume.list[[c]] <- mcmc.out[[c]]$chain.res
  }
  
  
  
  if (FALSE) {
    gp          = gp
    x0          = init.list[[chain]]
    nmcmc       = settings$assim.batch$iter
    rng         = rng
    format      = "lin"
    mix         = mix
    jmp0        = jmp.list[[chain]]
    ar.target   = settings$assim.batch$jump$ar.target
    priors      = prior.fn.all$dprior[prior.ind.all]
    settings    = settings
    run.block   = (run.normal | run.round)  
    n.of.obs    = n.of.obs
    llik.fn     = llik.fn
    hyper.pars  = hyper.pars
    resume.list = resume.list[[chain]]
  }
  
  ## ------------------------------------ Clean up ------------------------------------ 
  current.step <- "clean up"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
  ## Save emulator, outputs files
  settings$assim.batch$emulator.path <- file.path(settings$outdir,
                                                  paste0("emulator.pda", 
                                                         settings$assim.batch$ensemble.id, 
                                                         ".Rdata"))
  save(gp, file = settings$assim.batch$emulator.path)
  
  settings$assim.batch$ss.path <- file.path(settings$outdir, 
                                            paste0("ss.pda", 
                                                   settings$assim.batch$ensemble.id, 
                                                   ".Rdata"))
  save(SS, file = settings$assim.batch$ss.path)
  
  settings$assim.batch$mcmc.path <- file.path(settings$outdir, 
                                              paste0("mcmc.list.pda", 
                                                     settings$assim.batch$ensemble.id, 
                                                     ".Rdata"))
  save(mcmc.samp.list, file = settings$assim.batch$mcmc.path)
  
  settings$assim.batch$resume.path <- file.path(settings$outdir, 
                                                paste0("resume.pda", 
                                                       settings$assim.batch$ensemble.id, 
                                                       ".Rdata"))
  save(resume.list, file = settings$assim.batch$resume.path)
  
  # save prior.list with bias term
  if(any(unlist(any.mgauss) == "multipGauss")){
    settings$assim.batch$bias.path <- file.path(settings$outdir, 
                                                paste0("bias.pda", 
                                                       settings$assim.batch$ensemble.id, 
                                                       ".Rdata"))
    save(prior.list, file = settings$assim.batch$bias.path)
  }
  
  # save sf posterior
  if(!is.null(sf)){
    sf.post.filename <- file.path(settings$outdir, 
                             paste0("post.distns.pda.sf", "_", settings$assim.batch$ensemble.id, ".Rdata"))
    sf.samp.filename <- file.path(settings$outdir, 
                             paste0("samples.pda.sf", "_", settings$assim.batch$ensemble.id, ".Rdata"))
    sf.prior <- prior.list[[sf.ind]]
    sf.post.distns <- write_sf_posterior(sf.samp.list, sf.prior, sf.samp.filename)
    save(sf.post.distns, file = sf.post.filename)
    settings$assim.batch$sf.path <- sf.post.filename
    settings$assim.batch$sf.samp <- sf.samp.filename
  }
  
  # Separate each PFT's parameter samples (and bias term) to their own list
  mcmc.param.list <- list()
  ind <- 0
  for (i in seq_along(n.param.orig)) {
    mcmc.param.list[[i]] <- lapply(mcmc.samp.list, function(x) x[, (ind + 1):(ind + n.param.orig[i]), drop = FALSE])
    ind <- ind + n.param.orig[i]
  }
  
  # Collect non-model parameters in their own list
  if(length(mcmc.param.list) > length(settings$pfts)) { 
    # means bias parameter was at least one bias param in the emulator
    # it will be the last list in mcmc.param.list
    # there will always be at least one tau for bias
    for(c in seq_len(settings$assim.batch$chain)){
      mcmc.param.list[[length(mcmc.param.list)]][[c]] <- cbind( mcmc.param.list[[length(mcmc.param.list)]][[c]],
                                                                mcmc.out[[c]]$mcmc.par)
    }
    
  } else if (ncol(mcmc.out[[1]]$mcmc.par) != 0){
    # means no bias param but there are still other params, e.g. Gaussian
    mcmc.param.list[[length(mcmc.param.list)+1]] <- list()
    for(c in seq_len(settings$assim.batch$chain)){
      mcmc.param.list[[length(mcmc.param.list)]][[c]] <- mcmc.out[[c]]$mcmc.par
    }
  }
  
  settings <- pda.postprocess(settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig)
  
  ## close database connection
  if (!is.null(con)) {
    db.close(con)
  }
  
  ## Output an updated settings list
  current.step <- "pda.finish"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  return(settings)
  
}  ## end pda.emulator
