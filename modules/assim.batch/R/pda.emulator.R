##' Paramater Data Assimilation using emulator
##'
##' @title Paramater Data Assimilation using emulator
##' @param settings = a pecan settings list
##'
##' @return nothing. Diagnostic plots, MCMC samples, and posterior distributions
##'  are saved as files and db records.
##'
##' @author Mike Dietze
##' @author Ryan Kelly, Istem Fer
##' @export
pda.emulator <- function(settings, params.id = NULL, param.names = NULL, prior.id = NULL, 
                         chain = NULL, iter = NULL, adapt = NULL, adj.min = NULL, 
                         ar.target = NULL, jvar = NULL, n.knot = NULL) {
  
  ## this bit of code is useful for defining the variables passed to this function if you are
  ## debugging
  if (FALSE) {
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
 
  ## history restart
  pda.restart.file <- file.path(settings$outdir,paste0("history.pda",
                                                       settings$assim.batch$ensemble.id, ".Rdata"))
  current.step <- "START" 
  
  ## will be used to check if multiplicative Gaussian is requested
  any.mgauss <- sapply(settings$assim.batch$inputs, `[[`, "likelihood")
  isbias <- which(unlist(any.mgauss) == "multipGauss")
  
  
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
  temp        <- pda.load.priors(settings, bety$con, run.normal)
  prior.list  <- temp$prior
  settings    <- temp$settings
  pname       <- lapply(prior.list, rownames)
  n.param.all <- sapply(prior.list, nrow)
  
  ## Load data to assimilate against
  inputs      <- load.pda.data(settings, bety)
  n.input     <- length(inputs)
  
  ## Set model-specific functions
  do.call("library", list(paste0("PEcAn.", settings$model$type)))
  my.write.config <- paste("write.config.", settings$model$type, sep = "")
  if (!exists(my.write.config)) {
    logger.severe(paste(my.write.config, 
                        "does not exist. Please make sure that the PEcAn interface is loaded for", 
                        settings$model$type))
  }
  
  ## Select parameters to constrain
  prior.ind <- lapply(seq_along(settings$pfts), 
                      function(x) which(pname[[x]] %in% settings$assim.batch$param.names[[x]]))
  n.param <- sapply(prior.ind, length)
  
  ## Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  
  ## Create an ensemble id
  settings$assim.batch$ensemble.id <- pda.create.ensemble(settings, con, workflow.id)
  
  ## Set prior distribution functions (d___, q___, r___, and multivariate versions)
  prior.fn <- lapply(prior.list, pda.define.prior.fn)
  
  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)
  
  ## ------------------------------------ Emulator ------------------------------------ 
  ## Propose parameter knots (X) for emulator design
  knots.list <- lapply(seq_along(settings$pfts), 
                       function(x) pda.generate.knots(settings$assim.batch$n.knot, 
                                                      n.param.all[x], 
                                                      prior.ind[[x]], 
                                                      prior.fn[[x]], 
                                                      pname[[x]]))
  names(knots.list) <- sapply(settings$pfts,"[[",'name')
  
  current.step <- "GENERATE KNOTS"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
  ## Run this block if this is a "round" extension
  if (run.round) {
      
      # loads the posteriors of the the previous emulator run
      temp <- pda.load.priors(settings, con, extension.check = TRUE) 
      prior.list <- temp$prior
      
      ## set prior distribution functions for posterior of the previous emulator run
      prior.fn <- lapply(prior.list, pda.define.prior.fn)
      
      ## Propose a percentage (if not specified 75%) of the new parameter knots from the posterior of the previous run
      knot.par        <- ifelse(!is.null(settings$assim.batch$knot.par), 
                                as.numeric(settings$assim.batch$knot.par), 
                                0.75)
      
      n.post.knots    <- floor(knot.par * settings$assim.batch$n.knot)
      
      knots.list.temp <- lapply(seq_along(settings$pfts),
                                function(x) pda.generate.knots(n.post.knots, 
                                                               n.param.all[x], 
                                                               prior.ind[[x]],
                                                               prior.fn[[x]],
                                                               pname[[x]]))
      knots.params.temp <- lapply(knots.list.temp, `[[`, "params")
      
      for (i in seq_along(settings$pfts)) {
        # mixture of knots
        knots.list[[i]]$params <- rbind(knots.params[[i]][sample(nrow(knots.params[[i]]), 
                                                                 (settings$assim.batch$n.knot - n.post.knots)), ], 
                                        knots.list.temp[[i]]$params)
        names(knots.list)[i] <- settings$pfts[[i]]['name']
      }
      

      # Convert parameter values to probabilities according to current PDA prior distribution
      knots.list$probs <- knots.list$params
      for (pft in seq_along(settings$pfts)) {
        for (i in seq_len(n.param.all[[pft]])) {
          knots.list[[pft]]$probs[, i] <- eval(prior.fn[[pft]]$pprior[[i]], 
                                               list(q = knots.list[[pft]]$params[, i]))
        }
      }
      
      knots.params <- lapply(knots.list, `[[`, "params")
      knots.probs  <- lapply(knots.list, `[[`, "probs")
      current.step <- "Generate Knots: round-if block"
      save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  } # end round-if block
  
  print("emulator names")
  print(sapply(settings$pfts,"[[",'name'))
  print(names(knots.list))
  print(names(knots.params))
  print(names(knots.probs))
  
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
      start.model.runs(settings, settings$database$bety$write)
    
      ## Retrieve model outputs and error statistics
      model.out <- list()
      pda.errors <- list()
    
    
      ## read model outputs    
      for (i in seq_len(settings$assim.batch$n.knot)) {
        model.out[[i]] <- pda.get.model.output(settings, run.ids[i], bety, inputs)
      }
      current.step <- "pda.get.model.output"
      save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
      
      # handle bias parameters if multiplicative Gaussian is listed in the likelihoods
      if(any(unlist(any.mgauss) == "multipGauss")) {
        # how many bias parameters per dataset requested
        nbias <- ifelse(is.null(settings$assim.batch$inputs[[isbias]]$nbias), 1,
                      as.numeric(settings$assim.batch$inputs[[isbias]]$nbias))
        bias.list <- return.bias(isbias, model.out, inputs, prior.list, nbias, run.round, settings$assim.batch$bias.path)
        bias.terms <- bias.list$bias.params
        prior.list <- bias.list$prior.list.bias
        prior.fn <- lapply(prior.list, pda.define.prior.fn)
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
  # keep this ind.all w/o bias until extracting prob values below 
  prior.ind.all <- which(unlist(pname) %in% unlist(settings$assim.batch$param.names))
  
    
  if (run.normal | run.round) {
    
    # retrieve n
    n.of.obs <- sapply(inputs,`[[`, "n") 
    names(n.of.obs) <- sapply(model.out[[1]],names)
      
    ## GPfit optimization routine assumes that inputs are in [0,1] Instead of drawing from parameters,
    ## we draw from probabilities
    knots.probs.all <- do.call("cbind", knots.probs)

    X <- knots.probs.all[, prior.ind.all, drop = FALSE]
      
    # retrieve SS
    error.statistics <- list()
    SS.list <- list()
    bc <- 1
    
    for(inputi in seq_len(n.input)){
      error.statistics[[inputi]] <- sapply(pda.errors,`[[`, inputi)
        
      if(unlist(any.mgauss)[inputi] == "multipGauss") {
          
          # if yes, then we need to include bias term in the emulator
          bias.probs <- bias.list$bias.probs
          biases <- c(t(bias.probs[[bc]]))
          bc <- bc + 1
            
          # replicate model parameter set per bias parameter
          rep.rows <- rep(1:nrow(X), each = nbias)
          X.rep <- X[rep.rows,]
          X <- cbind(X.rep, biases)
          colnames(X) <- c(colnames(X.rep), paste0("bias.", names(n.of.obs)[inputi]))
          SS.list[[inputi]] <- cbind(X, c(error.statistics[[inputi]]))

      } else {
          SS.list[[inputi]] <- cbind(X, error.statistics[[inputi]])
      } # if-block
        
      # remove failed runs
      SS.list[[inputi]] <- SS.list[[inputi]][!rowSums(is.na(SS.list[[inputi]])), ]
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
      
    logger.info(paste0("Using 'GPfit' package for Gaussian Process Model fitting."))
    ## Generate emulator on SS, return a list
    GPmodel <- lapply(SS, function(x) GPfit::GP_fit(X = x[, -ncol(x), drop = FALSE], Y = x[, ncol(x), drop = FALSE]))
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
  
  # add indice and increase n.param for bias
  if(any(unlist(any.mgauss) == "multipGauss")){
    prior.ind.all <- c(prior.ind.all, 
                       (prior.ind.all[length(prior.ind.all)]+1):(prior.ind.all[length(prior.ind.all)] + length(isbias)))
    n.param <- c(n.param, length(isbias))
  }

  
  ## Change the priors to unif(0,1) for mcmc.GP
  prior.all[prior.ind.all, ] <- rep(c("unif", 0, 1, "NA"), each = sum(n.param))

  ## Set up prior functions accordingly
  prior.fn.all <- pda.define.prior.fn(prior.all)
  
  # define range to make sure mcmc.GP doesn't propose new values outside
  rng <- matrix(c(sapply(prior.fn.all$qprior[prior.ind.all],
                         eval,
                         list(p = 0)), 
                  sapply(prior.fn.all$qprior[prior.ind.all], 
                         eval, 
                         list(p = 1))), nrow = sum(n.param))
  
  if (run.normal | run.round) {
    
    resume.list <- list()
    
    for (c in seq_len(settings$assim.batch$chain)) {
      jmp.list[[c]] <- sapply(prior.fn.all$qprior, 
                              function(x) 0.1 * diff(eval(x, list(p = c(0.05, 0.95)))))[prior.ind.all]
      jmp.list[[c]] <- sqrt(jmp.list[[c]])
      
      init.x <- lapply(prior.ind.all, function(v) eval(prior.fn.all$rprior[[v]], list(n = 1)))
      names(init.x) <- rownames(prior.all)[prior.ind.all]
      init.list[[c]] <- init.x
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
  
  # start the clock
  ptm.start <- proc.time()
  
  # prepare for parallelization
  dcores <- parallel::detectCores() - 1
  ncores <- min(max(dcores, 1), settings$assim.batch$chain)
  cl <- parallel::makeCluster(ncores, type="FORK")
  current.step <- "pre-MCMC"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
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
            resume.list = resume.list[[chain]]
    )
  })
  
  parallel::stopCluster(cl)
  current.step <- "post-MCMC"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  
  # Stop the clock
  ptm.finish <- proc.time() - ptm.start
  logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(settings$assim.batch$iter), " iterations."))
  
  
  mcmc.samp.list <- list()
  
  for (c in seq_len(settings$assim.batch$chain)) {
    
    m <- mcmc.out[[c]]$mcmc.samp
    
      ## Set the prior functions back to work with actual parameter range
      
      prior.all <- do.call("rbind", prior.list)
      prior.fn.all <- pda.define.prior.fn(prior.all)
      
      ## Convert probabilities back to parameter values
      for (i in seq_len(sum(n.param))) {
        m[, i] <- eval(prior.fn.all$qprior[prior.ind.all][[i]], 
                       list(p = mcmc.out[[c]]$mcmc.samp[, i]))
      }
    
    colnames(m) <- rownames(prior.all)[prior.ind.all]
    mcmc.samp.list[[c]] <- m
    
    # jmp.list[[c]] <- mcmc.out[[c]]$jump
    resume.list[[c]] <- mcmc.out[[c]]$chain.res
  }
  
  
  if (FALSE) {
    gp     <- kernlab.gp
    x0     <- init.x
    nmcmc  <- settings$assim.batch$iter
    rng    <- NULL
    format <- "lin"
    mix    <- "each"
    jmp0   <- apply(X, 2, function(x) 0.3 * diff(range(x)))
    jmp0   <- sqrt(unlist(settings$assim.batch$jump$jvar))
    ar.target <- settings$assim.batch$jump$ar.target
    priors <- prior.fn$dprior[prior.ind]
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

  
  # Separate each PFT's parameter samples (and bias term) to their own list
  mcmc.param.list <- list()
  ind <- 0
  for (i in seq_along(n.param)) {
    mcmc.param.list[[i]] <- lapply(mcmc.samp.list, function(x) x[, (ind + 1):(ind + n.param[i]), drop = FALSE])
    ind <- ind + n.param[i]
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
  
  settings <- pda.postprocess(settings, con, mcmc.param.list, pname, prior.list, prior.ind)
  
  ## close database connection
  if (!is.null(con)) {
    db.close(con)
  }
  
  ## Output an updated settings list
  current.step <- "pda.finish"
  save(list = ls(all.names = TRUE),envir=environment(),file=pda.restart.file)
  return(settings)
  
}  ## end pda.emulator
