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
  
  ## -------------------------------------- Setup ------------------------------------- 
  ## Handle settings
  settings <- pda.settings(
    settings=settings, params.id=params.id, param.names=param.names, 
    prior.id=prior.id, chain=chain, iter=iter, adapt=adapt, 
    adj.min=adj.min, ar.target=ar.target, jvar=jvar, n.knot=n.knot)
  
  ## if which package to use for creating the Gaussian Process is not specified, default to GPfit
  if (is.null(settings$assim.batch$GPpckg)) {
    settings$assim.batch$GPpckg <- "GPfit"
  }
  
  extension.check <- settings$assim.batch$extension == "longer"
  
  if (length(extension.check) == 0) {
    # not an extension run
    run.block <- TRUE
    path.flag <- TRUE
  } else if (length(extension.check) == 1 & extension.check == FALSE) {
    # 'round' extension
    run.block <- TRUE
    path.flag <- FALSE
  } else {
    # 'longer' extension
    run.block <- FALSE
    path.flag <- FALSE
  }
  
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

  bety <- betyConnect("~/pecan/web/config.php")
  
  ## Load priors
  temp        <- pda.load.priors(settings, bety$con, path.flag)
  prior.list  <- temp$prior
  settings    <- temp$settings
  pname       <- lapply(prior.list, rownames)
  n.param.all <- sapply(prior.list, nrow)
  
  ## Load data to assimilate against
  inputs      <- load.pda.data(settings, bety)
  n.input     <- length(inputs)
  
  ## Set model-specific functions
  do.call("require", list(paste0("PEcAn.", settings$model$type)))
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
  
  knots.params <- lapply(knots.list, `[[`, "params")
  knots.probs <- lapply(knots.list, `[[`, "probs")
  
  ## Check which emulator extension type requested if any
  if (!is.null(settings$assim.batch$extension)) {
    
    if (settings$assim.batch$extension == "round") {
      
      # save the original prior path
      temp.path <- settings$assim.batch$prior$path
      
      # set prior path to NULL to use the previous PDA's posterior densities as new priors this time
      settings$assim.batch$prior$path <- NULL
      
      ## Re-load priors
      temp <- pda.load.priors(settings, con)  # loads the posterior dist. from previous emulator run
      prior.list <- temp$prior
      settings$assim.batch$prior$path <- temp.path
      
      ## Re-set prior distribution functions
      prior.fn <- lapply(prior.list, pda.define.prior.fn)
      
      ## Propose a percentage of the new parameter knots from the posterior of previous run
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
      }
      
      # Return to original prior distribution
      temp       <- pda.load.priors(settings, con)
      prior.list <- temp$prior
      prior.fn   <- lapply(prior.list, pda.define.prior.fn)
      
      # Convert parameter values to probabilities according to previous prior distribution
      knots.list$probs <- knots.list$params
      for (pft in seq_along(settings$pfts)) {
        for (i in seq_len(n.param.all[[pft]])) {
          knots.list[[pft]]$probs[, i] <- eval(prior.fn[[pft]]$pprior[[i]], 
                                               list(q = knots.list[[pft]]$params[, i]))
        }
      }
      
      knots.params <- lapply(knots.list, `[[`, "params")
      knots.probs  <- lapply(knots.list, `[[`, "probs")
      
    }  # end of round-if
  }  # end of extension-if
  
  if (run.block) {
    ## Set up runs and write run configs for all proposed knots
    run.ids <- pda.init.run(settings, con, my.write.config, workflow.id, knots.params, 
                            n = settings$assim.batch$n.knot, 
                            run.names = paste0(settings$assim.batch$ensemble.id, 
                                               ".knot.",
                                               1:settings$assim.batch$n.knot))    
    ## start model runs
    start.model.runs(settings, settings$database$bety$write)
    
    # ## Retrieve model outputs, calculate likelihoods (and store them in database)
    # LL.0 <- rep(NA, settings$assim.batch$n.knot)
    model.out <- list()
    pda.errors <- list()
    
    
    ## read model outputs    
    for (i in seq_len(settings$assim.batch$n.knot)) {
      model.out[[i]] <- pda.get.model.output(settings, run.ids[i], bety, inputs)
    }
    
    if(any(unlist(any.mgauss) == "multipGauss")) {
      isbias <- which(unlist(any.mgauss) == "multipGauss")
      bias.list <- return.bias(isbias, model.out, inputs, prior.list)
      bias.terms <- bias.list$bias.params
      prior.list <- bias.list$prior.list
      prior.fn <- lapply(prior.list, pda.define.prior.fn)
    } else {
      bias.terms <- matrix(1, nrow = settings$assim.batch$n.knot, ncol = 1) # just 1 for Gaussian
    }
    
    for (i in seq_len(settings$assim.batch$n.knot)) {
      ## calculate error statistics      
      pda.errors[[i]] <- pda.calc.error(settings, con, model_out = model.out[[i]], run.id = run.ids[i], inputs, bias.terms[i,])
    } 
      # # ## calculate likelihood
      # LL.0[i] <- pda.calc.llik(pda.errors[[i]]$pda.error)
      # 
  }
  

  
  init.list <- list()
  jmp.list <- list()
  
  if (settings$assim.batch$GPpckg == "GPfit") {
    # GPfit-if
    
    if (run.block) {
      
      ## GPfit optimization routine assumes that inputs are in [0,1] Instead of drawing from parameters,
      ## we draw from probabilities
      knots.probs.all <- do.call("cbind", knots.probs)
      prior.ind.all <- which(unlist(pname) %in% unlist(settings$assim.batch$param.names))
      
      X <- knots.probs.all[, prior.ind.all, drop = FALSE]
    
     
      # retrieve n
      n.of.obs <- sapply(inputs,`[[`, "n") 
      # retrieve SS
      estats <-lapply(pda.errors, function(x) sapply(x,`[[`, "statistics"))

      # retrieve SS
      error.statistics <- list()
      SS.list <- list()
      
      for(iknot in seq_len(n.input)){
        error.statistics[[iknot]] <- sapply(estats,`[[`, iknot)
        
        if(iknot == isbias){
          
          # if yes, then we need to include bias term in the emulator
          bias.probs <- bias.list$bias.probs
          biases <- c(t(bias.probs))
          
          # replicate model parameter set per bias parameter
          rep.rows <- rep(1:nrow(X), each = 3) # three for 3 bias params, leaving hard-coded for now
          X.rep <- X[rep.rows,]
          X <- cbind(X.rep, biases)
          
          SS.list[[iknot]] <- cbind(X, c(error.statistics[[iknot]]))
          
          # add indice and increase n.param for bias
          prior.ind.all <- c(prior.ind.all, prior.ind.all[length(prior.ind.all)]+1)
          n.param <- c(n.param, 1)
          
        } else{
          SS.list[[iknot]] <- cbind(X, error.statistics[[iknot]])
        } # if-block
        
      } # for-loop


      
      if (!is.null(settings$assim.batch$extension)) {
        # check whether another 'round' of emulator requested
        
        # load original knots
        load(settings$assim.batch$llik.path)
        # TODO: loop over SS.list
        # add on
        SS <- rbind(SS.X, SS)
        
      } else {
        SS <- SS.list
      }
      
      logger.info(paste0("Using 'GPfit' package for Gaussian Process Model fitting."))
      library(GPfit)
      ## Generate emulator on SS, return a list
      GPmodel <- lapply(SS, function(x) GP_fit(X = x[, -ncol(x), drop = FALSE], Y = x[, ncol(x), drop = FALSE]))
      gp <- GPmodel
      
    } else {
      load(settings$assim.batch$emulator.path)  # load previously built emulator to run a longer mcmc
      load(settings$assim.batch$llik.path)
      load(settings$assim.batch$resume.path)
      # load(settings$assim.batch$mcmc.path)
      
      prior.all <- do.call("rbind", prior.list)
      prior.ind.all <- which(unlist(pname) %in% unlist(settings$assim.batch$param.names))
      prior.fn.all <- pda.define.prior.fn(prior.all)
      
      
      for (c in seq_len(settings$assim.batch$chain)) {
        init.list[[c]] <- resume.list[[c]]$prev.samp[nrow(resume.list[[c]]$prev.samp), ]
        jmp.list[[c]] <- resume.list[[c]]$jump
      }
    }
    
    ## Change the priors to unif(0,1) for mcmc.GP
    prior.all <- do.call("rbind", prior.list)
    prior.all[prior.ind.all, ] <- rep(c("unif", 0, 1, "NA"), each = sum(n.param))

    ## Set up prior functions accordingly
    prior.fn.all <- pda.define.prior.fn(prior.all)
    pckg <- 1
    
  } else {
    # GPfit-else
    
    if (run.block) {
      X <- data.frame(knots.params[, prior.ind])
      names(X) <- pname[prior.ind]
      
      LL.X <- data.frame(LLik = LL.0, X)
      
      if (!is.null(settings$assim.batch$extension)) {
        # check whether another 'round' of emulator requested
        
        # load original knots
        load(settings$assim.batch$llik.path)
        LL <- rbind(LL.X, LL)
        
      } else {
        LL <- LL.X
      }
      
      logger.info(paste0("Using 'kernlab' package for Gaussian Process Model fitting."))
      library(kernlab)
      ## Generate emulator on LL-params
      kernlab.gp <- gausspr(LLik ~ ., data = LL)
      gp <- kernlab.gp
    } else {
      load(settings$assim.batch$emulator.path)
    }
    
    pckg <- 2
  }
  
  # define range to make sure mcmc.GP doesn't propose new values outside
  
  rng <- matrix(c(sapply(prior.fn.all$qprior[prior.ind.all],
                         eval,
                         list(p = 0)), 
                  sapply(prior.fn.all$qprior[prior.ind.all], 
                         eval, 
                         list(p = 1))), nrow = sum(n.param))
  
  if (run.block) {
    
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
  
  ## Sample posterior from emulator
  mcmc.out <- lapply(1:settings$assim.batch$chain, function(chain) {
    mcmc.GP(gp          = gp, ## Emulator(s)
            pckg        = pckg, ## flag to determine which predict method to use
            x0          = init.list[[chain]],     ## Initial conditions
            nmcmc       = settings$assim.batch$iter,       ## Number of reps
            rng         = rng,       ## range
            format      = "lin",      ## "lin"ear vs "log" of LogLikelihood 
            mix         = mix,     ## Jump "each" dimension independently or update them "joint"ly
            jmp0        = jmp.list[[chain]],  ## Initial jump size
            ar.target   = settings$assim.batch$jump$ar.target,   ## Target acceptance rate
            priors      = prior.fn.all$dprior[prior.ind.all], ## priors
            settings    = settings,
            run.block   = run.block,  
            n.of.obs    = n.of.obs,
            llik.fn     = llik.fn,
            resume.list = resume.list[[chain]]
    )
  })
  
  mcmc.list <- list()
  
  for (c in seq_len(settings$assim.batch$chain)) {
    
    m <- mcmc.out[[c]]$mcmc
    
    if (settings$assim.batch$GPpckg == "GPfit") {
      ## Set the prior functions back to work with actual parameter range
      
      prior.all <- do.call("rbind", prior.list)
      prior.fn.all <- pda.define.prior.fn(prior.all)
      
      ## Convert probabilities back to parameter values
      for (i in seq_len(sum(n.param))) {
        m[, i] <- eval(prior.fn.all$qprior[prior.ind.all][[i]], 
                       list(p = mcmc.out[[c]]$mcmc[, i]))
      }
    }
    colnames(m) <- unlist(pname)[prior.ind.all]
    mcmc.list[[c]] <- m
    
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
  ## Save emulator, outputs files
  settings$assim.batch$emulator.path <- file.path(settings$outdir,
                                                  paste0("emulator.pda", 
                                                         settings$assim.batch$ensemble.id, 
                                                         ".Rdata"))
  save(gp, file = settings$assim.batch$emulator.path)
  
  settings$assim.batch$llik.path <- file.path(settings$outdir, 
                                              paste0("llik.pda", 
                                                     settings$assim.batch$ensemble.id, 
                                                     ".Rdata"))
  save(LL, file = settings$assim.batch$llik.path)
  
  settings$assim.batch$mcmc.path <- file.path(settings$outdir, 
                                              paste0("mcmc.list.pda", 
                                                     settings$assim.batch$ensemble.id, 
                                                     ".Rdata"))
  save(mcmc.list, file = settings$assim.batch$mcmc.path)
  
  settings$assim.batch$resume.path <- file.path(settings$outdir, 
                                                paste0("resume.pda", 
                                                       settings$assim.batch$ensemble.id, 
                                                       ".Rdata"))
  save(resume.list, file = settings$assim.batch$resume.path)
  
  # Separate each PFT's parameter samples to their own list
  mcmc.param.list <- list()
  ind <- 0
  for (i in seq_along(settings$pfts)) {
    mcmc.param.list[[i]] <- lapply(mcmc.list, function(x) x[, (ind + 1):(ind + n.param[i]), drop = FALSE])
    ind <- ind + n.param[i]
  }
  
  settings <- pda.postprocess(settings, con, mcmc.param.list, pname, prior.list, prior.ind)
  
  ## close database connection
  if (!is.null(con)) {
    db.close(con)
  }
  
  ## Output an updated settings list
  return(settings)
  
}  ## end pda.emulator
