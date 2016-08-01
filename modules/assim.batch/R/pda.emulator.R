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
pda.emulator <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL, 
                     iter=NULL, adapt=NULL, adj.min=NULL, ar.target=NULL, jvar=NULL, n.knot=NULL, burnin=NULL) {
  
  ## this bit of code is useful for defining the variables passed to this function 
  ## if you are debugging
  if(FALSE){
    params.id <- param.names <- prior.id <- chain <- iter <- NULL 
    n.knot <- adapt <- adj.min <- ar.target <- jvar <- burnin <- NULL
  }

  ## -------------------------------------- Setup ------------------------------------- ##
  ## Handle settings
    settings <- pda.settings(
                  settings=settings, params.id=params.id, param.names=param.names, 
                  prior.id=prior.id, chain=chain, iter=iter, adapt=adapt, 
                  adj.min=adj.min, ar.target=ar.target, jvar=jvar, n.knot=n.knot, burnin=burnin)

   burnin <- ifelse(!is.null(settings$assim.batch$burnin), 
                    as.numeric(settings$assim.batch$burnin), 
                    ceiling(min(2000,0.2*settings$assim.batch$iter)))
   
  ## Open database connection
  if(settings$database$bety$write){
    con <- try(db.open(settings$database$bety), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    con <- NULL
  }

  ## Load priors
  temp <- pda.load.priors(settings, con)
  prior <- temp$prior
  settings <- temp$settings
  pname <-  rownames(prior) 
  n.param.all  <- nrow(prior)

  ## Load data to assimilate against
  inputs <- load.pda.data(settings, con)
  n.input <- length(inputs)

  ## Set model-specific functions
  do.call("require",list(paste0("PEcAn.", settings$model$type)))
  my.write.config <- paste("write.config.", settings$model$type,sep="")
  if(!exists(my.write.config)){
    logger.severe(paste(my.write.config,"does not exist. Please make sure that the PEcAn interface is loaded for", settings$model$type))
  }

  ## Select parameters to constrain
  prior.ind <- which(rownames(prior) %in% settings$assim.batch$param.names)
  n.param <- length(prior.ind)

  ## Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }

  ## Create an ensemble id
  settings$assim.batch$ensemble.id <- pda.create.ensemble(settings, con, workflow.id)

  ## Set prior distribution functions (d___, q___, r___, and multivariate versions)
  prior.fn <- pda.define.prior.fn(prior)

  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)


  ## ------------------------------------ Emulator ------------------------------------ ##
  ## Propose parameter knots (X) for emulator design
  knots.list <- pda.generate.knots(settings$assim.batch$n.knot, n.param.all, prior.ind, prior.fn, pname)
  knots.params <- knots.list$params
  knots.probs <- knots.list$probs
  
  
  ## Check which emulator extension type requested if any
  if(!is.null(settings$assim.batch$extension)){
    
    if(settings$assim.batch$extension == "round"){
      
      # save the original prior path
      temp.path = settings$assim.batch$prior$path
      
      # set prior path to NULL to use the previous PDA's posterior densities as new priors this time
      settings$assim.batch$prior$path = NULL
      
      ## Re-load priors
      temp <- pda.load.priors(settings, con) # loads the posterior dist. from previous emulator run
      prior <- temp$prior
      settings$assim.batch$prior$path = temp.path
      
      ## Re-set prior distribution functions 
      prior.fn <- pda.define.prior.fn(prior)
      
      ## Propose half of the new parameter knots (X) from the posterior of previous run
      n.knots.post <- floor(0.75*settings$assim.batch$n.knot)
      knots.list.temp <- pda.generate.knots(n.knots.post, n.param.all, prior.ind, prior.fn, pname)
      knots.params.temp <- knots.list.temp$params
      knots.probs.temp <- knots.list.temp$probs
      
      knots.list$params <- rbind(knots.params[sample(nrow(knots.params), (settings$assim.batch$n.knot-n.knots.post)),], 
                                 knots.list.temp$params)
      knots.list$probs <- rbind(knots.probs[sample(nrow(knots.probs), (settings$assim.batch$n.knot-n.knots.post),], 
                                knots.list.temp$probs)
      
      knots.params <- knots.list$params
      knots.probs <- knots.list$probs
      
    } # end of round-if
  } # end of extension-if
  
  
  
  extension.check <- settings$assim.batch$extension == "longer"
  
  if(length(extension.check)==0){
    ## Set up runs and write run configs for all proposed knots X
    run.ids <- pda.init.run(settings, con, my.write.config, workflow.id, knots.params, 
                            n=settings$assim.batch$n.knot, 
                            run.names=paste0(settings$assim.batch$ensemble.id,".knot.",1:settings$assim.batch$n.knot))
    
    ## start model runs
    start.model.runs(settings,settings$database$bety$write)
    
    ## Retrieve model outputs, calculate likelihoods (and store them in database)
    LL.0 <- rep(NA, settings$assim.batch$n.knot)
    model.out <- list()
    
    for(i in 1:settings$assim.batch$n.knot) {
      ## read model outputs
      model.out[[i]] <- pda.get.model.output(settings, run.ids[i], inputs)
      
      ## calculate likelihood
      LL.0[i] <- pda.calc.llik(settings, con, model.out[[i]], run.ids[i], inputs, llik.fn)
    }
  } 
  
  
  
  ## if it is not specified, default to GPfit
  if(is.null(settings$assim.batch$GPpckg)) settings$assim.batch$GPpckg="GPfit"
  
  if(settings$assim.batch$GPpckg=="GPfit"){
    
    if(length(extension.check)==0){
      ## GPfit optimization routine assumes that inputs are in [0,1]
      ## Instead of drawing from parameters, we draw from probabilities
      X <- knots.probs[, prior.ind, drop=FALSE]
      
      LL.X <- cbind(X, LL.0)
      
      if(!is.null(settings$assim.batch$extension)){
        
        # load original knots
        if(settings$assim.batch$extension == "round"){
          load(settings$assim.batch$llik.path)
        }
        
        LL <- rbind(LL.X, LL)
      }else{
        LL <- LL.X
      }
      
      logger.info(paste0("Using 'GPfit' package for Gaussian Process Model fitting."))
      require(GPfit)
      ## Generate emulator on LL-probs
      GPmodel <- GP_fit(X = LL[,-ncol(LL), drop=FALSE],
                        Y = LL[,ncol(LL), drop=FALSE])
      gp=GPmodel
    } else{
      load(settings$assim.batch$emulator.path)
    }
    
    ## Change the priors to unif(0,1) for mcmc.GP
    prior[prior.ind,]=rep(c("unif",0,1,"NA"),each=n.param)
    ## Set up prior functions accordingly
    prior.fn <- pda.define.prior.fn(prior)
    pckg=1
    
  } else{
    
    if(length(extension.check)==0){
      X <- data.frame(knots.params[, prior.ind])
      names(X) <- pname[prior.ind]
      df <- data.frame(LL = LL.X, X)
      
      logger.info(paste0("Using 'kernlab' package for Gaussian Process Model fitting."))
      require(kernlab)
      ## Generate emulator on LL-params
      kernlab.gp <- gausspr(LL~., data=df)
      gp=kernlab.gp
    }else{
      load(settings$assim.batch$emulator.path)
    }
    pckg=2
  }
  
  # define range to make sure mcmc.GP doesn't propose new values outside 
  
  rng <- matrix(c(sapply(prior.fn$qprior[prior.ind] ,eval,list(p=0)),
                  sapply(prior.fn$qprior[prior.ind] ,eval,list(p=1))),
                nrow=n.param)
  
  
  jvar.list <- list() 
  init.list <- list()
  
  for(c in 1:settings$assim.batch$chain){
    jvar.list[[c]]  <- sapply(prior.fn$qprior, 
                              function(x) 0.1 * diff(eval(x, list(p=c(0.05,0.95)))))[prior.ind]
    
    init.x <- lapply(prior.ind, function(v) eval(prior.fn$rprior[[v]], list(n=1)))
    names(init.x) <- pname[prior.ind]
    init.list[[c]] <- init.x
  }
  
  if(length(extension.check)==1){
    load(settings$assim.batch$jvar.path)
    load(settings$assim.batch$mcmc.path)
    
    for(c in 1:settings$assim.batch$chain){
      init.list[[c]] <- as.list(mcmc.list[[c]][nrow(mcmc.list[[c]]),])
    }
  }
  
  
  if(!is.null(settings$assim.batch$mix)){
    mix <- settings$assim.batch$mix
  }else if(n.param > 1){
    mix <- "joint"
  }else{
    mix <- "each"
  } 
  

  ## Sample posterior from emulator
  mcmc.out <- lapply(1:settings$assim.batch$chain, function(chain){
    mcmc.GP(gp        = gp, ## Emulator
            pckg      = pckg, ## flag to determine which predict method to use
            x0        = init.list[[chain]],     ## Initial conditions
            nmcmc     = settings$assim.batch$iter,       ## Number of reps
            rng       = rng,       ## range
            format    = "lin",      ## "lin"ear vs "log" of LogLikelihood 
            mix       = mix,     ## Jump "each" dimension independently or update them "joint"ly
            #                  jmp0 = apply(X,2,function(x) 0.3*diff(range(x))), ## Initial jump size
            jmp0      = jvar.list[[chain]],  ## Initial jump size
            ar.target = settings$assim.batch$jump$ar.target,   ## Target acceptance rate
            priors    = prior.fn$dprior[prior.ind], ## priors
            settings  = settings
    )})
  
  mcmc.list.tmp <- list()
  
  for(c in 1:settings$assim.batch$chain) {
    
    m <- mcmc.out[[c]]$mcmc
    
    
    if(settings$assim.batch$GPpckg=="GPfit"){
      ## Set the prior functions back to work with actual parameter range
      prior <- temp$prior
      prior.fn <- pda.define.prior.fn(prior)
      
      ## Convert probabilities back to parameter values
      for(i in 1:n.param) {
        m[,i] <- eval(prior.fn$qprior[prior.ind][[i]], list(p=mcmc.out[[c]]$mcmc[,i]))
      }
    }
    colnames(m) <- pname[prior.ind]
    mcmc.list.tmp[[c]] <- m
    
    jvar.list[[c]] <- mcmc.out[[c]]$jump@history[nrow(mcmc.out[[c]]$jump@history),]
  }
  
  if(length(extension.check)==1){
    # merge with previous run's mcmc samples
    mcmc.list <- mapply(c, mcmc.list, mcmc.list.tmp, SIMPLIFY=FALSE)
  }else{
    mcmc.list <- mcmc.list.tmp
  }
  
  if(FALSE) {
    gp = kernlab.gp; x0 = init.x; nmcmc = settings$assim.batch$iter; rng= NULL; format = "lin"
    mix = "each"; jmp0 = apply(X,2,function(x) 0.3*diff(range(x)))
    jmp0 = sqrt(unlist(settings$assim.batch$jump$jvar)); ar.target = settings$assim.batch$jump$ar.target
    priors = prior.fn$dprior[prior.ind]
  }


  ## ------------------------------------ Clean up ------------------------------------ ##
  ## Save outputs to plots, files, and db
  settings <- pda.postprocess(settings, con, mcmc.list, jvar.list, pname, prior, prior.ind, burnin)

  ## close database connection
  if(!is.null(con)) db.close(con)

  ## Output an updated settings list
  return(settings)
  
} ## end pda.emulator
