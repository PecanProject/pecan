##' Paramater Data Assimilation using MCMC
##'
##' Brute-force, only to be used on simple models
##'
##' @title Paramater Data Assimilation using MCMC
##' @param settings = a pecan settings list
##'
##' @return nothing. Diagnostic plots, MCMC samples, and posterior distributions
##'  are saved as files and db records.
##'
##' @author Mike Dietze
##' @author Ryan Kelly
##' @export
pda.emulator <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL, 
                     iter=NULL, adapt=NULL, adj.min=NULL, ar.target=NULL, jvar=NULL, n.knot=NULL) {
  
  ## this bit of code is useful for defining the variables passed to this function 
  ## if you are debugging
  if(FALSE){
    params.id <- param.names <- prior.id <- chain <- iter <- NULL 
    n.knot <- adapt <- adj.min <- ar.target <- jvar <- NULL
  }

  ## -------------------------------------- Setup ------------------------------------- ##
  ## Handle settings
    settings <- pda.settings(
                  settings=settings, params.id=params.id, param.names=param.names, 
                  prior.id=prior.id, chain=chain, iter=iter, adapt=adapt, 
                  adj.min=adj.min, ar.target=ar.target, jvar=jvar, n.knot=n.knot)

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

  # Default jump variances. Looped for clarity
  ind <- which(is.na(settings$assim.batch$jump$jvar))
  for(i in seq_along(ind)) {
    # default to 0.1 * 90% prior CI
    settings$assim.batch$jump$jvar[[i]] <- 
      0.1 * diff(eval(prior.fn$qprior[[prior.ind[ind[i]]]], list(p=c(0.05,0.95))))
  }

  ## ------------------------------------ Emulator ------------------------------------ ##
  ## Propose parameter knots (X) for emulator design
  knots.list <- pda.generate.knots(settings$assim.batch$n.knot, n.param.all, prior.ind, prior.fn, pname)
  knots.params <- knots.list$params
  knots.probs <- knots.list$probs
  
  
  ## Set up runs and write run configs for all proposed knots X
  run.ids <- pda.init.run(settings, con, my.write.config, workflow.id, knots.params, 
                          n=settings$assim.batch$n.knot, 
                          run.names=paste0("Knot.",1:settings$assim.batch$n.knot))

  ## start model runs
  start.model.runs(settings,settings$database$bety$write)

  ## Retrieve model outputs, calculate likelihoods (and store them in database)
  LL.X <- rep(NA, settings$assim.batch$n.knot)
  for(i in 1:settings$assim.batch$n.knot) {
    ## read model outputs
    model.out <- pda.get.model.output(settings, run.ids[i], inputs)

    ## calculate likelihood
    LL.X[i] <- pda.calc.llik(settings, con, model.out, run.ids[i], inputs, llik.fn)
  }

  ## Collect all likelihoods (Y)
  # For now, just the likelihoods from the runs we just did. 
  # *** TODO: Soon, need to add ability to retrieve them from previous runs, because we want to build the emulator from as many points as possible. The likelihoods are easy—they're being stored in BETY. But we need to know the parameter values associated with them too, and that will take a bit of doing.
 
  ## if it is not specified, default to GPfit
  if(is.null(settings$assim.batch$GPpckg)) settings$assim.batch$GPpckg="GPfit"

  if(settings$assim.batch$GPpckg=="GPfit"){
    ## GPfit optimization routine assumes that inputs are in [0,1]
    ## Instead of drawing from parameters, we draw from probabilities
    X <- knots.probs[, prior.ind]

    logger.info(paste0("Using 'GPfit' package for Gaussian Process Model fitting."))
    require(GPfit)
    ## Generate emulator on LL-probs
    GPmodel <- GP_fit(X=X,Y=LL.X)
    gp=GPmodel
    
    ## Change the priors to unif(0,1) for mcmc.GP
    prior[prior.ind,]=rep(c("unif",0,1,"NA"),each=n.param)
    ## Set up prior functions accordingly
    prior.fn <- pda.define.prior.fn(prior)
    pckg=1
  } else{
    X <- data.frame(knots.params[, prior.ind])
    df <- data.frame(LL = LL.X, X)
    
    logger.info(paste0("Using 'kernlab' package for Gaussian Process Model fitting."))
    require(kernlab)
    ## Generate emulator on LL-params
    kernlab.gp <- gausspr(LL~., data=df)
    gp=kernlab.gp
    pckg=2
  }
  
  # define range to make sure mcmc.GP doesn't propose new values outside 
  
  rng=matrix(c(apply(X,2,min), apply(X ,2,max)),nrow=n.param)
        

  ## Sample posterior from emulator
  m <- lapply(1, function(chain){
         init.x <- lapply(prior.ind, function(v) eval(prior.fn$rprior[[v]], list(n=1)))
         names(init.x) <- pname[prior.ind]
         mcmc.GP(gp        = gp, ## Emulator
                 pckg      = pckg, ## flag to determine which predict method to use
                 x0        = init.x,     ## Initial conditions
                 nmcmc     = settings$assim.batch$iter,       ## Number of reps
                 rng       = rng,       ## range
                 format    = "lin",      ## "lin"ear vs "log" of LogLikelihood 
                 mix       = "each",     ## Jump "each" dimension independently or update them "joint"ly
#                  jmp0 = apply(X,2,function(x) 0.3*diff(range(x))), ## Initial jump size
                 jmp0      = sqrt(unlist(settings$assim.batch$jump$jvar)),  ## Initial jump size
                 ar.target = settings$assim.batch$jump$ar.target,   ## Target acceptance rate
                 priors    = prior.fn$dprior[prior.ind] ## priors
          )$mcmc
        })
  
  mcmc.out=m[[1]]
  
  if(settings$assim.batch$GPpckg=="GPfit"){
    ## Set the prior functions back to work with actual parameter range
    prior <- temp$prior
    prior.fn <- pda.define.prior.fn(prior)
    
    ## Convert probabilities back to parameter values
    for(i in 1:n.param) {
      mcmc.out[,i] <- eval(prior.fn$qprior[prior.ind][[i]], list(p=m[[1]][,i]))
    }
  }


  if(FALSE) {
    gp = kernlab.gp; x0 = init.x; nmcmc = settings$assim.batch$iter; rng= NULL; format = "lin"
    mix = "each"; jmp0 = apply(X,2,function(x) 0.3*diff(range(x)))
    jmp0 = sqrt(unlist(settings$assim.batch$jump$jvar)); ar.target = settings$assim.batch$jump$ar.target
    priors = prior.fn$dprior[prior.ind]
  }

  ## Create params matrix
  # *** TODO: Generalize to >1 chain
  params <- matrix(knots.params[1,], nrow=nrow(mcmc.out), ncol=n.param.all, byrow=T)
  params[, prior.ind] <- mcmc.out


  ## ------------------------------------ Clean up ------------------------------------ ##
  ## Save outputs to plots, files, and db
  settings <- pda.postprocess(settings, con, params, pname, prior, prior.ind)

  ## close database connection
  if(!is.null(con)) db.close(con)

  ## Output an updated settings list
  return(settings)
  
} ## end pda.emulator
