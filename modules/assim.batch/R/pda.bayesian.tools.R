##' Paramater Data Assimilation using BayesianTools R Package
##'
##'
##' @title Paramater Data Assimilation using BayesianTools
##' @param settings = a pecan settings list
##'
##' @return nothing. Diagnostic plots, MCMC samples, and posterior distributions
##'  are saved as files and db records.
##'
##' @author Mike Dietze
##' @author Istem Fer, Ryan Kelly
##' @export
pda.bayesian.tools <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL, 
                       iter=NULL, adapt=NULL, adj.min=NULL, ar.target=NULL, jvar=NULL, n.knot=NULL) {
  
  # library(devtools)
  # install_url("https://dl.dropboxusercontent.com/s/hy9l6mokresqyel/BayesianTools_0.0.0.9000.tar.gz")
  library(BayesianTools)
  

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
  
  # Warn the user if jvar is not provided
  if(is.null(settings$assim.batch$jump$jvar)) logger.warn("Jump variances are not provided, default to 0.1 * 90% prior CI. If chains are not mixing well, provide smaller values for low acceptance rates and bigger values for high acceptance rates.")
  
  # Default jump variances. Looped for clarity
  ind <- which(is.na(settings$assim.batch$jump$jvar))
  for(i in seq_along(ind)) {
    # default to 0.1 * 90% prior CI
    settings$assim.batch$jump$jvar[[i]] <- 
      0.1 * diff(eval(prior.fn$qprior[[prior.ind[ind[i]]]], list(p=c(0.05,0.95))))
    logger.info(paste("--- current jvar for ",settings$assim.batch$param.names[[i]]," is ",settings$assim.batch$jump$jvar[[i]]," ---"))
  }
  
  ## Set initial conditions
  parm <- sapply(prior.fn$qprior,eval,list(p=0.5))
  names(parm) <- pname
  
  ## Create prior class object for BayesianTools
  bt.prior <- pda.create.btprior(prior[prior.ind,])
  
  ## Create log-likelihood function for createbayesianSetup{BayesianTools}
  bt.likelihood <- function(x, sum = T){
    parm[prior.ind]=x
    
    now <- format(Sys.time(), "%Y%m%d%H%M%S")
    
    run.id <- pda.init.run(settings, con, my.write.config, workflow.id, parm, n=1, run.names=paste("run", now, sep="."))
    
    ## Start model run
    start.model.runs(settings,settings$database$bety$write)
    
    ## Read model outputs
    model.out <- pda.get.model.output(settings, run.id, inputs)
    
    ## calculate likelihood
    LL <- pda.calc.llik(settings, con, model.out, run.id, inputs, llik.fn)

    return(LL)
  }
  
  ## Create bayesianSetup object for BayesianTools
  bayesianSetup = createBayesianSetup(bt.likelihood, bt.prior)
  
  ## Set starting values
  bayesianSetup$prior$best = parm[prior.ind]
  
  ## Generate proposal  
  
  # TODO: pass jump variances to proposalGenerator from settings
  # sqrt(unlist(settings$assim.batch$jump$jvar))
  # proposalGenerator <- createProposalGenerator(covariance = sqrt(c(settings$assim.batch$jump$jvar,0.000005)), message = T)

  # TODO: this should have the broadest options possible
  # bt.settings=list(iterations = settings$assim.batch$bt.settings$iter, proposalGenerator=proposalGenerator, adapt = settings$assim.batch$bt.settings$adapt, DRlevels = settings$assim.batch$bt.settings$DRlevels, gibbsProbabilities = settings$assim.batch$bt.settings$gibbsProbs, temperingFunction = NULL, optimize = settings$assim.batch$bt.settings$optim)

  bt.settings=list(iterations = as.numeric(settings$assim.batch$bt.settings$iter), optimize=F)
  out <- runMCMC(bayesianSetup = bayesianSetup, sampler = settings$assim.batch$bt.settings$sampler, settings = bt.settings)
  
  # save(out,file=file.path(settings$outdir, "out.Rda"))
       
  ## Create params matrix
  # *** TODO: Generalize to >1 chain
  if(settings$assim.batch$bt.settings$sampler=="Metropolis"){
    params <- matrix(rep(parm,nrow(out$chain)), nrow=nrow(out$chain), ncol=n.param.all, byrow=T)
    #colnames(params) <- pname
    params[, prior.ind] <- out$chain[,1]
  }

       
       
  ## ------------------------------------ Clean up ------------------------------------ ##
  ## Save outputs to plots, files, and db
  settings <- pda.postprocess(settings, con, params, pname, prior, prior.ind)
       
  ## close database connection
  if(!is.null(con)) db.close(con)
       
  ## Output an updated settings list
  return(settings)
       
} ## end pda.bayesian.tools
