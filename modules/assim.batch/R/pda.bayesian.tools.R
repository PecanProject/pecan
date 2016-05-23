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
  
  library(BayesianTools)
  
  sampler = settings$assim.batch$bt.settings$sampler
  

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
  
  ## NOTE: The listed samplers here require more than 1 parameter for now because of the way their cov is calculated 
  if(sampler %in% c("M","AM","DR","DRAM") & n.param<2) logger.error(paste0(sampler, " sampler can be used with >=2 paramaters"))

  
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
  
  ## Set initial conditions
  parm <- sapply(prior.fn$qprior,eval,list(p=0.5))
  names(parm) <- pname
  
  ## Create prior class object for BayesianTools
  bt.prior <- pda.create.btprior(prior[prior.ind,])
  
  ## Create log-likelihood function for createbayesianSetup{BayesianTools}
  
  bt.likelihood <- function(x){
    parm[prior.ind]=x
    
    now <- format(Sys.time(), "%Y%m%d%H%M%OS3")
    
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
  
  
  ## Set starting values and numPars
  bayesianSetup$prior$best = parm[prior.ind]
  bayesianSetup$numPars=n.param
  

  logger.info(paste0("Extracting upper and lower boundaries from priors."))
  rng=matrix(c(sapply(prior.fn$qprior[prior.ind] ,eval,list(p=0.00001)), # M/AM/DR/DRAM can't work with -Inf, Inf values
               sapply(prior.fn$qprior[prior.ind] ,eval,list(p=0.99999))),
               nrow=n.param)
      
  bayesianSetup$prior$lower=rng[,1]
  bayesianSetup$prior$upper=rng[,2]

  ## Apply BayesianTools specific settings
  bt.settings=pda.settings.bt(settings)
  
  ## central function in BayesianTools
  out <- runMCMC(bayesianSetup = bayesianSetup, sampler = sampler, settings = bt.settings)
  
  # save the out object for further inspection
  save(out, file=file.path(settings$outdir, paste0(sampler, '_out.Rdata')))
  
  ## Create params matrix
  # *** TODO: Generalize to >1 chain, DREAM has 3 chains
  
  samples=getSample(out, parametersOnly = T) # getSample{BayesianTools}
  n.row=length(samples)/n.param
  params <- matrix(rep(parm,n.row), nrow=n.row, ncol=n.param.all, byrow=T)
  params[, prior.ind] <- samples
  

  ## ------------------------------------ Clean up ------------------------------------ ##
  ## Save outputs to plots, files, and db
  settings <- pda.postprocess(settings, con, params, pname, prior, prior.ind)
       
  ## close database connection
  if(!is.null(con)) db.close(con)
       
  ## Output an updated settings list
  return(settings)
       
} ## end pda.bayesian.tools
