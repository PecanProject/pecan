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
pda.mcmc <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL, 
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


  ## ----------------------------------- MCMC Setup ----------------------------------- ##
  ## Initialize empty params matrix (concatenated to params from a previous PDA, if provided)
  params <- pda.init.params(settings, con, pname, n.param.all)
    start  <- params$start
    finish <- params$finish
    params <- params$params

  ## File for temp storage of params (in case of crash)
  #  Using .txt here to allow quick append after each iteration (maybe a better way?)
  #  At the end of MCMC the entire object is saved as .Rdata
  filename.mcmc.temp <- file.path(settings$outdir, "pda.mcmc.txt")

  ## Set initial conditions
  if(start==1) {
    parm <- sapply(prior.fn$qprior,eval,list(p=0.5))
  } else {
    parm <- params[start-1, ]
  }
  names(parm) <- pname
  LL.old <- prior.old <- -Inf

  ## Jump distribution setup
  accept.rate <- numeric(n.param)  ## Create acceptance rate vector of 0's (one zero per parameter)

  # Default jump variances. Looped for clarity
  ind <- which(is.na(settings$assim.batch$jump$jvar))
  for(i in seq_along(ind)) {
    # default to 0.1 * 90% prior CI
    settings$assim.batch$jump$jvar[[i]] <- 
      0.1 * diff(eval(prior.fn$qprior[[prior.ind[ind[i]]]], list(p=c(0.05,0.95))))
  }

  ## Create dir for diagnostic output
  if(!is.null(settings$assim.batch$diag.plot.iter)) {
    dir.create(file.path(settings$outdir, paste0('diag.pda', settings$assim.batch$ensemble.id)),
      showWarnings=F, recursive=T)
  }

  ## save updated settings XML. Will be overwritten at end, but useful in case of crash
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 
    paste0('pecan.pda', settings$assim.batch$ensemble.id, '.xml')))

  ## --------------------------------- Main MCMC loop --------------------------------- ##
  for(i in start:finish){
    logger.info(paste("Data assimilation MCMC iteration",i,"of",finish))

    ## Adjust Jump distribution
    if(i %% settings$assim.batch$jump$adapt < 1){
      settings <- pda.adjust.jumps(settings, accept.rate, pnames=pname[prior.ind])
      accept.rate <- numeric(n.param)

      # Save updated settings XML. Will be overwritten at end, but useful in case of crash
      saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 
        paste0('pecan.pda', settings$assim.batch$ensemble.id, '.xml')))
    }

    for(j in 1:n.param){
      pstar <- parm
      
      ## Propose parameter values
      if(i > 1) {
        pnew  <- rnorm(1, parm[prior.ind[j]], settings$assim.batch$jump$jvar[[j]])
        pstar[prior.ind[j]] <- pnew
      }

      ## Check that value falls within the prior
      prior.star <- prior.fn$dmvprior(pstar)
      if(is.finite(prior.star)){
        ## Set up run and write run configs
        run.id <- pda.init.run(settings, con, my.write.config, workflow.id, pstar, n=1,
                               run.names=paste0("MCMC_chain.",chain,"_iteration.",i,"_variable.",j))

        ## Start model run
        start.model.runs(settings,settings$database$bety$write)

        ## Read model outputs
        model.out <- pda.get.model.output(settings, run.id, inputs)
    
        ## Calculate likelihood (and store in database)
        LL.new <- pda.calc.llik(settings, con, model.out, run.id, inputs, llik.fn)

        ## Accept or reject step
        a <- LL.new - LL.old + prior.star - prior.old
        if(is.na(a)) a <- -Inf  # Can occur if LL.new == -Inf (due to model crash) and LL.old == -Inf (first run)

        if(a > log(runif(1))){
          LL.old <- LL.new
          prior.old <- prior.star
          parm <- pstar 
          accept.rate[j] <- accept.rate[j] + 1
        }
      } ## end if(is.finite(prior.star))
    } ## end loop over variables

    ## Diagnostic figure
    if(!is.null(settings$assim.batch$diag.plot.iter) && is.finite(prior.star) && 
        (i==start | i==finish | (i %% settings$assim.batch$diag.plot.iter == 0))) {
      pdf(file.path(settings$outdir, paste0('diag.pda', settings$assim.batch$ensemble.id),
        paste0("data.vs.model_", gsub(" ", "0",sprintf("%5.0f", i)), ".pdf")))
        NEEo <- inputs[[1]]$NEEo

        NEEm <- model.out[[1]]
        NEE.resid <- NEEm - NEEo

        par(mfrow=c(1,2))
        plot(NEEo)
        points(NEEm, col=2, cex=0.5)
        legend("topleft", col=c(1,2), pch=1, legend=c("data","model"))
        hist(NEE.resid, 100, main=paste0("LLik: ", round(LL.new,1)))
      dev.off()
    }

    ## Store output
    params[i,] <- parm
    
    ## Add to temp file (overwrite when i=1, append thereafter)
    cat(c(parm,'\n'), file=filename.mcmc.temp, sep='\t', append=(i != 1))
  }


  ## ------------------------------------ Clean up ------------------------------------ ##
  ## Save outputs to plots, files, and db
  settings <- pda.postprocess(settings, con, params, pname, prior, prior.ind)

  ## close database connection
  if(!is.null(con)) db.close(con)

  ## Output an updated settings list
  return(settings)
  
} ## end pda.mcmc