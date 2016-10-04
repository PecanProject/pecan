##' Paramater Data Assimilation using MCMC with block sampling
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
pda.mcmc.bs <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL, 
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
  prior.list <- temp$prior
  settings <- temp$settings
  pname <-  lapply(prior.list, rownames)
  n.param.all  <- sapply(prior.list, nrow)

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

  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)
  
  prior.all <- do.call("rbind", prior.list)
  ## Set prior distribution functions (d___, q___, r___, and multivariate versions)
  prior.fn.all <- pda.define.prior.fn(prior.all)
  prior.ind.all <- which(unlist(pname) %in% unlist(settings$assim.batch$param.names))
  pname.all <- unlist(pname)
  


  ## ----------------------------------- MCMC Setup ----------------------------------- ##
  
  mcmc.list <- jcov.list <- list()
  
  for(chain in 1:settings$assim.batch$chain){
    ## Initialize empty params matrix (concatenated to params from a previous PDA, if provided)
    params.list <- pda.init.params(settings, chain, pname.all, sum(n.param.all))
    start  <- params.list$start
    finish <- params.list$finish
    params <- params.list$params
    
    ## File for temp storage of params (in case of crash)
    #  Using .txt here to allow quick append after each iteration (maybe a better way?)
    #  At the end of MCMC the entire object is saved as .Rdata
    #filename.mcmc.temp <- file.path(settings$outdir, "pda.mcmc.txt")
    
    ## Set initial conditions
    if(start==1) {
      parm <- sapply(prior.fn.all$qprior,eval,list(p=0.5))
    } else {
      parm <- params[start-1, ]
    }
    names(parm) <- pname.all
    LL.old <- prior.old <- -Inf
    
    ## Jump distribution setup
    accept.count <- 0
    
    # Default jump variances. 
    # default to 0.1 * 90% prior CI
    if(!is.null(settings$assim.batch$extension)) {
      load(settings$assim.batch$jcov.path) # load jcov
      jcov <- jcov.list[[chain]]
    }else{
      jmp.vars <- sapply(prior.fn.all$qprior, function(x) 0.1 * diff(eval(x, list(p=c(0.05,0.95)))))[prior.ind.all]
      jcov = diag(jmp.vars) 
    }
    
    ## Create dir for diagnostic output
    if(!is.null(settings$assim.batch$diag.plot.iter)) {
      dir.create(file.path(settings$outdir, paste0('diag.pda', settings$assim.batch$ensemble.id)),
                 showWarnings=F, recursive=T)
    }
    
    
    ## --------------------------------- Main MCMC loop --------------------------------- ##
    for(i in start:finish){
      logger.info(paste("Data assimilation MCMC iteration",i,"of",finish))
      
      ## Adjust Jump distribution
      if((i > (start + 1)) && ((i - start) %% settings$assim.batch$jump$adapt == 0)){
        jcov <- pda.adjust.jumps.bs(settings, jcov, accept.count, 
                                    params[(i - settings$assim.batch$jump$adapt):(i-1), prior.ind.all])
        accept.count <- 0 # Reset counter
        
        # Save updated settings XML. Will be overwritten at end, but useful in case of crash
        settings$assim.batch$jump$jvar <- as.list(diag(jcov))
        names(settings$assim.batch$jump$jvar) <- rep('jvar', n.param)
        saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 
                                                             paste0('pecan.pda', settings$assim.batch$ensemble.id, '.xml')))
      }
      
      pstar <- parm
      
      ## Propose parameter values
      if(i > 1) {
        pstar[prior.ind.all] <- mvrnorm(1, parm[prior.ind.all], jcov)
      }
      
      ## Check that value falls within the prior
      prior.star <- prior.fn.all$dmvprior(pstar)
      
      # Convert pstar to a list of 1-row data frame 
      if(is.null(dim(pstar))) {
        pnames <- names(pstar)
        run.params <- as.data.frame(matrix(pstar, nrow=1))
        names(run.params) <- pnames
      }
      run.params=list(run.params)
      
      
      if(is.finite(prior.star)){
        ## Set up run and write run configs
        run.id <- pda.init.run(settings, con, my.write.config, workflow.id, run.params, n=1,
                               run.names=paste0("MCMC_chain.",chain,"_iteration.",i))
        
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
          accept.count <- accept.count + 1
        }
      } ## end if(is.finite(prior.star))
      
      ## Diagnostic figure
      if(!is.null(settings$assim.batch$diag.plot.iter) && is.finite(prior.star) && 
         (i==start | i==finish | (i %% settings$assim.batch$diag.plot.iter == 0))) {
        pdf(file.path(settings$outdir, paste0('diag.pda', settings$assim.batch$ensemble.id),
                      paste0("data.vs.model_", gsub(" ", "0",sprintf("%5.0f", i)), ".pdf")))
        NEEo <- inputs[[1]]$obs
        
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
      
    } #end of mcmc-loop
    
    mcmc.list[[chain]] <- params
    jcov.list[[chain]] <- jcov
    
  }#end of chain-loop
  
  # save as it is for extension
  settings$assim.batch$mcmc.path <- file.path(settings$outdir, 
                                              paste0('mcmc.list.pda', settings$assim.batch$ensemble.id, '.Rdata'))
  save(mcmc.list, file = settings$assim.batch$mcmc.path)
  
  # subset to params of interst only
  mcmc.list <-lapply(mcmc.list, "[", , prior.ind.all, drop=FALSE)
  
  # Separate each PFT's parameter samples to their own list
  mcmc.param.list <- list()
  ind <- 0
  for(i in seq_along(settings$pfts)){
    mcmc.param.list[[i]] <-  lapply(mcmc.list, function(x) x[, (ind+1):(ind + n.param[i]), drop=FALSE])
    ind <- ind + n.param[i]
  }

  ## ------------------------------------ Clean up ------------------------------------ ##
  ## Save outputs to plots, files, and db
  settings$assim.batch$jcov.path <- file.path(settings$outdir, 
                                              paste0('jcov.pda', settings$assim.batch$ensemble.id, '.Rdata'))
  save(jcov.list, file=settings$assim.batch$jcov.path)
  
  settings <- pda.postprocess(settings, con, mcmc.param.list, pname, prior.list, prior.ind)

  ## close database connection
  if(!is.null(con)) db.close(con)

  ## Output an updated settings list
  return(settings)
  
} ## end pda.mcmc
