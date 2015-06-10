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
                     iter=NULL, adapt=NULL, adj.min=NULL, ar.target=NULL, jvar=NULL) {
  # Quit if pda not requested in settings
  if(!('assim.batch' %in% names(settings))) {
    return()
  }

  require(coda)
  
  ## this bit of code is useful for defining the variables passed to this function 
  ## if you are debugging
  if(FALSE){
    params.id <- param.names <- prior.id <- chain <- iter <- NULL 
    adapt <- adj.min <- ar.target <- jvar <- NULL
  }


  ## settings
    settings <- pda.settings(
                  settings=settings, params.id=params.id, param.names=param.names, 
                  prior.id=prior.id, chain=chain, iter=iter, adapt=adapt, 
                  adj.min=adj.min, ar.target=ar.target, jvar=jvar)


  ## open database connection
  if(settings$database$bety$write){
    con <- try(db.open(settings$database$bety), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    con <- NULL
  }


  ## priors
  prior <- pda.load.priors(settings, con)
  pname <-  rownames(prior) 
  n.param.all  <- nrow(prior)


  ## Select parameters to constrain
  prior.ind <- which(rownames(prior) %in% settings$assim.batch$param.names)
  n.param <- length(prior.ind)


  ## Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }


  ## create an ensemble id
  ensemble.id <- pda.create.ensemble(settings, con, workflow.id)


  ## model-specific functions
  do.call("require",list(paste0("PEcAn.", settings$model$type)))
  my.write.config <- paste("write.config.", settings$model$type,sep="")
  if(!exists(my.write.config)){
    logger.severe(paste(my.write.config,"does not exist. Please make sure that the PEcAn interface is loaded for", settings$model$type))
  }


  ## Set up prior distribution functions (d___, q___, r___, and multivariate versions)
  prior.fn <- pda.define.prior.fn(prior)


  ## load data
  inputs <- load.pda.data(settings$assim.batch$inputs)
  n.input <- length(inputs)


  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)


  ## Initialize empty params matrix (concatenated to params from a previous PDA, if provided)
  params <- pda.init.params(settings, con, pname, n.param.all)
    start  <- params$start
    finish <- params$finish
    params <- params$params

  
  ## File for temp storage of params (in case of crash)
  #  Using .txt here to allow quick append after each iteration (maybe a better way?)
  #  At the end of MCMC the entire object is saved as .Rdata
  filename.mcmc.temp <- file.path(settings$outdir, "pda.mcmc.txt")


  ## set initial conditions
  if(start==1){
    parm <- sapply(prior.fn$qprior,eval,list(p=0.5))
  } else{
    parm <- params[start-1, ]
  }
  names(parm) <- pname
  LL.old <- -Inf
  prior.old <- -Inf


  ## Jump distribution setup
  accept.rate <- numeric(n.param)  ## Create acceptance rate vector of 0's (one zero per parameter)


  ## main MCMC loop
  for(i in start:finish){
    logger.info(paste("Data assimilation MCMC iteration",i,"of",finish))

    ## Adjust Jump distribution
    if(i %% settings$assim.batch$jump$adapt < 1){
      settings <- pda.adjust.jumps(settings, accept.rate, pnames=pname[prior.ind])
      accept.rate <- numeric(n.param)
    }

    for(j in 1:n.param){
      ## propose parameter values
      pnew  <- rnorm(1,parm[prior.ind[j]],settings$assim.batch$jump$jvar[j])
      pstar <- parm
      pstar[prior.ind[j]] <- pnew


      ## check that value falls within the prior
      prior.star <- prior.fn$dmvprior(pstar)
      if(is.finite(prior.star)){
        ## Set up run and write run configs
        run.id <- pda.init.run(settings, con, my.write.config, workflow.id, ensemble.id, pstar, n=1,
                               run.names=paste0("MCMC_chain.",chain,"_iteration.",i,"_variable.",j))

        ## start model run
        start.model.runs(settings,settings$database$bety$write)

        ## read model outputs
        model.out <- pda.get.model.output(settings, run.id, inputs)
    
        ## calculate likelihood (and store in database)
        LL.new <- pda.calc.llik(settings, con, model.out, inputs, llik.fn)

        ## accept or reject step
        a <- LL.new - LL.old + prior.star - prior.old
        if(a > log(runif(1))){
          LL.old <- LL.new
          prior.old <- prior.star
          parm <- pstar 
          accept.rate[j] <- accept.rate[j] + 1
        }
      } ## end if(is.finite(prior.star))
    } ## end loop over variables

    ## save output
    params[i,] <- parm
    if(i == 1){
      cat(c(parm,'\n'), file=filename.mcmc.temp, sep='\t', append=F)
    } else {
      cat(c(parm,'\n'), file=filename.mcmc.temp, sep='\t', append=T)
    }
  } ## end MCMC loop


  ## Save raw MCMC
  filename.mcmc <- file.path(settings$outdir, "pda.mcmc.Rdata")
  save(params, file = filename.mcmc)


  ## Assess MCMC output
  # *** TODO: Generalize for multiple PFTS
  pdf(file.path(settings$pfts$pft$outdir,"pda.mcmc.diagnostics.pdf"))

  burnin <- min(2000,0.2*nrow(params))
  params.subset <- as.data.frame(params[burnin:nrow(params),prior.ind])
    names(params.subset) <- pname[prior.ind]
  dm <- as.mcmc(params.subset)

  plot(dm)
  summary(dm)
  if(length(prior.ind)>1){
    crosscorr(dm)
    pairs(params.subset)
  }


  dev.off()


  ## create a new Posteriors DB entry
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  pft.id <- db.query(paste0(
    "SELECT id from pfts where name = '", settings$pfts$pft$name,"'"), con)

  db.query(paste0(
    "INSERT INTO posteriors (pft_id, created_at, updated_at) VALUES (", 
    pft.id, ", '", now, "', '", now, "')"), con)

  posteriorid <- db.query(paste0(
    "SELECT id FROM posteriors WHERE pft_id=", pft.id, " AND created_at='", now, "'"), con)[['id']]

  settings$assim.batch$params.id <- dbfile.insert(
    dirname(filename.mcmc), basename(filename.mcmc), 'Posterior', posteriorid, con, reuse=TRUE)


  ## save named distributions
  # *** TODO: Generalize for multiple PFTS
  filename <- file.path(settings$pfts$pft$outdir, 'post.distns.Rdata')
  post.distns <- approx.posterior(params.subset, prior, outdir = settings$pfts$pft$outdir)
  save(post.distns, file = filename)
  dbfile.insert(dirname(filename), basename(filename), 'Posterior', posteriorid, con)


  ## coerce parameter output into the same format as trait.mcmc
  # ********* TODO: Check/fix... do we really want to save mcmc for only the parameters that were updated???
  pname <- rownames(post.distns)
  trait.mcmc <- list()
  for(i in prior.ind){
    beta.o <- array(params[,i],c(nrow(params),1))
    colnames(beta.o) <- "beta.o"
    if(pname[i] %in% names(trait.mcmc)) {
      trait.mcmc[[pname[i]]] <- mcmc.list(as.mcmc(beta.o))
    } else {
      k <- length(trait.mcmc) + 1
      trait.mcmc[[k]] <- mcmc.list(as.mcmc(beta.o))
      names(trait.mcmc)[k] <- pname[i]      
    }
  }


  ## save updated parameter distributions as trait.mcmc so that they can be read by the ensemble code
  # *** TODO: Generalize for multiple PFTS
  filename <- file.path(settings$pfts$pft$outdir, 'trait.mcmc.Rdata')
  save(trait.mcmc, file = filename)
  dbfile.insert(dirname(filename), basename(filename), 'Posterior', posteriorid, con)


  ## close database connection
  if(!is.null(con)) db.close(con)


  ## Output an updated settings list
  return(settings$assim.batch)
  
} ## end pda.mcmc

