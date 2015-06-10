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


  ## Handle settings
    settings <- pda.settings(
                  settings=settings, params.id=params.id, param.names=param.names, 
                  prior.id=prior.id, chain=chain, iter=iter, adapt=adapt, 
                  adj.min=adj.min, ar.target=ar.target, jvar=jvar)

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

  ## Create an ensemble id
  ensemble.id <- pda.create.ensemble(settings, con, workflow.id)

  ## Set model-specific functions
  do.call("require",list(paste0("PEcAn.", settings$model$type)))
  my.write.config <- paste("write.config.", settings$model$type,sep="")
  if(!exists(my.write.config)){
    logger.severe(paste(my.write.config,"does not exist. Please make sure that the PEcAn interface is loaded for", settings$model$type))
  }

  ## Set prior distribution functions (d___, q___, r___, and multivariate versions)
  prior.fn <- pda.define.prior.fn(prior)
  
  ## Load data to assimilate against
  inputs <- load.pda.data(settings$assim.batch$inputs)
  n.input <- length(inputs)

  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)

  ## Initialize empty params matrix (concatenated to params from a previous PDA, if provided)
  params <- pda.init.params(settings, con, pname, n.param.all)
    start  <- params$start
    finish <- params$finish
    params <- params$params

  ## Propose parameter knots (X) for emulator design
  params <- pda.generate.knots(n.knot, n.param.all, prior.ind, prior.fn)

  

  ## Set up runs and write run configs for all proposed knots X
  run.ids <- pda.init.run(settings, con, my.write.config, workflow.id, ensemble.id, params, 
                          n=n.knot, run.names=paste0("Knot.",1:n.knot))


  ## start model runs
  start.model.runs(settings,settings$database$bety$write)


  ## Retrieve model outputs, calculate likelihoods (and store them in database)
  LL.X <- rep(NA, n.knot)
  for(i in 1:n.knot) {
    ## read model outputs
    model.out <- pda.get.model.output(settings, run.ids[i], inputs)

    ## calculate likelihood
    LL.X[i] <- pda.calc.llik(settings, con, model.out, inputs, llik.fn)
  }


  ## Collect all likelihoods (Y)
  # For now, just the likelihoods from the runs we just did. 
  # *** TODO: Soon, need to add ability to retrieve them from previous runs, because we want to build the emulator from as many points as possible. The likelihoods are easy—they're being stored in BETY. But we need to know the parameter values associated with them too, and that will take a bit of doing.
  X <- data.frame(params[, prior.ind])
    names(X) <- pname[prior.ind]
  Y <- LL.X
  

  ## Generate emulator on (X,Y)
  require(kernlab)
  df <- data.frame(LL = Y, X)
  kernlab.gp <- gausspr(LL~., data=df)
  
  
  ## Sample posterior from emulator

  m<-lapply(1, function(chain){
        init.x <- lapply(prior.ind, function(v) eval(prior.fn$rprior[[v]], list(n=1)))
        names(init.x) <- pname[prior.ind]
        mcmc.GP(kernlab.gp, ## emulator
            init.x,  ## initial conditions
            nmcmc = 2000,      ## number of reps
            NULL,   ## 'rng' -- is not used except to specify default jmp0, which is included below
            "lin",      ## "lin"ear vs "log" of LogLikelihood 
            mix = "each", ## jump "each" dimension independently or update them "joint"ly
            jmp0 = apply(X,2,function(x) 0.3*diff(range(x))),
            priors=prior.fn$dprior[prior.ind]
        )$mcmc
      })


  ## Create params matrix
  # *** TODO: Generalize to >1 chain
  params.modeled <- params # rename for clarity
  params.emulated <- matrix(params[1,], nrow=nrow(m[[1]]), ncol=ncol(params), byrow=T)
  params.emulated[, prior.ind] <- m[[1]]
  params <- rbind(params, params.emulated)
  

  ## Save params
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


  ## Output an updates settings list
  return(settings$assim.batch)
  
} ## end pda.mcmc


