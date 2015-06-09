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


  ## set up prior density (d) and random (r) functions
  prior.fn <- pda.define.prior.fn(prior)
  

  ## load data
  inputs <- load.pda.data(settings$assim.batch$inputs)
  n.input <- length(inputs)
  
  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)


  ## Load params from previous run, if provided. 
  if(!is.null(settings$assim.batch$params.id)) {
    params.db <- db.query(paste0("SELECT * FROM dbfiles WHERE id = ", params.id), con)
    load(file.path(params.db$file_path, params.db$file_name)) # replaces params
  }


  ## Allocate storage for params
  if(exists('params')) {  # Matrix of params was just loaded
    start  <- nrow(params) + 1
    finish <- nrow(params) + as.numeric(settings$assim.batch$iter)
    params <- rbind(params, matrix(NA, finish - start + 1, n.param.all))
  } else {              # No input given, starting fresh
    start  <- 1
    finish <- as.numeric(settings$assim.batch$iter)
    params <- matrix(NA, finish, n.param.all)
  }
  colnames(params) <- pname
  

  ## Propose parameter knots (X) for emulator design
    n.samp <- 10 # Number of values of each parameter to try  **** MOVE TO ARGUMENT

    probs <- matrix(0.5, nrow=n.samp, ncol=n.param.all) # By default, all parameters will be fixed at their median
  
    # Fill in parameters to be sampled with probabilities sampled in a LHC design
    probs[, prior.ind] <- lhc(t(matrix(0:1, ncol=n.param, nrow=2)), n.samp)

    # Convert probabilities to parameter values
    params <- NA*probs
    for(i in 1:n.param.all) {
      params[,i] <- eval(prior.fn$qprior[[i]], list(p=probs[,i]))
    }
    colnames(params) <- pname
  
  # *********************** LOOOOOOOOOOOOOOOOOOOOOOOOOOOOP!
  ## Run model at proposed X
  LL.X <- rep(NA, n.samp)
  for(i in 1:n.samp) {
    ## set RUN.ID
    if (!is.null(con)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      paramlist <- paste("Evaluating knot",i)
      db.query(
        paste(
          "INSERT INTO runs", 
            "(model_id, site_id, start_time, finish_time, outdir,",
            "created_at, ensemble_id, parameter_list)",
          "values ('", 
            settings$model$id, "','", settings$run$site$id, "','", settings$run$start.date, "','", 
            settings$run$end.date, "','", settings$run$outdir , "','", now, "',", ensemble.id, ",'", 
            paramlist, 
          "')", 
        sep=''), 
      con)
      run.id <- db.query(
        paste("SELECT id FROM runs WHERE created_at='", now, "' AND parameter_list='", paramlist, "'", 
        sep=''),
        con)[['id']]
    } else {
      run.id <- paste("Knot",i,sep=".")
    }
    dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
    dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)


    ## write config
    do.call(my.write.config,args=list(settings$pfts, list(pft=params[i,],env=NA), settings, run.id))


    ## write a README for the run
    cat("runtype     : pda.mcmc\n",
        "workflow id : ", as.character(workflow.id), "\n",
        "ensemble id : ", as.character(ensemble.id), "\n",
        "chain       : ", chain, "\n",
        "run         : ", i, "\n",
        "variable    : ", paste(pname[prior.ind], sep=", "), "\n",
        "run id      : ", as.character(run.id), "\n",
        "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
        "model       : ", settings$model$type, "\n",
        "model id    : ", settings$model$id, "\n",
        "site        : ", settings$run$site$name, "\n",
        "site  id    : ", settings$run$site$id, "\n",
        "met data    : ", settings$run$site$met, "\n",
        "start date  : ", settings$run$start.date, "\n",
        "end date    : ", settings$run$end.date, "\n",
        "hostname    : ", settings$run$host$name, "\n",
        "rundir      : ", file.path(settings$run$host$rundir, run.id), "\n",
        "outdir      : ", file.path(settings$run$host$outdir, run.id), "\n",
        file=file.path(settings$rundir, run.id, "README.txt"), sep='')


    ## add the job to the list of runs
    cat(as.character(run.id), file=file.path(settings$rundir, "runs.txt"), sep="\n", append=FALSE)


    ## start model run
    start.model.runs(settings,settings$database$bety$write)


    ## read model outputs
    # TODO: Generalize
    model.out <- list()
    for(k in 1:n.input){
      NEEm <- read.output(run.id, outdir = file.path(settings$run$host$outdir, run.id),
                          strftime(settings$run$start.date,"%Y"), 
                          strftime(settings$run$end.date,"%Y"), 
                          variables="NEE")$NEE*0.0002640674
      ## unit conversion kgC/ha/yr -> umolC/m2/sec
      # NPPvecm <-read.output(run.id, outdir = file.path(outdir, run.id),
      #                       start.year, end.year, variables="NPP")$NPP
      # NPPm<- sum(NPPvecm)


      ## match model and observations
      NEEm <- rep(NEEm,each= nrow(inputs[[k]]$data)/length(NEEm))
      set <- 1:length(NEEm)  ## ***** need a more intellegent year matching!!!
        # NPPm <- rep(NPPm,each=length(NPPo)/length(NPPm))
        # set <- 1:length(NPPm) 

      model.out[[k]] <- NEEm[set]
    }


    ## calculate likelihood
    LL.vec <- n.vec <- numeric(n.input)
    for(k in 1:n.input) {
      llik <- llik.fn[[k]](model.out[[k]], inputs[[k]])
      LL.vec[k] <- llik$LL
      n.vec[k]  <- llik$n
    }
    weights <- rep(1/n.input, n.input) # TODO: Implement user-defined weights
    LL.X[i] <- sum(LL.vec * weights)
    neff <- n.vec * weights


    ## insert Likelihood records in database
    if (!is.null(con)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      # BETY requires likelihoods to be associated with inputs, so only proceed 
      # for inputs with valid input ID (i.e., not the -1 dummy id). 
      # Note that analyses requiring likelihoods to be stored therefore require 
      # inputs to be registered in BETY first.
      db.input.ind <- which( sapply(inputs, function(x) x$input.id) != -1 )
      for(k in db.input.ind) {
        db.query(
          paste0("INSERT INTO likelihoods ", 
            "(run_id,            variable_id,                     input_id, ",
            " loglikelihood,     n_eff,                           weight,   ",
            " created_at) ",
          "values ('", 
              run.id, "', '",    inputs[[k]]$variable.id, "', '", inputs[[k]]$input.id, "', '", 
              LL.vec[k], "', '", floor(neff[k]), "', '",          weights[k] , "', '", 
              now,"')"
          ), 
        con)
      }
    }
  } # end loop over X



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


  dbfile.insert(dirname(filename.mcmc), basename(filename.mcmc), 'Posterior', posteriorid, con)
  params.id <- db.query(paste0(
    "SELECT id FROM dbfiles WHERE 
      container_type = 'Posterior' AND file_name = 'pda.mcmc.Rdata' AND
      container_id = ", posteriorid),con)

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


