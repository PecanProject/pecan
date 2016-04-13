##' Run Batch PDA
##'
##' @title Run Batch PDA
##' @param settings a PEcAn settings list
##'
##' @return Updated settings list
##'
##' @author Ryan Kelly
##' @export
assim.batch <- function(settings) {
  # Quit if pda not requested in settings
  if(!('assim.batch' %in% names(settings))) {
    return(settings)
  }
  require(coda)

  if(is.null(settings$assim.batch$method)) settings$assim.batch$method = "bruteforce.bs"
  
  if(settings$assim.batch$method == "bruteforce") {
    settings <- pda.mcmc(settings)
  } else if(settings$assim.batch$method == "bruteforce.bs") {
    settings <- pda.mcmc.bs(settings)
  } else if(settings$assim.batch$method == "emulator") {
    settings <- pda.emulator(settings)
  } else {
    logger.error(paste0("PDA method ", settings$assim.batch$method, " not found!"))
  }
    
  return(settings)
}




##' Set PDA Settings
##'
##' @title Set PDA Settings
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return An updated settings list
##'
##' @author Ryan Kelly
##' @export
pda.settings <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL,
                         iter=NULL, adapt=NULL, adj.min=NULL, ar.target=NULL, jvar=NULL, n.knot=NULL) {
  # Some settings can be supplied via settings (for automation) or explicitly (interactive). 
  # An explicit argument overrides whatever is in settings, if anything.
  # If neither an argument or a setting is provided, set a default value in settings. 
  
  # Each assignment below includes an explicit type conversion to avoid problems later. 

 
  # params.id: Either null or an ID used to query for a matrix of MCMC samples later
  if(!is.null(params.id)) {
    settings$assim.batch$params.id <- params.id
  }
  if(!is.null(settings$assim.batch$params.id)) {
    settings$assim.batch$params.id <- as.character(settings$assim.batch$params.id)
  }


  # param.names: Names of parameters to assimilate against
  if(!is.null(param.names)) {
    settings$assim.batch$param.names <- param.names
  }
  if(is.null(settings$assim.batch$param.names)) {
    logger.error('Parameter data assimilation requested, but no parameters specified for PDA')
  } else {
    settings$assim.batch$param.names <- as.list(as.character(settings$assim.batch$param.names))
  }
  # have to add names or listToXml() won't work
  names(settings$assim.batch$param.names) <- rep("param", length(settings$assim.batch$param.names))
  # Finally, check that none of the names listed are specified as pft constants
  constant.names <- unlist(sapply(settings$pfts, function(x) names(x$constants)))
  params.in.constants <- which(unlist(settings$assim.batch$param.names) %in% constant.names)
  if(length(params.in.constants) > 0) {
    logger.severe(paste0("PDA requested for parameter(s) [",
      paste(settings$assim.batch$param.names[params.in.constants], collapse=", "), 
      "] but these parameters are specified as constants in pecan.xml!"))
  }
  

  # prior: Either null or an ID used to query for priors later
  if(!is.null(prior.id)) {
    settings$assim.batch$prior$posterior.id <- prior.id
  }
  if(!is.null(settings$assim.batch$prior$posterior.id)) {
    settings$assim.batch$prior$posterior.id <- as.character(settings$assim.batch$prior$posterior.id)
  }


  # chain: An identifier for the MCMC chain. Currently not used for anything but a label.
  if(!is.null(chain)) {
    settings$assim.batch$chain <- chain
  }
  if(is.null(settings$assim.batch$chain)) {   # Default
    settings$assim.batch$chain <- 1
  }
  settings$assim.batch$chain <- as.numeric(settings$assim.batch$chain)


  # iter: Number of MCMC iterations. 
  if(!is.null(iter)) {
    settings$assim.batch$iter <- iter
  }
  if(is.null(settings$assim.batch$iter)) {   # Default
    settings$assim.batch$iter <- 100
  }
  settings$assim.batch$iter <- as.numeric(settings$assim.batch$iter)


  # n.knot: Number of emulator knots
  if(!is.null(n.knot)) {
    settings$assim.batch$n.knot <- n.knot
  }
  if(is.null(settings$assim.batch$n.knot)) {   # Default
    settings$assim.batch$n.knot <- 100
  }
  settings$assim.batch$n.knot <- as.numeric(settings$assim.batch$n.knot)



  # ----- Jump distribution / tuning parameters
  # adapt: How often to adapt the MCMC. Defaults to iter/10
  if(!is.null(adapt)) {
    settings$assim.batch$jump$adapt <- adapt
  }
  if(is.null(settings$assim.batch$jump$adapt)) {   # Default
    settings$assim.batch$jump$adapt <- floor(settings$assim.batch$iter/10)
  }
  settings$assim.batch$jump$adapt <- as.numeric(settings$assim.batch$jump$adapt)


  # adj.min: minimum amount to reduce jump distribution by. 
  if(!is.null(adj.min)) {   # Default
    settings$assim.batch$jump$adj.min <- adj.min
  }
  if(is.null(settings$assim.batch$jump$adj.min)) {   # Default
    settings$assim.batch$jump$adj.min <- 0.1
  }
  settings$assim.batch$jump$adj.min <- as.numeric(settings$assim.batch$jump$adj.min)
  
  
  # ar.target: Target acceptance rate. Can be a single value of vector, one for each variable assimilated against. 
  if(!is.null(ar.target)) {
    settings$assim.batch$jump$ar.target <- ar.target
  }
  if(is.null(settings$assim.batch$jump$ar.target)) {   # Default
    settings$assim.batch$jump$ar.target <- 0.5
  }
  settings$assim.batch$jump$ar.target <- as.numeric(settings$assim.batch$jump$ar.target)


  # jvar: Initial jump variances. Defaults to NA to be based on priors later. 
  if(!is.null(jvar)) {
    settings$assim.batch$jump$jvar <- jvar
  } 
  if(is.null(settings$assim.batch$jump$jvar)) {   # Default
    settings$assim.batch$jump$jvar <- rep(NA, length(settings$assim.batch$param.names))
  }
  settings$assim.batch$jump$jvar <- as.list(as.numeric(settings$assim.batch$jump$jvar))
  # have to add names or listToXml() won't work
  names(settings$assim.batch$jump$jvar) <- rep("jvar", length(settings$assim.batch$jump$jvar))

  # diag.plot.iter: How often to do diagnostic plots. Just need to convert to numeric. 
  if(!is.null(settings$assim.batch$diag.plot.iter)) {
    settings$assim.batch$diag.plot.iter <- as.numeric(settings$assim.batch$diag.plot.iter)
  } 

  return(settings)
}


##' Load Priors for Paramater Data Assimilation
##'
##' @title Load Priors for Paramater Data Assimilation
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A previously-generated posterior distribution, to be used as the prior for PDA.
##'
##' @author Ryan Kelly
##' @export
pda.load.priors <- function(settings, con) {
  # Load a prior.distns or post.distns file directly by path
  if(!is.null(settings$assim.batch$prior$path)) {
    if(file.exists(settings$assim.batch$prior$path)) load(settings$assim.batch$prior$path)
    if(exists("prior.distns")) {
      logger.info(paste0("Loaded prior ", basename(settings$assim.batch$prior$path), " as PDA prior."))
      prior.out <- prior.distns
    } else if(exists("post.distns")) {
      logger.info(paste0("Loaded posterior ", basename(settings$assim.batch$prior$path), " as PDA prior."))
      prior.out <- post.distns
    } else {
      logger.warn("Didn't find a valid PDA prior at ", settings$assim.batch$prior$path)
    }
  }

  # If no path given or didn't find a valid prior, proceed to using a posterior specified by ID, either as specified in settings or get the most recent as default
  if(!exists("prior.out")) {
    if(is.null(settings$assim.batch$prior$posterior.id)){
      logger.info(paste0("Defaulting to most recent posterior as PDA prior."))
      ## by default, use the most recent posterior as the prior
      pft.id <-  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
      priors <-  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)

      prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id IN (", paste(priors$id, collapse=','), ")"),con)

      prior.db <- prior.db[ grep("^post\\.distns\\..*Rdata$", prior.db$file_name),]

      settings$assim.batch$prior$posterior.id <- prior.db$container_id[which.max(prior.db$updated_at)]
    }
    logger.info(paste0("Using posterior ID ", settings$assim.batch$prior$posterior.id, " as PDA prior."))
    prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ", settings$assim.batch$prior$posterior.id),con)
    prior.db <- prior.db[ grepl("^post\\.distns\\..*Rdata$", prior.db$file_name),]

    # Load the file
    load(file.path(prior.db$file_path, prior.db$file_name))
    prior.out <- post.distns
  }
  
  # Finally, check that PDA parameters requested are in the prior; can't assimilate them if not.
  # Could proceed with any valid params. But probably better to just bonk out now to avoid wasting
  # a lot of time in case the mis-specified parameter(s) is really important to the analysis. 
  params.no.priors <- which(is.na(match(settings$assim.batch$param.names, rownames(prior.out))))
  if(length(params.no.priors) > 0) {
    logger.severe(paste0("PDA requested for parameter(s) [",
      paste(settings$assim.batch$param.names[params.no.priors], collapse=", "), 
      "] but no prior found!"))
  }
  
  return(list(prior=prior.out, settings=settings))
}



##' Create PDA Ensemble
##'
##' @title Create ensemble record for PDA ensemble
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return Ensemble ID of the created ensemble
##'
##' @author Ryan Kelly
##' @export
pda.create.ensemble <- function(settings, con, workflow.id) {
  if (!is.null(con)) {
    # Identifiers for ensemble 'runtype'
    if(settings$assim.batch$method == "bruteforce" | settings$assim.batch$method == "bruteforce.bs") {
      ensemble.type <- "pda.MCMC"
    } else if(settings$assim.batch$method == "emulator") {
      ensemble.type <- "pda.emulator"
    }
    
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                   now, "', '", ensemble.type,"', ", workflow.id, ")", sep=''), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep=''), con)[['id']]
  } else {
    ensemble.id <- NA
  }
  
  return(ensemble.id)
}


##' Define PDA Prior Functions
##'
##' @title Define PDA Prior Functions
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return List of prior functions containing dprior, rprior, qprior, dmvprior, rmvprior.
##'         Each of these is a list with one distribution function per parameter.
##'
##' @author Ryan Kelly
##' @export
pda.define.prior.fn <- function(prior) {
  n.param.all <- nrow(prior)
  dprior <- rprior <- qprior <-list()
  for(i in 1:n.param.all){
    if(prior$distn[i] == 'exp'){
      dprior[[i]] <- parse(text=paste("dexp(x,",prior$parama[i],",log=TRUE)",sep=""))
      rprior[[i]] <- parse(text=paste("rexp(n,",prior$parama[i],")",sep=""))
      qprior[[i]] <- parse(text=paste("qexp(p,",prior$parama[i],")",sep=""))
    }else{
      dprior[[i]] <- parse(text=paste("d",prior$distn[i],"(x,",prior$parama[i],",",prior$paramb[i],",log=TRUE)",sep=""))
      rprior[[i]] <- parse(text=paste("r",prior$distn[i],"(n,",prior$parama[i],",",prior$paramb[i],")",sep=""))
      qprior[[i]] <- parse(text=paste("q",prior$distn[i],"(p,",prior$parama[i],",",prior$paramb[i],")",sep=""))
    }
  }
  dmvprior <- function(x,log=TRUE){  #multivariate prior - density
    p <- rep(NA,n.param.all)
    for(i in 1:n.param.all){
      p[i] <- eval(dprior[[i]],list(x=x[i]))
    }
    p = sum(p)
    if(log) return(p)
    return(exp(p))
    return(p)
  }
  rmvprior <- function(n){  #multivariate prior - random number
    p <- matrix(NA,n,n.param.all)
    for(i in 1:n.param.all){
      p[,i] <- eval(rprior[[i]],list(n=n))
    }
    return(p)
  }
  
  return(list(dprior=dprior, rprior=rprior, qprior=qprior, dmvprior=dmvprior, rmvprior=rmvprior))
}


##' Define PDA Likelihood Functions
##'
##' @title Define PDA Likelihood Functions
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return List of likelihood functions, one for each dataset to be assimilated against.
##'
##' @author Ryan Kelly
##' @export
pda.define.llik.fn <- function(settings) {
  # *** TODO: Generalize!
  # Currently just returns a single likelihood, assuming the data are flux NEE.
  llik.fn <- list()
  for(i in 1:length(settings$assim.batch$input)) {
    llik.fn[[i]] <- function(NEEm, obs) {
      # NEE + heteroskedastic Laplace likelihood
      if(settings$assim.batch$inputs[[i]]$variable.id == 297 && 
         settings$assim.batch$inputs[[i]]$likelihood == "Laplace") {
        llik.fn[[i]] <- function(NEEm, obs) {
          NEE.resid <- abs(NEEm - obs$NEEo)
          NEE.pos <- (NEEm >= 0)
          LL <- c(dexp(NEE.resid[NEE.pos], 1/(obs$b0 + obs$bp*NEEm[NEE.pos]), log=TRUE), 
                  dexp(NEE.resid[!NEE.pos],1/(obs$b0 + obs$bn*NEEm[!NEE.pos]),log=TRUE))
          return(list(LL=sum(LL,na.rm=TRUE), n=sum(!is.na(LL))))
        }
      } else {
        # Default to Normal(0,1)
        llik.fn[[i]] <- function(model.out, obs.data) {
          LL <- dnorm(model.out - obs.data$data, log=TRUE)
          return(list(LL=sum(LL,na.rm=TRUE), n=sum(!is.na(LL))))
        }
      }
  }

  return(llik.fn)
}


##' Initialise Parameter Matrix for PDA
##'
##' @title Initialise Parameter Matrix for PDA
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A list containing 'start' and 'finish' counters for MCMC, as well as the params
##'         table, which is an empty matrix concatenated to any param samples from a previous
##'         PDA run, if provided. 
##'
##' @author Ryan Kelly
##' @export
pda.init.params <- function(settings, con, pname, n.param.all) {
  ## Load params from previous run, if provided. 
  if(!is.null(settings$assim.batch$params.id)) {
    params.db <- db.query(paste0("SELECT * FROM dbfiles WHERE id = ", settings$assim.batch$params.id), con)
    load(file.path(params.db$file_path, params.db$file_name)) # loads params

    start  <- nrow(params) + 1
    finish <- nrow(params) + as.numeric(settings$assim.batch$iter)
    params <- rbind(params, matrix(NA, finish - start + 1, n.param.all))
  } else {              # No input given, starting fresh
    start  <- 1
    finish <- as.numeric(settings$assim.batch$iter)
    params <- matrix(NA, finish, n.param.all)
  }
  colnames(params) <- pname
  
  return(list(start=start, finish=finish, params=params))
}


##' Initialise Model Runs for PDA
##'
##' @title Initialise Model Runs for PDA
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return Vector of run IDs for all model runs that were set up (including write.configs)
##'
##' @author Ryan Kelly
##' @export
pda.init.run <- function(settings, con, my.write.config, workflow.id, params, 
                         n=ifelse(is.null(dim(params)), 1, nrow(params)), 
                         run.names=paste("run", 1:n, sep=".")) {

  # If n=1, convert params to a 1-row data frame (for generically accessing it below)
  if(is.null(dim(params))) {
    pnames <- names(params)
    params <- as.data.frame(matrix(params, nrow=1))
    names(params) <- pnames
  }


  run.ids <- rep(NA, n)
  for(i in 1:n) {
    ## set RUN.ID
    if (!is.null(con)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      paramlist <- run.names[i]
      db.query(
        paste(
          "INSERT INTO runs", 
            "(model_id, site_id, start_time, finish_time, outdir,",
            "created_at, ensemble_id, parameter_list)",
          "values ('", 
            settings$model$id, "','", settings$run$site$id, "','", settings$run$start.date, "','", 
            settings$run$end.date, "','", settings$run$outdir , "','", now, "',",
            settings$assim.batch$ensemble.id, ",'", paramlist, 
          "')", 
        sep=''), 
      con)
      run.ids[i] <- db.query(
        paste("SELECT id FROM runs WHERE created_at='", now, "' AND parameter_list='", paramlist, "'", 
        sep=''),
        con)[['id']]
    } else {
      run.ids[i] <- run.names[i]
    }
    dir.create(file.path(settings$rundir, run.ids[i]), recursive=TRUE)
    dir.create(file.path(settings$modeloutdir, run.ids[i]), recursive=TRUE)

    ## write config
    do.call(my.write.config,
            args=list(defaults = settings$pfts, 
                      trait.values = list(pft=params[i,],env=NA), 
                      settings = settings, run.id = run.ids[i]))

    # Identifiers for ensemble 'runtype'
    if(settings$assim.batch$method == "bruteforce") {
      ensemble.type <- "pda.MCMC"
    } else if(settings$assim.batch$method == "emulator") {
      ensemble.type <- "pda.emulator"
    }

    ## write a README for the run
    cat("runtype     : ", paste("pda", settings$assim.batch$method, sep="."), "\n",
        "workflow id : ", as.character(workflow.id), "\n",
        "ensemble id : ", as.character(settings$assim.batch$ensemble.id), "\n",
        "chain       : ", settings$assim.batch$chain, "\n",
        "run         : ", run.names[i], "\n",
        "run id      : ", as.character(run.ids[i]), "\n",
        "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
        "model       : ", settings$model$type, "\n",
        "model id    : ", settings$model$id, "\n",
        "site        : ", settings$run$site$name, "\n",
        "site  id    : ", settings$run$site$id, "\n",
        "met data    : ", settings$run$site$met, "\n",
        "start date  : ", settings$run$start.date, "\n",
        "end date    : ", settings$run$end.date, "\n",
        "hostname    : ", settings$run$host$name, "\n",
        "rundir      : ", file.path(settings$run$host$rundir, run.ids[i]), "\n",
        "outdir      : ", file.path(settings$run$host$outdir, run.ids[i]), "\n",
        file=file.path(settings$rundir, run.ids[i], "README.txt"), sep='')

    ## add the job to the list of runs
    append <- ifelse(i==1, FALSE, TRUE)
    cat(as.character(run.ids[i]), file=file.path(settings$rundir, "runs.txt"), sep="\n", append=append)
  } # end for

  return(run.ids)
}


##' Adjust PDA MCMC jump size
##'
##' @title Adjust PDA MCMC jump size
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A PEcAn settings list updated to reflect adjusted jump distributions
##'
##' @author Ryan Kelly
##' @export
pda.adjust.jumps <- function(settings, accept.rate, pnames=NULL) {
  logger.info(paste0("Acceptance rates were (", 
                    paste(pnames, collapse=", "), ") = (", 
                    paste(round(accept.rate/settings$assim.batch$jump$adapt,3), 
                      collapse=", "), ")"))
  logger.info(paste0("Using jump variances (", 
                    paste(round(unlist(settings$assim.batch$jump$jvar),3), collapse=", "), ")"))

  adj <- accept.rate / settings$assim.batch$jump$adapt / settings$assim.batch$jump$ar.target
  adj[adj < settings$assim.batch$jump$adj.min] <- settings$assim.batch$jump$adj.min
  settings$assim.batch$jump$jvar <- as.list(unlist(settings$assim.batch$jump$jvar) * adj)
  logger.info(paste0("New jump variances are (", 
                    paste(round(unlist(settings$assim.batch$jump$jvar),3), collapse=", "), ")"))
  return(settings)
}


##' Adjust PDA blcok MCMC jump size
##'
##' @title Adjust PDA block MCMC jump size
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A PEcAn settings list updated to reflect adjusted jump distributions
##'
##' @author Ryan Kelly
##' @export
pda.adjust.jumps.bs <- function(settings, jcov, accept.count, params.recent) {
  if(FALSE) {
    params.recent = params[(i - settings$assim.batch$jump$adapt):(i-1), prior.ind]
  }
  pnames <- colnames(params.recent)
  logger.info(paste0("Acceptance rate was ", 
                     round(accept.count / settings$assim.batch$jump$adapt,3)))
  logger.info(paste0("Using jump variance diagonals (", 
                    paste(pnames, collapse=", "), ") = (", 
                    paste(round(diag(jcov),3), collapse=", "), ")"))

  r <- ncol(params.recent)
  if(accept.count == 0) {
    rescale <- diag(rep(settings$assim.batch$jump$adj.min,r))
    jcov <- rescale %*% jcov %*% rescale
  } else {
    stdev <- apply(params.recent, 2, sd)
    corr <- cor(params.recent)
    if(any(is.na(corr))) corr <- diag(rep(1,r))
    
    arate <- accept.count / settings$assim.batch$jump$adapt
    adjust <- max(arate / settings$assim.batch$jump$ar.target, settings$assim.batch$jump$adj.min)

    rescale <- diag(stdev * adjust)
    jcov <- rescale %*% corr %*% rescale
  }

  logger.info(paste0("New jump variance diagonals are (", 
                    paste(round(diag(jcov),3), collapse=", "), ")"))
  return(jcov)
}

##' Get Model Output for PDA
##'
##' @title Get Model Output for PDA
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A list containing model outputs extracted to correspond to each observational
##'         dataset being used for PDA. 
##'
##' @author Ryan Kelly
##' @export
pda.get.model.output <- function(settings, run.id, inputs) {
  # TODO: Generalize to multiple outputs and outputs other than NEE

  # Placeholder code to remind us that this function should eventually deal with assimilating
  # multiple variables. If so, look at the list of PDA inputs to determine which corresponding
  # model outputs to grab.
  input.info <- settings$assim.batch$inputs

  
  model.out <- list()
  n.input <- length(inputs)
  for(k in 1:n.input){
    NEEm <- read.output(run.id, outdir = file.path(settings$run$host$outdir, run.id),
                        strftime(settings$run$start.date,"%Y"), 
                        strftime(settings$run$end.date,"%Y"), 
                        variables="NEE")$NEE*0.0002640674

    if(length(NEEm) == 0) {   # Probably indicates model failed entirely
      return(NA)
    }
      
    ## match model and observations
    NEEm <- rep(NEEm,each= nrow(inputs[[k]]$data)/length(NEEm))
    set <- 1:length(NEEm)  ## ***** need a more intellegent year matching!!!
      # NPPm <- rep(NPPm,each=length(NPPo)/length(NPPm))
      # set <- 1:length(NPPm) 

    model.out[[k]] <- NEEm[set]
  }
  
  return(model.out)
}


##' Calculate Likelihoods for PDA
##'
##' @title Calculate Likelihoods for PDA
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return Total log likelihood (i.e., sum of log likelihoods for each dataset)
##'
##' @author Ryan Kelly
##' @export
pda.calc.llik <- function(settings, con, model.out, run.id, inputs, llik.fn) {
  if(is.na(model.out)) { # Probably indicates model failed entirely
    return(-Inf)
  }

  n.input <- length(inputs)
  
  LL.vec <- n.vec <- numeric(n.input)
  for(k in 1:n.input) {
    llik <- llik.fn[[k]](model.out[[k]], inputs[[k]])
    LL.vec[k] <- llik$LL
    n.vec[k]  <- llik$n
  }
  weights <- rep(1/n.input, n.input) # TODO: Implement user-defined weights
  LL.total <- sum(LL.vec * weights)
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
  
  return(LL.total)
}


##' Generate Parameter Knots for PDA Emulator
##'
##' @title Generate Parameter Knots for PDA Emulator
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A matrix of parameter values, with one row for each knot in the emulator.
##'
##' @author Ryan Kelly
##' @export
pda.generate.knots <- function(n.knot, n.param.all, prior.ind, prior.fn, pname) {
  # By default, all parameters will be fixed at their median
  probs <- matrix(0.5, nrow=n.knot, ncol=n.param.all)

  # Fill in parameters to be sampled with probabilities sampled in a LHC design
  probs[, prior.ind] <- lhc(t(matrix(0:1, ncol=length(prior.ind), nrow=2)), n.knot)

  # Convert probabilities to parameter values
  params <- NA*probs
  for(i in 1:n.param.all) {
    params[,i] <- eval(prior.fn$qprior[[i]], list(p=probs[,i]))
  }
  colnames(params) <- pname
  
  return(params)
}


##' Plot PDA Parameter Diagnostics
##'
##' @title Plot PDA Parameter Diagnostics
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return Nothing. Plot is generated and saved to PDF.
##'
##' @author Ryan Kelly
##' @export
pda.plot.params <- function(settings, params.subset, prior.ind) {
  # *** TODO: Generalize for multiple PFTS
  pdf(file.path(settings$pfts$pft$outdir, 
        paste0('mcmc.diagnostics.pda', settings$assim.batch$ensemble.id, '.pdf')))
    dm <- as.mcmc(params.subset)

    plot(dm)
    summary(dm)
    if(length(prior.ind)>1){
      crosscorr(dm)
      pairs(params.subset)
    }
  dev.off()
}


##' Postprocessing for PDA Results
##'
##' @title Postprocessing for PDA Results
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return PEcAn settings list, updated with <params.id> pointing to the new params file.
##'
##' @author Ryan Kelly
##' @export
pda.postprocess <- function(settings, con, params, pname, prior, prior.ind, burnin=NULL) {
  if(is.null(burnin)) burnin <- ceiling(min(2000,0.2*nrow(params)))

  ## Save params
  filename.mcmc <- file.path(settings$pfts$pft$outdir, 
                     paste0('mcmc.pda', settings$assim.batch$ensemble.id, '.Rdata'))
  save(params, file = filename.mcmc)

  ## Assess MCMC output
  params.subset <- as.data.frame(params[burnin:nrow(params),prior.ind])
    names(params.subset) <- pname[prior.ind]
  pda.plot.params(settings, params.subset, prior.ind)

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
  post.distns <- approx.posterior(params.subset, prior, outdir = settings$pfts$pft$outdir,
                    filename.flag=paste0('.pda.', settings$assim.batch$ensemble.id))
  filename <- file.path(settings$pfts$pft$outdir, 
                paste0('post.distns.pda', settings$assim.batch$ensemble.id, '.Rdata'))
  save(post.distns, file = filename)
  dbfile.insert(dirname(filename), basename(filename), 'Posterior', posteriorid, con)
  
  # Symlink to post.distns.Rdata (no ensemble.id identifier)
  if(file.exists(file.path(dirname(filename), 'post.distns.Rdata'))) {
    file.remove(file.path(dirname(filename), 'post.distns.Rdata'))
  }
  file.symlink(filename, file.path(dirname(filename), 'post.distns.Rdata'))


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
  filename <- file.path(settings$pfts$pft$outdir, 
                paste0('trait.mcmc.pda', settings$assim.batch$ensemble.id, '.Rdata'))
  save(trait.mcmc, file = filename)
  dbfile.insert(dirname(filename), basename(filename), 'Posterior', posteriorid, con)

  ## save updated settings XML
  saveXML(listToXml(settings, "pecan"), file=file.path(settings$outdir, 
    paste0('pecan.pda', settings$assim.batch$ensemble.id, '.xml')))

  return(settings)
}