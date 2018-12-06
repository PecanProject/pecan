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
  if (!("assim.batch" %in% names(settings))) {
    return(settings)
  }
  
  if (is.null(settings$assim.batch$method)) {
    settings$assim.batch$method <- "bruteforce.bs"
  }
  
  if (settings$assim.batch$method == "bruteforce") {
    settings <- pda.mcmc(settings)
  } else if (settings$assim.batch$method == "bruteforce.bs") {
    settings <- pda.mcmc.bs(settings)
  } else if (settings$assim.batch$method == "emulator") {
    settings <- pda.emulator(settings)
  } else if (settings$assim.batch$method == "bayesian.tools") {
    settings <- pda.bayesian.tools(settings)
  } else {
    PEcAn.logger::logger.error(paste0("PDA method ", settings$assim.batch$method, " not found!"))
  }
  
  return(settings)
} # assim.batch


##' @export
runModule.assim.batch <- function(settings) {
  if (is.MultiSettings(settings)) {
    return(papply(settings, runModule.assim.batch))
  } else if (is.Settings(settings)) {
    return(assim.batch(settings))
  } else {
    stop("runModule.assim.batch only works with Settings or MultiSettings")
  }
} # runModule.assim.batch


##' Set PDA Settings
##'
##' @title Set PDA Settings
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return An updated settings list
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.settings <- function(settings, params.id = NULL, param.names = NULL, prior.id = NULL, 
                         chain = NULL, iter = NULL, adapt = NULL, adj.min = NULL,
                         ar.target = NULL, jvar = NULL, n.knot = NULL, run.round = FALSE) {
  # Some settings can be supplied via settings (for automation) or explicitly (interactive). 
  # An explicit argument overrides whatever is in settings, if anything.
  # If neither an argument or a setting is provided, set a default value in settings. 
  
  # Each assignment below includes an explicit type conversion to avoid problems later. 
  
  # params.id: Either null or an ID used to query for a matrix of MCMC samples later
  if (!is.null(params.id)) {
    settings$assim.batch$params.id <- params.id
  }
  if (!is.null(settings$assim.batch$params.id)) {
    settings$assim.batch$params.id <- as.character(settings$assim.batch$params.id)
  }
  
  # param.names: Names of parameters to assimilate against
  if (!is.null(param.names)) {
    settings$assim.batch$param.names <- param.names
  }
  if (is.null(settings$assim.batch$param.names)) {
    PEcAn.logger::logger.error("Parameter data assimilation requested, but no parameters specified for PDA")
  } else {
    settings$assim.batch$param.names <- lapply(settings$assim.batch$param.names, as.list)
  }
  
  # # have to add names or listToXml() won't work
  # names(settings$assim.batch$param.names) <- rep("param", length(settings$assim.batch$param.names))
  # Finally, check that none of the names listed are specified as pft constants
  constant.names <- unlist(sapply(settings$pfts, function(x) names(x$constants)))
  params.in.constants <- which(unlist(settings$assim.batch$param.names) %in% constant.names)
  if (length(params.in.constants) > 0) {
    PEcAn.logger::logger.severe(paste0("PDA requested for parameter(s) [", 
                         paste(unlist(settings$assim.batch$param.names)[params.in.constants], collapse = ", "), 
                         "] but these parameters are specified as constants in pecan.xml!"))
  }

  # # if settings$assim.batch$prior$prev.prior.id is not null, it means an extension run was already done
  # # store it to prior.id so that it's not overwritten for 3rd or more "longer" extension
  # # if it's 3rd or more "round" of emulator extension then we do want to overwrite it 
  # # Revisit this if you want to change knot proposal design 
  # if (!is.null(settings$assim.batch$prior$prev.prior.id) & !run.round) {
  #   settings$assim.batch$prior$prior.id <- settings$assim.batch$prior$prev.prior.id
  # }
  # 
  # # if settings$assim.batch$prior$prior.id is not null, it means a PDA run was already done
  # # store it to prev.prior.id, need it for extension runs
  if (is.null(settings$assim.batch$prior$prior.id)) {
    settings$assim.batch$prior$prior.id <- lapply(settings$pfts, `[[`, "posteriorid")
    names(settings$assim.batch$prior$prior.id) <- sapply(settings$pfts, `[[`, "name")
  }
  
  # # if settings$pfts$pft$posteriorid is not null, use it as new PDA prior:
  # 
  # # (a) settings$pfts$pft$posteriorid can be full if you went through meta.analysis and write.configs
  # # if it's empty pda.load.priors will handle it later
  # 
  # # (b) settings$pfts$pft$posteriorid will be full and overwritten by a PDA posterior id if a PDA was run
  # if (!is.null(settings$pfts$pft$posteriorid)) {
  #   settings$assim.batch$prior$prior.id <- lapply(settings$pfts, `[[`, "posteriorid")
  #   names(settings$assim.batch$prior$prior.id) <- sapply(settings$pfts, `[[`, "name")
  # }
  
  # if a prior.id is explicity passed to this function, overwrite and use it as PDA prior
  if (!is.null(prior.id)) {
    settings$assim.batch$prior$prior.id <- prior.id
  }
  
  
  # chain: An identifier for the MCMC chain.
  if (!is.null(chain)) {
    settings$assim.batch$chain <- chain
  }
  if (is.null(settings$assim.batch$chain)) {
    # Default
    settings$assim.batch$chain <- 1
  }
  settings$assim.batch$chain <- as.numeric(settings$assim.batch$chain)
  
  # iter: Number of MCMC iterations.
  if (!is.null(iter)) {
    settings$assim.batch$iter <- iter
  }
  if (is.null(settings$assim.batch$iter)) {
    # Default
    settings$assim.batch$iter <- 100
  }
  settings$assim.batch$iter <- as.numeric(settings$assim.batch$iter)
  
  # n.knot: Number of emulator knots
  if (!is.null(n.knot)) {
    settings$assim.batch$n.knot <- n.knot
  }
  if (is.null(settings$assim.batch$n.knot)) {
    settings$assim.batch$n.knot <- 100 # Default
  }
  settings$assim.batch$n.knot <- as.numeric(settings$assim.batch$n.knot)
  
  # ----- Jump distribution / tuning parameters 
  # adapt: How often to adapt the MCMC. Defaults to iter/10
  if (!is.null(adapt)) {
    settings$assim.batch$jump$adapt <- adapt
  }
  if (is.null(settings$assim.batch$jump$adapt)) {
    
    settings$assim.batch$jump$adapt <- floor(settings$assim.batch$iter/10) # Default
  }
  settings$assim.batch$jump$adapt <- as.numeric(settings$assim.batch$jump$adapt)
  
  # adj.min: minimum amount to reduce jump distribution by.  Default
  if (!is.null(adj.min)) {
    settings$assim.batch$jump$adj.min <- adj.min
  }
  if (is.null(settings$assim.batch$jump$adj.min)) {
    # Default
    settings$assim.batch$jump$adj.min <- 0.1
  }
  settings$assim.batch$jump$adj.min <- as.numeric(settings$assim.batch$jump$adj.min)
  
  # ar.target: Target acceptance rate. 
  # Can be a single value of vector, one for each variable assimilated against.
  if (!is.null(ar.target)) {
    settings$assim.batch$jump$ar.target <- ar.target
  }
  if (is.null(settings$assim.batch$jump$ar.target)) {
    settings$assim.batch$jump$ar.target <- 0.5 # Default
  }
  settings$assim.batch$jump$ar.target <- as.numeric(settings$assim.batch$jump$ar.target)
  
  # jvar: Initial jump variances. Defaults to NA to be based on priors later.
  if (settings$assim.batch$method != "emulator") {
    if (!is.null(jvar)) {
      settings$assim.batch$jump$jvar <- jvar
    }
    if (is.null(settings$assim.batch$jump$jvar)) {
      settings$assim.batch$jump$jvar <- rep(NA, length(unlist(settings$assim.batch$param.names))) # Default
    }
    settings$assim.batch$jump$jvar <- as.list(as.numeric(settings$assim.batch$jump$jvar))
    # have to add names or listToXml() won't work
    names(settings$assim.batch$jump$jvar) <- rep("jvar", length(settings$assim.batch$jump$jvar))
  }
  
  # diag.plot.iter: How often to do diagnostic plots. Just need to convert to numeric.
  if (!is.null(settings$assim.batch$diag.plot.iter)) {
    settings$assim.batch$diag.plot.iter <- as.numeric(settings$assim.batch$diag.plot.iter)
  }
  
  return(settings)
} # pda.settings


##' Load Priors for Paramater Data Assimilation
##'
##' @title Load Priors for Paramater Data Assimilation
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A previously-generated posterior distribution, to be used as the prior for PDA.
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.load.priors <- function(settings, con, extension.check = FALSE) {
  
  # settings$assim.batch$prior$prior.id is not NULL if you've done a PDA or meta.analysis and went through write.configs
  # then you can proceed loading objects by querying their paths according to their ids
  # if it's NULL get the most recent id from DB as default 

  if (is.null(settings$assim.batch$prior$prior.id)) {
      
    PEcAn.logger::logger.info(paste0("Defaulting to most recent posterior/prior as PDA prior."))
    ## by default, use the most recent posterior/prior as the prior
    priorids <- list()
    for (i in seq_along(settings$pfts)) {
        
      pft.id <- db.query(paste0("SELECT pfts.id FROM pfts, modeltypes WHERE pfts.name='", 
                                            settings$pfts[[i]]$name, 
                                            "' and pfts.modeltype_id=modeltypes.id and modeltypes.name='", 
                                            settings$model$type, "'"), 
                                     con)[["id"]]
      priors <- db.query(paste0("SELECT * from posteriors where pft_id = ", pft.id), con)
        
      prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id IN (", 
                                    paste(priors$id, collapse = ","), ")"), con)
        
      prior.db.grep <- prior.db[grep("^post\\.distns\\..*Rdata$", prior.db$file_name), ]
      if (nrow(prior.db.grep) == 0) {
        prior.db.grep <- prior.db[grep("^prior\\.distns\\..*Rdata$", prior.db$file_name), ]
      }
        
      priorids[[i]] <- prior.db.grep$container_id[which.max(prior.db.grep$updated_at)]
    }
    settings$assim.batch$prior$prior.id <- priorids
  }

  # extension.check == FALSE not an extension run
  # extension.check == TRUE a "round" extension run
  if(extension.check){
    priorids <- sapply(settings$pfts, `[[`, "posteriorid")
  } else{
    priorids <- settings$assim.batch$prior$prior.id
  }
  
  PEcAn.logger::logger.info(paste0("Using posterior ID(s) ", paste(unlist(priorids), collapse = ", "), " as PDA prior(s)."))
  

  prior.out <- list()
  prior.paths <- list()
    
  tmp_hostname <- ifelse(!PEcAn.remote::is.localhost(settings$host), PEcAn.remote::fqdn(), settings$host$name)
  
  # now that you filled priorids load the PDA prior objects
  # if files becomes NULL try loading objects from workflow oft folders
  for (i in seq_along(settings$pfts)) {
      
    files <- dbfile.check("Posterior", priorids[[i]], con, tmp_hostname, return.all  = TRUE)
      
    pid <- grep("post.distns.*Rdata", files$file_name)  ## is there a posterior file?
    
    if (length(pid) == 0) {
      pid <- grep("prior.distns.Rdata", files$file_name)  ## is there a prior file?
    }
    
    if (length(pid) > 0) {
      prior.paths[[i]] <- file.path(files$file_path[pid], files$file_name[pid])
    } else {
      ## is there a posterior in the current workflow's PFT directory?
      pft <- settings$pfts[[i]]
      fname <- file.path(pft$outdir, "post.distns.Rdata")
      if (file.exists(fname)) {
        prior.paths[[i]] <- fname
      } else {
        ## is there a prior in the current workflow's PFT directory?
        fname <- file.path(pft$outdir, "prior.distns.Rdata")
        if (file.exists(fname)) {
          prior.paths[[i]] <- fname
        }else{
          ## if no posterior or prior can be found, skip to the next PFT
          next
        }
      }
    }
    
    # make sure there are no left over distributions in the environment
    suppressWarnings(rm(post.distns, prior.distns))
    
    load(prior.paths[[i]])
    if (!exists("post.distns")) {
      prior.out[[i]] <- prior.distns
    } else {
      prior.out[[i]] <- post.distns
      rm(post.distns)
    }
      
  }
    
  
  # Finally, check that PDA parameters requested are in the prior; can't assimilate them if not.
  # Could proceed with any valid params. But probably better to just bonk out now to avoid wasting
  # a lot of time in case the mis-specified parameter(s) is really important to the analysis. 
  params.no.priors <- which(is.na(match(unlist(settings$assim.batch$param.names), 
                                        unlist(lapply(prior.out, rownames)))))
  if (length(params.no.priors) > 0) {
    PEcAn.logger::logger.severe(paste0("PDA requested for parameter(s) [", paste(unlist(settings$assim.batch$param.names)[params.no.priors], 
                                                                   collapse = ", "), "] but no prior found!"))
  }
  
  return(list(prior = prior.out, settings = settings))
} # pda.load.priors


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
    if (settings$assim.batch$method == "bruteforce" | 
        settings$assim.batch$method == "bruteforce.bs" | 
        settings$assim.batch$method == "bayesian.tools") {
      ensemble.type <- "pda.MCMC"
    } else if (settings$assim.batch$method == "emulator") {
      ensemble.type <- "pda.emulator"
    }
    
    ensemble.id <- db.query(paste("INSERT INTO ensembles (runtype, workflow_id) values ('", 
                                  ensemble.type, "', ", workflow.id, ") RETURNING id", sep = ""), con)

  } else {
    ensemble.id <- NA
  }
  
  return(ensemble.id)
} # pda.create.ensemble


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
  dprior <- rprior <- qprior <- pprior <- list()
  for (i in seq_len(n.param.all)) {
    if (prior$distn[i] == "exp") {
      dprior[[i]] <- parse(text = paste0("dexp(x,", prior$parama[i], ",log=TRUE)"))
      rprior[[i]] <- parse(text = paste0("rexp(n,", prior$parama[i], ")"))
      qprior[[i]] <- parse(text = paste0("qexp(p,", prior$parama[i], ")"))
      pprior[[i]] <- parse(text = paste0("pexp(q,", prior$parama[i], ")"))
    } else {
      dprior[[i]] <- parse(text = paste0("d", prior$distn[i], "(x,", prior$parama[i], ",", prior$paramb[i], ",log=TRUE)"))
      rprior[[i]] <- parse(text = paste0("r", prior$distn[i], "(n,", prior$parama[i], ",", prior$paramb[i], ")"))
      qprior[[i]] <- parse(text = paste0("q", prior$distn[i], "(p,", prior$parama[i], ",", prior$paramb[i], ")"))
      pprior[[i]] <- parse(text = paste0("p", prior$distn[i], "(q,", prior$parama[i], ",", prior$paramb[i], ")"))
    }
  }
  dmvprior <- function(x, log = TRUE) {
    # multivariate prior - density
    p <- rep(NA, n.param.all)
    for (i in seq_len(n.param.all)) {
      p[i] <- eval(dprior[[i]], list(x = x[i]))
    }
    p <- sum(p)
    ifelse(log, p, exp(p))
  } # dmvprior
  
  rmvprior <- function(n) {
    # multivariate prior - random number
    p <- matrix(NA, n, n.param.all)
    for (i in seq_len(n.param.all)) {
      p[, i] <- eval(rprior[[i]], list(n = n))
    }
    p
  }
  
  return(list(dprior = dprior, rprior = rprior, qprior = qprior,
              pprior = pprior, dmvprior = dmvprior, rmvprior = rmvprior))
} # pda.define.prior.fn


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
pda.init.params <- function(settings, chain, pname, n.param.all) {
  ## Load params from previous run, if provided.
  if (!is.null(settings$assim.batch$extension)) {
    load(settings$assim.batch$mcmc.path)  # loads params
    params <- mcmc.list[[chain]]
    start  <- nrow(params) + 1
    finish <- nrow(params) + as.numeric(settings$assim.batch$iter)
    params <- rbind(params, matrix(NA, finish - start + 1, n.param.all))
    if(!is.null(settings$assim.batch$llpar.path)){ # load llik params
      load(settings$assim.batch$llpar.path)
      llpars <- llpar.list[[chain]]
    } else {
      llpars <- NULL
    }
  } else {
    # No input given, starting fresh
    start  <- 1
    finish <- as.numeric(settings$assim.batch$iter)
    params <- matrix(NA, finish, n.param.all)
    llpars <- NULL
  }
  colnames(params) <- pname
  
  return(list(start = start, finish = finish, params = params, llpars = llpars))
} # pda.init.params


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
                         n = ifelse(is.null(dim(params)), 1, nrow(params)), 
                         run.names = paste("run", 1:n, sep = ".")) {
  
  run.ids <- rep(NA, n)
  for (i in seq_len(n)) {
    ## set RUN.ID
    if (!is.null(con)) {
      paramlist <- run.names[i]
      run.ids[i] <- db.query(
        paste0(
          "INSERT INTO runs", 
          "(model_id, site_id, start_time, finish_time, outdir,",
          "ensemble_id, parameter_list) ",
          "values ('", 
          settings$model$id, "','", settings$run$site$id, "','", settings$run$start.date, "','", 
          settings$run$end.date, "','", settings$modeloutdir , "','", 
          settings$assim.batch$ensemble.id, "','", paramlist, 
          "') RETURNING id"), 
        con)
      
    } else {
      run.ids[i] <- run.names[i]
    }
    dir.create(file.path(settings$rundir, run.ids[i]), recursive = TRUE)
    dir.create(file.path(settings$modeloutdir, run.ids[i]), recursive = TRUE)
    
    # # works for one PFT
    # trait.values = list(pft=as.list(params[i,]),env=NA)
    # newnames <- sapply(settings$pfts, "[[", "name")
    # names(trait.values)[which(!(names(trait.values) %in% 'env'))] <- newnames
    
    ## write config
    do.call(my.write.config, args = list(defaults = settings$pfts, 
                                         trait.values = lapply(params, 
                                                               function(x, n) { as.data.frame(x[n, , drop=FALSE]) },
                                                               n = i), 
                                         settings = settings, 
                                         run.id = run.ids[i]))
    
    # Identifiers for ensemble 'runtype'
    if (settings$assim.batch$method == "bruteforce" | 
        settings$assim.batch$method == "bruteforce.bs" | 
        settings$assim.batch$method == "bayesian.tools") {
      ensemble.type <- "pda.MCMC"
    } else if (settings$assim.batch$method == "emulator") {
      ensemble.type <- "pda.emulator"
    }
    
    ## write a README for the run
    cat("runtype     : ", 
        paste("pda", settings$assim.batch$method, sep = "."), "\n", "workflow id : ", 
        as.character(workflow.id), "\n",
        "ensemble id : ", as.character(settings$assim.batch$ensemble.id), "\n",
        "chain       : ", settings$assim.batch$chain, "\n", 
        "run         : ", run.names[i], "\n", 
        "run id      : ", as.character(run.ids[i]), "\n", 
        "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
        "site        : ", settings$run$site$name, "\n", 
        "site  id    : ", settings$run$site$id, "\n", 
        "met data    : ", settings$run$site$met, "\n",
        "start date  : ", settings$run$start.date, "\n", 
        "end date    : ", settings$run$end.date, "\n",
        "hostname    : ", settings$host$name, "\n",
        "rundir      : ", file.path(settings$host$rundir, run.ids[i]), "\n", 
        "outdir      : ", file.path(settings$host$outdir, run.ids[i]), "\n",
        file = file.path(settings$rundir, run.ids[i], "README.txt"))
    
    ## add the job to the list of runs
    append <- ifelse(i == 1, FALSE, TRUE)
    cat(as.character(run.ids[i]),
        file = file.path(settings$rundir, "runs.txt"), 
        sep = "\n",
        append = append)
  }  # end for
  
  return(unlist(run.ids))
} # pda.init.run


##' Adjust PDA MCMC jump size
##'
##' @title Adjust PDA MCMC jump size
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A PEcAn settings list updated to reflect adjusted jump distributions
##'
##' @author Ryan Kelly
##' @export
pda.adjust.jumps <- function(settings, jmp.list, accept.rate, pnames = NULL) {
  PEcAn.logger::logger.info(paste0("Acceptance rates were (", paste(pnames, collapse = ", "), ") = (", 
                     paste(round(accept.rate/settings$assim.batch$jump$adapt, 3), collapse = ", "), ")"))
  
  # PEcAn.logger::logger.info(paste0('Using jump variances (',
  # paste(round(unlist(settings$assim.batch$jump$jvar),3), collapse=', '), ')'))
  PEcAn.logger::logger.info(paste0("Old jump variances were (", paste(round(jmp.list, 3), collapse = ", "), ")"))
  
  adj <- accept.rate / settings$assim.batch$jump$adapt / settings$assim.batch$jump$ar.target
  adj[adj < settings$assim.batch$jump$adj.min] <- settings$assim.batch$jump$adj.min
  # settings$assim.batch$jump$jvar <- as.list(unlist(settings$assim.batch$jump$jvar) * adj)
  # PEcAn.logger::logger.info(paste0('New jump variances are (',
  # paste(round(unlist(settings$assim.batch$jump$jvar),3), collapse=', '), ')'))
  jmp.list <- jmp.list * adj
  PEcAn.logger::logger.info(paste0("New jump variances are (", paste(round(jmp.list, 3), collapse = ", "), ")"))
  return(jmp.list)
} # pda.adjust.jumps


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
  if (FALSE) {
    params.recent <- params[(i - settings$assim.batch$jump$adapt):(i - 1), prior.ind]
  }
  pnames <- colnames(params.recent)
  PEcAn.logger::logger.info(paste0("Acceptance rate was ", 
                     round(accept.count / settings$assim.batch$jump$adapt, 3)))
  PEcAn.logger::logger.info(paste0("Using jump variance diagonals (", 
                     paste(pnames, collapse = ", "), ") = (", 
                     paste(round(diag(jcov), 3), collapse = ", "), ")"))
  
  r <- ncol(params.recent)
  if (accept.count == 0) {
    rescale <- diag(rep(settings$assim.batch$jump$adj.min, r))
    jcov    <- rescale %*% jcov %*% rescale
  } else {
    stdev  <- apply(params.recent, 2, sd)
    corr   <- cor(params.recent)
    if (any(is.na(corr))) {
      corr <- diag(rep(1, r))
    }
    
    arate <- accept.count / settings$assim.batch$jump$adapt
    adjust <- max(arate / settings$assim.batch$jump$ar.target, settings$assim.batch$jump$adj.min)
    rescale <- diag(stdev * adjust)
    jcov <- rescale %*% corr %*% rescale
  }
  
  PEcAn.logger::logger.info(paste0("New jump variance diagonals are (", 
                     paste(round(diag(jcov), 3), collapse = ", "), ")"))
  return(jcov)
} # pda.adjust.jumps.bs




##' Generate Parameter Knots for PDA Emulator
##'
##' @title Generate Parameter Knots for PDA Emulator
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return A list of probabilities and parameter values, with one row for each knot in the emulator.
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.generate.knots <- function(n.knot, sf, probs.sf, n.param.all, prior.ind, prior.fn, pname) {
  # By default, all parameters will be fixed at their median
  probs <- matrix(0.5, nrow = n.knot, ncol = n.param.all)
  
  # if the params are going to be scaled leave them out, if sf is NULL nothing happens
  inds <- prior.ind[!prior.ind %in% which(pname %in% sf)]
  
  if(length(inds) !=0){
    # Fill in parameters to be sampled with probabilities sampled in a LHC design
    probs[, inds] <- PEcAn.emulator::lhc(t(matrix(0:1, ncol = length(inds), nrow = 2)), n.knot)
  }
  
  inds <- prior.ind[prior.ind %in% which(pname %in% sf)]
  
  if(!is.null(sf) & length(inds) !=0){
    match.ind <- sapply(pname[inds], function(x) which(sf == x)) 
    probs[, inds] <- probs.sf[, match.ind]
  }
  
  # Convert probabilities to parameter values
  params <- NA * probs
  for (i in seq_len(n.param.all)) {
    params[, i] <- eval(prior.fn$qprior[[i]], list(p = probs[, i]))
  }
  colnames(params) <- pname
  colnames(probs) <- pname
  
  return(list(params = params, probs = probs))
} # pda.generate.knots

##' Generate scaling factor knots for PDA Emulator
##'
##' @author Istem Fer
##' @export
pda.generate.sf <- function(n.knot, sf, prior.list){
  
  n.sf <- length(sf)
  # prior for scaling factor
  prior.sf <- data.frame(distn = rep("beta", n.sf), 
                         parama = rep(1, n.sf), 
                         paramb = rep(1, n.sf), 
                         n = rep(NA, n.sf))
  rownames(prior.sf) <- paste0(sf,"_SF")
  prior.list[[length(prior.list)+1]] <- prior.sf
  
  probs.sf <- PEcAn.emulator::lhc(t(matrix(0:1, ncol = n.sf, nrow = 2)), n.knot)
  colnames(probs.sf) <- paste0(sf,"_SF")
  
  return(list(probs = probs.sf, priors = prior.list))
  
}


##' @title return.bias
##' @param settings settings list
##' @param isbias bias variable index
##' @param model.out model output list
##' @param inputs inputs list
##' @param prior.list.bias prior list, bias prior to be added
##' @param run.round extension flag
##' @param pass2bias if this is another round, this is re-sampled MCMC samples, will go with the rest of model params
##' 
##' @author Istem Fer
##' @export
return.bias <- function(settings, isbias, model.out, inputs, prior.list.bias, run.round = FALSE, pass2bias = NULL){
  
  # how many bias parameters per dataset requested
  nbias <- ifelse(is.null(settings$assim.batch$inputs[[isbias]]$nbias), 1,
                  as.numeric(settings$assim.batch$inputs[[isbias]]$nbias))

  prev.bias <- settings$assim.batch$bias.path
  
  # to store priors
  bprior <- data.frame(distn  = rep(NA,length(isbias)),
                       parama = rep(NA,length(isbias)),
                       paramb = rep(NA,length(isbias)),
                       n      = rep(NA,length(isbias)))
  for(b in seq_along(isbias)){
    # any prior passed via settings?
    if(!is.null(settings$assim.batch$inputs[[isbias]]$bprior)){
      bprior$distn[b]  <- settings$assim.batch$inputs[[isbias[b]]]$bprior$distn
      bprior$parama[b] <- settings$assim.batch$inputs[[isbias[b]]]$bprior$parama
      bprior$paramb[b] <- settings$assim.batch$inputs[[isbias[b]]]$bprior$paramb
    }else{ # assume log-normal(0,1)
      PEcAn.logger::logger.info(paste0("No prior is defined for the bias parameter, assuming standard log-normal"))
      bprior$distn[b]  <- "lnorm"
      bprior$parama[b] <- 0
      bprior$paramb[b] <- 1
    }
  }
  bias.prior <- bprior
  
  # there can be more than one multiplicative Gaussian requested
  ibias <- length(isbias)
  
  # to store bias parameters and probabilitied
  bias.params <- bias.probs <- list()

  prior.names <- rep(NA, ibias)
  
  for(i in seq_along(isbias)){
    bias.params[[i]] <- matrix(NA, nrow = length(model.out), ncol = nbias)
    
    for(iknot in seq_along(model.out)){
      if(anyNA(model.out[[iknot]], recursive = TRUE)){
        bias.params[[i]][iknot, ] <- NA
      }else {
        # calculate optimum bias parameter for the model output that has bias
        regdf <- data.frame(inputs[[isbias[i]]]$obs, model.out[[iknot]][[isbias[i]]])
        colnames(regdf) <- c("data","model")
        fit <- lm( regdf$data ~ (regdf$model - 1))
        bias.params[[i]][iknot,1] <- fit$coefficients[[1]]
        if(ncol(bias.params[[i]]) > 1){
          bias.params[[i]][iknot,  2:ncol(bias.params[[i]])] <- rnorm(ncol(bias.params[[i]])-1, bias.params[[i]][iknot,1], bias.params[[i]][iknot,1]*0.1)
        }
      }
    }
    
    prior.names[i] <- paste0("bias.", sapply(model.out[[1]],names)[isbias[i]])
    names(bias.params)[i] <- paste0("bias.", sapply(model.out[[1]],names)[isbias[i]])
  }
  
  rownames(bias.prior) <- prior.names
  prior.list.bias[[(length(prior.list.bias)+1)]] <- bias.prior

  # if this is another round, use the first priors
  if(run.round){
    load(prev.bias)
    prior.list.bias <- prior.list
    # TODO: implementation for multiple bias params, this requires multiple changes int he PDA workflow
    bias.params[[1]][(nrow(bias.params[[1]])-length(pass2bias)+1):nrow(bias.params[[1]]), 1] <- pass2bias
  }
  
  return(list(bias.params = bias.params, prior.list.bias = prior.list.bias, nbias = nbias))
  
} # return.bias


##' @title return_hyperpars
##' @author Istem Fer
##' @export
return_hyperpars <- function(assim.settings, inputs){
  
  check.hypers <- sapply(assim.settings$inputs, `[[`, "hyper.pars")
    
  hyper.pars <- list()
  
  if(length(unlist(check.hypers)) == 0){
    # no hyper parameters passed via settings
    # default to scaled hyper params
    for(k in seq_along(assim.settings$inputs)){
      hyper.pars[[k]] <- list()
      hyper.pars[[k]]$parama <- 0.001
      hyper.pars[[k]]$paramb <- 0.001 * var(inputs[[k]]$data[,1], na.rm = TRUE)
    }
    
  }else{
    
    # hyperparameters at least for one likelihood was passed
    for(k in seq_along(assim.settings$inputs)){
      
      if(is.null(check.hypers[[k]])){
        hyper.pars[[k]] <- list()
        hyper.pars[[k]]$parama <- 0.001
        hyper.pars[[k]]$paramb <- 0.001 * var(inputs[[k]]$data[,1], na.rm = TRUE)
      }else{
        hyper.pars[[k]] <- list()
        hyper.pars[[k]]$parama <- as.numeric(assim.settings$inputs[[k]]$hyper.pars$parama)
        hyper.pars[[k]]$paramb <- as.numeric(assim.settings$inputs[[k]]$hyper.pars$paramb)
      }

    }
    
  }
  
  return(hyper.pars)
} # return_hyperpars


##' Helper function to sample from previous MCMC chain while proposing new knots
##' 
##' @param mcmc_path path to previous emulator mcmc samples object
##' @param n.param.orig vector, number of parameters targeted in each (pft) sublist
##' @param prior.ind.orig list, actual indices of parameters targeted in each (pft) sublist
##' @param n.post.knots number of new samples requested
##' @param knots.params.temp list of parameter samples proposed from the original PDA-prior
##' @param prior.list PDA-prior list
##' @param prior.fn list for parameter d/r/q/p functions
##' @param sf SF parameter names
##' @param sf.samp SF parameters MCMC samples
##' 
##' @author Istem Fer
##' @export
sample_MCMC <- function(mcmc_path, n.param.orig, prior.ind.orig, n.post.knots, knots.params.temp, 
                        prior.list, prior.fn, sf, sf.samp){
  
  PEcAn.logger::logger.info("Sampling from previous round's MCMC")
  
  load(mcmc_path)
  
  mcmc.param.list <- params.subset <- list()
  ind <- 0
  for (i in seq_along(n.param.orig)) {
    mcmc.param.list[[i]] <- lapply(mcmc.samp.list, function(x) x[, (ind + 1):(ind + n.param.orig[i]), drop = FALSE])
    ind <- ind + n.param.orig[i]
  }
  
  
  burnins <- rep(NA, length(mcmc.param.list))
  for (i in seq_along(mcmc.param.list)) {
    params.subset[[i]] <- as.mcmc.list(lapply(mcmc.param.list[[i]], mcmc))
    
    burnin     <- getBurnin(params.subset[[i]], method = "gelman.plot")
    burnins[i] <- max(burnin, na.rm = TRUE)
  }
  maxburn <- max(burnins)
  
  if(maxburn ==1){ # if no convergence focus last bit
    maxburn <- 0.9*nrow(params.subset[[1]][[1]])
  }
  
  collect_samples <- list()
  for (i in seq_along(mcmc.samp.list)) {
    collect_samples[[i]] <- window(mcmc.samp.list[[i]], start = maxburn)
  }
  
  mcmc_samples <- do.call(rbind, collect_samples)
  
 
  get_samples <- sample(1:nrow(mcmc_samples), n.post.knots)
  new_knots <- mcmc_samples[get_samples,]
  pass2bias <- new_knots[, ncol(new_knots)] # if there is bias param, it will be the last col
  # if there is no bias param this won't be used anyway
  # the rest of the code is not ready for bias params for multiple variables
    
  # when using sf, need to sample from sf mcmc samples and calculate back actual parameter values
  if(!is.null(sf.samp)){
    
    sf_samples <- list()
    for (i in seq_along(sf.samp)) {
      sf_samples[[i]] <- window(sf.samp[[i]], start = maxburn)
    }
    sf_samples <- do.call(rbind, sf_samples)
    
    sf_knots    <- sf_samples[get_samples,]
    
    ind <- 0
    for(i in seq_along(n.param.orig)){
      if(all(sf %in% rownames(prior.list[[i]]))){
        temp.knots <- sapply(seq_along(prior.ind.orig[[i]]), function(z){
          eval(prior.fn[[i]]$qprior[prior.ind.orig[[i]]][[z]], list(p = sf_knots[,z]))
        })
        new_knots[, (ind + 1):(ind + n.param.orig[i])] <- temp.knots
      }
      ind <- ind + n.param.orig[i]
    }
    
  }else{
    sf_knots <- NULL
  }
  
  # now replace with new knots
  ind <- 0
  for(i in seq_along(n.param.orig)){
    sub_knots <- new_knots[, (ind + 1):(ind + n.param.orig[i]), drop = FALSE]
    knots.params.temp[[i]][, prior.ind.orig[[i]]] <- sub_knots

    ind <- ind + n.param.orig[i]
  }
  
  return(list(knots.params.temp = knots.params.temp, sf_knots = sf_knots, pass2bias = pass2bias))
  
}
