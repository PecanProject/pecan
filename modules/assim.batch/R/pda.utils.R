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
    logger.error(paste0("PDA method ", settings$assim.batch$method, " not found!"))
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
                         ar.target = NULL, jvar = NULL, n.knot = NULL) {
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
    logger.error("Parameter data assimilation requested, but no parameters specified for PDA")
  } else {
    settings$assim.batch$param.names <- lapply(settings$assim.batch$param.names, as.list)
  }
  
  # # have to add names or listToXml() won't work
  # names(settings$assim.batch$param.names) <- rep("param", length(settings$assim.batch$param.names))
  # Finally, check that none of the names listed are specified as pft constants
  constant.names <- unlist(sapply(settings$pfts, function(x) names(x$constants)))
  params.in.constants <- which(unlist(settings$assim.batch$param.names) %in% constant.names)
  if (length(params.in.constants) > 0) {
    logger.severe(paste0("PDA requested for parameter(s) [", 
                         paste(unlist(settings$assim.batch$param.names)[params.in.constants], collapse = ", "), 
                         "] but these parameters are specified as constants in pecan.xml!"))
  }
  
  # prior: Either null or an ID used to query for priors later
  if (!is.null(prior.id)) {
    settings$assim.batch$prior$posterior.id <- prior.id
  }
  if (!is.null(settings$pfts$pft$posteriorid)) {
    settings$assim.batch$prior$prior.id <- lapply(settings$pfts, `[[`, "posteriorid")
    names(settings$assim.batch$prior$prior.id) <- sapply(settings$pfts, `[[`, "name")
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
pda.load.priors <- function(settings, con, path.flag = TRUE) {
  
  # Load a prior.distns or post.distns file directly by path
  if (!is.null(settings$assim.batch$prior$path)) {
    prior.out <- list()
    for (i in seq_along(settings$pfts)) {
      if (file.exists(settings$assim.batch$prior$path[[i]])) 
        load(settings$assim.batch$prior$path[[i]])
      if (exists("prior.distns")) {
        logger.info(paste0("Loaded prior ",
                           basename(settings$assim.batch$prior$path[[i]]), 
                           " as PDA prior."))
        prior.out[[i]] <- prior.distns
        rm(prior.distns)
      } else if (exists("post.distns")) {
        logger.info(paste0("Loaded posterior ",
                           basename(settings$assim.batch$prior$path[[i]]), 
                           " as PDA prior."))
        prior.out[[i]] <- post.distns
        rm(post.distns)
      } else {
        logger.warn("Didn't find a valid PDA prior at ", settings$assim.batch$prior$path[[i]])
      }
    }
  }
  
  # If no path given or didn't find a valid prior, proceed to using a posterior specified by ID,
  # either as specified in settings or get the most recent as default
  if (!exists("prior.out")) {
    if (is.null(settings$assim.batch$prior$prior.id)) {
      
      logger.info(paste0("Defaulting to most recent posterior/prior as PDA prior."))
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
    logger.info(paste0("Using posterior ID(s) ", paste(unlist(settings$assim.batch$prior$prior.id), 
                                                       collapse = ", "), " as PDA prior(s)."))
    
    prior.out <- list()
    prior.paths <- list()
    
    for (i in seq_along(settings$pfts)) {
      
      files <- dbfile.check("Posterior", settings$pfts[[i]]$posteriorid, con, settings$host$name)
      
      pid <- grep("post.distns.*Rdata", files$file_name)  ## is there a posterior file?
      if (length(pid) == 0) {
        pid <- grep("prior.distns.Rdata", files$file_name)  ## is there a prior file?
      }
      if (length(pid) > 0) {
        prior.paths[[i]] <- file.path(files$file_path[pid], files$file_name[pid])
      } else {
        pft <- settings$pfts[[i]]
        fname <- file.path(pft$outdir, "post.distns.Rdata")
        if (file.exists(fname)) {
          prior.paths[[i]] <- fname
        } else {
          next
        }
      }
      load(prior.paths[[i]])
      if (!exists("post.distns")) {
        prior.out[[i]] <- prior.distns
      } else {
        prior.out[[i]] <- post.distns
        rm(post.distns)
      }
      
    }
    
    # if this is the first PDA round, save the initial PDA prior to path
    if (path.flag == TRUE) {
      settings$assim.batch$prior$path <- prior.paths
      names(settings$assim.batch$prior$path) <- sapply(settings$pfts, `[[`, "name")
    }
  }
  
  # Finally, check that PDA parameters requested are in the prior; can't assimilate them if not.
  # Could proceed with any valid params. But probably better to just bonk out now to avoid wasting
  # a lot of time in case the mis-specified parameter(s) is really important to the analysis. 
  params.no.priors <- which(is.na(match(unlist(settings$assim.batch$param.names), 
                                        unlist(lapply(prior.out, rownames)))))
  if (length(params.no.priors) > 0) {
    logger.severe(paste0("PDA requested for parameter(s) [", paste(unlist(settings$assim.batch$param.names)[params.no.priors], 
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
                                                               function(x, n) { x[n, ] },
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
  logger.info(paste0("Acceptance rates were (", paste(pnames, collapse = ", "), ") = (", 
                     paste(round(accept.rate/settings$assim.batch$jump$adapt, 3), collapse = ", "), ")"))
  
  # logger.info(paste0('Using jump variances (',
  # paste(round(unlist(settings$assim.batch$jump$jvar),3), collapse=', '), ')'))
  logger.info(paste0("Old jump variances were (", paste(round(jmp.list, 3), collapse = ", "), ")"))
  
  adj <- accept.rate / settings$assim.batch$jump$adapt / settings$assim.batch$jump$ar.target
  adj[adj < settings$assim.batch$jump$adj.min] <- settings$assim.batch$jump$adj.min
  # settings$assim.batch$jump$jvar <- as.list(unlist(settings$assim.batch$jump$jvar) * adj)
  # logger.info(paste0('New jump variances are (',
  # paste(round(unlist(settings$assim.batch$jump$jvar),3), collapse=', '), ')'))
  jmp.list <- jmp.list * adj
  logger.info(paste0("New jump variances are (", paste(round(jmp.list, 3), collapse = ", "), ")"))
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
  logger.info(paste0("Acceptance rate was ", 
                     round(accept.count / settings$assim.batch$jump$adapt, 3)))
  logger.info(paste0("Using jump variance diagonals (", 
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
  
  logger.info(paste0("New jump variance diagonals are (", 
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
pda.generate.knots <- function(n.knot, n.param.all, prior.ind, prior.fn, pname) {
  # By default, all parameters will be fixed at their median
  probs <- matrix(0.5, nrow = n.knot, ncol = n.param.all)
  
  # Fill in parameters to be sampled with probabilities sampled in a LHC design
  probs[, prior.ind] <- lhc(t(matrix(0:1, ncol = length(prior.ind), nrow = 2)), n.knot)
  
  # Convert probabilities to parameter values
  params <- NA * probs
  for (i in seq_len(n.param.all)) {
    params[, i] <- eval(prior.fn$qprior[[i]], list(p = probs[, i]))
  }
  colnames(params) <- pname
  colnames(probs) <- pname
  
  return(list(params = params, probs = probs))
} # pda.generate.knots




##' Helper function for creating log-priors compatible with BayesianTools package
##'
##' @title Create priors for BayesianTools
##' @param prior.sel prior distributions of the selected parameters
##'
##' @return out prior class object for BayesianTools package
##'
##' @author Istem Fer
##' @export
pda.create.btprior <- function(prior.sel) {
  
  dens.fn <- samp.fn <- list()
  
  # TODO: test exponential
  for (i in seq_len(nrow(prior.sel))) {
    # if(prior.sel$distn[i] == 'exp'){
    # dens.fn[[i]]=paste('d',prior.sel$distn[i],'(x[',i,'],',prior.sel$parama[i],',log=TRUE)',sep='')
    # samp.fn[[i]] <- paste('x[',i,']=r',prior.sel$distn[i],'(1,',prior.sel$parama[i],')',sep='')
    # }else{
    dens.fn[[i]] <- paste0("d", 
                           prior.sel$distn[i], 
                           "(x[", i, "],", 
                           prior.sel$parama[i],
                           ",", 
                           prior.sel$paramb[i], 
                           ",log=TRUE)")
    samp.fn[[i]] <- paste0("x[", i, "]=r", 
                           prior.sel$distn[i], 
                           "(1,", prior.sel$parama[i],
                           ",", 
                           prior.sel$paramb[i], 
                           ")")
    # }
  }
  
  to.density <- paste(dens.fn, collapse = ",")
  to.sampler <- paste(samp.fn, collapse = " ", "\n")
  
  density <- eval(parse(text = paste0("function(x){ \n return(sum(", to.density, ")) \n }")))
  sampler <- eval(parse(text = paste0("function(){ \n x=rep(NA,", nrow(prior.sel), ") \n", to.sampler, 
                                      "return(x) \n ", "}")))
  
  # Use createPrior{BayesianTools} function to create prior class object compatible
  # with rest of the functions
  out <- createPrior(density = density, sampler = sampler)
  return(out)
} # pda.create.btprior


##' Helper function for applying BayesianTools specific settings from PEcAn general settings
##'
##' @title Apply settings for BayesianTools
##' @param settings PEcAn settings
##'
##' @return bt.settings list of runMCMC{BayesianTools} settings
##'
##' @author Istem Fer
##' @export
##' 
pda.settings.bt <- function(settings) {
  
  sampler <- settings$assim.batch$bt.settings$sampler
  
  iterations <- as.numeric(settings$assim.batch$bt.settings$iter)
  optimize <- ifelse(!is.null(settings$assim.batch$bt.settings$optimize), 
                     settings$assim.batch$bt.settings$optimize, 
                     TRUE)
  # consoleUpdates = ifelse(!is.null(settings$assim.batch$bt.settings$consoleUpdates),
  # as.numeric(settings$assim.batch$bt.settings$consoleUpdates), max(round(iterations/10),100))
  adapt <- ifelse(!is.null(settings$assim.batch$bt.settings$adapt), 
                  settings$assim.batch$bt.settings$adapt, 
                  TRUE)
  adaptationInverval = ifelse(!is.null(settings$assim.batch$bt.settings$adaptationInverval),
                              as.numeric(settings$assim.batch$bt.settings$adaptationInverval),
                              max(round(iterations/100*5),100))
  adaptationNotBefore <- ifelse(!is.null(settings$assim.batch$bt.settings$adaptationNotBefore), 
                                as.numeric(settings$assim.batch$bt.settings$adaptationNotBefore), 
                                adaptationInverval)
  DRlevels <- ifelse(!is.null(settings$assim.batch$bt.settings$DRlevels),
                     as.numeric(settings$assim.batch$bt.settings$DRlevels), 
                     1)
  if (!is.null(settings$assim.batch$bt.settings$gibbsProbabilities)) {
    gibbsProbabilities <- as.numeric(unlist(settings$assim.batch$bt.settings$gibbsProbabilities))
  } else {
    gibbsProbabilities <- NULL
  }
  
  if (sampler == "Metropolis") {
    bt.settings <- list(iterations = iterations,
                        optimize = optimize, 
                        DRlevels = DRlevels, 
                        adapt = adapt, 
                        adaptationNotBefore = adaptationNotBefore,
                        gibbsProbabilities = gibbsProbabilities)
  } else if (sampler %in% c("AM", "M", "DRAM", "DR")) {
    bt.settings <- list(iterations = iterations, startValue = "prior")
  } else if (sampler %in% c("DE", "DEzs", "DREAM", "DREAMzs", "Twalk")) {
    bt.settings <- list(iterations = iterations)
  } else if (sampler == "SMC") {
    bt.settings <- list(initialParticles = list("prior", iterations))
  } else {
    logger.error(paste0(sampler, " sampler not found!"))
  }
  
  return(bt.settings)
} # pda.settings.bt


#' Flexible function to create correlation density plots
#' numeric matrix or data.frame
#' @author Florian Hartig
#' @param mat matrix or data frame of variables
#' @param density type of plot to do
#' @param thin thinning of the matrix to make things faster. Default is to thin to 5000 
#' @param method method for calculating correlations
#' @import IDPmisc
#' @import ellipse
#' @references The code for the correlation density plot originates from Hartig, F.; Dislich, C.; Wiegand, T. & Huth, A. (2014) Technical Note: Approximate Bayesian parameterization of a process-based tropical forest model. Biogeosciences, 11, 1261-1272.
#' @export
#' 
correlationPlot <- function(mat, density = "smooth", thin = "auto", method = "pearson", whichParameters = NULL) {
  
  if (inherits(mat, "bayesianOutput")) {
    mat <- getSample(mat, thin = thin, whichParameters = whichParameters, ...)
  }
  
  numPars <- ncol(mat)
  names <- colnames(mat)
  
  panel.hist.dens <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "blue4", ...)
  } # panel.hist.dens
  
  # replaced by spearman
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = "complete.obs", method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if (missing(cex.cor)) {
      cex.cor <- 0.8/strwidth(txt)
    }
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
  } # panel.cor
  
  plotEllipse <- function(x, y) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    cor <- cor(x, y)
    el <- ellipse::ellipse(cor)
    polygon(el[, 1] + mean(x), el[, 2] + mean(y), col = "red")
  } # plotEllipse
  
  correlationEllipse <- function(x) {
    cor <- cor(x)
    ToRGB <- function(x) {
      rgb(x[1] / 255, x[2] / 255, x[3] / 255)
    }
    C1 <- ToRGB(c(178, 24, 43))
    C2 <- ToRGB(c(214, 96, 77))
    C3 <- ToRGB(c(244, 165, 130))
    C4 <- ToRGB(c(253, 219, 199))
    C5 <- ToRGB(c(247, 247, 247))
    C6 <- ToRGB(c(209, 229, 240))
    C7 <- ToRGB(c(146, 197, 222))
    C8 <- ToRGB(c(67, 147, 195))
    C9 <- ToRGB(c(33, 102, 172))
    CustomPalette <- colorRampPalette(rev(c(C1, C2, C3, C4, C5, C6, C7, C8, C9)))
    ord <- order(cor[1, ])
    xc <- cor[ord, ord]
    colors <- unlist(CustomPalette(100))
    ellipse::plotcorr(xc, col = colors[xc * 50 + 50])
  } # correlationEllipse
  
  if (density == "smooth") {
    pairs(mat, lower.panel = function(...) {
      par(new = TRUE)
      IDPmisc::ipanel.smooth(...)
    }, diag.panel = panel.hist.dens, upper.panel = panel.cor)
  } else if (density == "corellipseCor") {
    pairs(mat, lower.panel = plotEllipse, diag.panel = panel.hist.dens, upper.panel = panel.cor)
  } else if (density == "ellipse") {
    correlationEllipse(mat)
  } else if (density == F) {
    pairs(mat, lower.panel = panel.cor, diag.panel = panel.hist.dens, upper.panel = panel.cor)
  } else stop("wrong sensity argument")
  
  # The if block above is generating return values
} # correlationPlot

##' @title return.bias
##' @author Istem Fer
##' @export
return.bias <- function(isbias, model.out, inputs, prior.list.bias, nbias, run.round = FALSE, prev.bias = NULL){
  
  # there can be more than one multiplicative Gaussian requested
  ibias <- length(isbias)
  
  # to store bias parameters and probabilitied
  bias.params <- bias.probs <- list()
  # to store priors
  bias.prior <- data.frame(distn = rep("unif", ibias), 
                           parama = rep(NA, ibias), 
                           paramb = rep(NA, ibias),  
                           n =rep(NA, ibias), stringsAsFactors=FALSE)
  prior.names <- rep(NA, ibias)
  
  for(i in seq_along(isbias)){
    bias.params[[i]] <- matrix(NA, nrow = length(model.out), ncol = nbias)
    
    for(iknot in seq_along(model.out)){
      # calculate optimum bias parameter for the model output that has bias
      regdf <- data.frame(inputs[[isbias[i]]]$obs, model.out[[iknot]][[isbias[i]]])
      colnames(regdf) <- c("data","model")
      fit <- lm( regdf$data ~ (regdf$model - 1))
      bias.params[[i]][iknot,1] <- fit$coefficients[[1]]
      if(ncol(bias.params[[i]]) > 1){
        bias.params[[i]][iknot,  2:ncol(bias.params[[i]])] <- rnorm(ncol(bias.params[[i]])-1, bias.params[[i]][iknot,1], bias.params[[i]][iknot,1]*0.1)
      }
    }
    
    bias.prior$parama[i] <- min(bias.params[[i]]) - sd(bias.params[[i]])
    bias.prior$paramb[i] <- max(bias.params[[i]]) + sd(bias.params[[i]])
    
    prior.names[i] <- paste0("bias.", sapply(model.out[[1]],names)[isbias[i]])
    names(bias.params)[i] <- paste0("bias.", sapply(model.out[[1]],names)[isbias[i]])
  }

  rownames(bias.prior) <- prior.names
  prior.list.bias[[(length(prior.list.bias)+1)]] <- bias.prior
  
  # convert params to probs for GPfit 
  # note: there can be new parameters out of previous min/max if this is a round extension
  bias.probs <- lapply(seq_along(isbias), 
                       function(b) punif(bias.params[[b]], 
                                         prior.list.bias[[length(prior.list.bias)]]$parama[b], 
                                         prior.list.bias[[length(prior.list.bias)]]$paramb[b]))

  
  # if this is another round, use the first priors
  if(run.round){
    load(prev.bias)
    prior.list.bias <- prior.list
    
    # convert params to probs for GPfit 
    # note: there can be new parameters out of previous min/max if this is a round extension
    bias.probs <- lapply(seq_along(isbias), 
                         function(b) punif(bias.params[[b]], 
                                           prior.list.bias[[length(prior.list.bias)]]$parama[b], 
                                           prior.list.bias[[length(prior.list.bias)]]$paramb[b]))
    
  }
  
  return(list(bias.params = bias.params, bias.probs = bias.probs, prior.list.bias = prior.list.bias))
  
}
