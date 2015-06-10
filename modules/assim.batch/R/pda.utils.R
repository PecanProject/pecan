##' Load Dataset for Paramater Data Assimilation
##'
##' @title Load Dataset for Paramater Data Assimilation
##' @param input.settings = settings$assim.batch$inputs from pecan.xml or similar
##'
##' @return A list containg the loaded input data, plus metadata
##'
##' @author Ryan Kelly
##' @export
load.pda.data <- function(input.settings) {

  ## load data
  # Outlining setup for multiple datasets, although for now the only option is to assimilate 
  # against a single NEE input
  inputs <- list()
  n.input <- length(input.settings)

  for(i in 1:n.input) {
    inputs[[i]] <- list()
    inputs[[i]]$variable.id <- input.settings[[i]]$data.model$variable.id

    ## Load input based on ID, PATH, or SOURCE
    if(!is.null(input.settings[[i]]$id)) {             # Input specified by ID
      ## Get file path from input id
      inputs[[i]]$input.id <- input.settings[[i]]$id
      file <- db.query(paste0('SELECT * FROM dbfiles WHERE container_id = ', input.settings[[i]]$id), con)
      file <- file.path(file$file_path, file$file_name)

      ## Load store data
      inputs[[i]]$data <- read.csv(file)
    } else if(!is.null(input.settings[[i]]$path)) {    # Input specified by PATH
      # Again, for now works with a single test case, Ameriflux NEE.
      inputs[[i]]$data <- read.csv(input.settings[[i]]$path)
      inputs[[i]]$input.id <- -1
    } else if(!is.null(input.settings[[i]]$source)) {  # Input specified by SOURCE
      # TODO: insert code to extract data from standard sources (e.g. AMF)
    } else {
      logger.error("Must provide ID, PATH, or SOURCE for all data assimilation inputs")
    }
      

    ## Preprocess data
    # TODO: Generalize
    if(as.numeric(inputs[[i]]$variable.id) == 297) {
      ## calculate flux uncertainty parameters
      NEEo <- inputs[[i]]$data$NEE_or_fMDS #data$Fc   #umolCO2 m-2 s-1
      NEEq <- inputs[[i]]$data$NEE_or_fMDSqc #data$qf_Fc
      dTa <- get.change(inputs[[i]]$data$Ta_f)
      flags <- dTa < 3   ## filter data to temperature differences that are less than 3 degrees
      NEE.params <- flux.uncertainty(NEEo,NEEq,flags,bin.num=20)
      inputs[[i]]$b0 <- NEE.params$intercept
      inputs[[i]]$bp <- NEE.params$slopeP
      inputs[[i]]$bn <- NEE.params$slopeN
    }
  } # end loop over files
  
  return(inputs)
}



##' Set PDA Settings
##'
##' @title Set PDA Settings
##' @param settings: pecan settings list
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


  # prior: Either null or an ID used to query for priors later
  if(!is.null(prior.id)) {
    settings$assim.batch$prior.id <- prior.id
  }
  if(!is.null(settings$assim.batch$prior.id)) {
    settings$assim.batch$prior.id <- as.character(settings$assim.batch$prior.id)
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


  # jvar: Initial jump variances. Defaults to 1, which is foolish but should be fixed adaptively. 
  if(!is.null(jvar)) {
    settings$assim.batch$jump$jvar <- jvar
  } 
  if(is.null(settings$assim.batch$jump$jvar)) {   # Default
    settings$assim.batch$jump$jvar <- rep(1, length(param.names))
  }
  settings$assim.batch$jump$jvar <- as.list(as.numeric(settings$assim.batch$jump$jvar))
  
  return(settings)
}








pda.load.priors <- function(settings, con) {
  if(is.null(settings$assim.batch$prior.id)){
    ## by default, use the most recent posterior as the prior
    pft.id <-  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
    priors <-  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)

    prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id IN (", paste(priors$id, collapse=','), ")"),con)

    prior.db <- prior.db[grep("post.distns.Rdata",prior.db$file_name),]

    settings$assim.batch$prior.id <- prior.db$container_id[which.max(prior.db$updated_at)]
  }
  prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ", settings$assim.batch$prior.id),con)
  prior.db <- prior.db[grep("post.distns.Rdata",prior.db$file_name),]

  # Load the file; return loaded variable 'post.distns' 
  load(file.path(prior.db$file_path,"post.distns.Rdata"))
  return(post.distns)
}







pda.create.ensemble <- function(settings, con, workflow.id) {
  if (!is.null(con)) {
    # Identifiers for ensemble 'runtype'
    if(settings$assim.batch$method == "bruteforce") {
      ensemble.type <- "pda.MCMC"
    } else if(settings$assim.batch$method == "emulator") {
      ensemble.type <- "pda.emulator"
    }
    
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                   now, "', '", ensemble.type,"', ", workflow.id, ")", sep=''), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep=''), con)[['id']]
  } else {
    ensemble.id <- "NA"
  }
  
  return(ensemble.id)
}



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




pda.define.llik.fn <- function(settings) {
  # *** TODO: Generalize!
  # Currently just returns a single likelihood, assuming the data are flux NEE.
  llik.fn <- list()
  for(i in 1:length(settings$assim.batch$input)) {
    llik.fn[[i]] <- function(model, obs) {
      NEEo <- obs$data$NEE_or_fMDS #data$Fc   #umolCO2 m-2 s-1
      NEEq <- obs$data$NEE_or_fMDSqc #data$qf_Fc
      NEEo[NEEq > 1] <- NA
    
      NEEm <- model
    
      NEE.resid <- abs(model - NEEo)
      NEE.pos <- (NEEm >= 0)
      LL <- c(dexp(NEE.resid[NEE.pos], 1/(obs$b0 + obs$bp*NEEm[NEE.pos]), log=TRUE), 
              dexp(NEE.resid[!NEE.pos],1/(obs$b0 + obs$bn*NEEm[!NEE.pos]),log=TRUE))
      n.obs = sum(!is.na(LL))
      return(list(LL=sum(LL,na.rm=TRUE), n=n.obs))
    }
  }

  return(llik.fn)
}




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






pda.init.run <- function(settings, con, my.write.config, workflow.id, ensemble.id, params, 
                         n=ifelse(is.null(dim(params)), 1, nrow(params)), 
                         run.names=paste("run", 1:n, sep=".")) {

  # If n=1, convert params to a 1-row matrix (for generically accessing it below)
  if(is.null(dim(params))) params <- matrix(params, nrow=1)

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
            settings$run$end.date, "','", settings$run$outdir , "','", now, "',", ensemble.id, ",'", 
            paramlist, 
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
    do.call(my.write.config,args=list(settings$pfts, list(pft=params[i,],env=NA), settings, run.ids[i]))


    # Identifiers for ensemble 'runtype'
    if(settings$assim.batch$method == "bruteforce") {
      ensemble.type <- "pda.MCMC"
    } else if(settings$assim.batch$method == "emulator") {
      ensemble.type <- "pda.emulator"
    }

    ## write a README for the run
    cat("runtype     : ", paste("pda", settings$assim.batch$method, sep="."), "\n",
        "workflow id : ", as.character(workflow.id), "\n",
        "ensemble id : ", as.character(ensemble.id), "\n",
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








pda.adjust.jumps <- function(settings, accept.rate, pnames=NULL) {
  logger.info(paste0("Acceptance rates were (", 
                    paste(pnames, collapse=", "), ") = (", 
                    paste(round(accept.rate/settings$assim.batch$jump$adapt,3), 
                      collapse=", "), ")"))
  logger.info(paste0("Using jump variances (", 
                    paste(round(settings$assim.batch$jump$jvar,3), collapse=", "), ")"))

  adj <- accept.rate / settings$assim.batch$jump$adapt / settings$assim.batch$jump$ar.target
  adj[adj < settings$assim.batch$jump$adj.min] <- settings$assim.batch$jump$adj.min
  settings$assim.batch$jump$jvar <- lapply(settings$assim.batch$jump$jvar, function(x) x * adj)
  logger.info(paste0("New jump variances are (", 
                    paste(round(settings$assim.batch$jump$jvar,3), collapse=", "), ")"))
  return(settings)
}




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

    ## match model and observations
    NEEm <- rep(NEEm,each= nrow(inputs[[k]]$data)/length(NEEm))
    set <- 1:length(NEEm)  ## ***** need a more intellegent year matching!!!
      # NPPm <- rep(NPPm,each=length(NPPo)/length(NPPm))
      # set <- 1:length(NPPm) 

    model.out[[k]] <- NEEm[set]
  }
  
  return(model.out)
}







pda.calc.llik <- function(settings, con, model.out, inputs, llik.fn) {
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


pda.plot.params <- function(settings, params.subset, prior.ind) {
  # *** TODO: Generalize for multiple PFTS
  pdf(file.path(settings$pfts$pft$outdir,"pda.mcmc.diagnostics.pdf"))
    dm <- as.mcmc(params.subset)

    plot(dm)
    summary(dm)
    if(length(prior.ind)>1){
      crosscorr(dm)
      pairs(params.subset)
    }
  dev.off()
}


pda.postprocess <- function(settings, con, params, pname, prior, prior.ind) {
  ## Save params
  filename.mcmc <- file.path(settings$outdir, "pda.mcmc.Rdata")
  save(params, file = filename.mcmc)

  ## Assess MCMC output
  burnin <- min(2000,0.2*nrow(params))
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
  
  return(settings)
}