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
pda.mcmc <- function(settings, params=NULL, jvar=NULL, var.names=NULL, prior=NULL, chain=NULL,
                     adapt=NULL, adj.min=NULL, ar.target=NULL){
  # Quit if pda not requested in settings
  if(!('assim.batch' %in% names(settings))) {
    return()
  }

  require(coda)
  
  ## this bit of code is useful for defining the variables passed to this function 
  ## if you are debugging
  if(FALSE){
    settings$model$type <- "SIPNET"
    chain <- 1
    params <- NULL
    var.names <- "Amax"
    jvar <- NULL
    var.id <- 297 ## NEE, canonical units umolC/m2/s

    params <- jvar <- var.names <- prior <- chain <- adapt <- adj.min <- ar.target <- NULL
  }


  ## settings
    # Some settings can be supplied via settings (for automation) or explicitly (interactive). 
    # An explicit argument overrides whatever is in settings, if anything.
    if(is.null(params)) {
      params <- settings$assim.batch$params
    }
    if(is.null(jvar)) {
      jvar <- as.numeric(settings$assim.batch$jump$jvar)
    }
    if(is.null(adapt)) {
      adapt <- as.numeric(settings$assim.batch$jump$adapt)
      if(is.null(adapt)) { # Default
        adapt = floor(settings$assim.batch$iter/10)
      }
    }
    if(is.null(adj.min)) {
      adj.min <- as.numeric(settings$assim.batch$jump$adj.min)
      if(is.null(adj.min)) { # Default
        adj.min = 0.1
      }
    }
    if(is.null(ar.target)) {
      ar.target <- as.numeric(settings$assim.batch$jump$ar.target)
      if(is.null(ar.target)) { # Default
        ar.target = 0.5
      }
    }
    if(is.null(var.names)) {
      var.names  <- as.character(settings$assim.batch$var.names)
    }
    if(is.null(prior)) {
      prior <- settings$assim.batch$prior
    }
    if(is.null(chain)) {
      chain <- ifelse(is.null(settings$assim.batch$chain), 1, settings$assim.batch$chain)
    }


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
  if(is.null(prior)){
    ## by default, use the most recent posterior as the prior
    pft.id <-  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
    priors <-  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)

    prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id IN (", paste(priors$id, collapse=','), ")"),con)

    prior.db <- prior.db[grep("post.distns.Rdata",prior.db$file_name),]

    prior.id <- prior.db$container_id[which.max(prior.db$updated_at)]
  } else {
    prior.id <- prior
  }
  prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ", prior.id),con)
  prior.db <- prior.db[grep("post.distns.Rdata",prior.db$file_name),]
  load(file.path(prior.db$file_path,"post.distns.Rdata"))
  prior <- post.distns
  pname <-  rownames(prior) 
  nvar  <- nrow(prior)


  ## Select parameters to constrain
  if(is.null(var.names)){
    vars <- 1:nvar
  } else {
    vars <- which(rownames(prior) %in% var.names)
  }
  nvar.sample <- length(vars)


  ## Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }


  ## create an ensemble id
  if (!is.null(con)) {
    # write enseblem first
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                   now, "', 'MCMC', ", workflow.id, ")", sep=''), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep=''), con)[['id']]
  } else {
    ensemble.id <- "NA"
  }


  ## model-specific functions
  do.call("require",list(paste0("PEcAn.", settings$model$type)))
  my.write.config <- paste("write.config.", settings$model$type,sep="")
  if(!exists(my.write.config)){
    logger.severe(paste(my.write.config,"does not exist. Please make sure that the PEcAn interface is loaded for", settings$model$type))
  }


  ## set up prior density (d) and random (r) functions
  dprior <- rprior <- qprior <-list()
  for(i in 1:nvar){
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
    p <- rep(NA,nvar)
    for(i in 1:nvar){
      p[i] <- eval(dprior[[i]],list(x=x[i]))
    }
    p = sum(p)
    if(log) return(p)
    return(exp(p))
    return(p)
  }
  rmvprior <- function(n){  #multivariate prior - random number
    p <- matrix(NA,n,nvar)
    for(i in 1:nvar){
      p[,i] <- eval(rprior[[i]],list(n=n))
    }
    return(p)
  }
  

  ## Calculate p.median
  p.median <- sapply(qprior,eval,list(p=0.5))


  ## load data
  # Outlining setup for multiple datasets, although for now the only option is to assimilate 
  # against a single NEE input
  inputs <- list()
  n.input <- length(settings$assim.batch$inputs)
  var.ids <- input.ids <- numeric(n.input)
  for(i in 1:n.input) {
    input.i <- settings$assim.batch$inputs[[i]]
    var.ids[i] <- input.i$data.model$variable.id
    inputs[[i]] <- list()

    if(is.null(input.i$id)) { # No input ID given. Obtain by PATH or SOURCE, then insert to db

      # Again, setting up to work with a single test case, Ameriflux NEE provided as a file path.
      # Lots to do to generalize. 
      if(!is.null(input.i$path)) {
        # Set input attributes (again, assuming ameriflux for now...)
      # Commenting out for now. May be useful to have inputs specified by path
      # automatically added, but needs some discussion
#         in.path <- dirname(input.i$path)
#         in.prefix <- basename(input.i$path)
#         mimetype <- 'text/csv'
#         formatname <- 'AmeriFlux.level4.h'
#         
#         year <- strsplit(basename(input.i$path), "_")[[1]][3]
#         startdate <- as.POSIXlt(paste0(year,"-01-01 00:00:00", tz = "GMT"))
#         enddate <- as.POSIXlt(paste0(year,"-12-31 23:59:59", tz = "GMT"))
        inputs[[i]]$data <- read.csv(input.i$path)
        input.ids[i] = -1
      } else if(!is.null(input.i$source)) {
        # TODO: insert code to extract data from standard sources (e.g. AMF)
        
      } else {
        logger.error("Must provide ID, PATH, or SOURCE for all data assimilation inputs")
      }
      

      ## Insert input to database
      # Commenting out for now. May be useful to have inputs specified by path
      # automatically added, but needs some discussion
#       raw.id <- dbfile.input.insert(in.path=in.path,
#                                     in.prefix=in.prefix, 
#                                     siteid = settings$run$site$id,  
#                                     startdate = startdate, 
#                                     enddate = enddate, 
#                                     mimetype=mimetype, 
#                                     formatname=formatname,
#                                     parentid = NA,
#                                     con = con,
#                                     hostname = settings$run$host$name)
#       input.i$id <- raw.id$input.id
    } else { # Input specified by ID
    ## Get file path from input id
    input.ids[i] <- input.i$id
    file <- db.query(paste0('SELECT * FROM dbfiles WHERE container_id = ', input.i$id), con)
    file <- file.path(file$file_path, file$file_name)

    ## Load store data
    inputs[[i]]$data <- read.csv(file)
    }
    
    
    ## Preprocess data
    # TODO: Generalize
    if(as.numeric(var.ids[i]) == 297) {
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
  
  
  ## Set up likelihood functions
  #  TODO: Generalize
  llik.fn <- list()
  for(i in 1:n.input) {
    llik.fn[[i]] <- function(model, data) {
      NEEo <- data$data$NEE_or_fMDS #data$Fc   #umolCO2 m-2 s-1
      NEEq <- data$data$NEE_or_fMDSqc #data$qf_Fc
      NEEo[NEEq > 0] <- NA
    
      NEEm <- model
    
      NEE.resid <- abs(model - NEEo)
      NEE.pos <- (NEEm >= 0)
      LL <- c(dexp(NEE.resid[NEE.pos], 1/(data$b0 + data$bp*NEEm[NEE.pos]), log=TRUE), 
              dexp(NEE.resid[!NEE.pos],1/(data$b0 + data$bn*NEEm[!NEE.pos]),log=TRUE))
      n.obs = sum(!is.na(LL))
      return(list(LL=sum(LL,na.rm=TRUE), n=n.obs))
    }
  }

    # NPPo<- state$NPP[,,23]
    # AGBo<- state$AGB[,,24]
    # 
    # #parameters for bivariate normal likelihood
    # mu = c(mean(NPPo),mean(AGBo))
    # sigma = cov(cbind(NPPo,AGBo))


  ## Load params from previous run, if provided. 
  #  Indicated if params is a single non-null value, i.e. a dbfile ID
  if(length(params)==1) {
    params.db <- db.query(paste0("SELECT * FROM dbfiles WHERE id = ", params), con)
    load(file.path(params.db$file_path, params.db$file_name)) # replaces params
  }


  ## Allocate storage for params
  if(is.null(params)) { # No input given, starting fresh
    start  <- 1
    finish <- as.numeric(settings$assim.batch$iter)
    params <- matrix(numeric(), finish, nvar)
  } else {  
    start  <- nrow(params) + 1
    finish <- nrow(params) + as.numeric(settings$assim.batch$iter)
    params <- rbind(params, matrix(numeric(), finish - start + 1, nvar))
  }
  colnames(params) <- pname


  ## set initial conditions
  if(start==1){
    parm <- as.vector(p.median)
  } else{
    parm <- params[start-1, ]
  }
  names(parm) = pname
  LL.old <- -Inf
  prior.old <- -Inf

  
  ## set jump variance
  if(is.null(jvar)){
    jvar <- rep(0.1,nvar) # Default
  }

  ## Jump distribution adjustment
  ar <- numeric(nvar.sample)  ## Create vector of 0's (one zero per parameter)

  ## main MCMC loop
  for(i in start:finish){
    logger.info(paste("Data assimilation MCMC iteration",i,"of",finish))

    ## Adjust Jump distribution
    if(i %% adapt < 1){
        logger.info(paste0("Acceptance rates were (", 
                          paste(pname[vars], collapse=", "), ") = (", 
                          paste(round(ar/adapt,3), collapse=", "), ")"))
        logger.info(paste0("Using jump variances (", 
                          paste(round(jvar,3), collapse=", "), ")"))

        adj <- ar / adapt / ar.target
        adj[adj < adj.min] <- adj.min
        jvar <- jvar * adj
        logger.info(paste0("New jump variances are (", 
                          paste(round(jvar,3), collapse=", "), ")"))

        ar <- numeric(nvar.sample)
    }

    for(j in 1:nvar.sample){
      ## propose parameter values
      pnew  <- rnorm(1,parm[vars[j]],jvar[j])
      pstar <- parm
      pstar[vars[j]] <- pnew


      ## check that value falls within the prior
      prior.star <- dmvprior(pstar)
      if(is.finite(prior.star)){
        ## set RUN.ID
        if (!is.null(con)) {
          now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          paramlist <- paste("MCMC: chain",chain,"iteration",i,"variable",j)
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
          run.id <- paste("MCMC",chain,i,j,sep=".")
        }
        dir.create(file.path(settings$rundir, run.id), recursive=TRUE)
        dir.create(file.path(settings$modeloutdir, run.id), recursive=TRUE)


        ## write config
        do.call(my.write.config,args=list(settings$pfts, list(pft=pstar,env=NA),
                                          settings, run.id))


        ## write a README for the run
        cat("runtype     : pda.mcmc\n",
            "workflow id : ", as.character(workflow.id), "\n",
            "ensemble id : ", as.character(ensemble.id), "\n",
            "chain       : ", chain, "\n",
            "run         : ", i, "\n",
            "variable    : ", pname[vars[j]], "\n",
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
          NEEm <- rep(NEEm,each=length(NEEo)/length(NEEm))
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
        LL.total <- sum(LL.vec * weights)
        neff <- n.vec * weights


        ## insert Likelihood records in database
        if (!is.null(con)) {
          now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          paramlist <- paste("MCMC: chain",chain,"iteration",i,"variable",j)
          for(k in 1:n.input) {
            db.query(
              paste0("INSERT INTO likelihoods ", 
                "(run_id,         variable_id,        input_id,             loglikelihood, ", 
                 "n_eff,                  weight,              created_at) ",
              "values ('", 
                 run.id, "', '", var.ids[k], "', '", input.ids[k], "', '", LL.vec[k], "', '",
                 floor(neff[k]), "', '", weights[k] , "', '", now,"')"
              ), 
            con)
          }
        }


        ## accept or reject step
        a = LL.total-LL.old + prior.star - prior.old
        if(a > log(runif(1))){
          LL.old <- LL.total
          prior.old <- prior.star
          parm <- pstar 
          ar[j] <- ar[j] + 1
        }
      } ## end if(is.finite(prior.star))
    } ## end loop over variables
    
    ## save output
    params[i,] <- parm

  } ## end MCMC loop


  ## Assess MCMC output
  pdf(file.path(settings$pfts$pft$outdir,"pda.mcmc.diagnostics.pdf"))

  burnin <- min(2000,0.2*nrow(params))
  params.subset <- as.data.frame(params[burnin:nrow(params),vars])
  dm <- as.mcmc(params.subset)

  plot(dm)
  summary(dm)
  if(length(vars)>1){
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


  ## Save raw MCMC
  filename <- file.path(settings$outdir, "pda.mcmc.Rdata")
  save(params, file = filename)
  dbfile.insert(dirname(filename), basename(filename), 'Posterior', posteriorid, con)
  params.id <- db.query(paste0(
    "SELECT id FROM dbfiles WHERE 
      container_type = 'Posterior' AND file_name = 'pda.mcmc.Rdata' AND
      container_id = ", posteriorid),con)

  ## save named distributions
  filename <- file.path(settings$pfts$pft$outdir, 'post.distns.Rdata')
  post.distns <- approx.posterior(params.subset, prior, outdir = settings$pfts$pft$outdir)
  save(post.distns, file = filename)
  dbfile.insert(dirname(filename), basename(filename), 'Posterior', posteriorid, con)


  ## coerce parameter output into the same format as trait.mcmc
  pname <- rownames(post.distns)
  trait.mcmc <- list()
  for(i in vars){
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
  filename <- file.path(settings$pfts$pft$outdir, 'trait.mcmc.Rdata')
  save(trait.mcmc, file = filename)
  dbfile.insert(dirname(filename), basename(filename), 'Posterior', posteriorid, con)


  ## close database connection
  if(!is.null(con)) db.close(con)


  ## Output an updates settings list
  out <- settings$assim.batch
  out$params    <- params.id
  out$jump$jvar      <- as.list(jvar)
    names(out$jump$jvar) <- rep('jvar', length(jvar))
  out$var.names <- as.list(var.names)
    names(out$var.names) <- rep('var', length(var.names))
  out$prior     <- prior.id
  out$chain     <- chain
  return(out)
  
} ## end pda.mcmc
