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
pda.mcmc <- function(settings, params=NULL, jvar=NULL, var.names=NULL, prior=NULL, chain=NULL){
  # Quit if pda not requested in settings
  if(!('assim.batch' %in% names(settings))) {
    return()
  }

  require(coda)
  
  ## this bit of code is useful for defining the variables passed to this function 
  ## if you are debugging
  if(FALSE){
    model = "SIPNET"
    chain = 1
    params = NULL
    var.names = "Amax"
    jvar = NULL
    var.id = 297 ## NEE, canonical units umolC/m2/s
  }


  ## settings
  weight     <- 0.001
  model      <- settings$model$type
  write      <- settings$database$bety$write
  defaults   <- settings$pfts
  outdir     <- settings$run$host$outdir
  host       <- settings$run$host
  start.year <- strftime(settings$run$start.date,"%Y")
  end.year   <- strftime(settings$run$end.date,"%Y")

    # Some settings can be supplied via settings (for automation) or explicitly (interactive)
    if(is.null(params)) {
      params <- settings$assim.batch$params
    }
    if(is.null(jvar)) {
      jvar <- as.numeric(settings$assim.batch$jvar)
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
  if(write){
    con <- try(db.open(settings$database$bety), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    con <- NULL
  }


  ## priors
  if(is.null(settings$assim.batch$prior)){
    ## by default, use the most recent posterior as the prior
    pft.id <-  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
    priors <-  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)

    prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id IN (", paste(priors$id, collapse=','), ")"),con)

    prior.db <- prior.db[grep("post.distns.Rdata",prior.db$file_name),]

    settings$assim.batch$prior <- prior.db$container_id[which.max(prior.db$updated_at)]
  }
  prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ", settings$assim.batch$prior),con)
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
  do.call("require",list(paste0("PEcAn.",model)))
  my.write.config <- paste("write.config.",model,sep="")
  if(!exists(my.write.config)){
    logger.severe(paste(my.write.config,"does not exist. Please make sure that the PEcAn interface is loaded for",model))
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
  input.id <- settings$assim.batch$input.id
  data <- read.csv(settings$assim.batch$input)
  NEEo <- data$NEE_or_fMDS #data$Fc   #umolCO2 m-2 s-1
  NEEq <- data$NEE_or_fMDSqc #data$qf_Fc
  NEEo[NEEq > 0] <- NA
  
    # NPPo<- state$NPP[,,23]
    # AGBo<- state$AGB[,,24]
    # 
    # #parameters for bivariate normal likelihood
    # mu = c(mean(NPPo),mean(AGBo))
    # sigma = cov(cbind(NPPo,AGBo))


  ## calculate flux uncertainty parameters
  dTa <- get.change(data$Ta_f)
  flags <- dTa < 3   ## filter data to temperature differences that are less thatn 3 degrees
  NEE.params <- flux.uncertainty(NEEo,NEEq,flags,bin.num=20)
  b0 <- NEE.params$intercept
  bp <- NEE.params$slopeP
  bn <- NEE.params$slopeN

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


  ## main MCMC loop
  for(i in start:finish){
    logger.info(paste("Data assimilation MCMC iteration",i,"of",finish))
    
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
        do.call(my.write.config,args=list(defaults,list(pft=pstar,env=NA),
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
            "model       : ", model, "\n",
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


        ## read model output        
        NEEm <- read.output(run.id, outdir = file.path(outdir, run.id),
                            start.year, end.year, variables="NEE")$NEE*0.0002640674
          ## unit conversion kgC/ha/yr -> umolC/m2/sec
          # NPPvecm <-read.output(run.id, outdir = file.path(outdir, run.id),
          #                       start.year, end.year, variables="NPP")$NPP
          # NPPm<- sum(NPPvecm)


        ## match model and observations
        NEEm <- rep(NEEm,each=length(NEEo)/length(NEEm))
        set <- 1:length(NEEm)  ## ***** need a more intellegent year matching!!!
          # NPPm <- rep(NPPm,each=length(NPPo)/length(NPPm))
          # set <- 1:length(NPPm) 

        ## calculate likelihood
        NEE.resid <- abs(NEEm-NEEo[set])
        NEE.pos <- (NEEm > 0)
        LL.star <- c(dexp(NEE.resid[NEE.pos], 1/(b0 + bp*NEEm[NEE.pos]), log=TRUE), 
                     dexp(NEE.resid[!NEE.pos],1/(b0 + bp*NEEm[!NEE.pos]),log=TRUE))
        n.obs = sum(!is.na(LL.star))
        LL.star <- sum(LL.star,na.rm=TRUE)
          #loglikelihood for bivar normal distn of NPP and AGB
          # LL.star1 <-sum(dmvnorm(cbind(NPPo,AGBo), mu, sigma, log=TRUE), na.rm=TRUE)
          LL.star1 =0
        
        LL.total<-LL.star*weight+LL.star1


        ## insert Likelihood record in database
#         if (!is.null(con)) {
#           now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#           paramlist <- paste("MCMC: chain",chain,"iteration",i,"variable",j)
#           db.query(paste("INSERT INTO likelihoods (run_id, variable_id, input_id, loglikelihood, n_eff, weight,created_at) values ('", 
#                          run.id, "', '", var.id, "', '", input.id, "', '", LL.total, "', '",
#                          floor(n.obs*weight), "', '", weight , "', '", now,"')", sep=''), con)
#         }


        ## accept or reject step
        a = LL.total-LL.old + prior.star - prior.old
        if(a > log(runif(1))){
          LL.old <- LL.total
          prior.old <- prior.star
          parm <- pstar 
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

  a = 1-rejectionRate(dm)
  logger.info("Acceptance Rates")
  logger.info(a)

  print("acceptance")
  print(a)

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
  
} ## end pda.mcmc
