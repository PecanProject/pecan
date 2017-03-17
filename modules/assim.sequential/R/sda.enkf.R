##' @title sda.enkf
##' @name  sda.enkf
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param settings    PEcAn settings object
##' @param obs.mean    list of observations of the means of state variable (time X nstate)
##' @param obs.cov     list of observations of covariance matrices of state variables (time X nstate X nstate)
##' @param IC          initial conditions
##' @param Q           process covariance matrix given if there is no data to estimate it
##' 
##' @description State Variable Data Assimilation: Ensemble Kalman Filter
##' 
##' @return NONE
##' @export
##' 
sda.enkf <- function(settings, obs.mean, obs.cov, IC = NULL, Q = NULL) {
  
  ymd_hms <- lubridate::ymd_hms
  hms     <- lubridate::hms
  second  <- lubridate::second
  
  ###-------------------------------------------------------------------###
  ### read settings                                                     ###
  ###-------------------------------------------------------------------### 
  
  model      <- settings$model$type
  write      <- settings$database$bety$write
  defaults   <- settings$pfts
  outdir     <- settings$host$outdir
  rundir     <- settings$host$rundir
  host       <- settings$host
  forecast.time.step <- settings$state.data.assimilation$forecast.time.step  #idea for later generalizing
  nens       <- as.numeric(settings$state.data.assimilation$n.ensemble)
  processvar <- settings$state.data.assimilation$process.variance
  sample_parameters <- settings$state.data.assimilation$sample.parameters
  var.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                             function(x) {
                               x$variable.name
                             }, 
                             USE.NAMES = FALSE), 
                      use.names = FALSE)
  names(var.names) <- NULL
  
  ###-------------------------------------------------------------------###
  ### get model specific functions                                      ###
  ###-------------------------------------------------------------------### 
  do.call("require", list(paste0("PEcAn.", model)))
  my.write.config  <- paste0("write.config.", model)
  my.read_restart  <- paste0("read_restart.", model)
  my.write_restart <- paste0("write_restart.", model)
  my.split_inputs  <- paste0("split_inputs.", model)
  
  if (!exists(my.write.config)) {
    print(paste(my.write.config, "does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for", model))
    stop()
  }
  
  if (!exists(my.split_inputs)) {
    print(paste(my.split_inputs, "does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for", model))
    stop()
  }
  
  ###-------------------------------------------------------------------###
  ### load model specific input ensembles for initial runs              ###
  ###-------------------------------------------------------------------### 
  n.inputs <- max(table(names(settings$run$inputs)))
  if(n.inputs > nens){
    sampleIDs <- 1:nens
  }else{
    sampleIDs <- c(1:n.inputs,sample.int(n.inputs, (nens - n.inputs), replace = TRUE))
  }
  
  ens.inputs <- list()
  inputs <- list()
  for(i in seq_len(nens)){
    ### get only nessecary ensemble inputs. Do not change in anaylysis
    ens.inputs[[i]] <- get.ensemble.inputs(settings = settings, ens = sampleIDs[i])
    
    ### model specific split inputs
    inputs[[i]] <- do.call(my.split_inputs, 
                      args = list(settings = settings, 
                                  start.time = settings$run$start.date, 
                                  stop.time = settings$run$end.date,
                                  inputs = ens.inputs[[i]]))
  }

  #### replaces stuff below
  
  # if(model == "LINKAGES"){
  #   new.met <- paste0(rundir,"/climate.Rdata") #doesn't do anything but write stuff to README
  #   met <- new.met #HACK
  # }
  # if(model == "SIPNET"){
  #   ## split clim file
  #      full.met <- c(settings$run$inputs$met$path) #
  #      new.met  <- file.path(settings$rundir,basename(full.met))
  #      file.copy(full.met,new.met)
  #      met <- split.met.SIPNET(new.met)
  # }
  
  ###-------------------------------------------------------------------###
  ### open database connection                                          ###
  ###-------------------------------------------------------------------### 
  if (write) {
    con <- try(db.open(settings$database$bety), silent = TRUE)
    if (is(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(db.close(con))
    }
  } else {
    con <- NULL
  }
  
  ###-------------------------------------------------------------------###
  ### get new workflow ids                                              ###
  ###-------------------------------------------------------------------### 
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  
  ###-------------------------------------------------------------------###
  ### create ensemble ids                                               ###
  ###-------------------------------------------------------------------### 
  if (!is.null(con)) {
    # write ensemble first
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", now, 
                   "', 'EnKF', ", workflow.id, ")", sep = ""), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep = ""), 
                            con)[["id"]]
  } else {
    ensemble.id <- -1
  }
  
  ###-------------------------------------------------------------------###
  ### perform initial set of runs                                       ###
  ###-------------------------------------------------------------------###  
  run.id <- list()
  X <- IC
  
  ## Load Parameters
  if (sample_parameters == TRUE) {
    settings$ensemble$size <- settings$state.data.assimilation$n.ensemble
  } else {
    settings$ensemble$size <- 1
  }
  get.parameter.samples(settings, ens.sample.method = settings$ensemble$method)  ## Aside: if method were set to unscented, would take minimal changes to do UnKF
  load(file.path(settings$outdir, "samples.Rdata"))  ## loads ensemble.samples
  
  if ("env" %in% names(ensemble.samples)) {
    ensemble.samples$env <- NULL
  }
  
  params <- list()
  for (i in seq_len(nens)) {
    if (sample_parameters == TRUE) {
      params[[i]] <- lapply(ensemble.samples, function(x, n) {
        x[i, ]
      }, n = i)
    } else {
      params[[i]] <- ensemble.samples
    }
  }
  
  for (i in seq_len(nens)) {
    
    ## set RUN.ID
    if (!is.null(con)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      paramlist <- paste("EnKF:", i)
      run.id[[i]] <- db.query(paste0("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id,", 
                                     " parameter_list) values ('", settings$model$id, "', '", settings$run$site$id, "', '", 
                                     settings$run$start.date, "', '", settings$run$end.date, "', '", settings$outdir, "', '", 
                                     now, "', ", ensemble.id, ", '", paramlist, "') RETURNING id"), con)
    } else {
      run.id[[i]] <- paste("EnKF", i, sep = ".")
    }
    dir.create(file.path(settings$rundir, run.id[[i]]), recursive = TRUE)
    dir.create(file.path(settings$modeloutdir, run.id[[i]]), recursive = TRUE)
    
    ## Write Configs
    do.call(my.write.config, args = list(defaults = NULL, 
                                         trait.values = params[[i]], 
                                         settings = settings, 
                                         run.id = run.id[[i]], 
                                         inputs = inputs[[i]], 
                                         IC = IC[i, ]))
    
    ## write a README for the run
    cat("runtype     : sda.enkf\n",
        "workflow id : ", as.character(workflow.id), "\n",
        "ensemble id : ", as.character(ensemble.id), "\n",
        "ensemble    : ", i, "\n",
        "run id      : ", as.character(run.id[[i]]), "\n",
        "pft names   : ", as.character(lapply(settings$pfts, function(x) x[["name"]])), "\n",
        "model       : ", model, "\n",
        "model id    : ", settings$model$id, "\n",
        "site        : ", settings$run$site$name, "\n",
        "site  id    : ", settings$run$site$id, "\n",
        "met data    : ", inputs$met$path, "\n",
        "start date  : ", settings$run$start.date, "\n",
        "end date    : ", settings$run$end.date, "\n",
        "hostname    : ", settings$host$name, "\n",
        "rundir      : ", file.path(settings$host$rundir, run.id[[i]]), "\n",
        "outdir      : ", file.path(settings$host$outdir, run.id[[i]]), "\n",
        file = file.path(settings$rundir, run.id[[i]], "README.txt"), 
        sep='')
  }
  
  ## add the jobs to the list of runs
  cat(as.character(unlist(run.id)), 
      file = file.path(settings$rundir, "runs.txt"),
      sep = "\n", 
      append = FALSE)
  
  ## start model runs
  start.model.runs(settings, settings$database$bety$write)
  save.image(file.path(outdir, "sda.initial.runs.Rdata"))
  
  ###-------------------------------------------------------------------###
  ### tests before data assimilation                                    ###
  ###-------------------------------------------------------------------###  
  
  # at some point add a lot of error checking 
  # read time from data if data is missing you still need
  # to have NAs or NULL with date name vector to read the correct netcdfs by read_restart
  
  obs.times <- names(obs.mean)
  obs.times.POSIX <- ymd_hms(obs.times,truncated = 3)
  
  for (i in seq_along(obs.times)) {
    if (is.na(obs.times.POSIX[i])) {
      if (is.na(ymd(obs.times[i]))) { #TO DO can't find function ymd(). fix it.
        print("Error: no dates associated with observations")
      } else {
        ### Data does not have time associated with dates 
        ### Adding 12:59:59PM assuming next time step starts one second later
        print("Pumpkin Warning: adding one minute before midnight time assumption to dates associated with data")
        obs.times.POSIX[i] <- ymd_hms(paste(obs.times[i], "23:59:59"))
      }
    }
  }
  obs.times <- obs.times.POSIX
  
  # need explicit forecast length variable in settings start time, stop time, restart time if
  # restart time is not provided restart in stop time
  
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###  
  
  nt          <- length(obs.times)
  FORECAST    <- ANALYSIS <- list()
  enkf.params <- list()
  aqq         <- NULL
  bqq         <- numeric(nt + 1)
  CI.X1       <- matrix(0, 3, nt)
  CI.X2       <- CI.X1
  q.bar        <- NULL #default process covariance matrix
  
  ##### Creating matrices that describe the bounds of the state variables
  ##### interval is remade everytime depending on the data at time t
  ##### state.interval stays constant and converts new.analysis to be within the correct bounds
  interval    <- NULL
  state.interval <- cbind(as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','min_value')),
          as.numeric(lapply(settings$state.data.assimilation$state.variables,'[[','max_value')))
  rownames(state.interval) <- var.names
  
  wish.df <- function(Om, X, i, j, col) {
    (Om[i, j]^2 + Om[i, i] * Om[j, j]) / var(X[, col])
  } # wish.df
  
#   ## JAGS models for numerical update of state and process error
#   #### Tobit Model
#   tobit.model <- "
#   model{ 
#   
#   q  ~ dwish(aq,bq) ## aq and bq are estimated over time
#   Q <- inverse(q)
#   X.mod ~ dmnorm(muf,pf) ## Model Forecast ##muf and pf are assigned from ensembles
# 
#   ## add process error
#   X  ~ dmnorm(X.mod,q)
#  
#   #agb linear
#   #y_star <- X[choose]
#   
#   #f.comp non linear
#   y_star <- X[1:9] / sum(X[1:9])
#   
#   ## Analysis
#   y.censored  ~ dmnorm(y_star,r) ##cannot be partially observed -- JAGS Manual
#   
#   for(i in 1:N){
#   y.ind[i] ~ dinterval(y.censored[i], interval[i,])
#   }
#   
# }"
#   
#   tobit2space <- "
#   model{ 
# 
#   for(n in 1:nens){
#       y.censored[n,] ~ dmnorm(muf,pf) ##cannot be partially observed -- JAGS Manual
#   
#       for(i in 1:N){
#           y.ind[n,i] ~ dinterval(y.censored[n,i], interval[i,])
#       }
#   }
# 
#   #Priors
#   pf  ~ dwish(aq,bq)
#   muf  ~ dmnorm(mu.prior,cov.prior)
#   
# }"
#   
  sampler_toggle <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
      type <- control$type
      nested_sampler_name <- paste0('sampler_', type)
      control_new <- nimbleOptions('MCMCcontrolDefaultList')
      control_new[[names(control)]] <- control
      nested_sampler_list <- nimbleFunctionList(sampler_BASE)
      nested_sampler_list[[1]] <- do.call(nested_sampler_name, list(model, mvSaved, target, control_new))
      toggle <- 1
    },
    run = function() {
      if(toggle == 1)
        nested_sampler_list[[1]]$run()
    },
    methods = list(
      reset = function()
        nested_sampler_list[[1]]$reset()
    )
  )
  
  tobit.model <- nimbleCode({ 
    
    q[1:N,1:N]  ~ dwish(R = aq[1:N,1:N], df = bq) ## aq and bq are estimated over time
    Q[1:N,1:N] <- inverse(q[1:N,1:N])
    X.mod[1:N] ~ dmnorm(muf[1:N], prec = pf[1:N,1:N]) ## Model Forecast ##muf and pf are assigned from ensembles
    
    ## add process error
    X[1:N]  ~ dmnorm(X.mod[1:N], prec = q[1:N,1:N])
    
    #agb linear
    #y_star[1:N,1:N] <- X[1:N,1:N] #[choose]
    
    #f.comp non linear
    #y_star <- X[1:9] / sum(X[1:9])
    
    ## Analysis
    y.censored[1:YN] ~ dmnorm(X[1:YN], prec = r[1:YN,1:YN]) #is it an okay assumpution to just have X and Y in the same order?
    
    #don't flag y.censored as data, y.censored in inits
    #remove y.censored samplers and only assign univariate samplers on NAs
    
    for(i in 1:YN){
      y.ind[i] ~ dconstraint(y.censored[i] > 0)
    }
    
  })
  
    tobit2space.model <- nimbleCode({

    for(n in 1:nens){
        y.censored[n,1:N] ~ dmnorm(muf[1:N],pf[1:N,1:N])

        for(i in 1:N){
            y.ind[n,i] ~ dinterval(y.censored[n,i], interval[i,1:2])
        }
    }

    #Priors
    pf[1:N,1:N]  ~ dwish(aq[1:N,1:N],bq)
    muf[1:N]  ~ dmnorm(mu.prior[1:N],cov.prior[1:N,1:N])

  })
  
  t1         <- 1
  pink       <- col2rgb("deeppink")
  alphapink  <- rgb(pink[1], pink[2], pink[3], 180, max = 255)
  green      <- col2rgb("green")
  alphagreen <- rgb(green[1], green[2], green[3], 75, max = 255)
  blue       <- col2rgb("blue")
  alphablue  <- rgb(blue[1], blue[2], blue[3], 75, max = 255)
  
  ###-------------------------------------------------------------------###
  ### loop over time                                                    ###
  ###-------------------------------------------------------------------###  
  for (t in 4)) {#seq_len(nt)
    
    ###-------------------------------------------------------------------###
    ### read restart                                                      ###
    ###-------------------------------------------------------------------###  
    X <- list()
    for (i in seq_len(nens)) {
      X[[i]] <- do.call(my.read_restart, args = list(outdir = outdir, 
                                                     runid = run.id[[i]], 
                                                     stop.time = obs.times[t], 
                                                     settings = settings, 
                                                     var.names = var.names, 
                                                     params = params[[i]]))
    }
    
    X <- do.call(rbind, X)
    
    FORECAST[[t]] <- X
    
    obs <- which(!is.na(obs.mean[[t]]))
    
    #mu.f <- as.numeric(apply(X, 2, mean, na.rm = TRUE))
    #Pf <- cov(X)
    
    ### create matrix the describes the support for each observed state variable at time t
    intervalX <- matrix(NA, ncol(X), 2)
    rownames(intervalX) <- colnames(X)
    for(i in 1:length(var.names)){
      intervalX[which(startsWith(rownames(intervalX),
                                var.names[i])), ] <- matrix(c(as.numeric(settings$state.data.assimilation$state.variables[[i]]$min_value),
                                                              as.numeric(settings$state.data.assimilation$state.variables[[i]]$max_value)),
                                                            length(which(startsWith(rownames(intervalX),
                                                                                    var.names[i]))),2,byrow = TRUE)
    }
    
    #### These vectors are used to categorize data based on censoring from the interval matrix
    x.ind <- matrix(NA, nens, ncol(X)) ; x.censored <- x.ind
    for(n in 1:nens){
      x.ind[n,] <- as.numeric(X[n,] > intervalX[,1])
      x.censored[n,] <- as.numeric(ifelse(X[n,] > intervalX[,1], X[n,], 0))
    }

    if(t == 1){ #| length(x.ind[1,]) > mu.f
      #y.obs = Y.dat[1,]
      constants.tobit2space = list(N = ncol(X), nens = nens)
      data.tobit2space = list(interval = intervalX,
                        y.ind = x.ind,
                        y.censored = x.censored,
                        aq = diag(ncol(X))*ncol(X), 
                        bq = ncol(X),
                        mu.prior = colMeans(X), #cheating? basically gives us back means
                        cov.prior = diag(ncol(X)))
      
      inits.tobit2space = list(pf = diag(ncol(X)), muf = rep(0,ncol(X))) #
      #set.seed(0)
      #ptm <- proc.time()
      tobit2space_pred <- nimbleModel(tobit2space.model, data = data.tobit2space,
                                constants = constants.tobit2space, inits = inits.tobit2space)
      ## Adding X.mod,q,r as data for building model.
      conf_tobit2space <- configureMCMC(tobit2space_pred, print=TRUE)
      conf_tobit2space$addMonitors(c("pf", "muf")) 
      ## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
      ## important!
      ## this is needed for correct indexing later
      samplerNumberOffset_tobit2space <- length(conf_tobit2space$getSamplers())
      
      for(n in 1:nens){
        for(i in 1:length(x.ind)) {
          node <- paste0('y.censored[',n,',',i,']')
          conf_tobit2space$addSampler(node, 'toggle', control=list(type='RW'))
          ## could instead use slice samplers, or any combination thereof, e.g.:
          ##conf$addSampler(node, 'toggle', control=list(type='slice'))
        }
      }
      
      
      conf_tobit2space$printSamplers()
      
      ## can monitor y.censored, if you wish, to verify correct behaviour
      #conf_tobit2space$addMonitors('y.censored')
      
      Rmcmc_tobit2space <- buildMCMC(conf_tobit2space)
      
      Cmodel_tobit2space <- compileNimble(tobit2space_pred)
      Cmcmc_tobit2space <- compileNimble(Rmcmc_tobit2space, project = tobit2space_pred)
      
    }else{
      Cmodel_tobit2space$y.ind <- x.ind
      Cmodel_tobit2space$y.censored <- x.censored
      #Cmodel_tobit2space$mu.prior <- as.vector(colMeans(X)) #doesn't work
      #Error in envRefSetField(x, what, refObjectClass(x), selfEnv, value) : 
      #‘mu.prior’ is not a field in class “Ccode”
      
      for(n in 1:nens){
        for(i in 1:ncol(x.ind)) {
          ## ironically, here we have to "toggle" the value of y.ind[i]
          ## this specifies that when y.ind[i] = 1,
          ## indicator variable is set to 0, which specifies *not* to sample
          valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[n,i])
        }
      }
    }
    
    set.seed(0)
    dat.tobit2space <- runMCMC(Cmcmc_tobit2space, niter = 10000, progressBar=FALSE)
    
    
    # #### JAGS update list
    # update.tobit2space <- list(interval = intervalX,
    #                N = ncol(X),
    #                y.ind = x.ind,
    #                y.censored = x.censored,
    #                aq = diag(ncol(X))*ncol(X), 
    #                bq = ncol(X),
    #                nens = nens,
    #                mu.prior = colMeans(X), #cheating? basically gives us back means
    #                cov.prior = diag(ncol(X)))
    # 
    # #### Run JAGS Tobit Model
    # mod.tobit2space <- jags.model(file = textConnection(tobit2space),
    #                   data = update.tobit2space,
    #                   n.adapt = 1000, 
    #                   n.chains = 3)  #inits for q?
    # 
    # jdat.tobit2space <- coda.samples(mod.tobit2space, variable.names = c("pf", "muf"), n.iter = 10000)
    # dat.tobit2space  <- as.matrix(jdat.tobit2space)
    
    
    ## update parameters
    dat.tobit2space  <- dat.tobit2space[3000:10000, ]
    iPf   <- grep("pf", colnames(dat.tobit2space))
    imuf   <- grep("muf[", colnames(dat.tobit2space), fixed = TRUE)
    mu.f <- colMeans(dat.tobit2space[, imuf])
    mPf <- dat.tobit2space[, iPf]  # Omega, Precision
    Pf <- matrix(apply(mPf, 2, mean), length(mu.f), length(mu.f))  # Mean Omega, Precision
    
    ###-------------------------------------------------------------------###
    ### analysis                                                          ###
    ###-------------------------------------------------------------------###  
    if (any(obs)) {
      # if no observations skip analysis
      choose <- na.omit(charmatch(
        na.omit(unlist(lapply(strsplit(colnames(X),
                                       split = paste('AGB.pft.')),
                              function(x) x[2]))),
        na.omit(unlist(lapply(strsplit(names(obs.mean[[t]]),
                                       split = paste('AGB.pft.')), #TO DO don't hardcode this
                              function(x) x[2]))))) #matches y to model
      
      Y <- unlist(obs.mean[[t]][choose])
      
      R <- as.matrix(obs.cov[[t]]) #TO DO: R probably needs to be organized like Y too?
      
      if (length(obs.mean[[t]]) > 1) {
        for (s in seq_along(obs.mean[[t]])) {
          if (diag(R)[s] == 0) {
            # if covariance is 0 then set it to half of the minimum covariance to avoid solve() problems
            diag(R)[s] <- min(diag(R)[which(diag(R) != 0)])/2
          }
          if (diag(Pf)[s] == 0) {
            # if covariance is 0 then set it to half of the minimum covariance to avoid solve() problems
            diag(Pf)[s] <- min(diag(Pf)[which(diag(Pf) != 0)])/2
          }
        }
      }
      
      ### TO DO: plotting not going to work because of observation operator i.e. y and x are on different scales
      
      #### Plot Data and Forecast
      if (FALSE) {#interactive() & t > 1
        t1 <- 1
        names.y <- unique(unlist(lapply(obs.mean[t1:t], function(x) { names(x) })))
        Ybar <- t(sapply(obs.mean[t1:t], function(x) {
          tmp <- rep(NA, length(names.y))
          names(tmp) <- names.y
          mch <- match(names(x), names.y)
          tmp[mch] <- x[mch]
          return(tmp)
        }))
        
        Ybar <- Ybar[, na.omit(pmatch(colnames(X), colnames(Ybar)))]
        YCI <- t(as.matrix(sapply(obs.cov[t1:t], function(x) {
          if (is.null(x)) {
            return(rep(NA, length(names.y)))
          }
          return(sqrt(diag(x)))
        })))
        
        for (i in sample(x = 1:ncol(X), size = 2)) {
          t1 <- 1
          Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
          Xci <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
          
          plot(as.Date(obs.times[t1:t]), 
               Xbar, 
               ylim = range(c(Ybar, Xci), na.rm = TRUE),
               type = "n", 
               xlab = "Year", 
               ylab = "kg/m^2",
               main = colnames(X)[i])
          
          # observation / data
          if (i <= ncol(Ybar)) {
            ciEnvelope(as.Date(obs.times[t1:t]), 
                       as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                       as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                       col = alphagreen)
            lines(as.Date(obs.times[t1:t]), as.numeric(Ybar[, i]), type = "l", 
                  col = "darkgreen", lwd = 2)
          }
          
          # forecast
          ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = alphablue)  # col='lightblue')
          lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2)
        }
      }
      
      ###-------------------------------------------------------------------###
      ### Kalman Filter                                                     ###
      ###-------------------------------------------------------------------###
      if (processvar == FALSE) {
        ## design matrix
        H <- matrix(0, length(Y), ncol(X)) #H maps true state to observed state
        #linear
        for (i in choose) {
          H[i, i] <- 1 
        }
        #non-linear fcomp
        for (i in choose) {
          H[i, i] <- 1/sum(mu.f) #? this seems to get us on the right track. mu.f[i]/sum(mu.f) doesn't work. 
        }
        ## process error
        if (exists("Q")) {
          Pf <- Pf + Q
        }
        ## Kalman Gain
        K <- Pf %*% t(H) %*% solve((R + H %*% Pf %*% t(H)))
        ## Analysis
        mu.a <- mu.f + K %*% (Y - H %*% mu.f)
        Pa   <- (diag(ncol(X)) - K %*% H) %*% Pf
        enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
      } else {
        
        ###-------------------------------------------------------------------###
        ### Generalized Ensemble Filter                                       ###
        ###-------------------------------------------------------------------###
        
        #### initial conditions
        bqq[1]     <- length(mu.f)
        if(is.null(aqq)){
          aqq      <- array(0, dim = c(nt,ncol(X),ncol(X)))
        }else{
          if(ncol(X)!=dim(aqq)[2]|ncol(X)!=dim(aqq)[3]){
            print('error: X has changed dimensions')
          }
        }
        aqq[1, , ] <- diag(length(mu.f)) * bqq[1]

        #### changing diagonal if the covariance is too small for the matrix to be inverted 
        #### This problem is different than R problem because diag(Pf) can be so small it can't be inverted 
        #### Need a different fix here someday
        for (i in seq_along(diag(Pf))) {
          if (diag(Pf)[i] == 0) {
            diag(Pf)[i] <- min(diag(Pf)[which(diag(Pf) != 0)])/2  #HACK
          }
        }
        
        ### create matrix the describes the support for each observed state variable at time t
        interval <- matrix(NA, length(obs.mean[[t]]), 2)
        
        ### TO DO interval not working
        rownames(interval) <- names(obs.mean[[t]])
        for(i in 1:length(var.names)){
          interval[which(startsWith(rownames(interval),
                                    var.names[i])), ] <- matrix(c(as.numeric(settings$state.data.assimilation$state.variables[[i]]$min_value),
                                                                  as.numeric(settings$state.data.assimilation$state.variables[[i]]$max_value)),
                                                                length(which(startsWith(rownames(interval),
                                                                                        var.names[i]))),2,byrow = TRUE)
        }
        interval[,1]<-0
        interval[,2]<-10000000 #TO DO need to make more general
        
        #### These vectors are used to categorize data based on censoring 
        #### from the interval matrix
        y.ind <- as.numeric(Y > interval[,1])
        y.censored <- as.numeric(ifelse(Y > interval[,1], Y, 0))
        
        if(t==1){ #TO need to make something that works to pick weather to compile or not
          #y.obs = Y.dat[1,]
          constants.tobit = list(N = ncol(X), YN = length(y.ind)) #, nc = 1
          dimensions.tobit = list(X = ncol(X), X.mod = ncol(X), Q = c(ncol(X),ncol(X))) #  b = dim(inits.pred$b),
          
          data.tobit = list(muf = as.vector(mu.f), pf = Pf, aq = aqq[t,,], bq = bqq[t],
                            y.ind = y.ind,
                            y.censored = y.censored,
                            r = solve(R))
          inits.pred = list(q = diag(ncol(X)), X.mod = as.vector(mu.f), X = rnorm(ncol(X),0,1)) #
          #set.seed(0)
          #ptm <- proc.time()
          model_pred <- nimbleModel(tobit.model, data = data.tobit, dimensions = dimensions.tobit,
                                    constants = constants.tobit, inits = inits.pred)
          ## Adding X.mod,q,r as data for building model.
          conf <- configureMCMC(model_pred, print=TRUE)
          conf$addMonitors(c("X","q","Q")) 
          ## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
          ## important!
          ## this is needed for correct indexing later
          samplerNumberOffset <- length(conf$getSamplers())
          
          for(i in 1:length(y.ind)) {
            node <- paste0('y.censored[',i,']')
            conf$addSampler(node, 'toggle', control=list(type='RW'))
            ## could instead use slice samplers, or any combination thereof, e.g.:
            ##conf$addSampler(node, 'toggle', control=list(type='slice'))
          }
          
          conf$printSamplers()
          
          ## can monitor y.censored, if you wish, to verify correct behaviour
          #conf$addMonitors('y.censored')
          
          Rmcmc <- buildMCMC(conf)
          
          Cmodel <- compileNimble(model_pred)
          Cmcmc <- compileNimble(Rmcmc, project = model_pred)
          
        }else{
          Cmodel$y.ind <- y.ind
          Cmodel$y.censored <- y.censored
          Cmodel$aq <- aqq[t,,]
          Cmodel$bq <- bqq[t]
          Cmodel$muf <- mu.f
          Cmodel$pf <- Pf
          Cmodel$r <- solve(R)
          
          for(i in 1:length(y.ind)) {
            ## ironically, here we have to "toggle" the value of y.ind[i]
            ## this specifies that when y.ind[i] = 1,
            ## indicator variable is set to 0, which specifies *not* to sample
            valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
          }
      
        }
        
        set.seed(0)
        dat <- runMCMC(Cmcmc, niter = 10000, progressBar=FALSE)
        
        # #### JAGS update list
        # update <- list(interval = interval,
        #                N = length(y.ind),
        #                y.ind = y.ind,
        #                y.censored = y.censored, 
        #                r = solve(R),
        #                muf = mu.f, 
        #                pf =  Pf, #check
        #                aq = aqq[t,,], 
        #                bq = bqq[t],
        #                choose = choose)
        # 
        # #### Run JAGS Tobit Model
        # mod <- jags.model(file = textConnection(tobit.model),
        #                   data = update,
        #                   n.adapt = 1000, 
        #                   n.chains = 3)  #inits for q?
        # 
        # jdat <- coda.samples(mod, variable.names = c("X", "q"), 
        #                      n.iter = 10000)
        # 
        # #gelman.diag(jdat[,1:10])
        # #is it reasonable to expect convergence every year of every parameter?
        # #should we put a stop in if params don't converge?
        
        ## update parameters
        dat  <- dat[3000:10000, ]
        iq   <- grep("q", colnames(dat))
        iX   <- grep("X[", colnames(dat), fixed = TRUE)
        mu.a <- colMeans(dat[, iX])
        Pa   <- cov(dat[, iX])
        Pa[is.na(Pa)] <- 0
        
        CI.X1[, t] <- quantile(dat[, iX[1]], c(0.025, 0.5, 0.975))
        CI.X2[, t] <- quantile(dat[, iX[2]], c(0.025, 0.5, 0.975))
        
        mq <- dat[, iq]  # Omega, Precision
        q.bar <- matrix(apply(mq, 2, mean), length(mu.f), length(mu.f))  # Mean Omega, Precision
        
        col <- matrix(1:length(mu.f) ^ 2, length(mu.f), length(mu.f))
        WV  <- matrix(0, length(mu.f), length(mu.f))
        for (i in seq_along(mu.f)) {
          for (j in seq_along(mu.f)) {
            WV[i, j] <- wish.df(q.bar, X = mq, i = i, j = j, col = col[i, j])
          }
        }
        
        n <- mean(WV)
        if (n < length(mu.f)) {
          n <- length(mu.f)
        }
        V <- solve(q.bar) * n
        
        aqq[t + 1, , ]   <- V
        bqq[t + 1]       <- n
        enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, 
                                 Pa = Pa, q.bar = q.bar, n = n)
      }
      
    } else {
      ###-------------------------------------------------------------------###
      ### No Observations -- Starts Here                                    ###
      ###-------------------------------------------------------------------### 
      
      ### no process variance -- forecast is the same as the analysis ###
      if (processvar==FALSE) {
        mu.a <- mu.f
        Pa   <- Pf + Q
        ### yes process variance -- no data
      } else {
        mu.a <- mu.f
        if(is.null(q.bar)){
          q.bar <- diag(ncol(X))
          print('Process variance not estimated. Analysis has been given uninformative process variance')
        } 
        Pa   <- Pf + solve(q.bar)
      }
      enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
    }
    
    ## update state matrix
    analysis <- as.data.frame(rmvnorm(as.numeric(nens), mu.a, Pa, method = "svd"))
    colnames(analysis) <- colnames(X)
    
    ##### Mapping analysis vectors to be in bounds of state variables
    for(i in 1:ncol(analysis)){
      int.save <- state.interval[which(startsWith(colnames(analysis)[i],
                      var.names)),]
      analysis[analysis[,i] < int.save[1],i] <- int.save[1]
      analysis[analysis[,i] > int.save[2],i] <- int.save[2]
    }
    
    ## in the future will have to be separated from analysis
    new.state  <- analysis
    new.params <- params
    
    ANALYSIS[[t]] <- analysis
    
    if (interactive() & t > 1) { #
      t1 <- 1
      names.y <- unique(unlist(lapply(obs.mean[t1:t], function(x) { names(x) })))
      Ybar <- t(sapply(obs.mean[t1:t], function(x) {
        tmp <- rep(NA, length(names.y))
        names(tmp) <- names.y
        mch <- match(names(x), names.y)
        tmp[mch] <- x[mch]
        tmp
      }))
      Ybar <- Ybar[, na.omit(pmatch(colnames(X), colnames(Ybar)))]
      YCI <- t(as.matrix(sapply(obs.cov[t1:t], function(x) {
        if (is.null(x)) {
          rep(NA, length(names.y))
        }
        sqrt(diag(x))
      })))
      
      par(mfrow = c(2, 1))
      for (i in sample(size = 2,x = 1:9)) {
        t1 <- 1
        Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
        Xci  <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
        
        Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
        XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
        
        ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                                    function(x) { x })[2, ], use.names = FALSE)
        
        plot(as.Date(obs.times[t1:t]), 
             Xbar, 
             ylim = range(c(XaCI, Xci), na.rm = TRUE), 
             type = "n", 
             xlab = "Year", 
             ylab = ylab.names[grep(colnames(X)[i], var.names)], 
             main = colnames(X)[i])
        
        # observation / data
        if (i <= ncol(Ybar)) {
          ciEnvelope(as.Date(obs.times[t1:t]),
                     as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                     as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                     col = alphagreen)
          lines(as.Date(obs.times[t1:t]), 
                as.numeric(Ybar[, i]), 
                type = "l", 
                col = "darkgreen", 
                lwd = 2)
        }
        
        # forecast
        ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = alphablue)  #col='lightblue')
        lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2)
        
        # analysis
        ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = alphapink)
        lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2)
      }
    }
    
    ###-------------------------------------------------------------------###
    ### forecast step                                                     ###
    ###-------------------------------------------------------------------### 
    if (t < nt) {
      
      ###-------------------------------------------------------------------###
      ### split model specific inputs for current runs                      ###
      ###-------------------------------------------------------------------### 
 
      inputs <- list()
      for(i in seq_len(nens)){
        inputs[[i]] <- do.call(my.split_inputs, 
                          args = list(settings = settings, 
                                      start.time = (ymd_hms(obs.times[t],truncated = 3) + second(hms("00:00:01"))), 
                                      stop.time = obs.times[t + 1],
                                      inputs = ens.inputs[[i]])) 
      }
      
      
      ###-------------------------------------------------------------------###
      ### write restart by ensemble                                         ###
      ###-------------------------------------------------------------------### 
      
      for (i in seq_len(nens)) {
        do.call(my.write_restart, 
                args = list(outdir = outdir, 
                            runid = run.id[[i]], 
                            start.time = (ymd_hms(obs.times[t],truncated = 3) + second(hms("00:00:01"))),
                            stop.time = ymd_hms(obs.times[t + 1],truncated = 3), 
                            settings = settings,
                            new.state = new.state[i, ], 
                            new.params = new.params[[i]], 
                            inputs = inputs, 
                            RENAME = TRUE))
      }
      ###-------------------------------------------------------------------###
      ### Run model                                                         ###
      ###-------------------------------------------------------------------### 
      print(paste("Running Model for Year", as.Date(obs.times[t]) + 1))
      start.model.runs(settings, settings$database$bety$write)
    }
    
    ###-------------------------------------------------------------------###
    ### save outputs                                                      ###
    ###-------------------------------------------------------------------### 
    save(t, FORECAST, ANALYSIS, enkf.params, file = file.path(settings$outdir, "sda.output.Rdata"))
    
  }  ## end loop over time
  ###-------------------------------------------
  
  ###-------------------------------------------------------------------###
  ### create diagnostics                                                ###
  ###-------------------------------------------------------------------### 
  
  ### LOAD CLIMATE ### HACK ### LINKAGES SPECIFIC
  if (model == "LINKAGES") {
    climate_file <- settings$run$inputs$met$path
    load(climate_file)
    temp.mat     <- temp.mat[year(obs.times) - 853, ]
    precip.mat   <- precip.mat[year(obs.times) - 853, ]
  } else {
    print("climate diagnostics under development")
  }
  
  ### Diagnostic graphs
  pdf(file.path(settings$outdir, "EnKF.pdf"))
  
  ###-------------------------------------------------------------------###
  ### time series                                                       ###
  ###-------------------------------------------------------------------### 
  
  names.y <- unique(unlist(lapply(obs.mean[t1:t], function(x) { names(x) })))
  Ybar <- t(sapply(obs.mean[t1:t], function(x) {
    tmp <- rep(NA, length(names.y))
    names(tmp) <- names.y
    mch <- match(names(x), names.y)
    tmp[mch] <- x[mch]
    return(tmp)
  }))
  Ybar <- Ybar[, choose]
  YCI <- t(as.matrix(sapply(obs.cov[t1:t], function(x) {
    if (is.null(x)) {
      rep(NA, length(names.y))
    }
    sqrt(diag(x))
  })))  #need to make this from quantiles for lyford plot data
  # YCI = YCI[,pmatch(colnames(X), names(obs.mean[[nt]][[1]]))]
  Xsum <- plyr::laply(FORECAST, function(x) { mean(rowSums(x[,1:9], na.rm = TRUE)) })

  pdf('fcomp.kalman.filter.pdf')
  for (i in seq_len(ncol(X))) {
    t1 <- 1
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    Xci[is.na(Xci)]<-0
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    
    plot(as.Date(obs.times[t1:t]),
         Xbar/Xsum, 
         ylim = c(0,1), #range(c(XaCI/Xsum, Xci/Xsum), na.rm = TRUE)
         type = "n", 
         xlab = "Year", 
         ylab = ylab.names[grep(colnames(X)[i], var.names)],
         main = colnames(X)[i])
    
    # observation / data
    if (i <= ncol(Ybar)) {
      ciEnvelope(as.Date(obs.times[t1:t]), 
                 as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                 as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                 col = alphagreen)
      lines(as.Date(obs.times[t1:t]), 
            as.numeric(Ybar[, i]), 
            type = "l", col = "darkgreen", lwd = 2)
    }
    
    # forecast
    ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1]/Xsum, Xci[, 2]/Xsum, col = alphablue)  #col='lightblue')
    lines(as.Date(obs.times[t1:t]), Xbar/Xsum, col = "darkblue", type = "l", lwd = 2)
    
    # analysis
    ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1]/Xsum, XaCI[, 2]/Xsum, col = alphapink)
    lines(as.Date(obs.times[t1:t]), Xa/Xsum, col = "black", lty = 2, lwd = 2)
  }
  
  ###-------------------------------------------------------------------###
  ### bias diagnostics                                                  ###
  ###-------------------------------------------------------------------### 
  # legend('topleft',c('Data','Forecast','Analysis'),col=c(4,2,3),lty=1,cex=1) Forecast minus data =
  # error
  for (i in seq_along(obs.mean[[1]])) {
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    
    reg <- lm(Xbar[t1:t]/Xsum - unlist(Ybar[t1:t, i]) ~ c(t1:t))
    plot(t1:t, 
         Xbar[t1:t]/Xsum - unlist(Ybar[t1:t, i]),
         pch = 16, cex = 1, 
         ylim = c(min(Xci[t1:t, 1]/Xsum - unlist(Ybar[t1:t, i])), max(Xci[t1:t, 2]/Xsum - unlist(Ybar[t1:t, i]))), 
         xlab = "Time", 
         ylab = "Error", 
         main = paste(colnames(X)[i], " Error = Forecast - Data"))
    ciEnvelope(rev(t1:t), 
               rev(Xci[t1:t, 1]/Xsum - unlist(Ybar[t1:t, i])), 
               rev(Xci[t1:t, 2]/Xsum - unlist(Ybar[t1:t, i])),
               col = alphapink)
    abline(h = 0, lty = 2, lwd = 2)
    abline(reg)
    mtext(paste("slope =", signif(summary(reg)$coefficients[2], digits = 3), 
                "intercept =", signif(summary(reg)$coefficients[1], digits = 3)))
    # d<-density(c(Xbar[t1:t] - unlist(Ybar[t1:t,i]))) lines(d$y+1,d$x)
    
    # forecast minus analysis = update
    reg1 <- lm(Xbar[t1:t]/Xsum - Xa[t1:t]/Xsum ~ c(t1:t))
    plot(t1:t, 
         Xbar[t1:t]/Xsum - Xa[t1:t]/Xsum, 
         pch = 16, cex = 1, 
         ylim = c(min(Xbar[t1:t]/Xsum - XaCI[t1:t, 2]/Xsum), max(Xbar[t1:t]/Xsum - XaCI[t1:t, 1]/Xsum)), 
         xlab = "Time", ylab = "Update", 
         main = paste(colnames(X)[i], 
                      "Update = Forecast - Analysis"))
    ciEnvelope(rev(t1:t), 
               rev(Xbar[t1:t]/Xsum - XaCI[t1:t, 1]/Xsum), 
               rev(Xbar[t1:t]/Xsum - XaCI[t1:t, 2]/Xsum), 
               col = alphagreen)
    abline(h = 0, lty = 2, lwd = 2)
    abline(reg1)
    mtext(paste("slope =", signif(summary(reg1)$coefficients[2], digits = 3),
                "intercept =", signif(summary(reg1)$coefficients[1], 
                                      digits = 3)))
    # d<-density(c(Xbar[t1:t] - Xa[t1:t])) lines(d$y+1,d$x)
  }
  ###-------------------------------------------------------------------###
  ### process variance plots                                            ###
  ###-------------------------------------------------------------------### 
  if (processvar) {
    library(corrplot)
    cor.mat <- cov2cor(aqq[t, , ] / bqq[t])
    colnames(cor.mat) <- colnames(X)
    rownames(cor.mat) <- colnames(X)
    par(mfrow = c(1, 1), mai = c(1, 1, 4, 1))
    corrplot(cor.mat, type = "upper", tl.srt = 45, addCoef.col = "black")
    
    plot(as.Date(obs.times[t1:t]), bqq[t1:t],
         pch = 16, cex = 1,
         ylab = "Degrees of Freedom", xlab = "Time")
  }
  
  ###-------------------------------------------------------------------###
  ### climate plots                                                     ###
  ###-------------------------------------------------------------------### 
  
  # plot(rowMeans(temp.mat[5:t,]),
  #      Xbar[5:t] -  unlist(Ybar[5:t,i]),
  #      xlim=range(rowMeans(temp.mat[5:t,])),
  #      ylim = range(Xbar[5:t] -  unlist(Ybar[5:t,i])),pch=16,cex=1,
  #      xlab="Average Monthly Temp",
  #      ylab="Error",
  #      main=colnames(Ybar)[i])
  # 
  # plot(rowSums(precip.mat[5:t,]),
  #      Xbar[5:t] - unlist(Ybar[5:t,i]),
  #      xlim=range(rowSums(precip.mat[5:t,])),
  #      ylim = range(Xbar [5:t]- unlist(Ybar[5:t,i])),
  #      pch=16,cex=1,xlab="Total Yearly Precip",
  #      ylab="Error",main=colnames(Ybar)[i])
  # 
  # plot(rowMeans(temp.mat[5:t,]),Xbar[5:t] - Xa[5:t],pch=16,
  #      cex=1,xlab="Average Monthly Temp",
  #      ylab="Update",main=colnames(Ybar)[i])
  # plot(rowSums(precip.mat[5:t,]),Xbar[5:t] - Xa[5:t],pch=16,
  #      cex=1, xlab="Total Yearly Precip",
  #      ylab="Update",main=colnames(Ybar)[i])
  
  dev.off()
  
} # sda.enkf
