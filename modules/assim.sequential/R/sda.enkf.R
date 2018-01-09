##' @title sda.enkf
##' @name  sda.enkf
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param settings    PEcAn settings object
##' @param obs.mean    list of observations of the means of state variable (time X nstate)
##' @param obs.cov     list of observations of covariance matrices of state variables (time X nstate X nstate)
##' @param IC          initial conditions
##' @param Q           process covariance matrix given if there is no data to estimate it
##' @param adjustment  flag for using ensemble adjustment filter or not
##' 
##' @description State Variable Data Assimilation: Ensemble Kalman Filter
##' 
##' @return NONE
##' @export
##' 
sda.enkf <- function(settings, obs.mean, obs.cov, IC = NULL, Q = NULL, adjustment = TRUE) {
  
  library(nimble)
  
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
    do.call(what = my.write.config, args = list(defaults = NULL, 
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
  PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
  save(list = ls(envir = environment(), all.names = TRUE), 
       file = file.path(outdir, "sda.initial.runs.Rdata"), envir = environment())
  
  
  
  ###-------------------------------------------------------------------###
  ### tests before data assimilation                                    ###
  ###-------------------------------------------------------------------###  
  
  # at some point add a lot of error checking 
  # read time from data if data is missing you still need
  # to have NAs or NULL with date name vector to read the correct netcdfs by read_restart
  
  obs.times <- names(obs.mean)
  obs.times.POSIX <- ymd_hms(obs.times)
  
  for (i in seq_along(obs.times)) {
    if (is.na(obs.times.POSIX[i])) {
      if (is.na(lubridate::ymd(obs.times[i]))) {
        print("Error: no dates associated with observations")
      } else {
        ### Data does not have time associated with dates 
        ### Adding 12:59:59PM assuming next time step starts one second later
        print("Pumpkin Warning: adding one minute before midnight time assumption to dates associated with data")
        obs.times.POSIX[i] <- ymd_hms(paste(obs.times[i], "23:59:59"))
        # if(nchar(year(obs.times.POSIX[i]))==3){
        #   #TODO: BROKEN: need to add leading zeros to years with less than 4 digits
        #   obs.times.POSIX[i] <- paste0('0',ymd_hms(paste(obs.times[i], "23:59:59")))
        # } 
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
  }
  
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
  
  y_star_create <-  nimbleFunction(
    run = function(X = double(1)) {
      returnType(double(1))
      
      X_use <- X
      X_use[X_use<0] <- 0
      y_star <- X_use/sum(X_use)
      
      return(y_star)
    })
  
  tobit.model <- nimbleCode({ 
    
    q[1:N,1:N]  ~ dwish(R = aq[1:N,1:N], df = bq) ## aq and bq are estimated over time
    Q[1:N,1:N] <- inverse(q[1:N,1:N])
    X.mod[1:N] ~ dmnorm(muf[1:N], prec = pf[1:N,1:N]) ## Model Forecast ##muf and pf are assigned from ensembles
    
    ## add process error
    X[1:N]  ~ dmnorm(X.mod[1:N], prec = q[1:N,1:N])
    
    #agb linear
    #y_star[1:YN,1:YN] <- X[1:YN,1:YN] #[choose]
    
    #f.comp non linear
    y_star[1:YN] <- y_star_create(X[1:YN])
    
    #truncation sum of positive numbers
    
    #for (i in 1:YN) {
    #  X.ind[i] ~ dinterval(X[i], 0)
    #}
    #data_constraint ~ dconstraint(max(X[1:YN]) > 0)
    
    #not good - causes positive bias
    #data_constraint ~ dconstraint(sum(X[1:YN]) > 0)
    
    ## Analysis
    y.censored[1:YN] ~ dmnorm(y_star[1:YN], prec = r[1:YN,1:YN]) #is it an okay assumpution to just have X and Y in the same order?
    
    #don't flag y.censored as data, y.censored in inits
    #remove y.censored samplers and only assign univariate samplers on NAs
    
    for(i in 1:YN){
      y.ind[i] ~ dinterval(y.censored[i], 0)
    }
    
  })
  
  tobit2space.model <- nimbleCode({
    for(i in 1:N){
      y.censored[i,1:J] ~ dmnorm(muf[1:J], cov = pf[1:J,1:J])
      for(j in 1:J){
        y.ind[i,j] ~ dinterval(y.censored[i,j], 0)
      }
    }
    
    muf[1:J] ~ dmnorm(mean = mu_0[1:J], cov = pf[1:J,1:J])
    
    Sigma[1:J,1:J] <- lambda_0[1:J,1:J]/nu_0
    pf[1:J,1:J] ~ dinvwish(S = Sigma[1:J,1:J], df = J)
    
  })
  
  t1         <- 1
  pink       <- col2rgb("deeppink")
  alphapink  <- rgb(pink[1], pink[2], pink[3], 180, max = 255)
  green      <- col2rgb("green")
  alphagreen <- rgb(green[1], green[2], green[3], 75, max = 255)
  blue       <- col2rgb("blue")
  alphablue  <- rgb(blue[1], blue[2], blue[3], 75, max = 255)
  purple       <- col2rgb("purple")
  alphapurple <- rgb(purple[1], purple[2], purple[3], 75, max = 255)
  brown       <- col2rgb("brown")
  alphabrown <- rgb(brown[1], brown[2], brown[3], 75, max = 255)
  
  ###-------------------------------------------------------------------###
  ### loop over time                                                    ###
  ###-------------------------------------------------------------------### 
  
  for(t in 5:nt) {
    ###-------------------------------------------------------------------###
    ### read restart                                                      ###
    ###-------------------------------------------------------------------###  
    X <- list()
    for (i in seq_len(length(run.id))) {
      X[[i]] <- do.call(my.read_restart, args = list(outdir = outdir, 
                                                     runid = run.id[[i]], 
                                                     stop.time = obs.times[t], 
                                                     settings = settings, 
                                                     var.names = var.names, 
                                                     params = params[[i]]))
    }
    
    for(i in seq_len(length(run.id))){
      if(is.na(X[[i]][1])) {
        print(i)
        run.id[[i]] <- NULL 
        X[[i]] <- NULL
      }
    }
    
    X <- do.call(rbind, X)
    
    FORECAST[[t]] <- X
    
    obs <- which(!is.na(obs.mean[[t]]))
    
    mu.f <- as.numeric(apply(X, 2, mean, na.rm = TRUE))
    Pf <- cov(X)
    
    ###-------------------------------------------------------------------###
    ### analysis                                                          ###
    ###-------------------------------------------------------------------###  
    if (any(obs)) {
      # if no observations skip analysis
      # choose <- na.omit(charmatch(
      #   na.omit(unlist(lapply(strsplit(colnames(X),
      #                                  split = var.names),
      #                         function(x) x[2]))),
      #   na.omit(unlist(lapply(strsplit(names(obs.mean[[t]]),
      #                                  split = var.names), #TO DO don't hardcode this
      #                         function(x) x[2]))))) #matches y to model
      # 
      
      choose <- na.omit(charmatch(colnames(X),names(obs.mean[[t]])))
      
      Y <- unlist(obs.mean[[t]][choose])
      Y[is.na(Y)] <- 0 
      
      R <- as.matrix(obs.cov[[t]][choose,choose])
      R[is.na(R)]<-0
      
      if (length(obs.mean[[t]]) > 1) {
        diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
        diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5
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
          H[i, i] <- 1/length(Y) 
        }
        #non-linear fcomp
        # for (i in choose) {
        #   H[i, i] <- 1/sum(mu.f) #? this seems to get us on the right track. mu.f[i]/sum(mu.f) doesn't work. 
        # }
        ## process error
        if (!is.null(Q)) {
          Pf <- Pf + Q
        }
        
        ## Kalman Gain
        K <- Pf %*% t(H) %*% solve((R + H %*% Pf %*% t(H)))
        # Analysis
        mu.a <- mu.f + K %*% (Y - H %*% mu.f)
        Pa   <- (diag(ncol(X)) - K %*% H) %*% Pf
        enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
        
      } else {
        
        ### create matrix the describes the support for each observed state variable at time t
        intervalX <- matrix(NA, ncol(X), 2)
        rownames(intervalX) <- colnames(X)
        #TO DO: Not working for fcomp
        for(i in 1:length(var.names)){
          intervalX[which(startsWith(rownames(intervalX),
                                     var.names[i])), ] <- matrix(c(as.numeric(settings$state.data.assimilation$state.variables[[i]]$min_value),
                                                                   as.numeric(settings$state.data.assimilation$state.variables[[i]]$max_value)),
                                                                 length(which(startsWith(rownames(intervalX),
                                                                                         var.names[i]))),2,byrow = TRUE)
        }
        
        #### These vectors are used to categorize data based on censoring from the interval matrix
        x.ind <- x.censored <- matrix(NA, ncol=ncol(X), nrow=nrow(X))
        for(j in seq_along(mu.f)){
          for(n in seq_len(nrow(X))){
            x.ind[n,j] <- as.numeric(X[n,j] > 0)
            x.censored[n,j] <- as.numeric(ifelse(X[n,j] > 100000, 0, X[n,j])) #
          }
        }
        
        if(t == 4 | length(run.id) < nens){
          #The purpose of this step is to impute data for mu.f 
          #where there are zero values so that 
          #mu.f is in 'tobit space' in the full model
          constants.tobit2space = list(N = nrow(X),
                                       J = length(mu.f))
          
          data.tobit2space = list(y.ind = x.ind,
                                  y.censored = x.censored,
                                  mu_0 = rep(0,length(mu.f)),
                                  lambda_0 = diag(10,length(mu.f)),
                                  nu_0 = 3)#some measure of prior obs
          
          inits.tobit2space = list(pf = Pf, muf = colMeans(X)) #pf = cov(X)
          #set.seed(0)
          #ptm <- proc.time()
          tobit2space_pred <- nimbleModel(tobit2space.model, data = data.tobit2space,
                                          constants = constants.tobit2space, inits = inits.tobit2space,
                                          name = 'space')
          ## Adding X.mod,q,r as data for building model.
          conf_tobit2space <- configureMCMC(tobit2space_pred, thin = 10, print=TRUE)
          conf_tobit2space$addMonitors(c("pf", "muf","y.censored")) 
          ## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
          ## important!
          ## this is needed for correct indexing later
          samplerNumberOffset_tobit2space <- length(conf_tobit2space$getSamplers())
          
          for(j in seq_along(mu.f)){
            for(n in seq_len(nrow(X))){
              node <- paste0('y.censored[',n,',',j,']')
              conf_tobit2space$addSampler(node, 'toggle', control=list(type='RW'))
              ## could instead use slice samplers, or any combination thereof, e.g.:
              ##conf$addSampler(node, 'toggle', control=list(type='slice'))
            }
          }
          
          #conf_tobit2space$printSamplers()
          
          Rmcmc_tobit2space <- buildMCMC(conf_tobit2space)
          
          Cmodel_tobit2space <- compileNimble(tobit2space_pred)
          Cmcmc_tobit2space <- compileNimble(Rmcmc_tobit2space, project = tobit2space_pred)
          
          for(i in seq_along(X)) {
            ## ironically, here we have to "toggle" the value of y.ind[i]
            ## this specifies that when y.ind[i] = 1,
            ## indicator variable is set to 0, which specifies *not* to sample
            valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[i])
          }
          
        }else{
          Cmodel_tobit2space$y.ind <- x.ind
          Cmodel_tobit2space$y.censored <- x.censored
          #Cmodel_tobit2space$lambda_0 <- diag(10,length(mu.f)) #enkf.params[[t-1]]$Pa
          #Cmodel_tobit2space$mu_0 <- rep(0,length(mu.f)) #enkf.params[[t-1]]$mu.a
          
          inits.tobit2space = list(pf = Pf, muf = colMeans(X))
          
          Cmodel_tobit2space$setInits(inits.tobit2space)
          
          for(i in seq_along(X)) {
            ## ironically, here we have to "toggle" the value of y.ind[i]
            ## this specifies that when y.ind[i] = 1,
            ## indicator variable is set to 0, which specifies *not* to sample
            valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[i])
          }
          
        }
        
        set.seed(0)
        dat.tobit2space <- runMCMC(Cmcmc_tobit2space, niter = 10000, progressBar=TRUE)
        
        pdf(file.path(outdir,paste0('assessParams',t,'.pdf')))
        
        assessParams(dat = dat.tobit2space[200:1000,], Xt = X)
        dev.off()
        
        ## update parameters
        dat.tobit2space  <- dat.tobit2space[200:1000, ]
        imuf   <- grep("muf", colnames(dat.tobit2space))
        mu.f <- colMeans(dat.tobit2space[, imuf])
        iPf   <- grep("pf", colnames(dat.tobit2space))
        Pf <- matrix(colMeans(dat.tobit2space[, iPf]),ncol(X),ncol(X))
        
        iycens <- grep("y.censored",colnames(dat.tobit2space))
        
        X.new <- matrix(colMeans(dat.tobit2space[,iycens]),nrow(X),ncol(X))
        
        #plot(dat.tobit2space[,16])
        
        #if(sum(diag(Pf)-diag(cov(X))) > 10 | sum(diag(Pf)-diag(cov(X))) < -10) logger.severe('Increase Sample Size')
        
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
        aqq[1, , ] <- diag(length(mu.f)) * bqq[1] #Q #Informative #uninformative #
        
        ### create matrix the describes the support for each observed state variable at time t
        interval <- matrix(NA, length(obs.mean[[t]]), 2)
        rownames(interval) <- names(obs.mean[[t]])
        ### TO DO: not working
        for(i in 1:length(var.names)){
          interval[which(startsWith(rownames(interval),
                                    var.names[i])), ] <- matrix(c(as.numeric(settings$state.data.assimilation$state.variables[[i]]$min_value),
                                                                  as.numeric(settings$state.data.assimilation$state.variables[[i]]$max_value)),
                                                                length(which(startsWith(rownames(interval),
                                                                                        var.names[i]))),2,byrow = TRUE)
        }
        #### These vectors are used to categorize data based on censoring 
        #### from the interval matrix
        y.ind <- as.numeric(Y > interval[,1])
        y.censored <- as.numeric(ifelse(Y > interval[,1], Y, 0))
        
        if(t == 4){ #TO need to make something that works to pick weather to compile or not
          #y.obs = Y.dat[1,]
          constants.tobit = list(N = ncol(X), YN = length(y.ind))
          dimensions.tobit = list(X = length(mu.f), X.mod = ncol(X),
                                  Q = c(length(mu.f),length(mu.f)))
          
          data.tobit = list(#data_constraint = 1,
            muf = as.vector(mu.f),
            pf = solve(Pf), 
            aq = aqq[t,,], bq = bqq[t],
            y.ind = y.ind,
            y.censored = y.censored,
            r = solve(R))
          inits.pred = list(q = diag(length(mu.f)), X.mod = as.vector(mu.f),
                            X = rnorm(length(mu.f),0,1)) #
          #set.seed(0)
          #ptm <- proc.time()
          model_pred <- nimbleModel(tobit.model, data = data.tobit, dimensions = dimensions.tobit,
                                    constants = constants.tobit, inits = inits.pred,
                                    name = 'base')
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
          
          for(i in 1:length(y.ind)) {
            ## ironically, here we have to "toggle" the value of y.ind[i]
            ## this specifies that when y.ind[i] = 1,
            ## indicator variable is set to 0, which specifies *not* to sample
            valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
          }
          
        }else{
          Cmodel$y.ind <- y.ind
          Cmodel$y.censored <- y.censored
          Cmodel$aq <- aqq[t,,]
          Cmodel$bq <- bqq[t]
          Cmodel$muf <- mu.f
          Cmodel$pf <- solve(Pf)
          Cmodel$r <- solve(R)
          
          inits.pred = list(q = diag(length(mu.f)), X.mod = as.vector(mu.f),
                            X = rnorm(ncol(X),0,1)) #
          Cmodel$setInits(inits.pred)
          
          for(i in 1:length(y.ind)) {
            ## ironically, here we have to "toggle" the value of y.ind[i]
            ## this specifies that when y.ind[i] = 1,
            ## indicator variable is set to 0, which specifies *not* to sample
            valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
          }
          
        }
        
        set.seed(0)
        dat <- runMCMC(Cmcmc, niter = 50000)
        
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
        dat  <- dat[10000:50000, ]
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
        
        #var.df1 <- data.frame(diag(Pa),diag(Pf),diag(R),diag(solve(q.bar)))
        #var.df1[order(var.df1$diag.Pa.),]
        
        #mu.df <- cbind(mu.a/sum(mu.a),mu.f/sum(mu.f),Y)
        #mu.df[order(mu.df[,3]),]
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
    ###-------------------------------------------------------------------###
    ### update state matrix                                               ###
    ###-------------------------------------------------------------------### 
    if(adjustment == TRUE){
      S_f  <- svd(Pf)
      L_f  <- S_f$d
      V_f  <- S_f$v
      
      ## normalize
      Z <- X*0
      
      for(i in seq_len(nrow(X))){
        if(processvar == TRUE) {
          Z[i,] <- 1/sqrt(L_f) * t(V_f)%*%(X.new[i,]-mu.f)
        }else{
          Z[i,] <- 1/sqrt(L_f) * t(V_f)%*%(X[i,]-mu.f)
        }
      }
      Z[is.na(Z)]<-0
      
      ## analysis
      S_a  <- svd(Pa)
      L_a  <- S_a$d
      V_a  <- S_a$v
      
      ## analysis ensemble
      X_a <- X*0
      for(i in seq_len(nrow(X))){
        X_a[i,] <- V_a %*%diag(sqrt(L_a))%*%Z[i,] + mu.a
      }
      
      if(sum(mu.a - colMeans(X_a)) > 1 | sum(mu.a - colMeans(X_a)) < -1) logger.warn('Problem with ensemble adjustment (1)')
      if(sum(diag(Pa) - diag(cov(X_a))) > 5 | sum(diag(Pa) - diag(cov(X_a))) < -5) logger.warn('Problem with ensemble adjustment (2)')
      
      analysis <- as.data.frame(X_a)
    }else{
      analysis <- as.data.frame(rmvnorm(as.numeric(nrow(X)), mu.a, Pa, method = "svd"))
    }
    
    colnames(analysis) <- colnames(X)
    
    par(mfrow=c(1,1))
    boxplot(prop.table(as.matrix(analysis[,1:9]),1),col='pink',ylim=c(0,1))
    points(Y,col='green',pch=19)
    points(mu.f[1:9]/sum(mu.f[1:9]),col='blue',pch=19)
    
    hist(X[,10],freq=FALSE)
    lines(density(analysis[,10]),col='pink',lwd=4)
    
    #hist(rnorm(1000,colMeans(X)[9],sd = sqrt(cov(X)[9,9])),freq=F,xlim=c(-.05,5))
    #lines(density(rnorm(1000,Y[9],sd = sqrt(R[9,9]))),col='green',lwd=2)
    #lines(density(rnorm(1000,mu.a[9],sd = sqrt(Pa[9,9]))),col='pink',lwd=2)
    
    ##### Mapping analysis vectors to be in bounds of state variables
    if(processvar==TRUE){
      for(i in 1:ncol(analysis)){
        int.save <- state.interval[which(startsWith(colnames(analysis)[i],
                                                    var.names)),]
        analysis[analysis[,i] < int.save[1],i] <- int.save[1]
        #analysis[analysis[,i] > int.save[2],i] <- int.save[2]
      }
    }
    
    ## in the future will have to be separated from analysis
    new.state  <- analysis
    new.params <- params
    
    ANALYSIS[[t]] <- analysis
    #pdf('ts-tree.rings-sda.pdf')
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
      
      if(any(obs)){
        Y.order <- na.omit(pmatch(colnames(X), colnames(Ybar)))
        Ybar <- Ybar[,Y.order]
        Ybar[is.na(Ybar)] <- 0
        Ybar <- Ybar
        
        YCI <- t(as.matrix(sapply(obs.cov[t1:t], function(x) {
          if (length(x)<2) {
            rep(NA, length(names.y))
          }
          sqrt(diag(x))
        })))
        
        YCI <- YCI[,Y.order]
        YCI[is.na(YCI)] <- 0
        
      }else{
        YCI <- matrix(NA,nrow=length(t1:t), ncol=length(names.y))
      }
      
      par(mfrow = c(2, 1))
      for (i in 1:9) { #ncol(FORECAST[[t]])
        
        Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i]/rowSums(x[,1:9]), na.rm = TRUE) })
        Xci  <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i]/rowSums(x[,1:9]), c(0.025, 0.975), na.rm = TRUE) })
        
        Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i]/rowSums(x[,1:9]), na.rm = TRUE) })
        XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i]/rowSums(x[,1:9]), c(0.025, 0.975), na.rm = TRUE) })
        
        ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                                    function(x) { x })[2, ], use.names = FALSE)
        
        # observation / data
        if (i <= ncol(Ybar)) {
          plot(as.Date(obs.times[t1:t]), 
               Xbar, 
               ylim = range(c(XaCI, Xci, Ybar[,i]), na.rm = TRUE), 
               type = "n", 
               xlab = "Year", 
               ylab = ylab.names[grep(colnames(X)[i], var.names)], 
               main = colnames(X)[i])
          ciEnvelope(as.Date(obs.times[t1:t]),
                     as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                     as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                     col = alphagreen)
          lines(as.Date(obs.times[t1:t]), 
                as.numeric(Ybar[, i]), 
                type = "l", 
                col = "darkgreen", 
                lwd = 2)
        }else{
          plot(as.Date(obs.times[t1:t]), 
               Xbar, 
               ylim = c(0,250),#range(c(XaCI, Xci), na.rm = TRUE), 
               type = "n", 
               xlab = "Year", 
               ylab = ylab.names[grep(colnames(X)[i], var.names)], 
               main = colnames(X)[i])
        }
        
        # forecast
        ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = alphablue)  #col='lightblue')
        lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2)
        
        # analysis
        ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = alphapink)
        lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2)
        #legend('topright',c('Forecast','Data','Analysis'),col=c(alphablue,alphagreen,alphapink),lty=1,lwd=5)
      }
      for (i in 1:10){ #ncol(FORECAST[[t]])
        
        Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
        Xci  <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975), na.rm = TRUE) })
        
        Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
        XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975), na.rm = TRUE) })
        
        ylab.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                                    function(x) { x })[2, ], use.names = FALSE)
        
        plot(as.Date(obs.times[t1:t]), 
             Xbar, 
             ylim = range(c(XaCI, Xci), na.rm = TRUE), 
             type = "n", 
             xlab = "Year", 
             ylab = ylab.names[grep(colnames(X)[i], var.names)], 
             main = colnames(X)[i])
        
        # forecast
        ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = alphablue)  #col='lightblue')
        lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2)
        
        # analysis
        ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = alphapink)
        lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2)
        #legend('topright',c('Forecast','Data','Analysis'),col=c(alphablue,alphagreen,alphapink),lty=1,lwd=5)
      }
    }
    #dev.off()
    ###-------------------------------------------------------------------###
    ### forecast step                                                     ###
    ###-------------------------------------------------------------------### 
    if (t < nt) {
      
      ###-------------------------------------------------------------------###
      ### split model specific inputs for current runs                      ###
      ###-------------------------------------------------------------------### 
      
      inputs <- list()
      for(i in seq_len(nrow(X))){
        inputs[[i]] <- do.call(my.split_inputs, 
                               args = list(settings = settings, 
                                           start.time = (ymd_hms(paste0(0,obs.times[t]),truncated = 3) + second(hms("00:00:01"))), 
                                           stop.time = obs.times[t + 1],
                                           inputs = ens.inputs[[i]])) 
      }
      
      
      ###-------------------------------------------------------------------###
      ### write restart by ensemble                                         ###
      ###-------------------------------------------------------------------### 
      
      for (i in seq_len(nrow(X))) {
        do.call(my.write_restart, 
                args = list(outdir = outdir, 
                            runid = run.id[[i]], 
                            start.time = strptime(obs.times[t],format="%Y-%m-%d %H:%M:%S"),
                            stop.time = strptime(obs.times[t + 1],format="%Y-%m-%d %H:%M:%S"), 
                            settings = settings,
                            new.state = new.state[i, ], 
                            new.params = new.params[[i]], 
                            inputs = inputs[[i]], 
                            RENAME = TRUE))
      }
      ###-------------------------------------------------------------------###
      ### Run model                                                         ###
      ###-------------------------------------------------------------------### 
      print(paste("Running Model for Year", as.Date(obs.times[t]) + 1))
      PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
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
  
  ###-------------------------------------------------------------------###
  ### time series                                                       ###
  ###-------------------------------------------------------------------### 
  
  pdf(file.path(settings$outdir, "sda.enkf.time-series-data-100.pdf"))
  
  names.y <- unique(unlist(lapply(obs.mean[t1:t], function(x) { names(x) })))
  Ybar <- t(sapply(obs.mean[t1:t], function(x) {
    tmp <- rep(NA, length(names.y))
    names(tmp) <- names.y
    mch <- match(names(x), names.y)
    tmp[mch] <- x[mch]
    tmp
  }))
  Y.order <- na.omit(pmatch(colnames(FORECAST[[t]]), colnames(Ybar)))
  Ybar <- Ybar[,Y.order]
  YCI <- t(as.matrix(sapply(obs.cov[t1:t], function(x) {
    if (is.null(x)) {
      rep(NA, length(names.y))
    }
    sqrt(diag(x))
  })))
  
  Ybar[is.na(Ybar)]<-0
  YCI[is.na(YCI)]<-0
  
  YCI <- YCI[,Y.order]
  Xsum <- plyr::laply(FORECAST, function(x) { mean(rowSums(x[,1:length(names.y)], na.rm = TRUE)) })[t1:t]
  Xasum <- plyr::laply(ANALYSIS, function(x) { mean(rowSums(x[,1:length(names.y)], na.rm = TRUE)) })[t1:t]
  
  for (i in seq_len(ncol(X))) {
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) {
      mean(x[, i], na.rm = TRUE) }) #/rowSums(x[,1:9],na.rm = T)
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { 
      quantile(x[, i], c(0.025, 0.975),na.rm = T) })
    
    Xci[is.na(Xci)]<-0
    
    Xbar <- Xbar
    Xci <- Xci
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { 
      
      mean(x[, i],na.rm = T) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { 
      quantile(x[, i], c(0.025, 0.975),na.rm = T )})
    
    Xa <- Xa
    XaCI <- XaCI
    
    plot(as.Date(obs.times[t1:t]),
         Xbar, 
         ylim = range(c(XaCI, Xci), na.rm = TRUE),
         type = "n", 
         xlab = "Year", 
         ylab = ylab.names[grep(colnames(X)[i], var.names)],
         main = colnames(X)[i])
    
    # observation / data
    if (i<10) { #
      ciEnvelope(as.Date(obs.times[t1:t]), 
                 as.numeric(Ybar[, i]) - as.numeric(YCI[, i]) * 1.96, 
                 as.numeric(Ybar[, i]) + as.numeric(YCI[, i]) * 1.96, 
                 col = alphagreen)
      lines(as.Date(obs.times[t1:t]), 
            as.numeric(Ybar[, i]), 
            type = "l", col = "darkgreen", lwd = 2)
    }
    
    # forecast
    ciEnvelope(as.Date(obs.times[t1:t]), Xci[, 1], Xci[, 2], col = alphablue)  #col='lightblue') #alphablue
    lines(as.Date(obs.times[t1:t]), Xbar, col = "darkblue", type = "l", lwd = 2) #"darkblue"
    
    # analysis
    ciEnvelope(as.Date(obs.times[t1:t]), XaCI[, 1], XaCI[, 2], col = alphapink) #alphapink
    lines(as.Date(obs.times[t1:t]), Xa, col = "black", lty = 2, lwd = 2) #"black"
    
    legend('topright',c('Forecast','Data','Analysis'),col=c(alphablue,alphagreen,alphapink),lty=1,lwd=5)
    
  }
  
  dev.off()
  ###-------------------------------------------------------------------###
  ### bias diagnostics                                                  ###
  ###-------------------------------------------------------------------###
  pdf(file.path(settings$outdir, "bias.diagnostic.pdf"))
  for (i in seq_along(obs.mean[[1]])) {
    Xbar <- plyr::laply(FORECAST[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    Xci <- plyr::laply(FORECAST[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    
    Xa <- plyr::laply(ANALYSIS[t1:t], function(x) { mean(x[, i], na.rm = TRUE) })
    XaCI <- plyr::laply(ANALYSIS[t1:t], function(x) { quantile(x[, i], c(0.025, 0.975)) })
    
    if(length(which(is.na(Ybar[,i])))>=length(t1:t)) next()
    reg <- lm(Xbar[t1:t] - unlist(Ybar[, i]) ~ c(t1:t))
    plot(t1:t, 
         Xbar - unlist(Ybar[, i]),
         pch = 16, cex = 1, 
         ylim = c(min(Xci[, 1] - unlist(Ybar[, i])), max(Xci[,2] - unlist(Ybar[, i]))), 
         xlab = "Time", 
         ylab = "Error", 
         main = paste(colnames(X)[i], " Error = Forecast - Data"))
    ciEnvelope(rev(t1:t), 
               rev(Xci[, 1] - unlist(Ybar[, i])), 
               rev(Xci[, 2] - unlist(Ybar[, i])),
               col = alphabrown)
    abline(h = 0, lty = 2, lwd = 2)
    abline(reg)
    mtext(paste("slope =", signif(summary(reg)$coefficients[2], digits = 3), 
                "intercept =", signif(summary(reg)$coefficients[1], digits = 3)))
    # d<-density(c(Xbar[t1:t] - unlist(Ybar[t1:t,i]))) lines(d$y+1,d$x)
    
    # forecast minus analysis = update
    reg1 <- lm(Xbar - Xa ~ c(t1:t))
    plot(t1:t, 
         Xbar - Xa, 
         pch = 16, cex = 1, 
         ylim = c(min(Xbar - XaCI[, 2]), max(Xbar - XaCI[, 1])), 
         xlab = "Time", ylab = "Update", 
         main = paste(colnames(X)[i], 
                      "Update = Forecast - Analysis"))
    ciEnvelope(rev(t1:t), 
               rev(Xbar - XaCI[, 1]), 
               rev(Xbar - XaCI[, 2]), 
               col = alphapurple)
    abline(h = 0, lty = 2, lwd = 2)
    abline(reg1)
    mtext(paste("slope =", signif(summary(reg1)$coefficients[2], digits = 3),
                "intercept =", signif(summary(reg1)$coefficients[1], 
                                      digits = 3)))
    # d<-density(c(Xbar[t1:t] - Xa[t1:t])) lines(d$y+1,d$x)
  }
  dev.off()
  
  ###-------------------------------------------------------------------###
  ### process variance plots                                            ###
  ###-------------------------------------------------------------------### 
  if (processvar) {
    
    library(corrplot)
    pdf('process.var.plots.pdf')
    
    cor.mat <- cov2cor(aqq[t,,] / bqq[t])
    colnames(cor.mat) <- colnames(X)
    rownames(cor.mat) <- colnames(X)
    par(mfrow = c(1, 1), mai = c(1, 1, 4, 1))
    corrplot(cor.mat, type = "upper", tl.srt = 45,order='FPC')
    
    par(mfrow=c(1,1))   
    plot(as.Date(obs.times[t1:t]), bqq[t1:t],
         pch = 16, cex = 1,
         ylab = "Degrees of Freedom", xlab = "Time")
    
    dev.off()
    
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
  
} # sda.enkf
