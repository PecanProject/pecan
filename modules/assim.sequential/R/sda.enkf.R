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
sda.enkf <- function(settings, obs.mean, obs.cov, IC = NULL, Q = NULL){
  
  ###-------------------------------------------------------------------###
  ### read settings                                                     ###
  ###-------------------------------------------------------------------### 
  
  model <- settings$model$type
  write <- settings$database$bety$write
  defaults <- settings$pfts
  outdir <- settings$host$outdir
  rundir <- settings$host$rundir
  host <- settings$host
  forecast.time.step <- settings$state.data.assimilation$forecast.time.step
  nens = settings$state.data.assimilation$n.ensemble
  start.year <- strftime(settings$state.data.assimilation$start.date,"%Y") #we need to make sure this matches the data years somehow
  end.year   <- strftime(settings$state.data.assimilation$end.date,"%Y")
  processvar <-settings$state.data.assimilation$process.variance
  #sample_parameters <-settings$state.data.assimilation$sample.parameters
  var.names <- unlist(sapply(settings$state.data.assimilation$state.variable,function(x){x}), use.names = FALSE)
  
  ###-------------------------------------------------------------------###
  ### get model specific functions                                      ###
  ###-------------------------------------------------------------------### 
  do.call("require",list(paste0("PEcAn.",model)))
  my.write.config <- paste("write.config.",model,sep="")
  my.read.restart <- paste("read.restart.",model,sep="")
  my.write.restart <- paste("write.restart.",model,sep="")
  my.split.inputs <- paste("split.inputs.",model,sep="")
  
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    stop()
  }
  
  ###-------------------------------------------------------------------###
  ### load model specific inputs                                        ###
  ###-------------------------------------------------------------------### 
  
  inputs <- do.call(my.split.inputs,args=list(settings = settings,
                  start.time = settings$run$start.date,
                  stop.time = settings$run$end.date))
  
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
  if(write){
    con <- try(db.open(settings$database$bety), silent=TRUE)
    if(is.character(con)){
      con <- NULL
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
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                   now, "', 'EnKF', ", workflow.id, ")", sep=''), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep=''), con)[['id']]
  } else {
    ensemble.id <- -1
  }
  
 
  ###-------------------------------------------------------------------###
  ### perform initial set of runs                                       ###
  ###-------------------------------------------------------------------###  
  run.id = list()
  X <- IC

  ## Load Parameters
  if(sample_parameters == TRUE){
    settings$ensemble$size <- settings$state.data.assimilation$n.ensemble
  } else {
    settings$ensemble$size <- 1
  }
  get.parameter.samples(settings, ens.sample.method=settings$ensemble$method)  ## Aside: if method were set to unscented, would take minimal changes to do UnKF
  load(file.path(settings$outdir, "samples.Rdata"))  ## loads ensemble.samples
    
  for(i in 1:nens){
    
    ## set RUN.ID
    if (!is.null(con)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      paramlist <- paste("EnKF:",i)
      run.id[[i]] <- db.query(paste0("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id,",
                     " parameter_list) values ('", 
                     settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", 
                     settings$run$end.date, "', '", settings$outdir , "', '", now, "', ", ensemble.id, ", '", 
                     paramlist, "') RETURNING id"), con)
    } else {
      run.id[[i]] = paste("EnKF",i,sep=".")
    }
    dir.create(file.path(settings$rundir, run.id[[i]]), recursive=TRUE)
    dir.create(file.path(settings$modeloutdir, run.id[[i]]), recursive=TRUE)
    
    ## Write Configs
    if(sample_parameters == TRUE){
      trait.values <- lapply(ensemble.samples, function(x, n){x[i,]},n = i)
    } else {
      trait.values <- ensemble.samples
    }
    do.call(my.write.config, args = list(defaults = NULL, trait.values = trait.values,
                                         settings = settings, run.id = run.id[[i]],
                                         inputs = inputs, IC = IC[i,]))
    
    ## write a README for the run
    cat("runtype     : sda.enkf\n",
        "workflow id : ", as.character(workflow.id), "\n",
        "ensemble id : ", as.character(ensemble.id), "\n",
        "ensemble    : ", i, "\n",
        "run id      : ", as.character(run.id[[i]]), "\n",
        "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
        "model       : ", model, "\n",
        "model id    : ", settings$model$id, "\n",
        "site        : ", settings$run$site$name, "\n",
        "site  id    : ", settings$run$site$id, "\n",
        "met data    : ", inputs$met, "\n",
        "start date  : ", settings$run$start.date, "\n",
        "end date    : ", settings$run$end.date, "\n",
        "hostname    : ", settings$host$name, "\n",
        "rundir      : ", file.path(settings$host$rundir, run.id[[i]]), "\n",
        "outdir      : ", file.path(settings$host$outdir, run.id[[i]]), "\n",
        file=file.path(settings$rundir, run.id[[i]], "README.txt"), sep='')
    
  }
  
  ## add the jobs to the list of runs
  cat(as.character(unlist(run.id)),file=file.path(settings$rundir, "runs.txt"),sep="\n",append=FALSE)
  
  ## start model runs
  start.model.runs(settings,settings$database$bety$write)
  save.image(file.path(outdir,"sda.spinup.Rdata"))
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###  
  
  ## vector to read the correct netcdfs by read.restart
  total.time = as.numeric(settings$run$end.date):as.numeric(end.year)
  
  nt = length(total.time) #could be different if time step was different right?
  FORECAST <- ANALYSIS <- list()
  enkf.params <- list()
  aqq = array(0,dim=c(nt,length(var.names),length(var.names)))
  bqq = numeric(nt+1)
  CI.X1 <- matrix(0,3,nt) ; CI.X2 = CI.X1
  
  wish.df <- function(Om,X,i,j,col){
    n = (Om[i,j]^2 + Om[i,i]*Om[j,j])/var(X[,col])
    return(n)
  }
  
  ## JAGS models for numerical update of state and process error
  AnalysisFilterQ <- "
  model{
  X.mod ~ dmnorm(muf,pf) ## Model Forecast
  
  for(i in 1:length(X.mod)){
  X.mod.not.zero[F2M[i]] <- X.mod[i]
  }
  
  for(i in 1:length(E)){
  X.mod.not.zero[E[i]]<-0
  }
  
  ## add process error
  q  ~ dwish(aq,bq)
  Q <- inverse(q) 
  X  ~ dmnorm(X.mod.not.zero,q)
  
  ## Analysis
  for(i in 1:length(X.keep)){
  X.keep[i] <- X[X2Y[i]]
  }
  Y  ~ dmnorm(X.keep,r)
}"     
  
  AnalysisFilterQ1 <- "
  model{
  X.mod ~ dmnorm(muf,pf) ## Model Forecast
  
  for(i in 1:length(X.mod)){
  X.mod.not.zero[F2M[i]] <- X.mod[i]
  }
  
  ## add process error
  q  ~ dwish(aq,bq)
  Q <- inverse(q) 
  X  ~ dmnorm(X.mod.not.zero,q)
  
  ## Analysis
  for(i in 1:length(X.keep)){
  X.keep[i] <- X[X2Y[i]]
  }
  Y  ~ dmnorm(X.keep,r)
}"     
  
  t1=1
  pink = col2rgb("deeppink")
  alphapink = rgb(pink[1],pink[2],pink[3],180,max=255)
  green = col2rgb("green")
  alphagreen = rgb(green[1],green[2],green[3],75,max=255)
  blue = col2rgb("blue")
  alphablue = rgb(blue[1],blue[2],blue[3],75,max=255)
  
  
  ###-------------------------------------------------------------------###
  ### loop over time                                                    ###
  ###-------------------------------------------------------------------###  
  for(t in 1:nt){
    
    ###-------------------------------------------------------------------###
    ### read restart                                                      ###
    ###-------------------------------------------------------------------###  
    X <- list()
    for(i in 1:nens){
      X[[i]] <- do.call(my.read.restart,args=list(outdir = outdir, runid = run.id[[i]],
                                                  stop.time = total.time[t],
                                                  multi.settings = multi.settings,
                                                  var.names = var.names,
                                                  params = params))
    }
    
    X <- do.call(rbind,X)
    
    FORECAST[[t]] = X
    
    obs = which(!is.na(obs.mean[[t]]))
    
    mu.f = as.numeric(apply(X,2,mean,na.rm=TRUE))
    Pf   = cov(X)
    
    ###-------------------------------------------------------------------###
    ### analysis                                                          ###
    ###-------------------------------------------------------------------###  
    if(any(obs)){ #if no observations skip analysis
    Y    = obs.mean[[t]][pmatch(names(obs.mean[[t]]),colnames(X))]

    R = as.matrix(obs.cov[[t]])
    
    if(length(obs.mean[[t]])>1){
      for(s in 1:length(obs.mean[[t]])){
        if(diag(R)[s]==0){ #if covariance is 0 then set it to half of the minimum covariance to avoid solve() problems
          diag(R)[s] <- min(diag(R)[which(diag(R)!=0)])/2
        }
        if(diag(Pf)[s]==0){ #if covariance is 0 then set it to half of the minimum covariance to avoid solve() problems
          diag(Pf)[s] <- min(diag(Pf)[which(diag(Pf)!=0)])/2
        }
      }
    }
    
    #### Plot Data and Forecast
    if(interactive() & t > 1){
      t1=1
      names.y <- unique(unlist(lapply(obs.mean[t1:t],function(x){return(names(x))})))
      Ybar = t(sapply(obs.mean[t1:t],function(x){
        tmp <- rep(NA,length(names.y))
        names(tmp) <- names.y 
        mch = match(names(x),names.y)
        tmp[mch] = x[mch]
        return(tmp)
      }))
      
      Ybar = Ybar[,na.omit(pmatch(colnames(X), colnames(Ybar)))]
      YCI = t(as.matrix(sapply(obs.cov[t1:t],function(x){
          if(is.null(x)) return(rep(NA,length(names.y)))
          return(sqrt(diag(x)))}))) 
      
      for(i in 2){
          t1=1
          Xbar = laply(FORECAST[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
          Xci  = laply(FORECAST[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})

          plot(total.time[t1:t],Xbar,ylim=range(c(Ybar,Xci),na.rm=TRUE),
               type='n',xlab="Year",ylab="kg/m^2",main=colnames(X)[i])
          
          #observation / data
            if(i<=ncol(Ybar)){
              ciEnvelope(total.time[t1:t],as.numeric(Ybar[,i])-as.numeric(YCI[,i])*1.96,
                         as.numeric(Ybar[,i])+as.numeric(YCI[,i])*1.96,col=alphagreen)
              lines(total.time[t1:t],as.numeric(Ybar[,i]),type='l',col="darkgreen",lwd=2)
            }
          
          #forecast
          ciEnvelope(total.time[t1:t],Xci[,1],Xci[,2],col=alphablue)#col="lightblue")
          lines(total.time[t1:t],Xbar,col="darkblue",type='l',lwd=2)
          
        }
    }

    ###-------------------------------------------------------------------###
    ### Kalman Filter                                                     ###
    ###-------------------------------------------------------------------###
    if(processvar == FALSE){
      ## design matrix
      H =  matrix(0,length(Y),ncol(X))
      choose<-na.omit(pmatch(colnames(X), names(obs.mean[[t]])))
      for(i in choose){
        H[i,i]<-1
      }
      ## process error
      if(exists('Q')) Pf <- Pf + Q
      ## Kalman Gain
      K    = Pf%*%t(H)%*%solve((R+H%*%Pf%*%t(H)))
      ## Analysis
      mu.a = mu.f + K%*%(Y-H%*%mu.f)
      Pa   = (diag(ncol(X)) - K%*%H)%*%Pf
      enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
    } else { 
      
      ###-------------------------------------------------------------------###
      ### Generalized Ensemble Filter                                       ###
      ###-------------------------------------------------------------------###
      
      #### initial conditions
      bqq[1] <- length(mu.f)
      aqq[1,,] <- diag(length(mu.f))*bqq[1]
      
      #### marks zeros from the forecast ensembles
      E <- which(colSums(Pf) == 0)
      
      #### forcast to modeled vector ## remove states that don't show up in forecast
      F2M<- seq(1,length(mu.f),1)
      F2M[E]<-NA
      F2M <- na.omit(F2M)
      
      #### translating x to y ## remove X that you don't have data for
      X2Y<- seq(1,length(Y),1)
      X2Y <- X2Y[which(!is.na(Y))]
      
      #### changing diagonal if the covariance is too small for the matrix to be inverted
      #### This problem is different than R problem because diag(Pf) can be so small it can't be inverted
      #### Need a different fix here someday
      for(i in 1:length(diag(Pf))){
        if(diag(Pf)[i]==0) diag(Pf)[i]<- min(diag(Pf)[which(diag(Pf)!=0)])/2 #HACK
      }
    
      ### analysis of model and data
      if(length(E)>0){ #if all ensemble members 
        update = list(Y=Y, r=solve(R[X2Y,X2Y]),
                      muf=mu.f[-E], pf=solve(Pf[-E,-E]),
                      aq=aqq[t,,], bq=bqq[t],
                      F2M=F2M,X2Y=X2Y,X.mod=rep(NA,length(mu.f[-E])),
                      X=rep(NA,length(mu.f)),
                      X.keep=rep(NA,length(X2Y)),
                      E=E)
        mod <- jags.model(file=textConnection(AnalysisFilterQ),
                          data=update,
                          n.adapt=1000,n.chains=3,
                          init=list(X.mod=as.vector(mu.f[-c(E)]))) #inits for q?
      }else{
        update = list(Y=Y, r=solve(R[X2Y,X2Y]),
                      muf=mu.f, pf=solve(Pf),
                      aq=aqq[t,,], bq=bqq[t],
                      F2M=F2M,X2Y=X2Y,X.mod=rep(NA,length(mu.f)),
                      X=rep(NA,length(mu.f)),
                      X.keep=rep(NA,length(na.omit(Y))))
        mod <- jags.model(file=textConnection(AnalysisFilterQ1),
                          data=update,
                          n.adapt=1000,n.chains=3,
                          init=list(X.mod=as.vector(mu.f))) #inits for q?
      }
      
     
      jdat <- coda.samples(mod,variable.names=c("X","q"),n.iter=10000) 
      
      ## update parameters  
      dat = as.matrix(jdat)
      dat = dat[3000:10000,]
      iq = grep("q",colnames(dat))
      iX = grep("X[",colnames(dat),fixed=TRUE)
      mu.a  = colMeans(dat[,iX])
      Pa  = cov(dat[,iX])
      Pa[is.na(Pa)]<- 0 
      
      CI.X1[,t] = quantile(dat[,iX[1]],c(0.025,0.5,0.975))
      CI.X2[,t] = quantile(dat[,iX[2]],c(0.025,0.5,0.975))
      
      mq = dat[,iq] #Omega, Precision
      q.bar = matrix(apply(mq,2,mean),length(mu.f),length(mu.f)) #Mean Omega, Precision
      
      col = matrix(1:length(mu.f)^2,length(mu.f),length(mu.f))
      WV = matrix(0,length(mu.f),length(mu.f))
      for(i in 1:length(mu.f)){
        for(j in 1:length(mu.f)){
          WV[i,j] <- wish.df(q.bar, X = mq, i=i, j=j, col=col[i,j])
        }
      }
      
      n = mean(WV)
      if(n < length(mu.f)) n = length(mu.f)
      V = solve(q.bar)*n
      
      aqq[t+1,,] = V
      bqq[t+1] = n
      enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa, q.bar=q.bar, n=n)
    }
    
    } else {
      ###-------------------------------------------------------------------###
      ### No Observations -- Starts Here                                    ###
      ###-------------------------------------------------------------------### 
      
      ### no process variance -- forecast is the same as the analysis ###
      if(processvar==FALSE){
        mu.a = mu.f
        Pa = Pf + Q
      ### yes process variance -- no data 
      }else{
        mu.a = mu.f
        Pa = Pf + solve(q.bar)
      }
      enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa)
    }
    
    ## update state matrix
    analysis <- as.data.frame(rmvnorm(as.numeric(nens),mu.a,Pa,method="svd"))
    colnames(analysis) <- colnames(X)
    
    ANALYSIS[[t]] = analysis
    
    if(interactive() & t>1){
      t1=1
      names.y <- unique(unlist(lapply(obs.mean[t1:t],function(x){return(names(x))})))
      Ybar = t(sapply(obs.mean[t1:t],function(x){
        tmp <- rep(NA,length(names.y))
        names(tmp) <- names.y 
        mch = match(names(x),names.y)
        tmp[mch] = x[mch]
        return(tmp)
      }))
      Ybar = Ybar[,na.omit(pmatch(colnames(X), colnames(Ybar)))]
      YCI = t(as.matrix(sapply(obs.cov[t1:t],function(x){
        if(is.null(x)) return(rep(NA,length(names.y)))
        return(sqrt(diag(x)))})))
        
      par(mfrow=c(2,1))
      for(i in 1:2){
          t1=1
          Xbar = laply(FORECAST[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
          Xci  = laply(FORECAST[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
          
          Xa = laply(ANALYSIS[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
          XaCI  = laply(ANALYSIS[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
          
          ylab.names<-unlist(sapply(settings$state.data.assimilation$state.variable,function(x){x})[2,], use.names = FALSE)
          
          plot(total.time[t1:t],Xbar,ylim=range(c(XaCI,Xci),na.rm=TRUE),
               type='n',xlab="Year",ylab=ylab.names[grep(colnames(X)[i],var.names)],main=colnames(X)[i])
          
          #observation / data
          if(i<=ncol(Ybar)){
            ciEnvelope(total.time[t1:t],as.numeric(Ybar[,i])-as.numeric(YCI[,i])*1.96,
                       as.numeric(Ybar[,i])+as.numeric(YCI[,i])*1.96,col=alphagreen)
            lines(total.time[t1:t],as.numeric(Ybar[,i]),type='l',col="darkgreen",lwd=2)
          }
          
          #forecast
          ciEnvelope(total.time[t1:t],Xci[,1],Xci[,2],col=alphablue)#col="lightblue")
          lines(total.time[t1:t],Xbar,col="darkblue",type='l',lwd=2)
          
          #analysis
          ciEnvelope(total.time[(t1:t)],XaCI[,1],XaCI[,2],col=alphapink)
          lines(total.time[t1:t],Xa,col="black",lty=2,lwd=2)
          
        }
      }

    
    ###-------------------------------------------------------------------###
    ### forecast step -- write restart                                    ###
    ###-------------------------------------------------------------------### 
    if(t < nt){
      for(i in 1:nens){
        if(sample_parameters == TRUE){
          trait.values <- lapply(ensemble.samples, function(x, n){x[i,]},n = i)
        } else {
          trait.values <- ensemble.samples
        }
        do.call(my.write.restart,
                args=list(outdir = outdir, runid = run.id[[i]],
                          start.time = total.time[t],
                          stop.time = total.time[t],
                          multi.settings = settings,
                          new.state = analysis[i,],
                          new.params = trait.values,
                          inputs = inputs))
      }
      ###-------------------------------------------------------------------###
      ### Run model                                                         ###
      ###-------------------------------------------------------------------### 
      print(paste("Running Model for Year",total.time[t]+1))
      start.model.runs(settings,settings$database$bety$write)
    }
    
    ###-------------------------------------------------------------------###
    ### save outputs                                                      ###
    ###-------------------------------------------------------------------### 
    save(t,FORECAST,ANALYSIS,enkf.params,file=file.path(settings$outdir,"sda.output.Rdata"))
    
    
  }  ## end loop over time
  ###-------------------------------------------
  
  
  
  ###-------------------------------------------------------------------###
  ### create diagnostics                                                ###
  ###-------------------------------------------------------------------### 
  
  ### LOAD CLIMATE ### HACK ### LINKAGES SPECIFIC
  if(model == 'LINKAGES'){
    climate_file <- settings$run$inputs$met$path
    load(climate_file)
    temp.mat <- temp.mat[total.time-853,]
    precip.mat <- precip.mat[total.time-853,]
  }else{
    print('climate diagnostics under development')
  }

  ### Diagnostic graphs  
  pdf(file.path(settings$outdir,"EnKF.pdf"))
    
    ###-------------------------------------------------------------------###
    ### time series                                                       ###
    ###-------------------------------------------------------------------### 
   
    names.y <- unique(unlist(lapply(obs.mean[t1:t],function(x){return(names(x))})))
    Ybar = t(sapply(obs.mean[t1:t],function(x){
      tmp <- rep(NA,length(names.y))
      names(tmp) <- names.y 
      mch = match(names(x),names.y)
      tmp[mch] = x[mch]
      return(tmp)
      }))
    Ybar = Ybar[,na.omit(pmatch(colnames(X), colnames(Ybar)))]
    YCI = t(as.matrix(sapply(obs.cov[t1:t],function(x){
      if(is.null(x)) return(rep(NA,length(names.y)))
      return(sqrt(diag(x)))})))  #need to make this from quantiles for lyford plot data
    #YCI = YCI[,pmatch(colnames(X), names(obs.mean[[nt]][[1]]))]
   
    for(i in 1:ncol(X)){
      t1=1
      Xbar = laply(FORECAST[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
      Xci  = laply(FORECAST[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
      
      Xa = laply(ANALYSIS[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
      XaCI  = laply(ANALYSIS[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
      
      plot(total.time[t1:t],Xbar,ylim=range(c(XaCI,Xci),na.rm=TRUE),
           type='n',xlab="Year",ylab=ylab.names[grep(colnames(X)[i],var.names)],main=colnames(X)[i])
     
       #observation / data
      if(i<=ncol(Ybar)){
        ciEnvelope(total.time[t1:t],as.numeric(Ybar[,i])-as.numeric(YCI[,i])*1.96,
                   as.numeric(Ybar[,i])+as.numeric(YCI[,i])*1.96,col=alphagreen)
        lines(total.time[t1:t],as.numeric(Ybar[,i]),type='l',col="darkgreen",lwd=2)
      }
      
      #forecast
      ciEnvelope(total.time[t1:t],Xci[,1],Xci[,2],col=alphablue)#col="lightblue")
      lines(total.time[t1:t],Xbar,col="darkblue",type='l',lwd=2)
      
      #analysis
      ciEnvelope(total.time[(t1:t)],XaCI[,1],XaCI[,2],col=alphapink)
      lines(total.time[t1:t],Xa,col="black",lty=2,lwd=2)
      
    }

    ###-------------------------------------------------------------------###
    ### bias diagnostics                                                  ###
    ###-------------------------------------------------------------------### 
      #legend("topleft",c("Data","Forecast","Analysis"),col=c(4,2,3),lty=1,cex=1)
      #Forecast minus data = error
    for(i in 1:nt){
      Xbar = laply(FORECAST[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
      Xci  = laply(FORECAST[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
      
      Xa = laply(ANALYSIS[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
      XaCI  = laply(ANALYSIS[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
      
      reg <- lm(Xbar[t1:t] - unlist(Ybar[t1:t,i])~c(t1:t))
      plot(t1:t,Xbar[t1:t] - unlist(Ybar[t1:t,i]),pch=16,cex=1,
           ylim=c(min(Xci[t1:t,1]-unlist(Ybar[t1:t,i])),
                  max(Xci[t1:t,2]-unlist(Ybar[t1:t,i]))),
           xlab="Time", ylab="Error",main=paste(colnames(X)[i]," Error = Forecast - Data"))
      ciEnvelope(rev(t1:t),rev(Xci[t1:t,1]-unlist(Ybar[t1:t,i])),
                 rev(Xci[t1:t,2]-unlist(Ybar[t1:t,i])),col=alphapink)
      abline(h=0,lty=2,lwd=2)
      abline(reg)
      mtext(paste("slope =",signif(summary(reg)$coefficients[2],digits=3),"intercept =",signif(summary(reg)$coefficients[1],digits=3)))
      #d<-density(c(Xbar[t1:t] - unlist(Ybar[t1:t,i])))
      #lines(d$y+1,d$x)
      
      #forecast minus analysis = update
      reg1 <- lm(Xbar[t1:t] - Xa[t1:t] ~ c(t1:t))
      plot(t1:t,Xbar[t1:t] - Xa[t1:t],pch=16,cex=1,
           ylim=c(min(Xbar[t1:t]-XaCI[t1:t,2]),max(Xbar[t1:t]-XaCI[t1:t,1])),
           xlab="Time", ylab="Update",main=paste(colnames(X)[i],"Update = Forecast - Analysis"))
      ciEnvelope(rev(t1:t),rev(Xbar[t1:t] - XaCI[t1:t,1]),
                 rev(Xbar[t1:t] - XaCI[t1:t,2]),col=alphagreen)
      abline(h=0,lty=2,lwd=2)
      abline(reg1)
      mtext(paste("slope =",signif(summary(reg1)$coefficients[2],digits=3),"intercept =",signif(summary(reg1)$coefficients[1],digits=3)))
      #d<-density(c(Xbar[t1:t] - Xa[t1:t]))
      #lines(d$y+1,d$x)
    }
  ###-------------------------------------------------------------------###
  ### process variance plots                                            ###
  ###-------------------------------------------------------------------### 
  if(processvar==TRUE){
    library(corrplot)
    cor.mat <- cov2cor(aqq[t,,]/bqq[t])
    colnames(cor.mat)<-colnames(X)
    rownames(cor.mat)<-colnames(X)
    par(mfrow=c(1,1),mai=c(1,1,4,1))
    corrplot(cor.mat,type="upper",tl.srt=45, 
             addCoef.col = "black")
    
    plot(total.time[t1:t],bqq[t1:t],pch=16,cex=1,ylab="Degrees of Freedom",
         xlab="Time")
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
   
}

