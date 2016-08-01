##' @title sda.enkf
##' @name  sda.enkf
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param settings    PEcAn settings object
##' @param obs.mean    data.frame of observations of the mean of variables (time X nstate)
##' @param obs.cov      data.frame of observations of the sd of variables (time X nstate)
##' @param pick.trait.params  character vector of traits to sample if sample.parameters == TRUE
##' @param given.process.variance flag for if parameters should come from meta.analysis or be defaults. if false parameters are set to defaults
##' 
##' @description State Variable Data Assimilation: Ensemble Kalman Filter
##' 
##' @return NONE
##' 
sda.enkf <- function(settings, obs.mean, obs.cov, pick.trait.params = NULL, given.process.variance = NULL){
  
  ###-------------------------------------------------------------------###
  ### read settings                                                     ###
  ###-------------------------------------------------------------------### 
  
  model <- settings$model$type
  write <- settings$database$bety$write
  defaults <- settings$pfts
  outdir <- settings$host$outdir
  rundir <- settings$host$rundir
  host <- settings$host
  #forecast.duration <- 1 #eventually in settings #try to run with dates #Do we need this?
  forecast.time.step <- settings$state.data.assimilation$forecast.time.step #probably want to think more about this
  spin.up.start <- strftime(settings$state.data.assimilation$spin.up$start.date,"%Y")
  spin.up.end <- strftime(settings$state.data.assimilation$spin.up$end.date,"%Y")
  nens = settings$ensemble$size#30#nrow(IC) #right?
  start.year <- strftime(settings$state.data.assimilation$start.date,"%Y") #we need to make sure this matches the data years somehow
  end.year   <- strftime(settings$state.data.assimilation$end.date,"%Y")
  processvar <-settings$state.data.assimilation$process.variance
  sample_parameters <-settings$state.data.assimilation$sample.parameters
  variables <- unlist(settings$state.data.assimilation$state.variables, use.names = FALSE)
  #### Should given.process.variance or pick.trait.parameters be in the xml?
  
  ###-------------------------------------------------------------------###
  ### load climate data                                                 ###
  ###-------------------------------------------------------------------### 
  new.met <- paste0(rundir,"/climate.Rdata") #This doesn't actually do anything right now...
  
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
    # write enseblem first
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                   now, "', 'EnKF', ", workflow.id, ")", sep=''), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep=''), con)[['id']]
  } else {
    ensemble.id <- -1
  }
  
  ###-------------------------------------------------------------------###
  ### get model specific functions                                      ###
  ###-------------------------------------------------------------------### 
  do.call("require",list(paste0("PEcAn.",model)))
  my.write.config <- paste("write.config.",model,sep="")
  my.read.restart <- paste("read.restart.",model,sep="")
  my.write.restart <- paste("write.restart.",model,sep="")
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    stop()
  }
 
  ###-------------------------------------------------------------------###
  ### perform initial set of runs                                       ###
  ###-------------------------------------------------------------------###  
  X = matrix(NA,as.numeric(settings$ensemble$size),length(settings$pft))
  run.id = list()
#   
#   pda.init.run(settings = settings, con = con, my.write.config = my.write.config, workflow.id = workflow.id,
#                params = c(1000000012),n=ifelse(is.null(dim(params)), 1, nrow(params)),
#                run.names=paste("run", 1:n, sep="."))
#   
  for(i in 1:nens){
    
    ## set RUN.ID
    if (!is.null(con)) {
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      paramlist <- paste("EnKF:",i)
      db.query(paste("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id,",
                     " parameter_list) values ('", 
                     settings$model$id, "', '", settings$run$site$id, "', '", settings$run$start.date, "', '", 
                     settings$run$end.date, "', '", settings$outdir , "', '", now, "', ", ensemble.id, ", '", 
                     paramlist, "')", sep=''), con)
      run.id[[i]]<- db.query(paste("SELECT id FROM runs WHERE created_at='", now, "' AND parameter_list='", paramlist, "'", 
                                   sep=''), con)[['id']]
    } else {
      run.id[[i]] = paste("EnKF",i,sep=".")
    }
    dir.create(file.path(settings$rundir, run.id[[i]]), recursive=TRUE)
    dir.create(file.path(settings$modeloutdir, run.id[[i]]), recursive=TRUE)
    
    settings$run$start.date <- paste0(as.numeric(spin.up.start),strftime(settings$run$end.date,"/%m/%d"))
    settings$run$end.date <- paste0(as.numeric(spin.up.end),strftime(settings$run$end.date,"/%m/%d"))
    
    if(sample_parameters == TRUE){
      get.parameter.samples(pfts = settings$pfts, ens.sample.method=settings$ensemble$method)
      load(file.path(settings$outdir, "samples.Rdata"))
      do.call(my.write.config, args = list(trait.values = lapply(ensemble.samples, function(x, n){x[n,pick.trait.params]},n = i),
                                           settings = settings, run.id = run.id[[i]]))
    } else {
      do.call(my.write.config,args=list(trait.values = NA,settings=settings,run.id = run.id[[i]],restart=FALSE))
    }
    
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
        "met data    : ", new.met, "\n",
        "start date  : ", settings$run$start.date, "\n",
        "end date    : ", settings$run$end.date, "\n",
        "hostname    : ", settings$host$name, "\n",
        "rundir      : ", file.path(settings$host$rundir, run.id[[i]]), "\n",
        "outdir      : ", file.path(settings$host$outdir, run.id[[i]]), "\n",
        file=file.path(settings$rundir, run.id[[i]], "README.txt"), sep='')
    
  }
  
  ## add the jobs to the list of runs
  cat(as.character(run.id),file=file.path(settings$rundir, "runs.txt"),sep="\n",append=FALSE)
  
  ## start model runs
  start.model.runs(settings,settings$database$bety$write)
  
  ###-------------------------------------------------------------------###
  ### set up for data assimilation                                      ###
  ###-------------------------------------------------------------------###  
  
  ## vector to read the correct netcdfs by read.restart
  total.time = as.numeric(start.year):as.numeric(end.year) #Is this going to work?
  
  nt = length(total.time) #could be different if time step was different right?
  FORECAST <- ANALYSIS <- list()
  enkf.params <- list()
  aqq = array(0,dim=c(nt+1,ncol(X)+1,ncol(X)+1)) #HACK
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
  
  ###-------------------------------------------------------------------###
  ### loop over time                                                    ###
  ###-------------------------------------------------------------------###  
  for(t in 1:nt){
    
    ###-------------------------------------------------------------------###
    ### read restart                                                      ###
    ###-------------------------------------------------------------------###  
    X <- list()
    for(i in 1:nens){
      X[[i]] <- do.call(my.read.restart,args=list(outdir=outdir, runid = run.id[[i]],
                                                  time = total.time[t], settings = settings,
                                                  variables = variables))
    }
    
    X <- do.call(rbind,X)
    
    FORECAST[[t]] = X
    
    obs = !is.na(obs.mean[[t]])
    
    ###-------------------------------------------------------------------###
    ### analysis                                                          ###
    ###-------------------------------------------------------------------###  
    if(any(obs)){ #if no observations skip analysis
    mu.f = as.numeric(apply(X,2,mean,na.rm=TRUE))
    Pf   = cov(X)
    Y    = obs.mean[[t]][[1]][pmatch(colnames(X), names(obs.mean[[t]][[1]]))]

    H = diag(length(obs.mean[[t]][[1]]))
    R = obs.cov[[t]]#how do we make sure this is in the right order?
    
    for(s in 1:length(obs.mean[[t]][[1]])){
      if(diag(R)[s]==0){ #if covariance is 0 then set it so model with
        diag(R)[s] <- min(diag(R)[which(diag(R)!=0)])/2#.01^2 #probably way low?
      }
    }
   
    if(processvar == FALSE){
      K    = Pf%*%t(H)%*%solve(R+H%*%Pf%*%t(H))
      mu.a = mu.f + K%*%(Y-H%*%mu.f)
      Pa   = (diag(ncol(X)) - K%*%H)%*%Pf
    } else { 
      
      #### initial conditions
      bqq[1] <- length(mu.f)
      aqq[1,,] <- diag(length(mu.f))*bqq[1]
      
      #### extinct from the forecast ensembles
      E <- which(colMeans(X) < .001)
      
      #### forcast to modeled vector ## remove spp. that don't show up in forecast
      F2M<- seq(1,length(mu.f),1)
      F2M[E]<-NA
      F2M <- na.omit(F2M)
      
      #### translating x to y ## remove X that you don't have data for
      X2Y<- seq(1,length(Y),1)
      X2Y <- X2Y[which(!is.na(Y))]
    
      ### analysis of model and data
      if(length(E)>0){ #if all ensemble members 
        update = list(Y=na.omit(Y), r=solve(R[X2Y,X2Y]),
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
        update = list(Y=na.omit(Y), r=solve(R[X2Y,X2Y]),
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
      
      n = mean(WV) #n + 1
      if(n < length(mu.f)) n = length(mu.f)
      V = solve(q.bar)*n
      
      #ifelse(eigen(V)$values>0,eigen(V)$values,print("matrix not positive definite"))
      
      aqq[t+1,,] = V
      bqq[t+1] = n
    }
    } else {
      #if no obs skip analysis
      if(processvar==FALSE){
        mu.a = mu.f
        Pa = Pa
      } else {
        mu.a = mu.f
        if(is.null(given.process.variance)){
          print("Error -- must define given.process.variance")
          break
        }else{
          Pa = given.process.variance #from full DA analysis #where are you going to get this without full DA?
        }
      }
      
    }
    
    enkf.params[[t]] <- list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa, q.bar=q.bar, n=n) 
    
    ## update state matrix
    analysis <- as.data.frame(rmvnorm(nens,mu.a,Pa,method="svd"))
    colnames(analysis) <- colnames(X)
    
    ANALYSIS[[t]] = analysis
    
    ###-------------------------------------------------------------------###
    ### forecast step -- write restart                                    ###
    ###-------------------------------------------------------------------### 
    if(t < nt){
      for(i in 1:nens){
        do.call(my.write.restart,
                args=list(out.dir = outdir, runid = run.id[[i]],
                          time = total.time[t], settings = settings,
                          analysis.vec = analysis[i,],
                          RENAME = TRUE, PLOT=FALSE, variables=variables,
                          sample_parameters = sample_parameters,
                          trait.values = lapply(ensemble.samples,
                                                function(x, n){x[n,pick.trait.params]},
                                                n = i),
                          my.write.config))
      }
      ###-------------------------------------------------------------------###
      ### Run model                                                         ###
      ###-------------------------------------------------------------------### 
      print(paste("Running Model for year",total.time[t]+1))
      start.model.runs(settings,settings$database$bety$write)
    }
    
    
  }  ## end loop over time
  ###-------------------------------------------
  
  ###-------------------------------------------------------------------###
  ### save outputs                                                      ###
  ###-------------------------------------------------------------------### 
  save(FORECAST,ANALYSIS,enkf.params,file=file.path(settings$outdir,"sda.ENKF.Rdata"))
  
  
  ###-------------------------------------------------------------------###
  ### create diagnostics                                                ###
  ###-------------------------------------------------------------------### 
  
  ### LOAD CLIMATE ### HACK ### LINKAGES SPECIFIC
  climate_file <- settings$run$inputs$met$path
  load(climate_file)
  temp.mat <- temp.mat[total.time-853,]
  precip.mat <- precip.mat[total.time-853,]
  
  ### Diagnostic graphs  
  pdf(file.path(settings$outdir,"EnKF.pdf"))
    
    ###-------------------------------------------------------------------###
    ### time series                                                       ###
    ###-------------------------------------------------------------------### 
    t1=1
    pink = col2rgb("deeppink")
    alphapink = rgb(pink[1],pink[2],pink[3],180,max=255)
    green = col2rgb("green")
    alphagreen = rgb(green[1],green[2],green[3],75,max=255)
    blue = col2rgb("blue")
    alphablue = rgb(blue[1],blue[2],blue[3],75,max=255)
    
    Ybar =  laply(obs.mean[t1:t],function(x){return(x[[1]])})
    Ybar = Ybar[,na.omit(pmatch(colnames(X), names(obs.mean[[nt]][[1]])))]
    YCI = as.matrix(laply(obs.cov[t1:t],function(x){return(sqrt(diag(x)))}))  #need to make this from quantiles
    YCI = YCI[,pmatch(colnames(X), names(obs.mean[[nt]][[1]]))]
   
    for(i in 1:ncol(X)){
      t1=1
      Xbar = laply(FORECAST[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
      Xci  = laply(FORECAST[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
      
      Xa = laply(ANALYSIS[t1:t],function(x){return(mean(x[,i],na.rm=TRUE))})
      XaCI  = laply(ANALYSIS[t1:t],function(x){return(quantile(x[,i],c(0.025,0.975)))})
      
      plot(total.time[t1:t],Xbar,ylim=range(XaCI),
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
      
      #analysis
      ciEnvelope(total.time[(t1:t)],XaCI[,1],XaCI[,2],col=alphapink)
      lines(total.time[t1:t],Xa,col="black",lty=2,lwd=2)
      
    }
      
    ###-------------------------------------------------------------------###
    ### bias diagnostics                                                  ###
    ###-------------------------------------------------------------------### 
      #legend("topleft",c("Data","Forecast","Analysis"),col=c(4,2,3),lty=1,cex=1)
      #Forecast minus data = error
      reg <- lm(Xbar[t1:t] - unlist(Ybar[t1:t,i])~c(t1:t))
      plot(t1:t,Xbar[t1:t] - unlist(Ybar[t1:t,i]),pch=16,cex=1,
           ylim=c(min(Xci[t1:t,1]-unlist(Ybar[t1:t,i])),
                  max(Xci[t1:t,2]-unlist(Ybar[t1:t,i]))),
           xlab="Time", ylab="Error",main="Error = Forecast - Data")
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
           xlab="Time", ylab="Update",main="Update = Forecast - Analysis")
      ciEnvelope(rev(t1:t),rev(Xbar[t1:t] - XaCI[t1:t,1]),
                 rev(Xbar[t1:t] - XaCI[t1:t,2]),col=alphagreen)
      abline(h=0,lty=2,lwd=2)
      abline(reg1)
      mtext(paste("slope =",signif(summary(reg1)$coefficients[2],digits=3),"intercept =",signif(summary(reg1)$coefficients[1],digits=3)))
      #d<-density(c(Xbar[t1:t] - Xa[t1:t]))
      #lines(d$y+1,d$x)
  
  ###-------------------------------------------------------------------###
  ### process variance plots                                            ###
  ###-------------------------------------------------------------------### 
  library(corrplot)
  cor.mat <- cov2cor(aqq[t,,]/bqq[t])
  colnames(cor.mat)<-colnames(X)
  rownames(cor.mat)<-colnames(X)
  par(mfrow=c(1,1),mai=c(1,1,4,1))
  corrplot(cor.mat,type="upper",tl.srt=45, 
           addCoef.col = "black")

  plot(total.time[t1:t],bqq[t1:t],pch=16,cex=1,ylab="Degrees of Freedom",
        xlab="Time")
  
   
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
