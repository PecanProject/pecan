##' @title sda.enkf
##' @name  sda.enkf
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param settings    PEcAn settings object
##' @param IC          data.frame of initial condition sample (nens X nstate)
##' @param prior       data.frame of model parameter sample (nense X nstate)
##' @param obs         data.frame of observations with columns: mean, sd
##' 
##' @description State Variable Data Assimilation: Ensemble Kalman Filter
##' 
##' @return NONE
##' 
sda.enkf <- function(settings,IC,prior,obs,processvar=NULL){
  
  if(is.null(processvar)) processvar = FALSE
  
  ## settings
  model <- settings$model$type
  write <- settings$database$bety$write
  defaults <- settings$pfts
  outdir <- settings$run$host$outdir
  host <- settings$run$host
  start.year <- strftime(settings$run$start.date,"%Y")
  end.year   <- strftime(settings$run$end.date,"%Y")
  forecast.duration <- 1 #eventually in settings
  forecast.time.step <- 1 #eventually in settings #dt
  spin.up <- 100
  nens = nrow(IC)
  
  if(nrow(prior) == 1 | is.null(nrow(prior))){
    var.names = names(prior)
    prior = as.data.frame(matrix(rep(prior,each=nens),nrow=nens))
    names(prior) = var.names
  }
  
  ## sda.demo <- TRUE  ## debugging flag
    
  ## open database connection
  if(write){
    con <- try(db.open(settings$database$bety), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    con <- NULL
  }
  
  # Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  
  # create an ensemble id
  if (!is.null(con)) {
    # write enseblem first
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                   now, "', 'EnKF', ", workflow.id, ")", sep=''), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep=''), con)[['id']]
  } else {
    ensemble.id <- -1
  }
  
  ## model-specific functions
  do.call("require",list(paste0("PEcAn.",model)))
  my.write.config <- paste("write.config.",model,sep="")
  my.read.restart <- paste("read.restart.",model,sep="")
  my.write.restart <- paste("write.restart.",model,sep="")
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    stop()
  }
  
  ## split clim file
  full.met <- settings$run$inputs$met$path
  new.met  <- file.path(settings$rundir,basename(full.met))
  file.copy(full.met,new.met)
  met <- new.met#split.met.SIPNET(new.met)
  
  
  ###-------------------------------------------------------------------###
  ### perform initial set of runs                                       ###
  ###-------------------------------------------------------------------###  
  X = IC
  run.id = list()
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
    
    ## write config
    # do.call(my.write.config,args=list(defaults,list(pft=prior[i,],env=NA),
    #                                  settings, run.id[[i]],inputs = settings$run,IC=IC[i,]))
    # linkages 15min for every 100 years
    settings$run$start.date <- paste0((as.numeric(start.year) - spin.up),strftime(settings$run$end.date,"/%m/%d"))
    settings$run$end.date <- paste0((as.numeric(start.year) + forecast.time.step),strftime(settings$run$end.date,"/%m/%d"))
    do.call(my.write.config,args=list(settings=settings,run.id = run.id[[i]],restart=FALSE))
    
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
        "hostname    : ", settings$run$host$name, "\n",
        "rundir      : ", file.path(settings$run$host$rundir, run.id[[i]]), "\n",
        "outdir      : ", file.path(settings$run$host$outdir, run.id[[i]]), "\n",
        file=file.path(settings$rundir, run.id[[i]], "README.txt"), sep='')
      
  }
  
  ## add the jobs to the list of runs
  cat(as.character(run.id),file=file.path(settings$rundir, "runs.txt"),sep="\n",append=FALSE)
      
  ## start model run
  start.model.runs(settings,settings$database$bety$write)
  
  #might need a way to rerun if some ensembles get stopped for errors?
  
  total.time = as.numeric(end.year):(as.numeric(end.year)+36)
  nt = length(total.time)
  #NPPm = rep(NA,nens)
  FORECAST <- ANALYSIS <- list()
  enkf.params <- list()
  aqq = array(0,dim=c(nt+1,ncol(IC),ncol(IC)))
  bqq = numeric(nt+1)
  CI.X1 <- matrix(0,3,nt) ; CI.X2 = CI.X1
  
  wish.df <- function(Om,X,i,j,col){
    n = (Om[i,j]^2 + Om[i,i]*Om[j,j])/var(X[,col])
    return(n)
  }
  
  ## numerical update of state and process error
  AnalysisFilterQ <- "
          model{ 
  X.mod ~ dmnorm(muf,pf) ## Model Forecast
  
  ## add process error
  q  ~ dwish(aq,bq)
  Q <- inverse(q) 
  X  ~ dmnorm(X.mod,q)
  
  ## Analysis
  Y  ~ dmnorm(X,r)
}"       
  ###-------------------------------------------
  ### loop over time
  ###-------------------------------------------
  for(t in 4:10){

    ### load output
    X <- do.call(my.read.restart,args=list(outdir,run.id,time = total.time[t],IC,prior,spin.up))
    FORECAST[[t]] = X
    
    ### Analysis step
    mu.f = apply(X,2,mean,na.rm=TRUE)
    Pf   = cov(X)
    Y    = t(obs[t,c(1,3,5,7)])#obs$mean[t]
#    Y = t(log(obs[t,c(1,3,5,7)]))
    R    = diag(as.numeric(obs[t,c(2,4,6,8)])^2)#obs$sd[t]^2
#    R = diag(log(as.numeric((1+obs[t,c(2,4,6,8)]^2/obs[t,c(1,3,5,7)]^2),4,4)))
#    H = matrix(c(1,rep(0,ncol(X)-1)),1,ncol(X))
    H    = diag(4)
    if(processvar == FALSE){
      K    = Pf%*%t(H)%*%solve(R+H%*%Pf%*%t(H))
      mu.a = mu.f + K%*%(Y-H%*%mu.f)
      Pa   = (diag(ncol(X)) - K%*%H)%*%Pf
    } else { 
        
        #### initial conditions
        bqq[1] <- length(mu.f)
        aqq[1,,] <- diag(length(mu.f))*bqq[1]
        
        ### analysis of model and data
        update = list(Y=Y, muf=mu.f, pf=solve(Pf,tol=0), aq=aqq[t,,], bq=bqq[t], r=solve(R))
        mod <- jags.model(file=textConnection(AnalysisFilterQ),
                          data=update,
                          n.adapt=1000,n.chains=3,
                          init=list(X.mod=as.vector(mu.f))) #inits for q?
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
        
        ifelse(eigen(V)$values>0,eigen(V)$values,print("matrix not positive definite"))
        
        aqq[t+1,,] = V
        bqq[t+1] = n
    }

    enkf.params[[t]] = list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa) 
 
 ## update state matrix
    analysis = as.data.frame(rmvnorm(nens,mu.a,Pa,method="svd"))
    #analysis = exp(analysis)
    analysis[is.na(analysis)] <- 0
    analysis <- abs(analysis)
    names(analysis) = names(X)
#  # EAKF
#  if(FALSE){
#    analysis = X
#    
#    ## Math from Anderson 2001. gives correct mean but incorrect var
#    A.svd = svd(Pf)
#    F = A.svd$v
#    G = diag(sqrt(A.svd$d))
#    B.svd = svd(t(G)%*%t(F)%*%t(H)%*%solve(R)%*%H%*%F%*%G)
#    U = B.svd$v
#    B = diag((1+B.svd$d)^(-0.5))
#    A = solve(t(F))%*%t(G)*solve(t(U))%*%t(B)%*%solve(t(G))%*%t(F)
#    for(i in 1:nens){
#      analysis[i,] = t(A)%*%matrix(as.numeric(X[i,])-mu.f)+mu.a
#    }
#    
#    ## HACK IGNORNING COVARIANCE
#    for(i in 1:nens){
#      analysis[i,] = mu.a + (matrix(as.numeric(X[i,]))-mu.f)*sqrt(diag(Pa)/diag(Pf))
#    }   
#    
#  }
## analysis sanity check
#for(i in 2:ncol(analysis)){
#  analysis[analysis[,i]<0,i] = 0.0
#}

    ANALYSIS[[t]] = analysis
 
    round(apply(FORECAST[[t]] - ANALYSIS[[t]],2,mean))
    round(obs[t,c(1,3,5,7)] - apply(ANALYSIS[[t]],2,mean))
    ### Forecast step
#save.image(file = "/home/araiho/start.here.RData")
    if(t < nt){
      #make sure write.configs is loaded. 
      #something is really slow in here.
      #don't worry about warnings @ t=1
      do.call(my.write.restart,args=list(nens,outdir,run.id,time = total.time[t],settings,prior,analysis))
      ## start model run
      start.model.runs(settings,settings$database$bety$write)
    }

}  ## end loop over time
###-------------------------------------------


## save all outputs
save(FORECAST,ANALYSIS,enkf.params,file=file.path(settings$outdir,"sda.ENKF.Rdata"))

# if(FALSE){
#   ### Load Data
#   if(sda.demo){
#     ## use one of the ensemble members as the true data
#     NPP <- read.output("ENS00001",settings$outdir,variables="NPP",model=model)$NPP
#     ytrue = tapply(NPP,Year,mean)*unit.conv
#     sd <- 0.3  ## pseudo data uncertainty
#     y <- rnorm(nt,ytrue,sd) ## add noise
#   } else {
#     load(file.path(settings$outdir,"plot2AGB.Rdata"))
#     mch = which(yrvec %in% time)
#     y = mNPP[1,mch]   ## data mean
#     sd = sNPP[1,mch]  ## data uncertainty 
#   }
# }  

#### Post-processing

  ### Diagnostic graphs  
  pdf(file.path(settings$outdir,"EnKF.pdf"))
  

par(mfrow=c(1,1))
plot.da(obs,FORECAST,ANALYSIS,"biomass_tsca","mean_tsca","sd_tsca",t=5)
plot.da(obs=obs,FORECAST,ANALYSIS,var.name="biomass_acsa3",
        mean.name="mean_acsa3",sd.name="sd_acsa3",t=5)
plot.da(obs,FORECAST,ANALYSIS,"biomass_beal2","mean_beal2","sd_beal2",t=5)
plot.da(obs,FORECAST,ANALYSIS,"biomass_thoc2","mean_thoc2","sd_thoc2",t=5)

  ## plot ensemble, filter, and data mean's and CI's
plot.da <- function(obs,FORECAST,ANALYSIS,var.name,mean.name,sd.name,t){
  y = obs[1:t,]
  
  pink = col2rgb("pink")
  alphapink = rgb(pink[1],pink[2],pink[3],100,max=255)
  Xbar = laply(FORECAST,function(x){return(mean(x[,var.name],na.rm=TRUE))})
  Xci  = laply(FORECAST,function(x){return(quantile(x[,var.name],c(0.025,0.975)))})

  green = col2rgb("green")
  alphagreen = rgb(green[1],green[2],green[3],100,max=255)
  Xa = laply(ANALYSIS,function(x){return(mean(x[,var.name],na.rm=TRUE))})
  XaCI  = laply(ANALYSIS,function(x){return(quantile(x[,var.name],c(0.025,0.975)))})
  
  plot(total.time[1:t],y[,mean.name],ylim=range(Xci,y[,mean.name],XaCI),
       type='n',xlab="total.time",ylab="kg/m^2/yr",main=var.name)
  
  ciEnvelope(total.time[1:t],y[,mean.name]-y[,sd.name]*1.96,y[,mean.name]+y[,sd.name]*1.96,col="lightblue")
  lines(total.time[1:t],y[,mean.name],type='b',col="darkblue")
  
  ciEnvelope(total.time[1:t],Xci[1:t,1],Xci[1:t,2],col=alphapink)
  lines(total.time[1:t],Xbar[1:t],col=2,type='b')
  
  ciEnvelope(total.time[1:t],XaCI[1:t,1],XaCI[1:t,2],col=alphagreen)
  lines(total.time[1:t],Xa[1:t],col="darkgreen",type='b')
  
  legend("topleft",c("Data","Forecast","Analysis"),col=c(4,2,3),lty=1,cex=1)
}

### Plots demonstrating how the constraint of your target variable 
### impacts the other model pools and fluxes



  ## plot scatter plots of outputs
  pairs(FORECAST[[nt]])
  pairs(ANALYSIS[[nt]])

  ## time series of outputs
  for(i in 1:ncol(X)){
    Xa = laply(ANALYSIS,function(x){return(mean(x[,i],na.rm=TRUE))})
    XaCI  = laply(ANALYSIS,function(x){return(quantile(x[,i],c(0.025,0.975)))})
    plot(time,Xa,ylim=range(XaCI),type='n',xlab="time",main=names(X)[i])
    ciEnvelope(time,XaCI[,1],XaCI[,2],col="lightblue")
    lines(time,Xa,type='b',col="darkblue")
  }
  
  dev.off()
  
}