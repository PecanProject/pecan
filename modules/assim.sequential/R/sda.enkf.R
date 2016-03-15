##' @title sda.enkf
##' @name  sda.enkf
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param settings    PEcAn settings object
##' @param IC          data.frame of initial condition sample (nens X nstate)
##' @param prior       data.frame of model parameter sample (nens X nstate)
##' @param obs.mean    data.frame of observations of the mean of variables
##' @param obs.sd      data.frame of observations of the sd of variables
##' @param processvar  flag for if process variance should be estimated or not
##' 
##' @description State Variable Data Assimilation: Ensemble Kalman Filter
##' 
##' @return NONE
##' 
sda.enkf <- function(settings,IC,prior,obs.mean,obs.sd,processvar=FALSE){

  ## settings
  model <- settings$model$type
  write <- settings$database$bety$write
  defaults <- settings$pfts
  outdir <- settings$run$host$outdir
  rundir <- settings$run$host$rundir
  host <- settings$run$host
  forecast.duration <- 1 #eventually in settings
  forecast.time.step <- 1 #eventually in settings #dt
  spin.up <- 100 #eventually in settings
  nens = nrow(IC)
  start.year <- strftime(settings$run$start.date,"%Y")
  end.year   <- strftime(settings$run$end.date,"%Y")

  
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
  ## leaving in for sipnet
#   full.met <- settings$run$inputs$met$path
#   new.met  <- file.path(settings$rundir,basename(full.met))
#   file.copy(full.met,new.met)
#   met <- new.met#split.met.SIPNET(new.met)
  
  
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
    settings$run$start.date <- paste0((as.numeric(start.year) - spin.up),strftime(settings$run$end.date,"/%m/%d"))
    settings$run$end.date <- paste0((as.numeric(end.year)),strftime(settings$run$end.date,"/%m/%d"))
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
   
  total.time = as.numeric(start.year):(as.numeric(start.year)+nrow(obs.mean)) #RETHINK

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
  for(t in 1:nt){

    ### load output    
    X = matrix(NA, nrow = nrow(IC), ncol = ncol(IC))
    for(i in 1:nens){
      X[i,] <- do.call(my.read.restart,args=list(outdir=outdir,run.id = run.id[[i]],
                                                 time = total.time[t],spin.up = spin.up,
                                                 X.vec = X[i,]))
    }
    FORECAST[[t]] = X
    
    ### Analysis step
    mu.f = apply(X,2,mean,na.rm=TRUE)
    Pf   = cov(X)
    Y    = t(obs.mean[t,])#obs$mean[t]
    R    = diag(as.numeric(obs.sd[t,])^2)#obs$sd[t]^2
    H    = diag(ncol(obs.mean))
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
        
        #ifelse(eigen(V)$values>0,eigen(V)$values,print("matrix not positive definite"))
        
        aqq[t+1,,] = V
        bqq[t+1] = n
    }

    enkf.params[[t]] = list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa) 
 
 ## update state matrix
    analysis = as.data.frame(rmvnorm(nens,mu.a,Pa,method="svd"))
    #analysis = exp(analysis)
    
 #HACK #not a good assumption #
    analysis[is.na(analysis)] <- 0
    analysis <- abs(analysis)
 #HACK
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
    ### Forecast step
    if(t < nt){
      for(i in 1:nens){
        do.call(my.write.restart,args=list(outdir = outdir, run.id = run.id[[i]],
                                           time = total.time[t], settings = settings,
                                           analysis = analysis[i,c(1,2,4,3)],
                                           RENAME = TRUE))
      }
      ## start model run
      start.model.runs(settings,settings$database$bety$write)
    }


}  ## end loop over time
###-------------------------------------------

## save all outputs
save(FORECAST,ANALYSIS,enkf.params,file=file.path(settings$outdir,"sda.ENKF.Rdata"))

#### Post-processing

### LOAD CLIMATE ### HACK ### LINKAGE SPECIFIC
climate_file <- settings$run$inputs$met$path
load(climate_file)
temp.mat <- temp.mat[total.time-849,]
precip.mat <- precip.mat[total.time-849,]

### Diagnostic graphs  
pdf(file.path(settings$outdir,"EnKF.pdf"))

if(processvar==TRUE){
  
  #Degrees of Freedom
  par(mfrow=c(1,1))
  pairs(dat[,iX])
  
  plot(total.time[t1:t],bqq[t1:t],pch=16,cex=1,ylab="Degrees of Freedom",
       xlab="Time")
  
  #Process Covariance
  par(mfrow=c(4,4),mar=c(2,1,1,1),oma=c(0,2,2,0))
  for(r in 1:4){
    for(c in 1:4){
      plot(aqq[2:nt,r,c]/bqq[2:nt],xlab=NA,
           ylab=NA,pch=16,cex=1)
      if(r==1) {
        mtext(paste(c("hemlock","maple","Y.birch",
                      "cedar")[c]),cex=2)
      }
      if(c==1) mtext(c("hemlock","maple","Y.birch",
                       "cedar")[r],2,cex=2)
      legend("right",c(paste("proc cor =",signif(cov2cor(aqq[nt,,]/bqq[nt])[r,c],digits=3)),
                          paste("proc cov =",signif(aqq[nt,r,c]/bqq[nt],digits=3)),
                       paste("model cor = ",signif(cov2cor(Pf)[r,c],digits=3)),
                       paste("model cov = ",signif(Pf[r,c],digits=3))),cex=1)
    }
  }
  
}

## plot ensemble, filter, and data mean's and CI's
plot.EnKF.time.series <- function(obs.mean,obs.sd,FORECAST,ANALYSIS,var.name,mean.name,sd.name,t1,t,ylim){
  y.mean = obs.mean[t1:t,]
  y.sd = obs.sd[t1:t,]
  
  pink = col2rgb("pink")
  alphapink = rgb(pink[1],pink[2],pink[3],100,max=255)
  Xbar = laply(FORECAST,function(x){return(mean(x[,var.name],na.rm=TRUE))})
  Xci  = laply(FORECAST,function(x){return(quantile(x[,var.name],c(0.025,0.975)))})
  
  green = col2rgb("green")
  alphagreen = rgb(green[1],green[2],green[3],100,max=255)
  Xa = laply(ANALYSIS,function(x){return(mean(x[,var.name],na.rm=TRUE))})
  XaCI  = laply(ANALYSIS,function(x){return(quantile(x[,var.name],c(0.025,0.975)))})
  
  plot(total.time[t1:t],y.mean[,mean.name],ylim=ylim,
       type='n',xlab="total.time",ylab="kg/m^2/yr",main=var.name)
  
  ciEnvelope(total.time[t1:t],y.mean[,mean.name]-y.sd[,sd.name]*1.96,y.mean[,mean.name]+y.sd[,sd.name]*1.96,col="lightblue")
  lines(total.time[t1:t],y.mean[,mean.name],type='b',col="darkblue")
  
  ciEnvelope(total.time[t1:t],Xci[t1:t,1],Xci[t1:t,2],col=alphapink)
  lines(total.time[t1:t],Xbar[t1:t],col=2,type='b')
  
  ciEnvelope(total.time[t1:t],XaCI[t1:t,1],XaCI[t1:t,2],col=alphagreen)
  lines(total.time[t1:t],Xa[t1:t],col="darkgreen",type='b')
  
  #Forecast minus data = error
  reg <- lm(Xbar[t1:t] - y.mean[t1:t,mean.name]~c(t1:t))
  plot(t1:t,Xbar[t1:t] - y.mean[t1:t,mean.name],pch=16,cex=1,ylim=c(min(Xci[t1:t,1]-y.mean[,mean.name]),max(Xci[t1:t,2]-y.mean[,mean.name])),xlab="Time", ylab="Error",main="Error = Forecast - Data")
  ciEnvelope(rev(t1:t),rev(Xci[t1:t,1]-y.mean[t1:t,mean.name]),rev(Xci[t1:t,2]-y.mean[t1:t,mean.name]),col=alphapink)
  abline(h=0,lty=2,lwd=2)
  abline(reg)
  mtext(paste("slope =",signif(summary(reg)$coefficients[2],digits=3),"intercept =",signif(summary(reg)$coefficients[1],digits=3)))
  d<-density(c(Xbar[t1:t] - y.mean[,mean.name]))
  lines(d$y+1,d$x)
  
  #forecast minus analysis = update
  reg1 <- lm(Xbar[t1:t] - Xa[t1:t] ~ c(t1:t))
  plot(t1:t,Xbar[t1:t] - Xa[t1:t],pch=16,cex=1,ylim=c(min(XaCI[t1:t,2]-Xbar[t1:t]),max(Xbar[t1:t]-XaCI[t1:t,1])),
       xlab="Time", ylab="Update",main="Update = Forecast - Analysis")
  ciEnvelope(rev(t1:t),rev(Xbar[t1:t] - XaCI[t1:t,1]),rev(Xbar[t1:t] - XaCI[t1:t,2]),col=alphagreen)
  abline(h=0,lty=2,lwd=2)
  abline(reg1)
  mtext(paste("slope =",signif(summary(reg1)$coefficients[2],digits=3),"intercept =",signif(summary(reg1)$coefficients[1],digits=3)))
  d<-density(c(Xbar[t1:t] - Xa[t1:t]))
  lines(d$y+1,d$x)
  
  #plot(temp.mat[,1],Xbar)
  par(mfrow=c(2,2))
  plot(rowMeans(temp.mat[t1:t,]),Xbar[t1:t] - y.mean[t1:t,mean.name],xlim=c(min(rowMeans(temp.mat[t1:t,]))-2,
                                      max(rowMeans(temp.mat[t1:t,]))+2),
       ylim = c(min(Xbar[t1:t] - y.mean[,mean.name]),max(Xbar[t1:t] - y.mean[,mean.name])),pch=16,cex=1,xlab="Average Monthly Temp",
       ylab="Error",main = paste(mean.name))
  plot(rowSums(precip.mat[t1:t,]),Xbar[t1:t] - y.mean[t1:t,mean.name],xlim=c(min(rowSums(precip.mat[t1:t,]))-10,
                                       max(rowSums(precip.mat[t1:t,]))+10),
       ylim = c(min(Xbar[t1:t] - y.mean[,mean.name]),max(Xbar[t1:t] - y.mean[,mean.name])),pch=16,cex=1,xlab="Total Yearly Precip",
       ylab="Error",main = paste(mean.name))
  

  plot(rowMeans(temp.mat[t1:t,]),Xbar[t1:t] - Xa[t1:t],pch=16,
       cex=1,xlab="Average Monthly Temp",
       ylab="Update",main = paste(mean.name))
  plot(rowSums(precip.mat[t1:t,]),Xbar[t1:t] - Xa[t1:t],pch=16,
       cex=1, xlab="Total Yearly Precip",
       ylab="Update",main = paste(mean.name))
  
  #legend("topleft",c("Data","Forecast","Analysis"),col=c(4,2,3),lty=1,cex=1)
}

par(mfrow=c(1,1))
t1=2
t = 36
plot.EnKF.time.series(obs.mean,obs.sd,FORECAST,ANALYSIS,"biomass_tsca","mean_tsca","sd_tsca",t1=t1,t=t,ylim=c(6,18))
plot.EnKF.time.series(obs.mean,obs.sd,FORECAST,ANALYSIS,var.name="biomass_acsa3",
        mean.name="mean_acsa3",sd.name="sd_acsa3",t1=t1,t=t,ylim=c(0,1))
plot.EnKF.time.series(obs.mean,obs.sd,FORECAST,ANALYSIS,"biomass_beal2","mean_beal2","sd_beal2",t1=t1,t=t,ylim=c(0,4))
plot.EnKF.time.series(obs.mean,obs.sd,FORECAST,ANALYSIS,"biomass_thoc2","mean_thoc2","sd_thoc2",t1=t1,t=t,ylim=c(0,.2))


### Read in output and restart files
# nt <- t
# forecast.ntrees <- array(0,dim=c(nens,4,nt))
# forecast.dbh <- array(list(),dim=c(nens,4,nt))
# forecast.nogro <- array(list(),dim=c(nens,4,nt))
# 
# for(i in 1:nens){
#   for(t in 1:nt){
#     if(t < nt){
#       outfile = file.path(outdir,run.id[[i]],paste0(total.time[t],"linkages.out.Rdata"))
#     }else{
#       outfile = file.path(outdir,run.id[[i]],"linkages.out.Rdata")
#     } 
#   load(outfile)
#   forecast.ntrees[i,,t] <- ntrees.kill[,1,1]
#   nl = 1
#   for(s in 1:4){
#       nu <- nl + forecast.ntrees[i,s,t] - 1
#       forecast.dbh[i,s,t] <- list(dbh.save[nl:nu,1,1])
#       forecast.nogro[i,s,t] <- list(nogro.save[nl:nu,1,1])
#       nl <- nu + 1
#   }
#  }
# }
# 
# nt <- nt
# restart.ntrees <- array(0,dim=c(nens,4,nt))
# restart.dbh <- array(list(),dim=c(nens,4,nt))
# 
# for(i in 1:nens){
#   for(t in 1:nt){
#     if(t < nt){
#       outfile = file.path(rundir,run.id[[i]],paste0(total.time[t],"linkages.restart.Rdata"))
#     }else{
#       outfile = file.path(rundir,run.id[[i]],"linkages.restart.Rdata")
#     } 
#     load(outfile)
#     restart.ntrees[i,,t] <- ntrees
#     nl = 1
#     for(s in 1:4){
#       nu <- nl + restart.ntrees[i,s,t] - 1
#       restart.dbh[i,s,t] <- list(dbh[nl:nu])
#       nl <- nu + 1
#     } 
#   }
# }
# 
# 
# diag.plot <- function(t,spp){
#   boxplot(FORECAST[[t]][,spp],ANALYSIS[[t]][,spp],FORECAST[[t+1]][,spp], ylab = "Biomass",
#           col=c('pink','lightgreen','pink'),main=colnames(X)[spp])
#   spp.select <- c(1,2,4,3)
#   boxplot(forecast.ntrees[,spp.select[spp],t],restart.ntrees[,spp.select[spp],t],
#           forecast.ntrees[,spp.select[spp],t+1], 
#           col=c('pink','lightgreen','pink'),ylab = "Number of Trees")
#   boxplot(unlist(forecast.dbh[,spp.select[spp],t]),unlist(restart.dbh[,spp.select[spp],t]),
#           unlist(forecast.dbh[,spp.select[spp],t+1]), ylab = "DBH",
#           col=c('pink','lightgreen','pink'))
# }
# par(mfrow=c(1,3))
# for(s in 1:4){
#   diag.plot(t=14,spp=s)
# }
# 
# for(t in 1:15){
#   diag.plot(t=t,spp=1)
# }
# 




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

  



### Plots demonstrating how the constraint of your target variable 
### impacts the other model pools and fluxes



#   ## plot scatter plots of outputs
#   pairs(FORECAST[[nt]])
#   pairs(ANALYSIS[[nt]])
# 
#   ## time series of outputs
#   for(i in 1:ncol(X)){
#     Xa = laply(ANALYSIS,function(x){return(mean(x[,i],na.rm=TRUE))})
#     XaCI  = laply(ANALYSIS,function(x){return(quantile(x[,i],c(0.025,0.975)))})
#     plot(time,Xa,ylim=range(XaCI),type='n',xlab="time",main=names(X)[i])
#     ciEnvelope(time,XaCI[,1],XaCI[,2],col="lightblue")
#     lines(time,Xa,type='b',col="darkblue")
#   }
  
  dev.off()
  
}