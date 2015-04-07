InventoryGrowthFusionDiagnostics <- function(jags.out,combined){
  
  #### Diagnostic plots
  
  ### DBH
  #par(mfrow=c(3,2))
  layout(matrix(1:8,4,2))
  out <- as.matrix(jags.out)
  x.cols = which(substr(colnames(out),1,1)=="x")
  ci <- apply(out[,x.cols],2,quantile,c(0.025,0.5,0.975))
  ci.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
  
  smp = sample.int(data$ni,4)
  for(i in smp){
    sel = which(ci.names$row == i)
    plot(data$time,ci[2,sel],type='n',ylim=range(ci[,sel],na.rm=TRUE),ylab="DBH (cm)",main=i)
    ciEnvelope(data$time,ci[1,sel],ci[3,sel],col="lightBlue")
    points(data$time,data$z[i,],pch="+",cex=1.5)
 #   lines(data$time,z0[i,],lty=2)
  }
  
  ## growth
  for(i in smp){
    sel = which(ci.names$row == i)
    inc.mcmc = apply(out[,x.cols[sel]],1,diff)
    inc.ci = apply(inc.mcmc,1,quantile,c(0.025,0.5,0.975))*5
    #inc.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
    
    plot(data$time[-1],inc.ci[2,],type='n',ylim=range(inc.ci,na.rm=TRUE),ylab="Ring Increment (mm)")
    ciEnvelope(data$time[-1],inc.ci[1,],inc.ci[3,],col="lightBlue")
    points(data$time,data$y[i,]*5,pch="+",cex=1.5,type='b',lty=2)
  }
  
  if(FALSE){
  ##check a DBH
  plot(out[,which(colnames(out)=="x[3,31]")])
  abline(h=z[3,31],col=2,lwd=2)
  hist(out[,which(colnames(out)=="x[3,31]")])
  abline(v=z[3,31],col=2,lwd=2)
  }
  
  ## process model
  vars = (1:ncol(out))[-c(which(substr(colnames(out),1,1)=="x"),grep("tau",colnames(out)),
                          grep("year",colnames(out)),grep("ind",colnames(out)))]
  par(mfrow=c(1,1))
  for(i in vars){
    hist(out[,i],main=colnames(out)[i])
  }
  if(length(vars)>1) pairs(out[,vars])
  
  ## Standard Deviations
  #layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
  par(mfrow=c(2,3))
  prec = out[,grep("tau",colnames(out))]
  for(i in 1:ncol(prec)){
    hist(1/sqrt(prec[,i]),main=colnames(prec)[i])
  }
  cor(prec)
#  pairs(prec)
  
    
  par(mfrow=c(1,1))
  ### YEAR
  year.cols = grep("year",colnames(out))
  if(length(year.cols>0)){
    ci.yr <- apply(out[,year.cols],2,quantile,c(0.025,0.5,0.975))
    plot(data$time,ci.yr[2,],type='n',ylim=range(ci.yr,na.rm=TRUE),ylab="Year Effect")
    ciEnvelope(data$time,ci.yr[1,],ci.yr[3,],col="lightBlue")
    lines(data$time,ci.yr[2,],lty=1,lwd=2)
    abline(h=0,lty=2)
  }
  
  ### INDIV
  ind.cols= which(substr(colnames(out),1,3)=="ind")
  if(length(ind.cols)>0){
    boxplot(out[,ind.cols],horizontal=TRUE,outline=FALSE,col=combined$PLOT)
    abline(v=0,lty=2)
    tapply(apply(out[,ind.cols],2,mean),combined$PLOT,mean)
    table(combined$PLOT)
    
    spp = combined$SPP
#    boxplot(out[order(spp),ind.cols],horizontal=TRUE,outline=FALSE,col=spp[order(spp)])
    boxplot(out[,ind.cols],horizontal=TRUE,outline=FALSE,col=spp)
    abline(v=0,lty=2)
    spp.code = levels(spp)[table(spp)>0]
    legend("bottomright",legend=rev(spp.code),col=rev(which(table(spp)>0)),lwd=4)
    tapply(apply(out[,ind.cols],2,mean),combined$SPP,mean)
  }
}
########### NEXT STEPS: ############
#what explain's the year effects? climate
#what explain's the individual effects? size, species, canopy position, plot -> landscape
