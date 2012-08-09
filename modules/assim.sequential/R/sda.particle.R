###  State Variable Data Assimilation:
###     Particle Filter
###
###  Michael Dietze <dietze@bu.edu>
###

### Prerequisite:assumes that you have run an ensemble run

sda.particle <- function(model){
  sda.demo <- TRUE
  
  library(Hmisc)

  Year <- read.output("ENS00001",settings$outdir,variables="Year",model=model)
  time <- as.numeric(names(table(Year)))
  
### Load Ensemble
  ensp = read.ensemble.ts(model)[[1]]
  ensp = t(apply(ensp,1,tapply,Year,mean))
  np = nrow(ensp)      ## number of particles
  nt = ncol(ensp)      ## number of time steps
  w = matrix(1,np,nt)  ## matrix of weights

### Load Data
  if(sda.demo){
    ## use one of the ensemble members as the true data
    sd <- apply(ensp,2,sd)*0.3  ## pseudo data uncertainty
    ref <- sample(1:np,1)       ## choose an ensemble member
    y <- rnorm(nt,ensp[ref,],sd) ## add noise
  }
  
###analysis: generate log-weights
  for(t in 1:nt){    
    if(!is.na(y[t])){
      for(j in 1:np){
        w[j,t] = sum(dnorm(y[t],ensp[j,t],sd[t],log=TRUE)) #loglik
      }
    }
  }

###post-hoc processing of weights
wc = t(exp(apply(w,1,cumsum)))
wbar = apply(wc,2,mean)
Xap = 0
Pap = 0
XapCI = matrix(NA,2,nt)
for(i in 1:nt){
  Xap[i] = weighted.mean(ensp[,i],wc[,i])
  XapCI[,i] = wtd.quantile(ensp[,i],wc[,i]/wbar[i],c(0.025,0.975))
  Pap[i] = wtd.var(ensp[,i],wc[,i]/wbar[i])
}
Pap[Pap < 0] <- 0
Xbar = apply(ensp,2,mean)
Xci = apply(ensp,2,quantile,c(0.025,0.975))

### HERE
  
#pdf("ParticleFilter.pdf")
plot(time,y,ylim=range(Xci),type='b')
if(sda.demo) lines(time,ensp[ref,],col=2,lwd=2)
#for(i in 1:min(500,np)){
# lines(t2,ensp[,i],lty=3,col="grey")
#}
#lines(t,x,type='b')
#points(tvec,yvec,col=2,pch=19)
lines(time,Xbar[1:nt],col=6)
lines(time,Xci[1,1:nt],col=6,lty=2)
lines(time,Xci[2,1:nt],col=6,lty=2)
legend("topleft",c("True","Data","ens","ensmean"),col=c(1:2,"grey",6),lty=c(1,0,3,1),pch=c(1,19,1,1),cex=1.5)

lines(time,Xap,col=3,type='b',lwd=2)
lines(time,XapCI[1,],col=3,lty=2,lwd=2)
lines(time,XapCI[2,],col=3,lty=2,lwd=2)
#lines(t,Xap+1.96*sqrt(Pap),col=3,lty=2)
#lines(t,Xap-1.96*sqrt(Pap),col=3,lty=2)
legend("topleft",c("True","Data","PF","ens","ensmean"),col=c(1:3,"grey",6),lty=c(1,0,1,3,1),pch=c(1,19,1,1,0),cex=1.5)
  
## need to add figures for covariances and predictions for covariates
  
}
