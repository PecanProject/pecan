library(R2WinBUGS)
library(BRugs)
dat=read.csv("c4photosynthesis.csv",header=T)
#dat2=read.csv('c4covariates.csv',header=T)

my.model  = function(){
  alpha ~ dlnorm(-3.21,3.7) 	    	## initial slope of photosynthesis light response
  vmax~dlnorm(3,3)                  ## maximum rubisco capacity
  r ~ dlnorm(-0.2,2.8)              ## leaf respiration
  k ~ dlnorm(11.5, 2.8)             ## initial slope of photosynthetic CO2 response
  tau ~ dgamma(0.1,0.1)
#  beta1 ~ dnorm(0,0.1)             ## chlorophyll effects 
#  beta2 ~ dnorm(0,0.1)             ## SLA effects
#  beta5 ~ dnorm(0,0.1)             ## Vmax effects

#  tau.Vleaf~dgamma(0.01,0.01)      ## random leaf effects on vmax
#  tau.Kleaf~dgamma(0.01,0.01)      ## random leaf effects on k
#  tau.Vmon~dgamma(0.01,0.01)       ## month effects on vmax, fixed

#  for(i in 1:nrep){
#   Vleaf[i]~dnorm(0,tau.Vleaf)
#   Kleaf[i]~dnorm(0,tau.Kleaf)
#   }
#   Vmon[7]<-0                      ## reference month
#   Vmon[8]~ dnorm(0,tau.Vmon)
#   Vmon[5]~ dnorm(0,tau.Vmon)
#   Vmon[6]~ dnorm(0,tau.Vmon)
#   Vmon[9]~ dnorm(0,tau.Vmon)
#   Vmon[10]~ dnorm(0,tau.Vmon)

  for(i in 1:n  ){                 ## process model
#    al[i]<-(alpha+inc[1]*beta1*(chl[rep[i]]-chlave)+inc[2]*beta2*(sla[rep[i]]-slaave))*q[i]          ## light limited with chlorophyll, SLA and random leaf effects turned on
    al[i]<- alpha*q[i]                                                                                ## light limited without covariates
#    ac[i]<-(k + inc[3]*Kleaf[rep[i]])*pi[i]/100000                                                   ## CO2 limited with random leaf effects turned on
    ac[i]<-k*pi[i]/100000                                                    ## CO2 limited without covariates
#    ae[i]<-vmax + inc[4]*Vmon[month[i]]+inc[5]*beta5*(leafn[rep[i]]-leafnave)+inc[6]*Vleaf[rep[i]]   ## rubisco limited with leaf N, month and random leaf effects turned on
    ae[i]<-vmax                                                                                       ## rubisco limited without covariates
    prean[i]<-min(min(al[i],ac[i]),ae[i])-r
    an[i]~dnorm(prean[i],tau)                                                                         ## likelihood
    pA[i] ~ dnorm(prean[i],tau)                                                                       ## prediction
    }
}

write.model(my.model,"c4model.txt")

init<-list()
 init[[1]]<-list(r=0.8, vmax=30,alpha=0.03, tau=10, k=0.7*100000)   ## ,tau.Vleaf=300,tau.Kleaf=1e-10, beta1=4, beta2=1,beta5=3,tau.Vmon=10
 init[[2]]<-list(r=1, vmax=20, alpha=0.07, tau=20, k=0.8*100000)    ## ,tau.Vleaf=200,tau.Kleaf=2e-9,beta1=1,beta2=1,beta5=-1,tau.Vmon=20
 init[[3]]<-list(r=2, vmax=15,alpha=0.06, tau=20, k=0.2*1000000)    ## ,tau.Vleaf=100,tau.Kleaf=3e-8,beta1=1,beta2=2,beta5=2,tau.Vmon=3

 spp.list = unique(as.character(dat$id))   
 c4mcmc <- list()

 for(s in spp.list){
  sel = which(dat$id == s)
  an=dat$Photo[sel]
  pi=dat$Ci_Pa[sel]
  q=dat$PARi[sel]
#  rep=dat$rep[sel]      ## turn on random leaf effects
#  reps = unique(rep)
#  rep = match(rep,reps)
#  nrep = length(reps)
#  month=dat$month[sel]
    
## covariate data from dat2
#  sel2=which(dat2$id==s)
#  chl=dat2$chl[sel2]
#  leafn=dat2$leafn[sel2]
#  sla=dat2$sla[sel2]
#  rep2 = dat2$rep[sel2]  # didn't work
#  rep2 = match(rep2,reps)
#  chlave=mean(chl)
#  leafnave=mean(leafn)
#  slaave=mean(sla)
  
 c4 <- openbugs(data=list(an=an, pi=pi, q=q,n=length(an)), 
#rep=rep,nrep=nrep, chl=chl,leafn=leafn, sla=sla, chlave=chlave,leafnave=leafnave,slaave=slaave,month=month    
 init,
 model.file="c4model.txt",
 n.chains=3,
 n.iter=1000,
 n.burnin=500,
 n.thin =25,
 parameters.to.save=c("r","vmax","alpha", "k", "prean", "pA")
# "beta1","beta2","beta5","Vmon","Vleaf","Kleaf","tau","tau.Vmon","tau.Vleaf","tau.Kleaf",
 )
  c4mcmc[[s]]=c4
}
## make trace and density plots
for(s in spp.list){
pdf(paste(s, " model trace.pdf",sep=""))
plot(as.mcmc.list(c4mcmc[[s]]))
dev.off()
}

## predictions vs measurements
par(mfrow=c(1,2))
for(s in spp.list){
sel1=which(dat$id==s)
an=dat$Photo[sel1]
plot(an,c4mcmc[[s]]$mean$prean, ylim=c(-5,40),xlim=c(-5,40), pch=19,
main="Predicted photosynthesis vs measured phtosynthesis", xlab="Measured An (umol m-2 s-1)", 
ylab="Predicted An (umol m-2 s-1)",cex.main=1.6,cex.lab=1.4)
abline(0,1, col="dark green",lwd=3)
}