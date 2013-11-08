library(R2WinBUGS)
library(BRugs)
dat=read.csv("c3photosynthesis.csv",header=T)
#dat2=read.csv('c3covariates.csv',header=T)

my.model  = function(){
  Jmax ~ dlnorm(4.7,2.7)             ## maximum electron transport rate prior
  alpha~dnorm(0.25,100)              ##quantum yield  (mol electrons/mole photon) prior
  vmax ~dlnorm(4.6,2.7)              ## maximum rubisco capacity prior
  r ~ dlnorm(0.75,1.56)              ## leaf respiration prior
  cp ~ dlnorm(1.9,2.7)               ## CO2 compensation point prior
  tau ~ dgamma(0.1,0.1)
#  tpu~ dlnorm(3,2.8)                 ##tpu

#  tau.Vleaf~dgamma(0.1,0.1)          ## add random leaf effects
#  tau.Aleaf~dgamma(0.1,0.1)
#  for(i in 1:nrep){                  
#   Vleaf[i]~dnorm(0,tau.Vleaf)
#   Aleaf[i]~dnorm(0,tau.Aleaf)
#  }
#  beta1 ~ dnorm(0,0.1)              ## chlorophyll effects
#  beta2 ~ dnorm(0,0.1)              ## SLA effects
#  beta5 ~ dnorm(0,0.1)              ## Vcmax effects

#  tau.Vmon~dgamma(0.01,0.01)        ## add month effects on Vcmax, fixed effects
#   Vmon[7]<-0                       ## reference month
#   Vmon[8]~ dnorm(0,tau.Vmon)
#   Vmon[5]~ dnorm(0,tau.Vmon)
#   Vmon[6]~ dnorm(0,tau.Vmon)
#   Vmon[9]~ dnorm(0,tau.Vmon)
#   Vmon[10]~ dnorm(0,tau.Vmon)
#  Kc<-46                          ## Michaelis constant CO2 (Pa)
#  Ko<-33000                       ## Michaelis constant O2  (Pa)
#  po<-21000                       ## partial pressure of O2  (Pa)
#  k <- 0.21                       ## Vo/Vc
  for(i in 1:n){
#     al[i]<-((alpha+beta1*(chl[rep[i]]-chlave)+beta2*(sla[rep[i]]-slaave)
#     +Aleaf[rep[i]])*q[i]/(sqrt(1+((alpha+Aleaf[rep[i]])*(alpha+
#     Aleaf[rep[i]])*q[i]*q[i])/(Jmax*Jmax))))*(pi[i]-cp)/(4*pi[i]+8*cp)                            ## electron transport limited with chlorophyll, SLA and random leaf effects turned on

     al[i]<-(alpha*q[i]/(sqrt(1+(alpha*alpha*q[i]*q[i])/(Jmax*Jmax))))*(pi[i]-cp)/(4*pi[i]+8*cp)    ## electron transport limited without covariates

#     ae[i]<-(vmax+Vmon[month[i]]+beta5*(leafn[rep[i]]-leafnave)
#     +Vleaf[rep[i]])*((pi[i]-cp)/(pi[i]+Kc*(1+po/Ko)))                                              ## maximum rubisco limited with leaf N, month and random leaf effects turned on        

     ae[i]<- vmax*(pi[i]-cp)/(pi[i]+Kc*(1+po/Ko))                                                    ## maximum rubisco limited without covariates

#    ap[i]<-3*tpu                         ## phosphate limited

     prean[i]<-min(al[i], ae[i]) - r      ## predicted net photosynthesis
     an[i]~dnorm(prean[i],tau)            ## likelihood
     pA[i] ~ dnorm(prean[i],tau)          ## prediction
     }
}
 write.model(my.model,"c3model.txt")

init<-list()
 init[[1]]<-list(r=1.2, vmax=39,alpha=0.25, tau=10, cp=6, Jmax=80) ## tau.Vleaf=30,beta1=4, beta2=1,beta5=3,tau.Vmon=10,tpu=10,
 init[[2]]<-list(r=1, vmax=100, alpha=0.20, tau=20, cp=4, Jmax=150) ##tau.Vleaf=20,beta1=1,beta2=1,beta5=-1,tau.Vmon=20,tpu=13,
 init[[3]]<-list(r=2, vmax=60, alpha=0.28, tau=20, cp=5,Jmax=60)    ##tau.Vleaf=100,beta1=1,beta2=2,beta5=2,tau.Vmon=3,tpu=20,

 spp.list = unique(as.character(dat$id))  

 c3mcmc <- list()                                       

 for(s in spp.list){
  sel = which(dat$id == s)
  an=dat$Photo[sel]
  pi=dat$Ci_Pa[sel]
  q=dat$PARi[sel]
#  rep=dat$rep[sel]        ## turn on random leaf effects
#  reps = unique(rep)
#  rep = match(rep,reps)
#  nrep = length(reps)
#  month=dat$month[sel]    ## month information

## covariates data from dat2
#  sel2=which(dat2$id==s)
#  chl=dat2$chl[sel2]
#  leafn=dat2$leafn[sel2]
#  sla=dat2$sla[sel2]
#  rep2 = dat2$rep[sel2]
#  rep2 = match(rep2,reps)
#  rep3track[[s]]=rep2
#  chlave=mean(chl)
#  leafnave=mean(leafn)
#  slaave=mean(sla)

## temperature compoent   (Bernacchi et al. 2001,2002,2003)
#parameter=as.character(c("Vmax","Jmax","Kc","Ko","cp","Rd"))
#c=c(26.36,17.71,35.98,12.38,11.19,18.71)
#Ha=c(65.33,43.9,80.99,23.72,24.46,46.39)
#R=8.314
#value25=c(1,1,460,220,37,1)         ## when include temperature component, the unit of Kc (umol mol-1) Ko (mmol mol-1) and cp (umol mol-1) in this equation need to be converted into Pascals
#for(i in 1:6){
#y=value25[i]*exp(c[i]-(1000*Ha[i]/(R*(tleaf+273.15))))
#}


mydat<-list(an=an, pi=pi, q=q,n=length(an),Kc=46,Ko=22000,po=21000)  
## rep=rep,nrep=nrep,chl=chl,leafn=leafn, sla=sla, chlave=chlave,leafnave=leafnave,slaave=slaave,month=month
 
 mc3 <- openbugs(mydat,
 init,
 model.file="c3model.txt",
 n.chains=3,
 n.iter=1000,
 n.burnin=500,
 n.thin =25,
 parameters.to.save=c("r","vmax","alpha","Jmax", "cp","tau", "prean", "pA") 
##"beta1","beta2","beta5","Vmon","Vleaf","Aleaf","tau.Vmon","tau.Vleaf","tau.Aleaf",
 )                              
 c3mcmc[[s]]=mc3
}

## make trace and density plots
for(s in spp.list){
pdf(paste(s, " model trace.pdf",sep=""))
plot(as.mcmc.list(c3mcmc[[s]]))
dev.off()
}

## predictions vs measurements
sel1=which(dat$id==s)
an=dat$Photo[sel1]
plot(an,c3mcmc[[s]]$mean$prean, ylim=c(-5,40),xlim=c(-5,40), pch=19,
main="Predicted photosynthesis vs measured phtosynthesis", xlab="Measured An (umol m-2 s-1)", 
ylab="Predicted An (umol m-2 s-1)",cex.main=1.6,cex.lab=1.4)
abline(0,1, col="dark green",lwd=3)


