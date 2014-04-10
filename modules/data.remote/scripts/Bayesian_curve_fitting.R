
bayes.curve.fit<-function(outpath,coord.set,fia,n.reps,n.chain){
  
library(rjags)
require(R2HTML)

dat48<-read.csv(file=paste(outpath,"/",coord.set[fia+1],"_dat48.csv",sep=""),header=T,sep=",")
dir.create(file.path(outpath,"model_output"))
outpath<-file.path(outpath,"model_output")
# outpath <- file.path("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/output/data") ##For saving

x<-dat48$biomass
yvars<- c("dat48$HH.sigma.48", "dat48$HV.sigma.48")

# n.reps<- 500 #sets value for n.adapt and n.iter
# n.chain<-3 #number of MCMC chains to run

################
##Define Models
################

#Michaelis-Menton
MM = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-(b0*x[i])/(b1 + x[i])
#    x[i]~dnorm(xt[i],tau.x)
}#for

b0~dnorm(0,1)
b1~dunif(0.001,10)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"


#Holling Type 4
H4 = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-(a*x[i]^2)/(b+(c*x[i])+x[i]^2)
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dnorm(0,1)
b~dunif(0.001,100)
c~dunif(0.001,100)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"

#Holling Type 3
H3 = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-(a*x[i]^2)/(b^2+x[i]^2)
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dnorm(0,1)
b~dunif(0.001,100)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"

#Ricker
Ri = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<- a*x[i]*exp(-b*x[i])
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dunif(-10,10)
b~dunif(-10,10)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"

#Logistic
Log = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-a/(1+exp(a-b*x[i]))
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dunif(0,1)
b~dnorm(0,1)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"


###########################
#Same as above, but with Y intercepts added
###########################

#Michaelis-Menton with Y-int
MM.yint = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-((b0*x[i])/(b1 + x[i]))+yint
#    x[i]~dnorm(xt[i],tau.x)
}#for

b0~dnorm(0,1)
b1~dunif(0.001,10)
yint~dunif(0,1)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"


#Holling Type 4 with Y-int
H4.yint = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-((a*x[i]^2)/(b+(c*x[i])+x[i]^2))+yint
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dnorm(0,1)
b~dunif(0,100)
c~dnorm(0,1)
yint~dunif(0,1)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"

#Holling Type 3 with Y-int
H3.yint = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-((a*x[i]^2)/(b^2+x[i]^2))+yint
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dnorm(0,1)
b~dunif(0.001,100)
yint~dunif(0,1)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"

#Ricker with Y-int
Ri.yint = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-(a*x[i]*exp(-b*x[i]))+yint
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dunif(-10,10)
b~dunif(-10,10)
yint~dunif(0,1)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"

#Logistic with Y-int
Log.yint = "model{
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]<-(a/(1+exp(a-b*x[i])))+yint
#    x[i]~dnorm(xt[i],tau.x)
}#for

a~dunif(0,1)
b~dnorm(0,1)
yint~dunif(0,1)
tau~dgamma(3,2)
sd<-1/sqrt(tau)
#  tau.x<-dgamma(1,1)
}#model"


################
##Build lists for looping
################

mod.names<-c("MM","H3","H4","Ri","Log","MM.yint","H3.yint","H4.yint","Ri.yint","Log.yint")

#Initial conditions for HH pol band
MM.HH.init      = list(b1=4, b0=0.1,     tau = 2/var(y)) #for MM
H4.HH.init      = list(a=0.1,b=50,  c=2, tau = 2/var(y)) #for H4
H3.HH.init      = list(a=0.1,b=100,      tau = 2/var(y)) #for H3
Ri.HH.init      = list(a=0.5,b=0.5,      tau = 2/var(y)) #for Ri
Log.HH.init     = list(a=0.6,b=1,        tau = 2/var(y)) #for Log
MM.yint.HH.init = list(b1=4, b0=0.1,     yint=0.1, tau = 2/var(y)) #for MM.yint
H3.yint.HH.init = list(a=0.1,b=100,      yint=0.1, tau = 2/var(y)) #for H3.yint
H4.yint.HH.init = list(a=0.1,b=50,  c=2, yint=0.1, tau = 2/var(y)) #for H4.yint
Ri.yint.HH.init = list(a=0.5,  b=0.5,   yint=0.1, tau = 2/var(y)) #for Ri.yint
Log.yint.HH.init= list(a=0.6,b=1,        yint=0.1, tau = 2/var(y)) #for Log.yint

#Initial conditions for HV pol band
MM.HV.init      = list(b1=8,   b0=0.04,     tau = 2/var(y)) #for MM
H3.HV.init      = list(a=0.02, b=100,       tau = 2/var(y)) #for H3
H4.HV.init      = list(a=0.03, b=0.01, c=2, tau = 2/var(y)) #for H4
Ri.HV.init      = list(a=0,    b=0,         tau = 2/var(y)) #for Ri
Log.HV.init     = list(a=0.6,  b=1,         tau = 2/var(y)) #for Log
MM.yint.HV.init = list(b1=8,   b0=0.04,      yint=0.04,tau = 2/var(y)) #for MM.yint
H3.yint.HV.init = list(a=0.02, b=100,        yint=0.04, tau = 2/var(y)) #for H3.yint
H4.yint.HV.init = list(a=0.03, b=0.01, c=2,  yint=0.04, tau = 2/var(y)) #for H4.yint
Ri.yint.HV.init = list(a=0.5,  b=0.5,        yint=0.04, tau = 2/var(y)) #for Ri.yint
Log.yint.HV.init= list(a=0.6,  b=1,          yint=0.04, tau = 2/var(y)) #for Log.yint

MM.var.names<-c("b0","b1","sd") #for MM
H3.var.names<-c("a","b","sd") #for H3
H4.var.names<-c("a","b","c","sd") #for H4
Ri.var.names <-c("a","b","sd") #for Ri
Log.var.names<-c("a","b","sd") #for Log
MM.yint.var.names<-c("b0","b1","yint","sd") #for MM.yint
H3.yint.var.names<-c("a","b","yint","sd") #for H3.yint
H4.yint.var.names<-c("a","b","c","yint","sd") #for H4.yint
Ri.yint.var.names <-c("a","b","yint","sd") #for Ri.yint
Log.yint.var.names<-c("a","b","yint","sd") #for Log.yint

MM.lines<-"lines(xseq,(parm[1]*xseq)/(parm[2]+xseq),col=2,lwd=3)"  #For MM
H3.lines<-"lines(xseq,(parm[1]*xseq^2)/(parm[2]^2+xseq^2),col=2,lwd=3)" #For H3
H4.lines<-"lines(xseq,(parm[1]*xseq^2)/(parm[2]+parm[3]*xseq+xseq^2),col=2,lwd=3)"  #For H4
Ri.lines<-"lines(xseq,(parm[1]*xseq*exp(-parm[2]*xseq)),col=2,lwd=3)"  #For Ri  a*x[i]*exp(-b*x[i])
Log.lines<-"lines(xseq,parm[1]/(1+exp(parm[1]-parm[2]*xseq)),col=2,lwd=3)"  #For Log exp(a+b*x[i])/(1+exp(a+b*x[i]))
MM.yint.lines<-"lines(xseq,((parm[1]*xseq)/(parm[2]+xseq))+parm[4],col=2,lwd=3)"  #For MM.yint
H3.yint.lines<-"lines(xseq,((parm[1]*xseq^2)/(parm[2]^2+xseq^2))+parm[4],col=2,lwd=3)" #For H3.yint
H4.yint.lines<-"lines(xseq,((parm[1]*xseq^2)/(parm[2]+parm[3]*xseq+xseq^2))+parm[5],col=2,lwd=3)"  #For H4.yint
Ri.yint.lines<-"lines(xseq,(parm[1]*xseq*exp(-parm[2]*xseq))+parm[4],col=2,lwd=3)"  #For Ri.yint
Log.yint.lines<-"lines(xseq,(parm[1]/(1+exp(parm[1]-parm[2]*xseq)))+parm[4],col=2,lwd=3)"  #For Log.yint

MM.mod.eqn<-"(out[k,1]*xseq)/(out[k,2]+xseq)"
H3.mod.eqn<-"(out[k,1]*xseq^2)/(out[k,2]^2+xseq^2)"
H4.mod.eqn<-"(out[k,1]*xseq^2)/(out[k,2]+out[k,3]*xseq+xseq^2)"
Ri.mod.eqn<-"out[k,1]*xseq*exp(-out[k,2]*xseq)"                               
Log.mod.eqn<-"out[k,1]/(1+exp(out[k,1]-out[k,2]*xseq))"    
MM.yint.mod.eqn<-"((out[k,1]*xseq)/(out[k,2]+xseq))+out[k,4]"
H3.yint.mod.eqn<-"((out[k,1]*xseq^2)/(out[k,2]^2+xseq^2))+out[k,4]"
H4.yint.mod.eqn<-"((out[k,1]*xseq^2)/(out[k,2]+out[k,3]*xseq+xseq^2))+out[k,5]"
Ri.yint.mod.eqn<-"(out[k,1]*xseq*exp(-out[k,2]*xseq))+out[k,4]"
Log.yint.mod.eqn<-"(out[k,1]/(1+exp(out[k,1]-out[k,2]*xseq)))+out[k,4]"

################
##Compiled model inputs
################
#MODELS MUST BE IN THE SAME ORDER FOR EACH OF THE FOLLOWING:
models<-c(MM,
          H3,
          H4,
          Ri,
          Log,
          MM.yint,
          H3.yint,
          H4.yint,
          Ri.yint,
          Log.yint)
HH.init <-list(MM.HH.init,
               H3.HH.init,
               H4.HH.init,
               Ri.HH.init,
               Log.HH.init,
               MM.yint.HH.init,
               H3.yint.HH.init,
               H4.yint.HH.init,
               Ri.yint.HH.init,
               Log.yint.HH.init)

HV.init <-list(MM.HV.init,
               H3.HV.init,
               H4.HV.init,
               Ri.HV.init,
               Log.HV.init,
               MM.yint.HV.init,
               H3.yint.HV.init,
               H4.yint.HV.init,
               Ri.yint.HV.init,
               Log.yint.HV.init) 

init<-c("HH.init","HV.init")

var.names<-list(MM.var.names,
                H3.var.names,
                H4.var.names,
                Ri.var.names,
                Log.var.names,
                MM.yint.var.names,
                H3.yint.var.names,
                H4.yint.var.names,
                Ri.yint.var.names,
                Log.yint.var.names)
model.fits<-list(MM.lines,
                 H3.lines,
                 H4.lines,
                 Ri.lines,
                 Log.lines,
                 MM.yint.lines,
                 H3.yint.lines,
                 H4.yint.lines,
                 Ri.yint.lines,
                 Log.yint.lines)
mod.eqns <-list(MM.mod.eqn,
                H3.mod.eqn,
                H4.mod.eqn,
                Ri.mod.eqn,
                Log.mod.eqn,
                MM.yint.mod.eqn,
                H3.yint.mod.eqn,
                H4.yint.mod.eqn,
                Ri.yint.mod.eqn,
                Log.yint.mod.eqn)

################
##Loop over all models and backscatter polarization bands
################
for(i in 1:length(yvars)){ #loop over HH and HV (pol bands)
  y<-eval(parse(text=yvars[i]))
  
  if(length(y[is.na(y)]>0)){ #to get rid of NAs
    drop = which(is.na(y))
    data = list(x=x[-drop],y=y[-drop],n=length(x)-length(drop)) 
  } else{
    data = list(x=x,y=y,n=length(x)) 
  }
  
  for(j in 1:length(models)){#looping over models
    j1 = jags.model(file=textConnection(models[j]),
                    data = data,
                    inits = unlist(eval(parse(text=init[i]))[j],recursive=FALSE),
                    n.chains=n.chain,
                    n.adapt=min(0.1*n.reps,1000))
    
    jags.out = coda.samples(model=j1,
                            variable.names<-var.names[j][[1]],
                            n.iter = n.reps) 
    out <- as.matrix(jags.out)
    
    gelman.diag(jags.out)
    summary(jags.out)
    
    
    #Generate pdf of curve fits
    pdf(paste(paste(outpath,"/","curve_fit_",coord.set[fia+1],sep=""),substr(yvars[i],7,8),mod.names[j],".pdf",sep="_"),width = 6, height = 6, paper='special')
    
    par(mar = rep(2, 4))    
    plot(jags.out)
    pairs(out)
    
    #autocorr.plot(jags.out)
    gelman.plot(jags.out)
    
    #plot data
    par(mfrow=c(1,1))
    parm = apply(out,2,mean)
    scatter.smooth(x,y,pch=".",xlab="Biomass",ylab=yvars[i],main=paste(mod.names[j],"fit of",yvars[i],sep=" ")) #plot data
    xseq = seq(0,300,length=3000)
    eval(parse(text=model.fits[j])) #plot fitted curve line
    
    npred = 10
    ypred = matrix(NA,npred,length(xseq))
    samp = sample(1:nrow(out),npred)
    for(p in 1:npred){
      k = samp[p]
      ypred[p,] = eval(parse(text=mod.eqns[j]))
    }
    #Add confidence interval
    yci = apply(ypred,2,quantile,c(0.025,0.5,0.975))
    lines(xseq,yci[1,],col=3)
    lines(xseq,yci[3,],col=3)
    
    dev.off()

##Save model output as HTML
    target <- HTMLInitFile(outdir=outpath,
                           filename=paste("model_output",coord.set[fia+1],substr(yvars[i],7,8),mod.names[j],sep="_"))

    HTML(list(paste("n.reps=",n.reps,sep=" "),
              paste("n.chain=",n.chain,sep=" "),
              cbind(var.names[j][[1]],unlist(eval(parse(text=init[i]))[j])),
              summary(jags.out)),
         file=target)
    
    print(yvars[i])
    print(mod.names[j])
  }#looping over models
}#looping over HH & HV
}#function