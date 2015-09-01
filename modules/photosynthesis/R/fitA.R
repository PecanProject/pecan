##' @name fitA
##' @title fitA
##' @author Mike Dietze
##' @author Xiaohui Feng
##' @export
##' 
##' @param flux.data  data.frame of Licor data, concatenated by rows, and with a leading column "fname" that is used to count the number of curves and match to covariates
##' @param cov.data   data.frame of covariate data. Column names used in formulas
##' @param model      list including at least 6 components: the fixed effects model for alpha (a.fixed) and Vcmax (V.fixed), the random effects for these (a.random, V.random), the variable used to match the gas-exchange and covariate data (match), and the number of MCMC interations (n.iter). Additional optional arguments: TPU = TRUE turns on TPU limitation; Temp == "Bernacchi01" turns on the Bernacchi et al 2001 temperature correction. If this is turned on all parameters are estimated for 25C, otherwise no temperature correction is applied.
##' 
##' Right now the fixed effects are specified as a string using the standard R lm formula syntax, but without the LHS variable (e.g. "~ SLA + chl + SLA:chl"). The tilde is optional. For random effects, the two options right now are just "leaf" for leaf-level random effects and NULL. "model" has a default that sets all effects to NULL (fit one curve to all data) and n.iter=1000.
##' 
fitA <- function(flux.data,cov.data=NULL,model=NULL){

##  TO-DO: 
##  Random effects using design matrix
##  Model selection
##  output variable selection: Pred Loss, WAIC?
##  function to do: multiple response curves
##  specify priors in model object
##  integrate with meta-analysis
  
library(rjags)

if(is.null(model)) model = list(a.fixed=NULL,a.random=NULL,V.fixed=NULL,V.random=NULL,n.iter=5000,match="fname")
out.variables = c("r0","vmax0","alpha0","Jmax", "cp0","tau", "pmean", "pA")

a.fixed  = model$a.fixed
a.random = model$a.random
V.fixed  = model$V.fixed
V.random = model$V.random
if(is.null(model$match)) model$match = 'fname'

dat = as.data.frame(flux.data)

id = dat[,model$match]
n.curves = length(unique(id))
curve.id = as.numeric(as.factor(id))
curve.code = tapply(as.character(id),curve.id,unique)

##match between gas exchange data and covariates
if(!is.null(cov.data)){
  ord = match(curve.code,as.character(cov.data[,model$match]))
  cov.data = cov.data[ord,]
}

## Vcmax design matrix
if(is.null(V.fixed)){
  XV = NULL
} else {
  if(is.null(cov.data)) print("Vcmax formula provided but covariate data is absent:",V.fixed)
  if(length(grep("~",V.fixed)) == 0) V.fixed= paste("~",V.fixed)
  XV = with(cov.data,model.matrix(formula(V.fixed)))  
  XV = as.matrix(XV[,-which(colnames(XV)=="(Intercept)")])
  Vcenter = apply(XV,2,mean,na.rm=TRUE)
  XV = t(t(XV)-Vcenter)
}

## alpha design matrix
if(is.null(a.fixed)){
  Xa = NULL
} else {
  if(is.null(cov.data)) print("alpha formula provided but covariate data is absent:",a.fixed)
  a.fixed = ifelse(length(grep("~",a.fixed)) == 0,paste("~",a.fixed),a.fixed)
  Xa = with(cov.data,model.matrix(formula(a.fixed))) 
  Xa = as.matrix(Xa[,-which(colnames(Xa)=="(Intercept)")])
  acenter = apply(Xa,2,mean,na.rm=TRUE)
  Xa = t(t(Xa)-acenter)
}


## Define JAGS model

my.model = "  
model{

## Priors
  Jmax ~ dlnorm(4.7,2.7)             ## maximum electron transport rate prior
  alpha0~dnorm(0.25,100)             ##quantum yield  (mol electrons/mole photon) prior
  vmax0 ~dlnorm(4.6,2.7)             ## maximum rubisco capacity prior

  #Jmax ~ dweibull(2.0,260)          ## maximum electron transport rate prior Serbin 2012
  #alpha0 ~ dgamma(2.0,22.0)         ## quantum yield prior Serbin 2012
  #vmax0 ~ dweibull(1.7,80)          ## maximum rate of carboxylation prior Serbin 2012

  r0 ~ dlnorm(0.75,1.56)             ## leaf respiration prior
  #r ~ dweibull(2.0,6.0)             ## broad leaf respiration prior for trees
  cp0 ~ dlnorm(1.9,2.7)              ## CO2 compensation point prior
  tau ~ dgamma(0.1,0.1)
#TPU  tpu~ dlnorm(3,2.8)             ##tpu

## Constants: Bernacchi et al 2001, PC&E, Table 1
R <- 8.3144621 ## gas constant
r.c <- 18.72
r.H <- 46.39
Vc.c <- 26.35
Vc.H <- 65.33
Vo.c <- 22.98
Vo.H <- 60.11
cp.c <- 19.02
cp.H <- 37.83
cp.ref <- 42.75
Kc.c <- 38.05
Kc.H <- 79.43
Kc.ref <- 404.9
Ko.c <- 20.30
Ko.H <- 36.38
Ko.ref <- 278.4

## Vcmax BETAS

#RLEAF.V  tau.Vleaf~dgamma(0.1,0.1)          ## add random leaf effects
#RLEAF.V  for(i in 1:nrep){                  
#RLEAF.V   Vleaf[i]~dnorm(0,tau.Vleaf)
#RLEAF.V  }

## alpha BETAs

#RLEAF.A  tau.Aleaf~dgamma(0.1,0.1)
#RLEAF.A  for(i in 1:nrep){                  
#RLEAF.A   Aleaf[i]~dnorm(0,tau.Aleaf)
#RLEAF.A  }

  for(i in 1:n){

     r[i]  <- r0 ##B01* exp(r.c - r.H/R/T[i])
     cp[i] <- cp0 ##B01* exp(cp.c - cp.H/R/T[i])/cp.ref
     Kc.T[i] <- Kc ##B01* exp(Kc.c - Kc.H/R/T[i])/Kc.ref
     Ko.T[i] <- Ko ##B01* exp(Ko.c - Ko.H/R/T[i])/Ko.ref

     alpha[i] <- alpha0 #AFORMULA
     al[i]<-(alpha[i]*q[i]/(sqrt(1+(alpha[i]*alpha[i]*q[i]*q[i])/(Jmax*Jmax))))*(pi[i]-cp[i])/(4*pi[i]+8*cp[i])    ## electron transport limited without covariates

     vmax.refT[i] <- vmax0 #VFORMULA
     vmax[i] <- vmax.refT[i] ##B01* exp(Vc.c - Vc.H/R/T[i])
     ae[i]<- vmax[i]*(pi[i]-cp[i])/(pi[i]+Kc*(1+po/Ko))                                                    ## maximum rubisco limited without covariates

#TPU    ap[i]<-3*tpu                      ## phosphate limited

     pmean[i]<-min(al[i], ae[i]) - r[i]      ## predicted net photosynthesis
     an[i]~dnorm(pmean[i],tau)            ## likelihood
     pA[i] ~ dnorm(pmean[i],tau)          ## prediction
     }

     foo <- rep[1] + nrep + T[1]                ## prevent warnings
}
"



## prep data  
sel = 1:nrow(dat)#which(dat$spp == s)
if(!any(names(dat)=='Tleaf')){
  dat$Tleaf = rep(25+273.15,nrow(dat)) ## if leaf temperature is absent, assume 25C
}
mydat<-list(an=dat$Photo[sel], pi=dat$Ci[sel], q=dat$PARi[sel],T = dat$Tleaf,n=length(sel),
            Kc=46,Ko=22000,po=21000,rep=curve.id,nrep=n.curves)
#  Kc<-46                          ## Michaelis constant CO2 (Pa)
#  Ko<-33000                       ## Michaelis constant O2  (Pa)
#  po<-21000                       ## partial pressure of O2  (Pa)
#  k <- 0.21                       ## Vo/Vc

## TPU Limitation
if("TPU" %in% names(model)){
  if(model$TPU == TRUE){
    my.model = gsub(pattern="#TPU"," ",my.model)
    out.variables = c(out.variables,"tpu")  
  }
}

## Temperature scaling
Vformula = NULL
if("Temp" %in% names(model)){
  if(model$Temp == "Bernacchi01"){
    my.model = gsub(pattern="##B01"," ",my.model)
  }
}

## VCmax Formulas
Vformula = NULL
if("leaf" %in% V.random){
  Vformula = " + Vleaf[rep[i]]"
  my.model = gsub(pattern="#RLEAF.V"," ",my.model)
  out.variables = c(out.variables,"tau.Vleaf")  
}

if(!is.null(XV)){
  Vnames = gsub(" ","_",colnames(XV))
  Vformula = paste(Vformula,paste0("+ betaV",Vnames,"*XV[rep[i],",1:ncol(XV),"]",collapse=" "))
  Vpriors = paste0("     betaV",Vnames,"~dnorm(0,0.001)",collapse="\n")
  my.model = sub(pattern="## Vcmax BETAs",Vpriors,my.model)  
  mydat[["XV"]] = XV
  out.variables = c(out.variables,paste0("betaV",Vnames))  
}
if(!is.null(Vformula)) my.model = sub(pattern="#VFORMULA",Vformula,my.model)

## alpha Formulas
Aformula = NULL
if("leaf" %in% a.random){
  Aformula = " + Aleaf[rep[i]]"
  my.model = gsub(pattern="#RLEAF.A","",my.model)
  out.variables = c(out.variables,"tau.Aleaf")  
}

if(!is.null(Xa)){
  Anames = gsub(" ","_",colnames(Xa))
  Aformula = paste(Aformula,paste0("+ betaA",Anames,"*Xa[rep[i],",1:ncol(Xa),"]",collapse=" "))
  apriors = paste0("betaA",Anames,"~dnorm(0,0.001)",collapse="\n")
  my.model = sub(pattern="## alpha BETAs",apriors,my.model)  
  mydat[["Xa"]] = Xa
  out.variables = c(out.variables,paste0("betaA",Anames))  
}
if(!is.null(Aformula)) my.model = sub(pattern="#AFORMULA",Aformula,my.model)

## Define initial conditions
init<-list()
 init[[1]]<-list(r0=1.2, vmax0=39,alpha0=0.25, tau=10, cp0=6, Jmax=80) ## tau.Vleaf=30,beta1=4, beta2=1,beta5=3,tau.Vmon=10,tpu=10,
 init[[2]]<-list(r0=1, vmax0=100, alpha0=0.20, tau=20, cp0=4, Jmax=150) ##tau.Vleaf=20,beta1=1,beta2=1,beta5=-1,tau.Vmon=20,tpu=13,
 init[[3]]<-list(r0=2, vmax0=60, alpha0=0.28, tau=20, cp0=5,Jmax=60)    ##tau.Vleaf=100,beta1=1,beta2=2,beta5=2,tau.Vmon=3,tpu=20,

mc3 <- jags.model(file=textConnection(my.model),data=mydat,
 inits=init,
 n.chains=3)

mc3.out <- coda.samples(model=mc3, variable.names=out.variables, n.iter=model$n.iter)                              

## split output
out = list(params=NULL,predict=NULL,model=my.model)
mfit = as.matrix(mc3.out,chains=TRUE)
pred.cols = union(grep("pA",colnames(mfit)),grep("pmean",colnames(mfit)))
chain.col = which(colnames(mfit)=="CHAIN")
out$predict = mat2mcmc.list(mfit[,c(chain.col,pred.cols)])
out$params   = mat2mcmc.list(mfit[,-pred.cols])
return(out)

}  ## end photosynthesis fitting code

##' @name read.Licor
##' @title read.Licor
##' 
##' @author Mike Dietze
##' @export
##' 
##' @param filename  name of the file to read
##' @param sep       file delimiter. defaults to tab
##' @param ...       optional arguements forwarded to read.table
read.Licor <- function(filename,sep="\t",...){
  fbase = sub(".txt","",tail(unlist(strsplit(filename,"/")),n=1))
  print(fbase)
  full = readLines(filename)
  ## remove meta=data
  start = grep(pattern="OPEN",full)
  skip = grep(pattern="STARTOFDATA",full)  
  for(i in length(start):1){
    full = full[-(start[i]:(skip[i]+1*(i>1)))] # +1 is to deal with second header
  }
  full = full[grep("\t",full)]  ## skip timestamp lines
  dat = read.table(textConnection(full),header = TRUE,blank.lines.skip=TRUE,sep=sep,...)
  fname=rep(fbase,nrow(dat))
  dat = as.data.frame(cbind(fname,dat))
  return(dat)
}

mat2mcmc.list <- function(w){
  temp <- list()
  chain.col = which(colnames(w)=="CHAIN")
  for(i in unique(w[,"CHAIN"])){
    temp[[i]] <- as.mcmc(w[w[,"CHAIN"]==i,-chain.col])
  }
  return(as.mcmc.list(temp))
}