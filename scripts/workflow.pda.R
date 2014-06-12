##
.libPaths("~/lib/R")
library(coda)
library(PEcAn.all)

settings <- read.settings("~/inputs/sylvania.SIPNET.xml")

## load L3 data
data <- read.csv(settings$assim.batch$input)
NEEo <- data$Fc   #umolCO2 m-2 s-1
NEEq <- data$qf_Fc
NEEo[NEEq > 0] <- NA

## load data L4
w = 48*4
dataL4 <- read.csv("~/flux/USSyv2002_L4_h.txt")
NEEfil <- dataL4$NEE_st_fMDS; NEEfil[NEEfil < -1000] <- NA
pdf("SylvaniaFlux.pdf",width=11,height=8.5)
  par(mfrow=c(1,2))
  plot(dataL4$DoY,NEEfil,xlab="Day of year",ylab="umol/m2/s",type='l',lwd=1.5)
  lines(dataL4$DoY,filter(NEEfil,rep(1/w,w)),lwd=1.5,col=2)
  plot(sort(unique(dataL4$Hour)),tapply(NEEfil,dataL4$Hour,mean,na.rm=TRUE),type='l',xlab="hour",ylab="umol/m2/s",lwd=2)
dev.off()
NEEhr <- tapply(NEEfil,rep(1:(length(NEEfil)/2),each=2),mean,na.rm=TRUE)  ##use gapfilled for plotting
NEEhr[is.nan(NEEhr)] <- NA

## pre-assimilation ensemble analysis
ens <- read.ensemble.ts("SIPNET")
ens$NEE <- ens$NEE*1000/12*1e6/10000/86400/365  #convert kgC/ha/yr -> umol/m2/s
ensemble.ts(ens,observations=-NEEhr,window=24*3)  ## make ensemble plots
  
## Define vars
vars = NA
jvar = rep(0.5,22)
params = NULL

## MCMC
settings$assim.batch$iter=1500

params <- pda.mcmc("SIPNET",vars=vars,jvar=jvar,params=params)

## Assess MCMC output
burnin = 1
dm <- as.mcmc(params[burnin:nrow(params),vars])
plot(dm)
summary(dm)
crosscorr(dm)
pairs(params[,vars])

a = 1-rejectionRate(dm)
a = 1-rejectionRate(as.mcmc(params[nrow(params)-49:0,vars]))

## update jump variance
jvar[vars] = jvar[vars]*(a/0.4)

save.image(paste(settings$outdir,"/pda.mcmc.Rdata",sep=""))

############  POST MCMC ##################

###*** CHANGE OUTDIRS BEFORE GOING TO THE NEXT STEPS ***
### Also, revert met file back to 2002-2006

## coerce parameter output into the same format as trait.mcmc
pname <- rownames(post.distns)
for(i in vars){
  beta.o <- array(params[,i],c(nrow(params),1))
  colnames(beta.o) = "beta.o"
  if(pname[i] %in% names(trait.mcmc)){
    trait.mcmc[[pname[i]]] <- mcmc.list(as.mcmc(beta.o))
  } else {
    k = length(trait.mcmc)+1
    trait.mcmc[[k]] <- mcmc.list(as.mcmc(beta.o))
    names(trait.mcmc)[k] <- pname[i]      
  }
}
## save updated parameter distributions as trait.mcmc and prior.distns
## so that they can be read by the ensemble code
save(trait.mcmc,file=paste(settings$pfts$pft$outdir, 'trait.mcmc.Rdata', sep=''))
prior.distns <- approx.posterior(trait.mcmc,post.distns,outdir=settings$pfts$pft$outdir)
save(prior.distns, file = paste(settings$pfts$pft$outdir, 'prior.distns.Rdata', sep = ''))


## Re-run updated ensemble analysis, sensitivity analysis,  and variance decomp
run.write.configs(settings, settings$bety$write)        # Calls model specific write.configs e.g. write.config.ed.R
start.model.runs(settings, settings$bety$write)         # Start ecosystem model runs
get.results(settings)           # Get results of model runs
run.sensitivity.analysis()      # Run sensitivity analysis and variance decomposition on model output

## plot
ens.post <- read.ensemble.ts("SIPNET")
ens.post$NEE <- ens$NEE*1000/12*1e6/10000/86400/365  #convert kgC/ha/yr -> umol/m2/s
ensemble.ts(ens.post,observations=-NEEhr,window=24*3)
