library(PECAn, lib.loc = '~/lib/R')
source('R/pecan.samps.R')
library(xtable)
pft <- 'ebifarm.c4crop'
ITER  <- 50000
M     <- 500
outdir   <- system("echo $PECANOUT", intern = TRUE)
outfile1 <- paste(outdir, '/pecan.parms.Rdata', sep = '')
save.image(outfile1)
set.seed(1)

spp <- query.bety.pft_species(pft)
spstr <- spp$spstr 

trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA_gC_per_m2','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','water_conductance','Vm0','r_fract','storage_turnover_rate', 'T'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors

priors <- query.bety.priors(pft, trstr)
print(priors)

traits <- trvec <- rownames(priors) # vector of traits with prior distributions for pft 
trait.defs <- trait.dictionary(trvec)

save(trait.defs, file = paste(outdir, '/trait.defs.Rdata', sep=''))
## now it is time to query the data
trait.data <- query.bety.traits(spstr,trvec)

## returns list 'trait.data' with one dataframe per variable 
 
## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, priors, j.iter = ITER)

pecan.ma.summary(trait.mcmc, pft)
outfile2 <- paste(outdir, '/pecan.MA.Rdata', sep = '')
save.image(outfile2)
priors$distn[priors$distn=='weib'] <- 'weibull'
#trait.samps <- pecan.samps(trait.mcmc, priors)
#priors <- prior.dists <- trait.samps[['priors']]
#post.samps <- trait.samps[['post.samps']]     
#prior.samps <- trait.samps[['prior.samps']] #matrix of samples from all priors

const <- pecan.config.constants(pft)
PFT <- const$PFT
CONFIG <- const$CONFIG
traits <- tr <- colnames(post.samps)
filenames <- list()
outdir <- 'out'

trait.beta.o <- list()
for(i in names(trait.mcmc)){
  trait.beta.o[[i]] <-   as.matrix(trait.mcmc[[i]][,'beta.o'])
}

i <- 1:10000
trait.mat <- data.frame(SLA=trait.beta.o$SLA[i],
                        leaf_width=trait.beta.o$leaf_width[i],
                        seedling_mortality=trait.beta.o$seedling_mortality[i],
                        Vm0=trait.beta.o$Vm0[i],
                        q=trait.beta.o$q[i],
                        root_turnover_rate = trait.beta.o$root_turnover_rate[i])

n.samp <- 10000
traits <- names(trait.mcmc)
priors$n <- nrow(trait.mat)
colnames(priors)[which(colnames(priors) %in% c('parama','paramb'))] <- c('a', 'b')

prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,]))
colnames(prior.samps) <- rownames(priors)

post.samps <- prior.samps
for (tri in colnames(trait.mat)) post.samps[1:n.samp,tri] <- trait.mat[1:n.samp, tri]
save(post.samps, prior.samps,file='out/pecan.samps.Rdata')

quantiles <- 1-pnorm(-3:3)
calculate.quantiles <- function(x,samps, quantiles) {
  quantile(samps[,x], quantiles)
}

traits<-colnames(post.samps)
qpost <- llply(traits, calculate.quantiles, post.samps, quantiles)
qprior <- llply(traits, calculate.quantiles, prior.samps, quantiles)
names(qpost) <- traits
names(qprior) <- traits
save(qpost, qprior, file="out/qpostqprior.Rdata")

##write mean config files
n.lcl45 <- 1
n.lcl30 <- 2
n.lcl15 <- 3
n.mean  <- 4
n.ucl15 <- 5
n.ucl30 <- 6
n.ucl45 <- 7

PFTm <- PFT
for (tri in traits) { 
  PFTm <- append.xmlNode(PFTm, xmlNode(tri, qprior[[tri]][n.mean]))
}
CONFIGm <- append.xmlNode(CONFIG, PFTm)
file <- paste(outdir, "/config.priormeans.xml", sep = '')
filenames[['priormeans']] <- file
saveXML(CONFIGm, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')

PFTm <- PFT
for (tri in traits) {
  PFTm <- append.xmlNode(PFTm, xmlNode(tri, qpost[[tri]][n.mean]))
}
CONFIGm <- append.xmlNode(CONFIG, PFTm)
file <- paste(outdir, "/config.postmeans.xml", sep = '')
filenames[['postmeans']] <- file
saveXML(CONFIGm, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')

##make post ucl/lcl config files
for ( tri in traits){
  noti <- traits[-which(traits==tri)]
  PFTl45 <- append.xmlNode(PFT, xmlNode(tri, qpost[[tri]][n.lcl45]))
  PFTl30 <- append.xmlNode(PFT, xmlNode(tri, qpost[[tri]][n.lcl30]))
  PFTl15 <- append.xmlNode(PFT, xmlNode(tri, qpost[[tri]][n.lcl15]))
  PFTu15 <- append.xmlNode(PFT, xmlNode(tri, qpost[[tri]][n.ucl15]))
  PFTu30 <- append.xmlNode(PFT, xmlNode(tri, qpost[[tri]][n.ucl30]))
  PFTu45 <- append.xmlNode(PFT, xmlNode(tri, qpost[[tri]][n.ucl45]))
  for (trk in noti) {
    pmean <- qpost[[trk]][n.mean]
    PFTl45 <- append.xmlNode(PFTl45, xmlNode(trk, pmean))
    PFTl30 <- append.xmlNode(PFTl30, xmlNode(trk, pmean))
    PFTl15 <- append.xmlNode(PFTl15, xmlNode(trk, pmean))
    PFTu15 <- append.xmlNode(PFTu15, xmlNode(trk, pmean))
    PFTu30 <- append.xmlNode(PFTu30, xmlNode(trk, pmean))
    PFTu45 <- append.xmlNode(PFTu45, xmlNode(trk, pmean))
  }
  CONFIGl45 <- append.xmlNode(CONFIG, PFTl45)
  CONFIGl30 <- append.xmlNode(CONFIG, PFTl30)
  CONFIGl15 <- append.xmlNode(CONFIG, PFTl15)
  CONFIGu15 <- append.xmlNode(CONFIG, PFTu15)
  CONFIGu30 <- append.xmlNode(CONFIG, PFTu30)
  CONFIGu45 <- append.xmlNode(CONFIG, PFTu45)

  filel45 <- paste(outdir, "/config.postlcl45.", tri,".xml", sep="")
  filel30 <- paste(outdir, "/config.postlcl30.", tri,".xml", sep="")
  filel15 <- paste(outdir, "/config.postlcl15.", tri,".xml", sep="")
  fileu15 <- paste(outdir, "/config.postucl15.", tri,".xml", sep="")
  fileu30 <- paste(outdir, "/config.postucl30.", tri,".xml", sep="")
  fileu45 <- paste(outdir, "/config.postucl45.", tri,".xml", sep="")
  filenames[['postSA']] <- c(filel45, filel30, filel15, fileu15, fileu30, fileu45)
  saveXML(CONFIGl45, file = filel45, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGl30, file = filel30, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGl15, file = filel15, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGu15, file = fileu15, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGu30, file = fileu30, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGu45, file = fileu45, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
}

##make prior ucl/lcl config files
for ( tri in traits){
  noti <- traits[-which(traits==tri)]
  PFTl45 <- append.xmlNode(PFT, xmlNode(tri, qprior[[tri]][n.lcl45]))
  PFTl30 <- append.xmlNode(PFT, xmlNode(tri, qprior[[tri]][n.lcl30]))
  PFTl15 <- append.xmlNode(PFT, xmlNode(tri, qprior[[tri]][n.lcl15]))
  PFTu15 <- append.xmlNode(PFT, xmlNode(tri, qprior[[tri]][n.ucl15]))
  PFTu30 <- append.xmlNode(PFT, xmlNode(tri, qprior[[tri]][n.ucl30]))
  PFTu45 <- append.xmlNode(PFT, xmlNode(tri, qprior[[tri]][n.ucl45]))
  for (trk in noti) {
    pmean <- qprior[[trk]][n.mean]
    PFTl45 <- append.xmlNode(PFTl45, xmlNode(trk, pmean))
    PFTl30 <- append.xmlNode(PFTl30, xmlNode(trk, pmean))
    PFTl15 <- append.xmlNode(PFTl15, xmlNode(trk, pmean))
    PFTu15 <- append.xmlNode(PFTu15, xmlNode(trk, pmean))
    PFTu30 <- append.xmlNode(PFTu30, xmlNode(trk, pmean))
    PFTu45 <- append.xmlNode(PFTu45, xmlNode(trk, pmean))
  }
  CONFIGl45 <- append.xmlNode(CONFIG, PFTl45)
  CONFIGl30 <- append.xmlNode(CONFIG, PFTl30)
  CONFIGl15 <- append.xmlNode(CONFIG, PFTl15)
  CONFIGu15 <- append.xmlNode(CONFIG, PFTu15)
  CONFIGu30 <- append.xmlNode(CONFIG, PFTu30)
  CONFIGu45 <- append.xmlNode(CONFIG, PFTu45)

  filel45 <- paste(outdir, "/config.priorlcl45.", tri,".xml", sep="")
  filel30 <- paste(outdir, "/config.priorlcl30.", tri,".xml", sep="")
  filel15 <- paste(outdir, "/config.priorlcl15.", tri,".xml", sep="")
  fileu15 <- paste(outdir, "/config.priorucl15.", tri,".xml", sep="")
  fileu30 <- paste(outdir, "/config.priorucl30.", tri,".xml", sep="")
  fileu45 <- paste(outdir, "/config.priorucl45.", tri,".xml", sep="")
  filenames[['priorSA']] <- c(filel45, filel30, filel15, fileu15, fileu30, fileu45)
  saveXML(CONFIGl45, file = filel45, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGl30, file = filel30, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGl15, file = filel15, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGu15, file = fileu15, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGu30, file = fileu30, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
  saveXML(CONFIGu45, file = fileu45, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
}

if (M>0) { #if M==0, only do sens.anal.runs
  seqM <- seq(1,M)
  ## add leading zeroes to the file names to avoid confusion
  zerosM <- sprintf("%04.0f", seqM) #"%05.0f" if > 10^5 runs, etc.
  
  
  samps <- halton(n = M, dim = ncol(post.samps)) 
  colnames(samps) <-  traits

  ens.samps <- list(prior= matrix(nrow = M, ncol = length(traits)), post=matrix(nrow = M, ncol = length(traits)))
  colnames(ens.samps[[1]])<-colnames(ens.samps[[2]])<- traits
  ## Insert samples from trait posteriors into config.xml files
  for (m in seqM) {
    zm <- zerosM[m]
    PFTi <- PFT
    for (k in traits) {
      samp <- quantile( post.samps[,k], samps[m,k])
     PFTi <- append.xmlNode(PFTi, xmlNode(k, samp))
      ens.samps[['post']][m,k] <- samp
    }
    CONFIGi <- append.xmlNode(CONFIG, PFTi)
    file <- paste(outdir, "/config.postsamp",zm,".xml",sep="")
    saveXML(CONFIGi, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    filenames[['post.ensemble']][m]<-file
    rm(PFTi)
    

    ## Insert samples from trait priors into config.xml files
    PFTi <- PFT
    for (k in traits) {
      samp <- quantile(prior.samps[,k], samps[m,k])
      PFTi <- append.xmlNode(PFTi, xmlNode(k, samp))
      ens.samps[['prior']][m,k] <- samp
    }
    CONFIGi <- append.xmlNode(CONFIG, PFTi)
    file <- paste(outdir, "/config.priorsamp",zm,".xml",sep="")
    saveXML(CONFIGi, file = file, indent = TRUE, prefix = '<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n')
    filenames[['prior.ensemble']][m]<-file
  }
}
  
  
  
##next run cleanup script from pecan.writeconfigs.sh
system("
cd $PECANOUT \n
for i in config*xml; do mv \"${i}\" \"${i/config/c}\"; done \n
for i in *_*xml; do mv \"${i}\" \"${i/_/}\"; done \n
for i in *_*xml; do mv \"${i}\" \"${i/_/}\"; done \n
for i in *factor*xml; do mv \"${i}\" \"${i/factor/}\"; done \n
for i in *root*xml; do mv \"${i}\" \"${i/root/rt}\"; done \n
for i in *turnover*xml; do mv \"${i}\" \"${i/turnover/tnvr}\"; done \n
for i in *conductance*xml; do mv \"${i}\" \"${i/conductance/cndctnc}\"; done \n
for i in *respiration*xml; do mv \"${i}\" \"${i/respiration/resp}\"; done \n
for i in *nonlocaldispersal*xml; do mv \"${i}\" \"${i/nonlocaldispersal/nldisprs}\"; done \n
for i in *quantumefficiency*xml; do mv \"${i}\" \"${i/quantumefficiency/quantef}\"; done \n
for i in *water*xml; do mv \"${i}\" \"${i/water/h2o}\"; done \n
for i in *stomatalslope*xml; do mv \"${i}\" \"${i/stomatalslope/stmslope}\"; done \n
for i in c*xml; do mv \"${i}\" \"${i/.xml/}\"; done \n
echo \"zipping config files to saconfigs.tgz\" \n
tar zcf saconfigs.tgz c.* \n
rm c.*\n
cp saconfigs.tgz saconfigs20101223.tgz")

system("cd $PECANHOME\n
rsync -routi out/saconfigs.tgz ebi-cluster:/home/scratch/dlebauer/pecan/edin/ \n
ssh -T ebi-cluster < bash/pecan.ed2in.create.sh \n
ssh -T ebi-cluster < bash/pecan.ed.batchjobs.sh \n
")
save(traits, file = 'traits.Rdata')
save(ens.samps, file = 'ens.samps.Rdata')

## switch to cluster and run:
setwd("/home/scratch/dlebauer/pecan/")
system('rsync ebi-forecast:pecan/traits.Rdata ./')
load('traits.Rdata')
system('rsync ebi-forecast:pecan/R/trait.dictionary.R ./')
source('trait.dictionary.R')
trait.defs <- trait.dictionary(traits)
library(ggplot2)
source('R/pecan.edout.R')
pecanhome <- system("echo $PWD", intern=TRUE)
pecanout <- paste(pecanhome, '/out', sep = '')
date <-  20110119
M <- 500
outdir   <- paste(pecanout, date, sep = '')
yr0   <- 2001
yrf   <- 2003
 
## code from pecan.edout()
library('hdf5')
system('rsync ebi-forecast:pecan/out/trait.defs.Rdata ./')
load('trait.defs.Rdata')
#trait.defs <- as.matrix(trait.defs)
time.range <- c(yr0, yrf)

## create vectors sarun, saruntype
## sarun - run ids
## saruntype - e.g.: post, prior
saruns    <- saruntype <- list()
runid     <- c('prior', 'post')
cl        <- c('lcl45','lcl30','lcl15', 'ucl15','ucl30','ucl45')
runid.cl  <- melt(sapply(runid, paste,  cl, '.', sep = ''))
trait.fileid <- trait.defs$fileid

saruns    <- melt(sapply(runid.cl$value, paste,  trait.fileid, '-', sep = ""))

saruns$X2[saruns$X2%in%1:6] <- runid[1]
saruns$X2[saruns$X2%in%7:12] <- runid[2]
saruns <- saruns[order(saruns$X1),]

for(i in seq(trait.fileid)){
  saruns$trait[saruns$X1==i] <- as.character(trait.fileid[i])
}
colnames(saruns) <- c('trait','runtype', 'runnames')

edoutfiles    <- dir(outdir, full.names = TRUE)      ## grab all files in outdir
edoutfiles.yr <- edoutfiles[grep("-Y-", edoutfiles)]            ## select annual output
edoutfiles.mo <- edoutfiles[grep("-E-", edoutfiles)]            ## select annual output
readagb <- function(runname, year) {
  files <- edoutfiles.yr[grep(runname, edoutfiles.yr)]
  file  <- unique(files[grep(year, files)])
  if(!identical(file, character(0))) {
    data <- hdf5load(file, load=FALSE)[c('AGB_CO', 'NPLANT')]
    agb  <- sum(data$AGB_CO * data$NPLANT) * 20
  } else {
    agb <- NA
  }
  return(agb)
}

agbmeans <- data.frame(yr=1994:2006, po=rep(NA,13), pr=rep(NA,13))
for(i in 1:13){
  agbmeans$po[i] <- readagb('postmean', agbmeans$yr[i])
  agbmeans$pr[i] <- readagb('priormean', agbmeans$yr[i])
}

edout.sa  <- data.frame(saruns,
                     agb=rowMeans(sapply(yr0:yrf, function(x) sapply(saruns$runname, readagb, x))))

ensnames <- c(paste('priorsamp0',1:M,sep=''), paste('postsamp0',1:M,sep='')) 
edout.ens <- data.frame(ensnames,
                       runtype = c(rep('prior', M), rep('post', M)),
                       agb     = rowMeans(sapply(yr0:yrf, function(x) sapply(ensnames, readagb, x))))  

save(edout.ens, file='out/edout.ens.Rdata')
save(edout.sa, file='out/edout.sa.Rdata')
###
#Plot output
###
postmean.f  <- mean(sapply(yr0:yrf, readagb, runname = 'postmean'),na.rm =TRUE)
priormean.f <- mean(sapply(yr0:yrf, readagb, runname = 'priormean'),na.rm =TRUE)
mean.f <- list(post=postmean.f, prior=priormean.f) 
save(mean.f, edout.sa, file=paste(pecanout, '/edout.sa.satest.Rdata', sep=''))

############
stop('switch to forecast')
library(PECAn, lib.loc='~/lib/R')
system('rsync -routi ebi-cluster.igb.uiuc.edu:/home/scratch/dlebauer/pecan/out/*.Rdata ~/pecan/out/')

load('out/edout.sa.satest.Rdata')
load('out/pecan.MA.Rdata')
load('out/qpostqprior.Rdata')
load('out/pecan.samps.Rdata')

qpost.theta <- ldply(qpost)[,-1]
qprior.theta <- ldply(qprior)[,-1]

rownames(qprior.theta) <- rownames(qpost.theta) <- traits
colnames(qprior.theta) <- colnames(qpost.theta)  <- c('lcl45', 'lcl30','lcl15','mean', 'ucl15','ucl30', 'ucl45')

                                        #run defs from trait.dictionary
trait.defs <- trait.dictionary(traits)


fcls <-   function(trait, runtype){
  tr <- trait.defs$fileid[trait.defs$id == trait]
  a <- edout.sa[edout.sa$runtype == runtype, ]
  c(a$agb[grep(paste(tr, '-',sep=''), a$runnames)], mean.f[[runtype]])[c(1,2,3,7,4,5,6)]
}

qprior.f <- lapply(traits, fcls, 'prior')
qpost.f  <- lapply(traits, fcls, 'post')
names(qprior.f) <- names(qpost.f)  <- traits

qprior.f <- as.data.frame(qprior.f)
qpost.f <- as.data.frame(qpost.f)

qprior.theta <- as.data.frame(t(qprior.theta))
qpost.theta <- as.data.frame(t(qpost.theta))

rownames(qprior.f) <- rownames(qpost.f)  <- c('lcl45', 'lcl30','lcl15','mean', 'ucl15','ucl30','ucl45')
colnames(qprior.theta) <- colnames(qpost.theta)  <- traits
 
####Variance Decomposition
trait.samps<-list()
trait.samps[['post']] <- post.samps
trait.samps[['prior']] <- prior.samps
post.mean.theta <- adply(trait.samps[['post']],2, mean)
prior.mean.theta <- adply(trait.samps[['prior']],2, mean) 

traits <- names(qprior)

satable <- data.frame(var.f.hat = rep(NA,length(traits)),
                      var.theta = rep(NA,length(traits)),
                      sens      = rep(NA,length(traits)),
                      var.theta = rep(NA,length(traits)),
                      mean.f    = rep(NA,length(traits)),
                      mean.theta= rep(NA,length(traits)),
                      y95ci     = rep(NA,length(traits)),
                      row.names = traits)
satables <- list(post=satable, prior=satable)                      
qtheta <- list(post = qpost.theta, prior = qprior.theta)
qf <- list(post = qpost.f, prior = qprior.f)

splinelines <- list('prior'=list(), 'post'=list())
splinepoints<- list('prior'=list(), 'post'=list())
splinefuns  <- list('prior'=list(), 'post'=list())

for(runname in c('prior','post')){
  for(trait in traits) {
    x <- qtheta[[runname]][[trait]]
    y <- qf[[runname]][[trait]]
    f <- splinefun(x, y, method = "monoH.FC")
    splinefuns[[runname]][[trait]] <- f 
    splinepoints[[runname]][[trait]] <- data.frame(x=x,y=y)
    xpts <- seq(min(x),max(x),length.out=100)
    splinelines[[runname]][[trait]]  <- data.frame(x=xpts,y=f(xpts))
    x <- trait.samps[[runname]][,trait]
    #rangex <- quantile(x, pnorm(c(-3,3)))
    #x<-x[x>rangex[1] & x<rangex[2]]
    y <- f(x)
    y[y<0 | is.na(y)] <- 0
    satable[trait, 'var.f.hat'] <- var(y)
    satable[trait, 'var.theta'] <- var(x)
    satable[trait, 'sens']      <- f(mean(x),1)
    satable[trait, 'mean.theta']<- mean(x)
    satable[trait, 'mean.f']    <- mean(y)
    satable[trait, 'var.theta'] <- var(x)
    satable[trait, 'y95ci']     <- diff(quantile(y, c(0,1)))    
  }
  var.f.hat.total <- sum(satable$var.f.hat)
  satable$per.var <- satable$var.f.hat/var.f.hat.total
  satable$var     <- satable$var.f.hat
  satable$cv      <- sqrt(satable$var.theta)/satable$mean.theta
  vmi <- which(rownames(satable)=='Vm_low_temp')
  satable$cv[vmi] <- sqrt(satable$var.theta[vmi])/(satable$mean.theta[vmi]+273.15)
  satable$elas    <- with(satable, sens / (mean.f / mean.theta))
  satables[[runname]] <- satable
}

  

##start plotting SA plots

max.y <- 10
min.y <- 0 
plotsa<-function(trait) {
  dpr <- splinepoints[['prior']][[trait]]
  dpo <- splinepoints[['post']][[trait]]
  lpr <- splinelines[['prior']][[trait]]
  lpo <- splinelines[['post']][[trait]]
  ggplot() +
    scale_y_continuous(limits = c(0,50), breaks = c(0, 10, 20, 30))+
     scale_x_continuous(limits = c(ifelse((max(dpr$x)-min(dpr$x))/4>min(dpr$x),0,0.9*min(dpr$x)), signif(max(dpr$x)+min(dpr$x),2))) +
        coord_cartesian(x=c(0, max(dpr$x)*1.1)) +
          geom_line(aes(x, y),
                    color = 'grey',
                    size = 2,
                    data = lpr ) +
                      geom_point(aes(x,y),
                                 data = dpr,
                                 color = 'grey',
                                 size = 3) +
                                   geom_line(aes(x, y),                                                       
                                             color = 'black',
                                             size = 2,
                                             data = lpo) +
                                               geom_point(aes(x,y),
                                                          data=dpo,
                                                          color = 'black',
                                                          size = 3) +
                                                            geom_point(aes(x,y), data= dpr[4,], color = 'grey', size = 5) +
                                                              geom_point(aes(x,y), data= dpo[4,], color = 'black', size = 5) + scale_shape(solid=FALSE) +
                                                                theme_bw() +
                                                                  opts(title=trait.dictionary(trait)$figid,
                                                                       axis.text.x = theme_text(size=14),
                                                                       axis.text.y = theme_text(size=14),
                                                                       axis.title.x = theme_blank(), 
                                                                       axis.title.y = theme_blank(),
                                        #theme_text(size = 28),
                                                                       plot.title = theme_text(size = 20),
                                        #axis.ticks = theme_blank(),
                                        #panel.grid.major = theme_blank(),
                                        #panel.grid.minor = theme_blank(),
                                                                       panel.border = theme_blank()) 
}


plotsa(traits[2])
plotsa('q')
#plotsa('seedling_mortality')
#plotsa('SLA')
plotsa('Vm0')

plots <- lapply(traits,plotsa)


pdf('mikessensitivities.pdf', height = 12, width = 20)
do.call(grid.arrange, plots)#left='Aboveground Biomass', main='Parameter Sensitivity', nrow=3,ncol=5) 
dev.off()

pdf('out/sensitivity_analysis.pdf')
plots
dev.off()


##begin plotting Var Decomp Tryptich

pr    <- satables[['prior']]
po    <- satables[['post']]

pr$id <- rownames(pr)
po$id <- rownames(po)
pr    <-  merge(pr, trait.defs)
po    <-  merge(po, trait.defs)

po <- po[order(po$id),]
pr <- pr[order(pr$id),]

po_rank_per_var <- order(po$per.var)
po <- po[po_rank_per_var,]
pr <- pr[po_rank_per_var,]

data <- data.frame(trait = po$id,
                   title = po$figid,
                   pr.cv = pr$cv, #prior coef. var
                   po.cv = po$cv,  #post  " "
                   pr.el = pr$elas,     #prior elasticity
                   po.el = po$elas,     #post  "
                   pr.ev = pr$var,  #prior, explained variance by parameter i
                   po.ev = po$var,  #post   "         "        "  " 
                   null  = rep(0,nrow(po)))     #dummy for label plot
## fig. parameters
cv.ymax <- max(abs(data[,c('pr.cv', 'po.cv')])) * 1.1 #po.cv =< pr.cv by definition
el.ymax <- max(abs(data[,c('pr.el', 'po.el')])) * 1.1
el.ymin <- -el.ymax
ev.ymax <- max(data[,c('pr.ev', 'po.ev')])*1.1
fontsize = 14 
print(data$trait)
base.plot <- ggplot(data) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(expand = c(0,0)) + 
  opts(axis.line = theme_segment(),
       axis.text.x = theme_text(size=fontsize),
       axis.text.y = theme_text(size=0.01),
       axis.title.x = theme_blank(), 
       axis.title.y = theme_blank(),
       theme_text(size = 20),
       axis.ticks = theme_blank(),
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       panel.border = theme_blank())

 trait.plot <- base.plot +
    opts( title = 'parameter') +
      geom_text(aes(y = 1, x = seq(nrow(data)), label=title, data=data, hjust = 1)) +
        scale_y_continuous( breaks = c(0,0),
                           limits = c(0,1))
  
  cv.plot <- base.plot +
    opts( title = 'CV') +
      geom_pointrange(aes(seq(nrow(data)), pr.cv, ymin = 0, ymax = pr.cv ), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), po.cv, ymin = 0, ymax = po.cv), size = 1.25) +
          scale_y_continuous(breaks =  seq(0, cv.ymax, by=0.2), 
                             limits = c(0, cv.ymax)) 
 

  el.plot <- base.plot +
    opts( title = 'log Elasticity') +
      geom_pointrange(aes(seq(nrow(data)), log10(abs(pr.el)+1), ymin = 0, ymax = log10(abs(pr.el)+1)), size = 1.25, color = 'grey')+
       geom_pointrange(aes(seq(nrow(data)), log10(abs(po.el)+1), ymin = 0, ymax = log10(abs(po.el)+1)), size = 1.25) +
          scale_y_continuous(#breaks =  seq(el.ymin, el.ymax, by=???), 
                             limits = c(0, 2)) 


  ev.plot <- base.plot +
    opts( title = 'log Explained Variance') +
      geom_pointrange(aes(seq(nrow(data)), log10(pr.ev+1), ymin = 0, ymax = log10(pr.ev+1)), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), log10(po.ev+1), ymin = 0, ymax = log10(po.ev+1)), size = 1.25) +
          scale_y_continuous(#breaks =  seq(0, ev.ymax, by=ev.ymax/5), 
                            limits = c(0, 3))


  pdf(paste('out/vardecomp.pdf', sep=''), width = 18 , height = 8)
  grid.arrange(trait.plot, cv.plot, el.plot, ev.plot, ncol=4)
  dev.off()


#############
#Ens plots
############

load('out/edout.ens.Rdata')


 
ens <- edout.ens[edout.ens$agb<100 & edout.ens$agb > 0 & !is.na(edout.ens$agb),]

pr.q <- quantile(ens$agb[ens$runtype=='prior'], c(0.05, 0.5, 0.95))
po.q <- quantile(ens$agb[ens$runtype=='post'], c(0.05, 0.5, 0.95))
pr.dens <- density(ens$agb[ens$runtype=='prior'], from = 0)
po.dens <- density(ens$agb[ens$runtype=='post'], from = 0)

pr.densdf <- data.frame(x = c(0, pr.dens$x), y = c(0, pr.dens$y))
po.densdf <- data.frame(x = c(0, po.dens$x), y = c(0, po.dens$y))

pdf('out/ensemble_density.pdf')
fontsize <- 14
ggplot() +
  theme_bw() +
  opts(title = "Aboveground Biomass \n prior (grey) \n post (dark grey)",
       axis.text.y = theme_blank(),
       axis.text.x = theme_text(size=fontsize),
       axis.line = theme_blank(),
       axis.title.x = theme_blank(), 
       axis.title.y = theme_blank(),
       theme_text(size = 20),
       axis.ticks.x = theme_blank(),
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       panel.border = theme_blank(),
       axis.color.y = 'white',
       legend.position=c(-10,0)) +
  geom_area(data = subset(pr.densdf, x >= pr.q[1] & x<= pr.q[3]),
            aes(x=x, y=y), fill = 'grey50', alpha = 0.5) +
  geom_area(data = subset(po.densdf, x >= po.q[1] & x<= po.q[3]),
            aes(x=x, y=y), fill = 'grey20') +
  geom_line(data = pr.densdf, aes(x=x, y = y) , color = 'grey50') +
  geom_line(data = po.densdf, aes(x=x, y = y) , color = 'grey20') +
  scale_x_continuous(limits = c(0, 50))#, breaks = c(0, 20, 40, 60, 80, 100))

dev.off()

system('rsync -routi /home/dlebauer/pecan/out/ensemble_density.pdf dlebauer:research/writing/pecan/')
system('rsync -routi /home/dlebauer/pecan/out/sensitivity_analysis.pdf dlebauer:research/writing/pecan/')
system('rsync -routi /home/dlebauer/pecan/out/vardecomp.pdf dlebauer:research/writing/pecan/')

 
########Calculate var(f) and sum(var(theta))
prior.varf <- var(c(ens$agb[ens$runtype=='prior']))
post.varf  <- var(c(ens$agb[ens$runtype=='post']))
prior.varest <- sum(satables[['prior']]$var)
post.varest <- sum(satables[['post']]$var)
cat(' run ', ' varf ',' varest\n prior ', prior.varf, prior.varest,' \n post ', post.varf, post.varest,'\n')


######Get Dan's Pavi Yield data
paviyield <- query.bety("select mean from yields where specie_id = 938 and user_id != 11;")
plot(density(paviyield$mean))
abline(v=quantile(paviyield$mean,c(0.025,0.0975)))




## Spline ensemble plots
## Construct a estimate based on the residuals from the splines

spline.ens <- list()
load('ens.samps.Rdata')
pr.spline.mat <- mapply(do.call, splinefuns[['prior']], lapply(as.data.frame(ens.samps[['prior']]), list))
spline.ens[['prior']] <-  sapply(rowSums(pr.spline.mat - mean.f[['prior']]), function(x) max(0,x+mean.f[['prior']]))
po.spline.mat <- mapply(do.call, splinefuns[['post']], lapply(as.data.frame(ens.samps[['post']]), list))
spline.ens[['post']] <-  sapply(rowSums(po.spline.mat - mean.f[['post']]), function(x) max(0,x+mean.f[['post']]))

pdf('~/pecan/out/evaluatedbysplines.pdf',width=11,height=6)
par(mfrow=c(1,2))
plot(density(spline.ens[['prior']],from=0), main = 'model ensemble \nspline ensemble (dotted)', xlab='AGB',lty=2, ylim =c(0, 0.07),xlim=c(0,60),col='grey')
lines(density(spline.ens[['post']],,from=0),lty=2)
lines(density(ens$agb[ens$runtype=='prior' ],from=0),col='grey' )
lines(density(ens$agb[ens$runtype=='post'],from=0))


agb<-list()
agb[['prior']] <- subset(data.frame(runid=ens$ensnames, agb=ens$agb, run=ens$runtype),subset = run=='prior')
agb$prior$runid <- gsub('priorsamp0','',agb$prior$runid)

agb[['post']] <- subset(data.frame(runid=ens$ensnames, agb=ens$agb, run=ens$runtype),subset = run=='post')
agb$post$runid <- gsub('postsamp0','',agb$post$runid)

foo<-list()
foo$prior <- merge(agb$prior, data.frame(runid=1:500,spline.ens = spline.ens$prior))
foo$post  <- merge(agb$post,  data.frame(runid=1:500,spline.ens = spline.ens$post))


plot(foo$prior$spline.ens, foo$prior$agb, xlab='spline estimate', ylab = 'model estimate', main = '500 parameter sets',col='grey',cex=0.5,xlim=c(0,70),ylim=c(0,70))
points(foo$post$spline.ens, foo$post$agb, cex=0.5)
abline(1,1,lty=2)
abline(lm(foo$prior$agb~foo$prior$spline.ens), col='grey')
abline(lm(foo$post$agb~foo$post$spline.ens))
dev.off()
##abline(lm(foo$prior$agb~foo$prior$spline.ens, subset = foo$prior$spline.ens >0), col='grey')
##abline(lm(foo$post$agb~foo$post$spline.ens, subset = foo$post$spline.ens >0))


# Compare contributions to ens. va
x<- data.frame(id=po$id,
               pr.cvt = pr$cv,
               po.cv = po$cv,
               prior.pervar = pr$per.var*100,
               post.pervar = po$per.var*100,
               prior.var = pr$var,
               post.var = po$var,
               var.ratio = (pr$var-po$var)/pr$var
           )
print(cbind(x$id,round(x[,2:7],2),x[,8]))

save(splinefuns, ens.samps, mean.f, ens, file = 'splineens.Rdata') 


mean.f
quantile(foo$prior$agb, c(0.05, 0.95))
quantile(foo$post$agb, c(0.05, 0.95))

foofn <- function(n,runnname) replicate(10,var(sample(spline.ens[[runname]],n)))
x<- c(2, 4, 8, 16, 32, 64, 128, 256)

y <- data.frame(n=x, t(sapply(x, function(x) c(mean(foofn(x)), sd(foofn(x))))))

set.seed(1)
par(mfrow=c(2,1))
y <- data.frame(n=x, t(sapply(x, function(x) c(mean(foofn(x,'prior')), sd(foofn(x,'prior'))))))
plot(y$n, y$X1, main = 'prior ens size n vs. var(500 choose n)', ylim = c(0,200))
arrows(y$n, y$X1,y$n,y$X1+y$X2, angle = 90)

y <- data.frame(n=x, t(sapply(x, function(x) c(mean(foofn(x,'post')), sd(foofn(x,'post'))))))
plot(y$n, y$X1, main = 'post ens size n vs. var(500 choose n)', ylim = c(0,200))
arrows(y$n, y$X1,y$n,y$X1+y$X2, angle = 90)
