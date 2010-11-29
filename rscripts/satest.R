library(PECAn, lib.loc = '~/lib/R')
load('out/pecan.samps.Rdata')
load('out/pecan.MA.Rdata')
const <- pecan.config.constants(pft)
PFT <- const$PFT
CONFIG <- const$CONFIG
traits <- tr <- colnames(post.samps)
filenames <- list()
outdir <- 'out'


trait.mat <- lapply(trait.mcmc, as.matrix)
n.samp <- min(sapply(trait.mat, function(x) nrow(x)))
traits <- names(trait.mcmc)
priors$n <- nrow(trait.mat[[1]])
colnames(priors)[which(colnames(priors) %in% c('parama','paramb'))] <- c('a', 'b')

prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,]))
colnames(prior.samps) <- rownames(priors)

post.samps <- prior.samps
for (tri in traits) post.samps[1:n.samp,tri] <- trait.mat[[tri]][1:n.samp, 'beta.o']

quantiles <- c(0.05, 0.20, 0.35, 0.5, 0.65, 0.80, 0.95)

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
rm c.*\n")
system("cd $PECANHOME\n
rsync -routi out/saconfigs.tgz ebi-cluster:/home/scratch/dlebauer/pecan/edin/ \n
ssh -T ebi-cluster < bash/pecan.ed2in.create.sh \n
ssh -T ebi-cluster < bash/pecan.ed.batchjobs.sh \n
")



# switch to cluster and run:
library(ggplot2)
source('R/pecan.edout.R')
pecanhome <- system("echo $PWD", intern=TRUE)
pecanout <- paste(pecanhome, '/out', sep = '')
date <-  20101129
M <- 0
outdir   <- paste(pecanout, date, sep = '')
yr0   <- 2005
yrf   <- 2009

## code from pecan.edout()
library('hdf5')
load('out/trait.defs.Rdata')

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
  saruns$X1[saruns$X1 == i] <- as.character(trait.fileid[i])
}

colnames(saruns) <- c('trait','runtype', 'runnames')

edoutfiles    <- dir(outdir, full.names = TRUE)      ## grab all files in outdir
edoutfiles.yr <- edoutfiles[grep("-Y-", edoutfiles)]            ## select annual output

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

edout  <- data.frame(saruns,
                     agb=rowMeans(sapply(yr0:yrf, function(x) sapply(saruns$runname, readagb, x))))

postmean.f  <- mean(sapply(yr0:yrf, readagb, runname = 'postmeans'),na.rm =TRUE)
priormean.f <- mean(sapply(yr0:yrf, readagb, runname = 'priormeans'))
mean.f <- list(post=postmean.f, prior=priormean.f) 
save(mean.f, edout, file=paste(pecanout, '/edout.satest.Rdata', sep=''))
 
############
stop('switch to desktop')
library(ggplot2)
system('rsync -routi ebi-cluster.igb.uiuc.edu:/home/scratch/dlebauer/pecan/out/*.Rdata ~/pecan/out/')
load('out/qpostqprior.Rdata')
load('out/edout.satest.Rdata')

qpost.theta <- ldply(qpost)[,-1]
qprior.theta <- ldply(qprior)[,-1]
rownames(qprior.theta) <- rownames(qpost.theta) <- names(qpost)
colnames(qprior.theta) <- colnames(qpost.theta)  <- c('lcl45', 'lcl30','lcl15','mean', 'ucl15','ucl30', 'ucl45')

#run defs from trait.dictionary
source('rscripts/defs.R')
trait.defs <- defs[defs$id %in% names(qpost),]

fcls <-   function(x, runtypei){
  y <- trait.defs$fileid[match(x, trait.defs$id)]
  c(edout$agb[edout$trait == y & edout$runtype==runtypei], mean.f[[runtypei]])[c(1,2,5,3,4)]
}

qprior.f <- lapply(traits, fcls, 'prior')
qpost.f  <- lapply(traits, fcls, 'post')
names(qprior.f) <- names(qpost.f)  <- traits

qprior.f <- as.data.frame(qprior.f)
qpost.f <- as.data.frame(qpost.f)

qprior.theta <- as.data.frame(t(qprior.theta))
qpost.theta <- as.data.frame(t(qpost.theta))

rownames(qprior.f) <- rownames(qpost.f)  <- c('lcl30','lcl15','mean', 'ucl15','ucl30')
#colnames(qprior.theta) <- colnames(qpost.theta)  <- paste(c('lcl30','lcl15','mean', 'ucl15','ucl30'),'theta', sep = '')



save(qprior.f, qpost.f, qprior.theta, qpost.theta, file = 'out/satest.Rdata')
##start plotting

#system('rsync -routi ebi-forecast.igb.uiuc.edu:/home/dlebauer/pecan/out/satest.Rdata ~/pecan/out/')

load('out/satest.Rdata')


library(JGR)
JGR()

traits <- colnames(qpost.f)
library(splines)
library(ggplot2)

plotsa <- function(trait, runtype){
  if(runtype == 'post') {
    f<-qpost.f[,trait]
    theta<-qpost.theta[,trait]
  }
  if(runtype == 'prior') {
    f<-qprior.f[,trait]
    theta<-qprior.theta[,trait]
  }
  dq30<-c(1,3,5)
  dq15<-c(2,3,4)
  data <- data.frame(x = theta, y=f)
  ggplot(data=data) + opts(title=paste(runtype,trait)) +
    geom_point(aes(x=x, y= y), data=data) +
       stat_smooth(aes(x = x[dq15],y = y[dq15]),data=data,method = 'lm', formula = 'y ~ poly(x,2)', color = 'blue', fill = '#fefefe') +
       stat_smooth(aes(x = x[dq30],y = y[dq30]),data=data,method = 'lm', formula = 'y ~ poly(x,2)', color = 'blue', fill = '#fefefe') +
        ##stat_smooth(aes(x = x,y = y),data=data,method = 'lm', formula = 'y ~ poly(x,3)') +
        stat_smooth(aes(x = x,y = y),data=data,method = 'lm', formula = 'y ~ ns(x)', color = 'green', fill = '#fefefe') +
          stat_smooth(aes(x = x,y = y),data=data,method = 'loess', color = 'red', fill = '#fefefe')  +
            theme_bw()
}

plotsa('SLA', 'prior')
plotsa('q', 'prior')

plots <- list(prior=list(), post=list())
for(trait in traits){
  for(runtype in c('prior', 'post')) {
    plots[[runtype]][[trait]] <- plotsa(trait, runtype)
  }
}
 
pdf('SAtest.pdf')
plots
dev.off()



