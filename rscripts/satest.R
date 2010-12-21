library(PECAn, lib.loc = '~/lib/R')
library(xtable)
pft <- 'ebifarm.c4crop'
ITER  <- 250000
M     <- 500
outdir   <- system("echo $PECANOUT", intern = TRUE)
outfile1 <- paste(outdir, '/pecan.parms.Rdata', sep = '')
save.image(outfile1)

spp <- query.bety.pft_species(pft)
spstr <- spp$spstr 

trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA_gC_per_m2','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','c2n_leaf','water_conductance','Vm0','r_fract','storage_turnover_rate'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors

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
save(outdir, file='outdir.Rdata')
trait.samps <- pecan.samps(trait.mcmc, priors)
prior.dists <- trait.samps[['priors']]
post.samps <- trait.samps[['post.samps']]     
prior.samps <- trait.samps[['prior.samps']] #matrix of samples from all priors
save(post.samps, prior.samps,file='pecan.samps.Rdata')

load('out/pecan.samps.Rdata')
load('out/pecan.MA.Rdata')
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
                        c2n_leaf=trait.beta.o$c2n_leaf[i],
                        leaf_width=trait.beta.o$leaf_width[i],
                        Vm0=trait.beta.o$Vm0[i],
                        q=trait.beta.o$q[i])

n.samp <- 10000
traits <- names(trait.mcmc)
priors$n <- nrow(trait.mat)
colnames(priors)[which(colnames(priors) %in% c('parama','paramb'))] <- c('a', 'b')

prior.samps <- sapply(1:nrow(priors), function(x) do.call(pr.samp,priors[x,]))
colnames(prior.samps) <- rownames(priors)

post.samps <- prior.samps
for (tri in traits) post.samps[1:n.samp,tri] <- trait.mat[1:n.samp, tri]

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
date <-  20101215
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
  saruns$X1[saruns$X1==i] <- as.character(trait.fileid[i])
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
system('rsync -routi ebi-forecast.igb.uiuc.edu:/home/dlebauer/pecan/out/qpostqprior.Rdata ~/pecan/out/')

load('out/qpostqprior.Rdata')
load('out/edout.satest.Rdata')

qpost.theta <- ldply(qpost)[,-1]
qprior.theta <- ldply(qprior)[,-1]
traits <- names(qpost)
rownames(qprior.theta) <- rownames(qpost.theta) <- traits
colnames(qprior.theta) <- colnames(qpost.theta)  <- c('lcl45', 'lcl30','lcl15','mean', 'ucl15','ucl30', 'ucl45')

                                        #run defs from trait.dictionary
source('rscripts/defs.R')
trait.defs <- defs[defs$id %in% names(qpost),]

traits <- trait.defs$id
fcls <-   function(x, runtypei){
  y <- trait.defs$fileid[match(x, trait.defs$id)]
  c(edout$agb[edout$trait == y & edout$runtype==runtypei], mean.f[[runtypei]])[c(1,2,3,7,4,5,6)]
}

qprior.f <- lapply(traits, fcls, 'prior')
qpost.f  <- lapply(traits, fcls, 'post')
names(qprior.f) <- names(qpost.f)  <- traits

qprior.f <- as.data.frame(qprior.f)
qpost.f <- as.data.frame(qpost.f)

qprior.theta <- as.data.frame(t(qprior.theta))
qpost.theta <- as.data.frame(t(qpost.theta))

rownames(qprior.f) <- rownames(qpost.f)  <- c('lcl45', 'lcl30','lcl15','mean', 'ucl15','ucl30','ucl45')
                                        #colnames(qprior.theta) <- colnames(qpost.theta)  <- paste(c('lcl30','lcl15','mean', 'ucl15','ucl30'),'theta', sep = '')



save(qprior.f, qpost.f, qprior.theta, qpost.theta, file = 'out/satest.Rdata')
##start plotting

                                        #system('rsync -routi ebi-forecast.igb.uiuc.edu:/home/dlebauer/pecan/out/satest.Rdata ~/pecan/out/')

load('out/satest.Rdata')


traits <- colnames(qpost.f)
library(splines)
library(ggplot2)

max.y <- 180
min.y <- 0
max.x <- sapply(names(qpost.f), function(x) signif(max(qpost[[x]], qprior[[x]]),2)*1.1)
min.x <- sapply(names(qpost.f), function(x) signif(min(qpost[[x]], qprior[[x]]),2)*0.9)

plotsa <- function(trait, runtype){
  if(runtype == 'post') {
    f<-qpost.f[,trait]
    theta<-qpost.theta[,trait]
  }
  if(runtype == 'prior') {
    f<-qprior.f[,trait]
    theta<-qprior.theta[,trait]
  }
  xmax <- max.x[[trait]]
  xmin <- min.x[[trait]]
  
  data <- data.frame(x = theta, y=f)
  ggplot(data=data) +
    opts(title=paste(runtype,trait)) +
      coord_cartesian(xlim = c(xmin, xmax),ylim = c(min.y,max.y)) +
        theme_bw() +
          stat_smooth(aes(x = x,y = y),data=data,method = 'lm', formula = 'y ~ ns(x,6)', color = 'black', size = 2, fill = '#fefefe') +
  geom_point(aes(x=x, y= y), data=data, size = 2, color = 'white')
  ##stat_smooth(aes(x = x,y = y),data=data,method = 'loess', color = 'red', fill = '#fefefe')  +
}

plotsa('SLA', 'prior')
plotsa('q', 'prior')
plotsa('c2n_leaf', 'prior')
plots <- list(prior=list(), post=list())
for(trait in traits){
  for(runtype in c('prior', 'post')) {
    plots[[runtype]][[trait]] <- plotsa(trait, runtype)
  }
}

pdf('stomatal_slope_sensitivity.pdf')
  for(runtype in c('prior', 'post')) {
    plots[[runtype]][[trait]] <- plotsa('stomatal_slope', runtype)
  }
dev.off()
pdf('SAtest.pdf')
plots
dev.off()

####Variance Decomposition
##on forecast
library(PECAn, lib.loc='~/lib/R')
load('out/qpostqprior.Rdata')
load('out/satest.Rdata')
load('out/pecan.samps.Rdata')
load('out/edout.satest.Rdata')


trait.samps<-list()
trait.samps[['post']] <- post.samps
trait.samps[['prior']] <- prior.samps
post.mean.theta <- adply(trait.samps[['post']],2, mean)
prior.mean.theta <- adply(trait.samps[['prior']],2, mean) 

traits <- names(qprior)

satable <- data.frame(var.f.hat = rep(NA,length(traits)),
                      sens      = rep(NA,length(traits)),
                      var.theta = rep(NA,length(traits)),
                      mean.f    = rep(NA,length(traits)),
                      mean.theta= rep(NA,length(traits)),
                      row.names = traits)
satables <- list(post=satable, prior=satable)                      
qtheta <- list(post = qpost.theta, prior = qprior.theta)
qf <- list(post = qpost.f, prior = qprior.f)

for(runname in c('prior','post')){
  for(tr in traits) {
    tr. <- ifelse(tr=='leafN','c2n_leaf',tr)
    f <- splinefun(qtheta[[runname]][[tr.]], qf[[runname]][[tr.]])
    x <- trait.samps[[runname]][,tr]
    x[x<0 | is.na(x)] <- 0
    satable[tr, 'var.f.hat'] <- var(f(x))
    satable[tr, 'sens']      <- f(mean(x),1)/(mean(f(x))/mean(x))
    satable[tr, 'mean.theta']<- mean(x)
    satable[tr, 'mean.f']    <- mean(f(x))
    satable[tr, 'var.theta'] <- var(x)
  }
  var.f.hat.total <- sum(satable$var.f.hat)
  satable$per.var <- satable$var.f.hat/var.f.hat.total
  satable$cv      <- sqrt(satable$var.theta)/satable$mean.theta
  vmi <- which(rownames(satable)=='Vm_low_temp')
  satable$cv[vmi] <- sqrt(satable$var.theta[vmi])/(satable$mean.theta[vmi]+273.15)
  satable$elas    <- with(satable, sens / (mean.f / mean.theta))
  satables[[runname]] <- satable
}

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

data <- data.frame(
                   trait = pr$figid,
                   pr.cv = pr$cv, #prior coef. var
                   po.cv = po$cv,  #post  " "
                   pr.el = pr$elas,     #prior elasticity
                   po.el = po$elas,     #post  "
                   pr.ev = pr$per.var,  #prior, explained variance by parameter i
                   po.ev = po$per.var,  #post   "         "        "  " 
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
       ## theme_text(size = 20),
       ## axis.ticks = theme_blank(),
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       panel.border = theme_blank())

 trait.plot <- base.plot +
    opts( title = 'agb') +
      geom_text(aes(y = 1, x = seq(nrow(data)), label=trait), data=data, hjust = 1) +
        scale_y_continuous( breaks = c(0,0),
                           limits = c(0,1))
  
  cv.plot <- base.plot +
    opts( title = 'CV') +
      geom_pointrange(aes(seq(nrow(data)), pr.cv, ymin = 0, ymax = pr.cv ), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), po.cv, ymin = 0, ymax = po.cv), size = 1.25) +
          scale_y_continuous(breaks =  seq(0, cv.ymax, by=0.2), 
                             limits = c(0, cv.ymax)) 
  

  el.plot <- base.plot +
    opts( title = 'Elasticity') +
      geom_pointrange(aes(seq(nrow(data)), pr.el, ymin = 0, ymax = pr.el), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), po.el, ymin = 0, ymax = po.el), size = 1.25) +
          scale_y_continuous(#breaks =  seq(el.ymin, el.ymax, by=???), 
                             limits = c(el.ymin, el.ymax)) 


  ev.plot <- base.plot +
    opts( title = 'Explained Variance') +
      geom_pointrange(aes(seq(nrow(data)), pr.ev, ymin = 0, ymax = pr.ev), size = 1.25, color = 'grey')+
        geom_pointrange(aes(seq(nrow(data)), po.ev, ymin = 0, ymax = po.ev), size = 1.25) +
          scale_y_continuous(#breaks =  seq(0, ev.ymax, by=ev.ymax/5), 
                             limits = c(0, ev.ymax))
  pdf(paste('saplotagb.pdf', sep=''), width = 18 , height = 8)
  grid.arrange(trait.plot, cv.plot, el.plot, ev.plot, ncol=4)
  dev.off()


##diagnostics
tr <- 'SLA'
f <- splinefun(qtheta[[runname]][[tr]], qf[[runname]][[tr]])
x <- trait.samps[[paste(runname,'.samps',sep='')]][,tr]
