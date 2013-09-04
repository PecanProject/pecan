## Code to analyse the spectral signal of model error
## between models and fluxtowers
##
## Analysis part of the NACP site-level intercomparison
##
## Michael Dietze, University of Illinois
##


### Specify required functions
require(dplR) ## Andy Bunn's Dendrochronology package

WAVE <- function (crn.vec, yr.vec, p2=NULL, dj = 0.25, siglvl = 0.99, ...) 
{
  ## simple function based on Bunn's wavelet.plot fcn that returns wavelet info
  if(is.null(p2)) p2 <- floor(log2(length(crn.vec)))
    n <- length(crn.vec)
    Dt <- 1
    s0 <- 1
    j1 <- p2/dj
    mother <- "morlet"
    crn.vec.ac <- acf(crn.vec, lag.max = 2, plot = FALSE)
    lag1 <- (crn.vec.ac$acf[2] + sqrt(crn.vec.ac$acf[3]))/2
    return(wavelet(y1 = crn.vec, Dt = Dt, s0 = s0, dj = dj, 
        J = j1, mother = mother, siglvl = siglvl))
}


## directory to find model files
model.dir <- "NEEm"

## set of models to analyze
model.set <- c(sort(c("BEPS","CNCLASS","ISOLSM","TECO","ecosys","SiBCASA","SiB","DLEM","ED2","LoTEC_DA","AgroIBIS","DNDC","SiBcrop","can.ibis","EDCM","ORCHIDEE","LPJ","BIOME_BGC","SSiB2","TRIPLEX","EPIC")),"MEAN")

Nmodel <- length(model.set)  ## number of models

## listing of available site files
site.files <- dir(model.dir,"txt")


#########  LOOP OVER SITES ##########
#for(i in 1:length(site.files)){
#for(i in c(5,25,38,45)){
for(i in c(1,5,7,8,9,25,26,30,38,45)){

  yset = 1990:2010
  if(i == 5)  yset = 1999:2007
  if(i == 25) yset = 1992:2005
  if(i == 38) yset = 2002:2004
  if(i == 45) yset = 1999:2003 
  
  ## load site data table
  dat <- read.table(paste(model.dir,site.files[i],sep="/"),
                    header=TRUE,
                    na.string="-999.000"
                    )
  ysel = which(dat$X.YEAR %in% yset)
  dat = dat[ysel,]
  
  m2c <- match(model.set,names(dat))  ## match model names to data table columns
  day <-  1/diff(dat$FDOY[1:2])       ## number of observations per day
  Nperiod <- 1 + 4*log2(nrow(dat))


  
  ##POWER <- array(NA,c(Nmodel,Nperiod,nrow(dat)))
  ##SIGNIF <- matrix(NA,Nmodel,Nperiod)
#########  LOOP OVER MODELS ##########
#  for(j in 1:Nmodel){
 j = 22   
    k <- m2c[j]  ## desired column in data table for specified model

    ## option 1 - absolute residuals
    y <- dat$NEE_FILLED - dat[,k] ### Model error fcn

    ## option 2 - normalized residuals (post)
    if(FALSE){
      ybar <- mean(y,na.rm=TRUE)
      ysd  <- NA
      if(is.nan(ybar)) {
        ybar <- NA
      } else {
        ysd  <- sqrt(var(y,na.rm=TRUE))
      }
      if(is.nan(ysd)) ysd <- NA    
      y <- (y-ybar)/ysd ## normalize error
    }
    
    ## option 3 - normalized residuals (pre)
    ## subscripts: t = tower, m = model
    
    ## normalize tower
    NEEt.bar <- mean(dat$NEE_FILLED,na.rm=TRUE)
    NEEt.sd  <- NA
    if(is.nan(NEEt.bar)){
      NEEt.bar <- NA
    } else {
      NEEt.sd <- sqrt(var(dat$NEE_FILLED,na.rm=TRUE))
    }    
    NEEt.norm <- (dat$NEE_FILLED - NEEt.bar)/NEEt.sd
    ## normalize model
    NEEm.bar <- mean(dat[,k],na.rm=TRUE)
    NEEm.sd  <- NA
    if(is.nan(NEEm.bar)){
      NEEm.bar <- NA
    } else {
      NEEm.sd <- sqrt(var(dat[,k],na.rm=TRUE))
    }    
    NEEm.norm <- (dat[,k] - NEEm.bar)/NEEm.sd
    y <- NEEm.norm-NEEt.norm  ## calc residuals of normalized
          
    y[is.na(y)] <- 0 ## need to fill in missing values
    
    wv <- WAVE(y)#,p2=17)  ## Calculate wavelet spectrum
    period <- wv$period/day  ## wavelet periods
    Power <- (abs(wv$wave))^2 ## wavelet power
    for(t in 1:length(wv$Scale)){  ### bias correction
      Power[,t] = Power[,t]/wv$Scale[t]
    }
    ## Crop out cone of influence
    coi <- wv$coi  ## cone of influence (valid if below value)
    for(t in 1:length(coi)){
      sel <- which(period>coi[t])
      Power[t,sel] <- NA 
    }
    
    ## update storage
    ##  POWER[j,,] <- Power
    ##  SIGNIF[j,] <- wv$Signif
    
    ##save(wv,Power,day,file=paste("NACPspecNORMpre2v2_17.",i,".",j,".Rdata",sep=""))
    save(wv,Power,day,file=paste("NACPspecNORMpre2.clip.",i,".",j,".Rdata",sep=""))
    
    print(c(i,j))
#  }  ## end model loop
  
} ## loop over sites


 if(FALSE){

plot(period,apply(Power,2,mean),log='xy',xlab="Period (days)")
abline(v=1)
abline(v=365.25)
abline(v=365.25/4)










###########################   SCRATCH ################################

j <- 9   ## model


tm <- seq(1,by=1/48,length=nrow(dat))
NEE <- ts(dat$NEE_FILLED,deltat=1/48)
plot(spectrum(NEE))

plot(dat$FDOY,dat$NEE)
plot(dat$FDOY,dat$NEE_FILLED,pch=".")

m2c <- match(model.set,names(dat))  ## match model names to data table columns
k <- m2c[j]  ## desired column in data table for specified model

plot(dat$FDOY,dat[,k],pch=".")

dat[is.na(dat)] <- 0


## coherence spectra
ct <- which(names(dat) == "NEE_FILLED")
sp <- spectrum(dat[,c(ct,k)])
par(mfrow=c(3,1))
plot(sp$spec[,1],log='y')
plot(sp$spec[,2],log='y')
plot(sp$spec[,1]-sp$spec[,2],log='y')
X <- dat[,c(ct,k)]
wt <- dwt(X)


## absolute residuals spectra
ra  <- dat$NEE_FILLED - dat[,k]
rav <- abs(ra)
spectrum(ra)
spectrum(rav)
ra.w <- dwt(ra)
ra.wm <- modwt(ra[1:10])
plot(ra.w)
plot(ra.wm)

plot(acf(ra))

wavelet.plot(ra,tm,p2=10)##floor(log2(nrow(dat)))))


### TESTING
y <- sin((1:512)/pi)+sin((1:512)/10)

## null model construction:
t <- 1:nrow(dat)
y <- sin(2*pi*t/day) + sin(2*pi*t/(365.25*day))

wavelet.plot(y,1:length(y),log2(length(y)))
plot(y,type='l')

wv <- WAVE(y)
day <-  1/diff(dat$FDOY[1:2])
period <- wv$period/day
Power <- (abs(wv$wave))^2
Signif = t(matrix(wv$Signif, dim(wv$wave)[2], dim(wv$wave)[1]))
Signif = Power/Signif
image(Power)

plot(apply(Power,2,mean),log='y')
plot(period,apply(Power,2,mean),log='y')
plot(period,apply(Power,2,mean))

plot(period,apply(Power,2,mean),log='xy',xlab="Period (days)")
abline(v=1)
abline(v=365.25)
abline(v=365.25/4)

## divide up spectra
Pglobe <- apply(Power,2,mean)
day.mid <- findInterval(1,period)
day.bin <- day.mid + c(-4:4)
abline(v=period[day.bin])

year.mid <- findInterval(365.25,period)
year.bin <- year.mid + c(-4:4)
abline(v=period[year.bin])

synop.bin <- (max(day.bin)+1):(min(year.bin)-1)
subday.bin <- 1:(min(day.bin)-1)
inter.bin <- (max(year.bin)+1):length(period)
if(length(period) <= max(year.bin)) inter.bin <- NA

pow.bin <- c(sum(Pglobe[subday.bin]),sum(Pglobe[day.bin]),sum(Pglobe[synop.bin]),sum(Pglobe[year.bin]),sum(Pglobe[inter.bin]))



plot(1/period,apply(Power,2,mean),log='y')
plot(apply(Signif,2,mean),log='y')
plot(apply(Signif,2,mean))
abline(h=1)

## Crop out cone of influence
coi <- wv$coi  ## cone of influence (valid if below value)
for(t in 1:length(coi)){
  sel <- which(period>coi[t])
  Power[t,sel] <- NA 
}

## normalized residuals spectra

## histogram approach (% of significant failure events)

i
## analysis of spectra - mixed model

##
## TODO:
##  - define model set
##  - define time-step
##  - define sites
##  - incorporation of uncertainty
##  - gaps or gap-filled
##  - alternative visualizations
##  - identification of whether model failures are consistent in time
##  - separation of pattern from accuracy
##     - look at cross-spectra?
##     - absolute vs normalised residuals
##
}
