## Code to analyse the spectral signal of model error
## between models and fluxtowers
##
## Analysis part of the NACP site-level intercomparison
##
## Michael Dietze, University of Illinois
##


### Specify required functions
library(dplR,lib.loc="~/lib/R") ## Andy Bunn's Dendrochronology package
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

if(FALSE){

## set of models to analyze
model.set <- c(sort(c("BEPS","CNCLASS","ISOLSM","TECO","ecosys","SiBCASA","SiB","DLEM","ED2","LoTEC_DA","AgroIBIS","DNDC","SiBcrop","can.ibis","EDCM","ORCHIDEE","LPJ","BIOME_BGC","SSiB2","TRIPLEX","EPIC")),"MEAN")

Nmodel <- length(model.set)  ## number of models


vars <- c("NEEm","GPPm","REm")

## directory to find model files
for(model.dir in vars){


  ## listing of available site files
  site.files <- dir(model.dir,"txt")


#########  LOOP OVER SITES ##########
  for(i in 1:length(site.files)){
    ##for(i in c(5,25,38,45)){
    ##for(i in c(1,5,7,8,9,25,26,30,38,45)){

    site = sub(".txt","",site.files[i])
    
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
    
    day <-  1/diff(dat$FDOY[1:2])       ## number of observations per day
    Nperiod <- 1 + 4*log2(nrow(dat))

    POWER <- NULL
  
#########  LOOP OVER MODELS ##########
    for(j in 3:ncol(dat)){
      
      ## option 1 - absolute fluxes
      y <- dat[,j] ### Model error fcn
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
      Pglobe <- apply(Power,2,sum,na.rm=TRUE)

      if(j == 3){
        POWER = matrix(NA,length(Pglobe),ncol(dat)-2)
        colnames(POWER) = colnames(dat)[3:ncol(dat)]
      }
      POWER[,j-2] <- Pglobe
      
      save(Nperiod,period,day,POWER,file=paste("NACPspec.Keenan.",site,".Rdata",sep=""))
      
      print(c(site,colnames(dat)[j]))
    }  ## end model loop
    
  } ## loop over sites
  
} ## loop over variables

}

###############  PROCESSING for 1000 Monte Carlo reps
model.dir <- "NEEm"
site.files <- dir(model.dir,"txt")

for(sitenum in c(1,5,7,8,9,25,26,38,45)){

  site.name <- site.files[sitenum]
  site.name <- sub("_NEE.txt","",site.name)
  site.name <- sub("-","",site.name)
  prefix <- paste("MDSNEE_",site.name,"-",sep="")
    
  yset = 1990:2010
  if(sitenum == 5)  yset = 1999:2007
  if(sitenum == 25) yset = 1992:2005
  if(sitenum == 38) yset = 2002:2004
  if(sitenum == 45) yset = 1999:2003 

  ## load the pseudo data
  load(paste(prefix,"pseudo.Rdata",sep=""))

  POWER <- NULL
  
#########  LOOP OVER REPS ##########
  for(j in 1:ncol(dat)){
      
      ## option 1 - absolute fluxes
      y <- dat[,j] ### Model error fcn
      y[is.na(y)] <- 0 ## need to fill in missing values
      
      wv <- WAVE(y)#,p2=17)  ## Calculate wavelet spectrum
      period <- wv$period  ## wavelet periods
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
      Pglobe <- apply(Power,2,sum,na.rm=TRUE)

      if(j == 1){
        POWER = matrix(NA,length(Pglobe),ncol(dat))
      }
      POWER[,j] <- Pglobe
      
      save(period,POWER,file=paste("NACPspecNULL.Keenan.",site.name,".Rdata",sep=""))
      
      print(c(site.name,j))
    }  ## end model loop
    
} ## loop over sites
  

