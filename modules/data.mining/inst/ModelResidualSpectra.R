## Script to analyse the spectral signal of model error
##
## Michael Dietze, Boston University 
##

## read functions
source("ResidSpectra.R")

## directory to find model files
model.dir <- "NEEm"

## set of models to analyze
model.set <- c(sort(c("BEPS","CNCLASS","ISOLSM","TECO","ecosys","SiBCASA","SiB","DLEM","ED2","LoTEC_DA","AgroIBIS","DNDC","SiBcrop","can.ibis","EDCM","ORCHIDEE","LPJ","BIOME_BGC","SSiB2","TRIPLEX","EPIC")),"MEAN")
Nmodel <- length(model.set)  ## number of models

## listing of available "site" files
## Files should contain a time column, an observed flux column, and a column for each model
site.files <- dir(model.dir,"txt")

## set of possible years to consider
yset = 1990:2010

#########  LOOP OVER SITES ##########
for(i in 1:length(site.files)){
  
  ## load site data table
  dat <- read.table(paste(model.dir,site.files[i],sep="/"),
                    header=TRUE,
                    na.string="-999.000"
                    )
  ysel = which(dat$X.YEAR %in% yset)
  dat = dat[ysel,]
  
  m2c <- match(model.set,names(dat))  ## match model names to data table columns
  day <-  1/diff(dat$FDOY[1:2])       ## number of observations per day
  Nperiod <- 1 + 4*log2(nrow(dat))    ## number of periods (# of col's in Power matrix)

  #########  LOOP OVER MODELS ##########
  for(j in 1:Nmodel){

    k <- m2c[j]  ## desired column in data table for specified model

    wv = ResidSpectra(data=dat$NEE_FILLED, model=dat[,k], obsPerDay == day,case=3 )
    
    ## save the spectra for each model and each site to a separate file
    save(wv,Power,day,file=paste("spec",i,".",j,".Rdata",sep=""))
    
    print(c(i,j))
  }  ## end model loop
  
} ## loop over sites


 if(FALSE){
## Useful Diagnostics
   
  ##overall power spectrum
  Power = wv$Power
  period = wv$period
  Pglobe <- apply(Power,2,mean)
  plot(period,Pglobe,log='xy',xlab="Period (days)",ylab="Power")
  
  ## divide up spectra into bins
  
  ## daily peak
  day.mid <- findInterval(1,period)
  day.bin <- day.mid + c(-4,4)
  abline(v=period[day.bin])

  ## annual peak
  year.mid <- findInterval(365.25,period)
  year.bin <- year.mid + c(-4,4)
  abline(v=period[year.bin])

  synop.bin <- (max(day.bin)+1):(min(year.bin)-1)  ## period between daily and annual
  subday.bin <- 1:(min(day.bin)-1)                 ## period < daily
  inter.bin <- (max(year.bin)+1):length(period)    ## interannual
  if(length(period) <= max(year.bin)) inter.bin <- NA

  ## bin power by periods
  pow.bin <- c(sum(Pglobe[subday.bin]),sum(Pglobe[day.bin]),sum(Pglobe[synop.bin]),sum(Pglobe[year.bin]),sum(Pglobe[inter.bin]))
  names(pow.bin) <- c("subdaily","daily","synoptic","annual","interannual")
  format(pow.bin/sum(pow.bin,na.rm=TRUE)*100,digits=3,scientific=FALSE)  ## % of spectral power in each bin
}
