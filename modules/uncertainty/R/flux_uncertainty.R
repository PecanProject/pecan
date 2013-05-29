#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------#
read.ameriflux.L2 <- function(file.name, year){
  data<-as.data.frame(read.table(file.name, header=TRUE, sep=',', 
          na.strings=c('-9999','-6999'), stringsAsFactors=FALSE))
  #data$time <- year + (data$DTIME / 366.0)
  return(data)
}

get.change <- function(measurement){
  gaps <- measurement %in% c(-6999, -9999)
         #| quality > 0
  measurement[gaps] <- NA
  even <- seq(measurement) %% 2 == 0
  odd  <- seq(measurement) %% 2 == 1
  return(measurement[even] - measurement[odd])
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## measurement = flux time-series
## QC = quality control flag time series (0 = best)
## flags = additional flags on flux filtering of PAIRS (length = 1/2 that of the time series, TRUE = use).
## bin.num = number of bins (default = 10)
## transform = transformation of magnitude (default = identity)
## minBin = minimum number of points allowed in a bin
flux.uncertainty <- function(measurement, QC=0, flags=TRUE, bin.num=10, transform=identity, minBin=5,...){
  ## calcuate paired differences between points
  change <- get.change(measurement)
  
  ## convert gaps to NA
  gaps <- measurement %in% c(-6999, -9999)
          #| quality > 0
  measurement[gaps] <- NA

  ## combine all indicators
  even <- seq(measurement) %% 2 == 0
  odd <- seq(measurement) %% 2 == 1
  Q2  <- QC[even]==0 & QC[odd]==0 & flags & !is.na(measurement[even]) & !is.na(measurement[odd])

  ## calulate error and flux magnitude for each pair of points
  indErr <- abs(change[Q2]) / sqrt(2)
  magnitude <- measurement[even][Q2]

  ## calculate bins
  bins <- seq(from=min(magnitude, na.rm=TRUE), 
              to=max(magnitude, na.rm=TRUE), 
              length.out = bin.num)

  ## calculate binned mean, error, bias, and sample size
  magBin <- c()
  errBin <- c()
  biasBin <- c()
  nBin <- c()
  for(k in 1:(length(bins)-1)){
    use <- magnitude >= bins[k] & magnitude < bins[k+1]
    nBin[k] <- sum(use,na.rm=TRUE)    
    magBin[k] <- mean(transform(magnitude[use]), na.rm=TRUE)

    if(nBin[k] > minBin ){ ## && sum(!is.na(change[use])) > 50) {
      errBin[k] <- sd(indErr[use], na.rm=TRUE)
      biasBin[k] <- mean(indErr[use],na.rm=TRUE)
      print(paste(length(magnitude[use]), sum(!is.na(change[use])), magBin[k], errBin[k]))
    }
    else {
      if(nBin[k] == 0) magBin[k] <- NA
      errBin[k] <- NA
      biasBin[k] <- NA
      print(paste(length(magnitude[use]), sum(!is.na(change[use]))))
    }
  }

  ## separate fluxes into positive, negative, and zero bins
  zero <- diff(sign(bins)) > 0
  pos <- magBin > 0  & !zero
  neg <- magBin < 0 & !zero

  ## subtract off zero bin, fit regression to positive and negative components
  ## would be better to fit a two line model with a common intercept, but this
  ## is quicker to implement for the time being
  E2 = errBin - errBin[zero]
  mp <- lm(E2[pos] ~ magBin[pos]-1)
  mn <- lm(E2[neg] ~ magBin[neg]-1)
  intercept <- errBin[zero]
  slopeP <- mp$coefficients[1]
  slopeN <- mn$coefficients[1]
  
  return(list(mag=magBin, err=errBin,bias=biasBin,n=nBin,intercept=intercept,slopeP=slopeP,slopeN=slopeN))
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
plot.flux.uncertainty <- function(f, ...){
  plot(f$mag, f$err, ...)
  big = 10000
  lines(c(0,big),c(f$intercept,f$slopeP*big))
  lines(c(0,-big),c(f$intercept,-f$slopeN*big))
  legend('bottomleft', 
         legend=c('intercept', f$intercept,
                  'slopeP',     f$slopeP,
                   'slopeN', f$slopeN))
  
}
  
plot.oechel.flux <- function(observations, site){
  par(mfrow=c(2,2))
  #only use data from March 1 through November 1
  observations <- observations[observations$DOY >60 & observations$DOY < 305,]
  
  #dRg <- get.change(observations$Rg)
  #dD <- get.change(observations$D)
  dTa <- get.change(observations$TA)
  flags <- #abs(DRg)<200 & 
           #abs(DD)<5 & 
           abs(dTa)<3 
  
  print('NEE+')
  plot.flux.uncertainty((observations$FC[observations$FC >= 0]),
    flags = flags,
    main = site, xlab='NEE bin (+)', ylab='NEE random error', bin.num=80)
  print('NEE-')
  plot.flux.uncertainty((observations$FC[observations$FC <= 0]),
    flags = flags,
    main = site, xlab='NEE bin (-)', ylab='NEE random error', bin.num=80)
  print('LE')
  plot.flux.uncertainty((observations$LE[observations$LE >= 0]),
    flags = flags,
    main = site, xlab='LE bin (+)', ylab='LE random error')
  plot.flux.uncertainty((observations$LE[observations$LE <= 0]),
    flags = flags,
    main = site, xlab='LE bin (-)', ylab='LE random error')
  print('Soil Temperature')
  plot.flux.uncertainty(observations$TS1[observations$TS1 >= 0],
    flags = flags,
    main = site, xlab='Soil Temp bin (+)', ylab='Soil Temp random error')
  plot.flux.uncertainty(observations$TS1[observations$TS1 <= 0],
    flags = flags,
    main = site, xlab='Soil Temp bin (-)', ylab='Soil Temp random error')
}

tundra.flux.uncertainty <- function(){
## dummy function created temporarly to encapsulate Carl's analysis
  
itex.climate <- lapply(1998:2011, 
    function(year){
      folder <- '../SoilMoistureHollister'
      file <- paste(year, '%20ITEX', sep='')
      file <- dir(folder, pattern=file, full.names=TRUE)
      print(file)
      data<-as.data.frame(read.table(file, header=FALSE, 
                          sep='\t',stringsAsFactors=FALSE))
      columns <- c('UTC', 'ATZ', 'location', 'site', 'treatment', 'plot',
                   'year', 'julian', 'month', 'hour')
      colnames(data)[1:length(columns)] <- columns
      colnames(data)[ncol(data)-1] <- 'wfv'
      data <- data[data$wfv > -999,]
      data <- data[,colnames(data) %in% c(columns, 'wfv')] #standardize columns
      print(names(data))
      return(data)
    })
itex.climate <- do.call(rbind, itex.climate)

oechel.atqasuk<-lapply(1999:2006, 
    function(year){
      file <- paste('usatqasu_', year, '_L2.csv', sep='')
      print(file)
      return(read.ameriflux.L2(file, year))
    })
oechel.atqasuk<-do.call(rbind, oechel.atqasuk)
plot.oechel.flux(oechel.atqasuk, 'Atqasuk')
plot.flux.uncertainty(itex.climate$wfv[itex.climate$site %in% c('AD')], 
  main = 'Atqasuk', xlab='Soil Moisture bin (%vol)', ylab='Soil Moisture random error (%vol)')

oechel.barrow<-lapply(1998:2006, 
    function(year){
      file <- paste('usakbarr_', year, '_L2.csv', sep='')
      print(file)
      return(read.ameriflux.L2(file, year))
    })
oechel.barrow<-do.call(rbind, oechel.barrow)
plot.oechel.flux(oechel.barrow, 'Barrow')
plot.flux.uncertainty(itex.climate$wfv[itex.climate$site %in% c('BD')], 
  main = 'Barrow', xlab='Soil Moisture bin (%vol)', ylab='Soil Moisture random error (%vol)')


}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
