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

plot.flux.uncertainty <- function(measurement, flags=TRUE, bin.num=10, transform=identity, ...){
  change <- get.change(measurement)
  
  gaps <- measurement %in% c(-6999, -9999)
          #| quality > 0
  measurement[gaps] <- NA

  beta <- mean(abs(change))
  sigma <- sqrt(2) * beta
  indErr <- abs(change[flags]) / sqrt(2)
  
  even <- seq(measurement) %% 2 == 0
  magnitude <- measurement[even][flags]

  bins <- seq(from=min(magnitude, na.rm=TRUE), 
              to=max(magnitude, na.rm=TRUE), 
              length.out = bin.num)
  magBin <- c()
  errBin <- c()
  for(k in 1:length(bins)-1){
    use <- magnitude >= bins[k] & magnitude < bins[k+1]
    if(length(magnitude[use]) > 5 && sum(!is.na(change[use])) > 50) {
      magBin[k] <- mean(transform(magnitude[use]), na.rm=TRUE)
      errBin[k] <- sd(indErr[use], na.rm=TRUE)
      print(paste(length(magnitude[use]), sum(!is.na(change[use])), magBin[k], errBin[k]))
    }
    else { 
      magBin[k] <- NA
      errBin[k] <- NA
      print(paste(length(magnitude[use]), sum(!is.na(change[use]))))
    }
  }
  plot(magBin, errBin, ...)
  model <- lm(errBin ~ magBin)
  abline(model)
  intercept <- model$coefficients[1]
  slope <- model$coefficients[2]
  legend('topleft', 
         legend=c('intercept', intercept,
                  'slope',     slope))
  return(list(mag=magBin, err=errBin))
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








