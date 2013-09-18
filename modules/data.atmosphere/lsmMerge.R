## Code to create synthetic met file by merging in multiple files
## currently set up to rescale a high-resolution met file (e.g. flux tower)
## to the daily met values given at another site (prefereably near by)
## First version: create met file for Hubbard Brook from Bartlett flux tower
## and HB daily means

## settings
mettype <- 1  ## 0 = SOI      Type of met file being READ
              ## 1 = regional
loc <- c(43.93,-71.75)  ## Bartlett

## libraries and constants
library(ed2)
Kelvin <- 273.15

## Primary hi-res file (in RAMS format)
#metbase <- "/home/mcd/inputs/fluxnet/bartlett/hires/lat43.5lon-71.5/"
#metbase <- "/home/mcd/inputs/fluxnet/bartlett/hourly/lat43.5lon-71.5/"
#metbase <- "/home/mcd/inputs/fluxnet/bartlett/lat43.5lon-71.5/"
metbase <- "/n/Moorcroft_Lab/Users/mcd/inputs/reanalysis/ncep/"
#metstart <- c(1,2004)
#metstart <- c(1,2000)
#metstart <- c(8,2002)
metstart <- c(1,1974)
metstop <- c(12,2005)
#metres <- 96 ##divisions/day
#metres <- 24 ##divisions/day
metres <- 4

## Secondary daily file
##daily <- read.table("/n/Moorcroft_Lab/Users/mcd/hydro_test/hubbard/met/dailyPPTwatershed.txt",header=TRUE,na.strings="-99.0")
daily <- read.csv("/n/Moorcroft_Lab/Users/mcd/hydro_test/hubbard/met/pwd_all.txt",header=TRUE,na.strings="-99.0")
daily.time <- string2day(daily$DATE)
daily.dd <- jday(daily.time[,1],daily.time[,2],daily.time[,3]) 
daily.ppt <- daily$WS_6/86400.0  ## select coln and rescale to mm/s
temp <- read.csv("/n/Moorcroft_Lab/Users/mcd/hydro_test/hubbard/met/TempDailyMean.txt",header=TRUE,na.strings="-99.0")
temp.time <- string2day(temp$DATE)
temp.dd <- jday(temp.time[,1],temp.time[,2],temp.time[,3]) 
daily.temp <- temp$STA_6 + Kelvin ## select coln and rescale to Kelvin

### ALT Secondary: use a NOAA met station
stationfolder <-"/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/NorthConwayMet/"
stationfile <- dir(stationfolder,"dat.txt") 
smet <- ExtractNOAAstation(read.csv(paste(stationfolder,stationfile,sep="")))
daily.time <- temp.time <- smet$date
daily.ppt <- smet$PRCP/86400
daily.temp <- 0.5*(smet$TMIN+smet$TMAX) + Kelvin

## output path
##outpath <- "/home/mcd/hydro_test/hubbard/met/hires/lat43.5lon-71.5/"
##outpath <- "/home/mcd/hydro_test/hubbard/met/hourly/"
##outpath <- "/home/mcd/hydro_test/hubbard/met/lowresbartlett/lat43.5lon-71.5/"
##outpath <- "/n/Moorcroft_Lab/Users/mcd/hydro_test/hubbard/met/NCEP/"
outpath <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/NorthConwayMet/"

##parameters for nocturnal smoothing on splice
##hires
#width <- 5 ## width of smoothing window
#nsmooth <-  4 ## number of measurements on each side of the day to be smoothed
##hourly
#width <- 2 ## width of smoothing window
#nsmooth <- 2 ## number of measurements on each side of the day to be smoothed
##6hr
width <- 0 ## width of smoothing window
nsmooth <- 0 ## number of measurements on each side of the day to be smoothed

sfun <- combl(width)

## Loop over met/rad files
for(yr in metstart[2]:metstop[2]){
  fmo <- 1
  lmo <- 12
  if(yr == metstart[2]) fmo <- metstart[1]
  if(yr == metstop[2]) lmo <- metstop[1]
  for(mo in fmo:lmo){

    print(c(mo,yr))
    
    ## read met file
    met <- rad <- NULL
    if(mettype == 0){
      met <- read.met(metbase,mo,yr)
      rad <- read.rad(metbase,mo,yr)
    } else {
      met <- read.met.region(metbase,mo,yr,loc)
      rad <- read.rad.region(metbase,mo,yr,loc)      
    }
    ##    print(c(as.integer(yr),as.integer(mo),as.integer(nrow(met)),nrow(met)/96))
    ##  }
    ##}
    
    ## select matching daily
    sel <- which(apply(cbind(daily.time[,1] == yr,daily.time[,2]==mo),1,sum)==2)
    dtemp <- daily.ppt[sel]
    dtemp.days <- daily.time[sel,3]

    sel <- which(apply(cbind(temp.time[,1] == yr,temp.time[,2]==mo),1,sum)==2)
    temp.lowres <- daily.temp[sel]
    temp.lowres.days <- temp.time[sel,3]
    
    ## first establish wet/dry day status in both records, calculate daily ppt
    wetThresh <- 2.45/86400 # if < .1 inch/day assume is dry (below detection)
    days <- rep(1:31,each=metres)[1:nrow(met)]
    raddays <- rep(1:31,each=96)[1:nrow(rad)]
    wet <- tapply((met$conprr+met$pcpg),days,sum)/metres
    stormlen <- tapply((met$conprr+met$pcpg)>0,days,sum)
    iswet <- wet > wetThresh
    all.days <- sort(unique(days))
    
    ## create temperature record
    temp.hires <- tapply((met$theta),days,mean)
    
    ## id miss-match days
    daymatch <- 1:length(all.days)
    daymatch[dtemp.days[apply(cbind(iswet[dtemp.days],dtemp > 0),1,sum) == 1]] <- NA
    daymatch[is.na(dtemp)] <- which(is.na(dtemp)) ## set missing to "match" to keep original data
    for(t in 1:length(daymatch)){
      dmatch <- match(t,dtemp.days)
      if(is.na(daymatch[t])&&!is.na(dmatch)){
        ##if target is wet, find nearest wet day
        if(dtemp[dmatch] >  0){
          j <- t
          for(i in 1:length(all.days)){
            j <- max(1,t-i)
            if(iswet[j]) break;
            j <- min(length(all.days),t+i)
            if(iswet[j]) break;
          }
          daymatch[t] <- j
        }

        ##if target is dry, find nearest dry day
        if(dtemp[dmatch] ==  0){
          j <- t
          for(i in 1:length(all.days)){
            j <- max(1,t-i)
            if(!iswet[j]) break;
            j <- min(length(all.days),t+i)
            if(!iswet[j]) break;
          }
          daymatch[t] <- j
        }
      }
    }    
    #temp.bias <- mean(temp.hires-temp.lowres)
    

    ## make new met & rad files
    metnew <- met
    radnew <- rad
    for(i in 1:length(all.days)){
      dmatch <- match(i,dtemp.days)
      ## select individual measurements to swap
      sel1 <- which(days == i)
      sel2 <- which(days == daymatch[i])
      rsel1 <- which(raddays == i)
      rsel2 <- which(raddays == daymatch[i])
      nrep <- length(sel2) - length(sel1)
      if(nrep > 0){
        sel1 <- c(sel1,rep(sel1[length(sel1)],nrep))
      }
      if(nrep < 0){
        nrep <- -nrep
        sel2 <- c(sel2,rep(sel2[length(sel2)],nrep))
      }
      ## splice in nearest match day (both met and rad)      
      metnew[sel1,] <- met[sel2,]
      radnew[sel1,] <- rad[sel2,]
      ## rescale ppt to preserve daily sum
      if(!is.na(dmatch) && !is.na(dtemp[dmatch])){
        if(dtemp[dmatch] > 0){
          fac <- dtemp[dmatch]/wet[daymatch[i]]
          metnew$conprr[sel1] <- metnew$conprr[sel1] * fac
          metnew$pcpg[sel1] <- metnew$pcpg[sel1] * fac  
        } else {  ## make sure dry days stay dry
          metnew$conprr[sel1] <- 0.0
          metnew$pcpg[sel1] <- 0.0
        }
      }
      
      ## linearly rescale temperature
      tmatch <- match(i,temp.lowres.days) 
      if(!is.na(tmatch) && !is.na(temp.lowres[tmatch])){
        airtemp <- met$theta[sel1]*met$pi0[sel1]/1004
        b <-mean(airtemp)-temp.lowres[tmatch]  ##mean(metnew$theta[sel1])
        metnew$theta[sel1] <- metnew$theta[sel1] - b*1004/met$pi0[sel1]     
      }
    
      ## smooth transition on spliced
      if(i > 1 && (i != daymatch[i] || (i-1) != daymatch[i-1])){
        splice <- matrix(NA,2*nsmooth+1,12)
        rsplice <- matrix(NA,2*nsmooth+1,ncol(rad))
        ## loop over data to be smoothed
        for(w in 1:(2*nsmooth + 1)){
          row <- w - nsmooth -1 + sel1[1] 
          window <- 1:length(sfun) + row-floor(length(sfun)/2) - 1
          ##print(c(row,window))
          splice[w,] <- apply(metnew[window,]*sfun,2,sum)

          rrow <- w - nsmooth -1 + rsel1[1] 
          rwindow <- 1:length(sfun) + rrow-floor(length(sfun)/2) - 1
          ##print(c(row,window))
          rsplice[w,] <- apply(radnew[rwindow,]*sfun,2,sum)
        }
        metnew[-nsmooth:nsmooth+sel1[1],] <- splice
        radnew[-nsmooth:nsmooth+rsel1[1],] <- rsplice
      }
      
    }
    
    redowet <- tapply((metnew$conprr+metnew$pcpg),days,sum)/metres
      
    ## write out new file
    write.met(metnew,outpath,mo,yr)
    write.rad(radnew,outpath,mo,yr)

    
  }  ### end MONTH
} ### end YEAR



######################################################
###                                                ###
###           SNOW CORRECTION                      ###
###                                                ###
######################################################

### obs ppt at Bartlett tower appears to be off when temp is below freezing
### likely equipment malfunction with tipping bucket gauge
### replace observed ppt with NCEP ppt when Tair < 0 C

## code currently assumes that metres is the same for both files

origfolder <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/lat44.5lon-71.5/"
#NCEPfolder <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/temp/"
NCEPfolder <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/NorthConwayMet/"
outfolder  <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/snowcor3/lat44.5lon-71.5/"

start <- c(1,2004)
end   <- c(12,2005)

for(y in start[2]:end[2]){
  for(m in 1:12){

    ## read files
    met <- read.met(origfolder,m,y)
    ncep <- read.met(NCEPfolder,m,y)

    ## calc temp
    temp <- met$theta*met$pi0/1004-273.15

    ## replace values
    sel <- temp < 0
    met$conprr[sel] <- ncep$conprr[sel]
    met$pcpg[sel] <- ncep$pcpg[sel]

    ## write new file
    write.met(met,outfolder,m,y)
  }
}

############################################
##                                        ##
##  MERGE REGIONAL NOAA STATION DATA      ##
##                                        ##
############################################

origfolder <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/lat44.5lon-71.5/"
NCEPfolder <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/temp/"
outfolder  <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/station/lat44.5lon-71.5/"
stationfolder <-"/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/NorthConwayMet/"

stationfile <- dir(stationfolder,"dat.txt")
station <- read.csv(paste(stationfolder,stationfile,sep=""))
smet <- ExtractNOAAstation(station)

start <- c(1,2004)
end   <- c(12,2005)

for(y in start[2]:end[2]){
  for(m in 1:12){

    ## read files
    met <- read.met(origfolder,m,y)
    ncep <- read.met(NCEPfolder,m,y)

    ## calc temperatures
    Ttower <- met$theta*met$pi0/1004-273.15
    Treg   <- ncep$theta*ncep$pi0/1004-273.15
    ysel <- which(smet$date[,1] == y)
    msel <- ysel[which(smet$date[ysel,2] == m)]
    Ts <- rep(0.5*(smet$TMIN[msel]+smet$TMAX[msel]),each=4)

    ## calc PPT
    Pt <- met$pcpg+met$conprr
    Pr <- ncep$pcpg+ncep$conprr
    Ps <- rep(smet$PRCP[msel]/8640,each=4) ## cm/day -> mm/sec
    print(c(y,m,apply(cbind(Pt,Ps,Pr),2,sum)))
    
    ## replace values
#    sel <- temp < 0
#    met$conprr[sel] <- ncep$conprr[sel]
#    met$pcpg[sel] <- ncep$pcpg[sel]

    ## write new file
#    write.met(met,outfolder,m,y)
  }
}


###########################################
#####                                 #####
#####     PRECIP WEATHER GENERATOR    #####
#####                                 #####
###########################################

## code reads existing met files and condenses DAILY ppt
## to shorter time step storms based on observed dist.
## of storm lengths and time of day

samplefolder <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/lat44.5lon-71.5/"
#infolder  <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/snowcor2/lat44.5lon-71.5/"
infolder  <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/NorthConwayMet/"
#outfolder  <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/snowcor2/lat44.5lon-71.5/"
outfolder <- infolder

start.sample <- c(6,1,2004)
end.sample <- c(12,31,2005)
freq <- 4  #obs/day

start <- c(1,2000)
end <- c(12,2005)

## process sample
samp <- read.met.ts(samplefolder,start.sample,end.sample,"pcpg",average=FALSE)
tod <- rep(1:freq,length=length(samp))
day <- rep(1:(length(samp)),each=freq)[1:length(samp)]
rain <- as.integer(samp>0)
drain <- c(0,diff(rain))
rstart <- which(drain == 1)
rstop <- which(drain == -1) 
lmin <- min(length(rstart),length(rstop))
storm.len <- rstop[1:lmin]-rstart[1:lmin]
storm.len2 <- storm.len
storm.len2[storm.len2 > freq] <- freq

slen <- table(storm.len2)
stod <- table(tod[rstart])
slen <- slen/sum(slen)  ## storm length frequency distribution
stod <- stod/sum(stod)  ## storm start time of day frequency distribution

for(y in start[2]:end[2]){
  mstart <- 1; if(y == start[2]) {mstart<- start[1]}
  mend <- 12;  if(y == end[2]) {mend <- end[1]}
  for(m in mstart:mend){

    ## read files
    met <- read.met(infolder,m,y)

    ## set vars
    tod <- rep(1:freq,length=nrow(met))
    day <- rep(1:(nrow(met)),each=freq)[1:nrow(met)]
    nday <- max(day)
    ppt <- met$conprr + met$pcpg
    dppt <- tapply(ppt,day,sum)
    
    ## draw start times
    sstart <- findInterval(runif(nday),cumsum(stod))
    send <-  sstart + findInterval(runif(nday),cumsum(slen))
    send[send>(freq-1)] <- (freq-1)
    wt <- 1/((send-sstart)+1)

    ## create pseudo-precip record
    for(i in 1:nday){
      d <- which(day == i)
      ppt[d] <- 0.0
      ppt[d[1]+(sstart[i]:send[i])] <- dppt[i]*wt[i]
    }
    met$conprr <- 0.0
    met$pcpg <- ppt
   
    ## write out new ppt
    write.met(met,outfolder,m,y)
  }
}


###########  COMPARE AVERAGE TEMP AND PPT ACROSS MET FILES ############

orig <- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/lat44.5lon-71.5/"
cor2<- "/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/snowcor2/lat44.5lon-71.5/"

start <- c(1,1,2000)
end <- c(12,31,2003)

Mo <- read.met.ts(orig,start,end,c("theta","conprr","pcpg"))
M2 <- read.met.ts(cor2,start,end,c("theta","conprr","pcpg"))


