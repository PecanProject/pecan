## Code to create synthetic met file by merging in multiple files
## currently set up to rescale a high-resolution met file (e.g. flux tower, NCEP)
## to the daily met values given at another site (prefereably near by)
##
## code swaps nearest wet/dry days to observed wet/dry
## and rescales temperature and ppt to observed means
## then smooths the splice btw days


## variables that should be set BEFORE calling
## met type   0 = SOI, 1=regional
## loc        c(lat,lon)
## metbase    path to base files
## metstart   c(mo,yr)
## metstop    c(mo,yr)
## metres     divisions/day
## daily.ppt  precip data
## daily.time precip time
## daily.temp temperature data
## temp.time  temperature time
## outpath    output path
## width      width of smoothing window
## nsmooth    number of measurements on each side of day to be smoothed
month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

Kelvin <- 273.15
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
      if(mettype == 1){
        met <- read.met.region(metbase,mo,yr,loc)
        rad <- read.rad.region(metbase,mo,yr,loc)
      } else { ## met is hdf
        met <- hdf5load(paste(metbase,yr,month[mo],".h5",sep=""),load=FALSE)
      }
    }
    
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

