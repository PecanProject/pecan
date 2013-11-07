###########################################
#####                                 #####
#####     PRECIP WEATHER GENERATOR    #####
#####                                 #####
###########################################

## code reads existing met files and condenses DAILY ppt
## to shorter time step storms based on observed dist.
## of storm lengths and time of day

## Set the following INPUTS before running
##
## samplefolder  - location of hi-res met data used to set stats
## infolder      - location of met that's read in
## outfolder     - location of met written out
## start.sample  - c(mo,day,yr)
## end.sample    - c(mo,day,yr)
## freq          - sample frequency
## start         - c(mo,yr)
## end           - c(mo,yr)

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
