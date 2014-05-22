#-------------------------------------------------------------------------------
# Extract tree-level variables per plot for PalEON domain from FIA database.
# Aggregate tree data by unique species per plot.
# Spatial interpolation of plot-level data to PalEON domain.
# 
# Jaclyn Hatala Matthes, 11/27/13
# Edited by Toni Viskari, 05/12/14
# email: jaclyn.hatala.matthes@gmail.com, tt.viskari@gmail.com
#-------------------------------------------------------------------------------

library(RPostgreSQL)
library(plyr)

##FYI, to clear SQL result set:
#dbClearResult(dbListResults(con)[[1]])

fia.database <- 'fia5data'

#constants
year    <- 2000       #find surveys closest to present year
ac2ha   <- 0.404686   #conversion from 1 acre to hectares

# create an MySQL instance and create one connection.
drv <- dbDriver("PostgreSQL")
# open the connection using user, passsword, etc., as root user
con <- dbConnect(drv,dbname='fia5',user='bety',host='psql-pecan.bu.edu')

# Query DB for all states and years
query       <- 'SELECT "INVYR", "STATECD", "STATEAB", "STATENM", "CYCLE", "SUBCYCLE" from "SURVEY"'
state.query <- dbSendQuery(con, query)
state.surv  <- fetch(state.query, n = -1)
states <- sort(unique(state.surv$STATECD))
states <- states[states < 72 & states != 56 & states !=35]


#NEW METHOD: Chooses the cycle and subcycle closest to the specified year 
#in order to get a complete 5-year survey across all state plots
state.max.subcycle <- vector()
#first need to find max. subcycle year for each state (some states took > 5 years to sample all plots)
for(s in states){
  sel <- which(state.surv$STATECD == s)
  state.max.subcycle[s] <- max(state.surv$SUBCYCLE[sel][state.surv$SUBCYCLE[sel]!=99]) 
}

state.cycle <- list() #list to hold most recent year, cycle, subcycle per state

#have to loop everything into a list because no a priori knowledge about how long matrix would be
for(s in states){
  sel <- which(state.surv$STATECD == s)
  closest.year.ind <- which(abs(state.surv$INVYR[sel]-year)==min(abs(state.surv$INVYR[sel]-year))) #find the closest survey year to selected year
  max.rep.cycle <- min(state.surv$CYCLE[sel[which(abs(state.surv$INVYR[sel]-year)==min(abs(state.surv$INVYR[sel]-year)))]]) #find the newest cycle
  max.subcycle  <- 1:max(state.surv$SUBCYCLE[sel[state.surv$CYCLE[sel]==max.rep.cycle&state.surv$SUBCYCLE[sel]!=99]]) #find the max subcycle within that cycle
  
  if(max(max.subcycle)<state.max.subcycle[s]){ #if the cycle isn't complete
    
    #get older chunk of 5-year survey to make complete set
    old.cycle    <- max.rep.cycle-1
    old.subcycle <- sort(seq(state.max.subcycle[s],1,by=-1)[which(seq(state.max.subcycle[s],1,by=-1)-max(max.subcycle)>0)])
    match.old.years <- state.surv$INVYR[sel[which(state.surv$CYCLE[sel]==old.cycle&(state.surv$SUBCYCLE[sel] %in% old.subcycle))]]
    match.old.cycle <- state.surv$CYCLE[sel[which(state.surv$CYCLE[sel]==old.cycle&(state.surv$SUBCYCLE[sel] %in% old.subcycle))]]
    match.old.subcycle <- state.surv$SUBCYCLE[sel[which(state.surv$CYCLE[sel]==old.cycle&(state.surv$SUBCYCLE[sel] %in% old.subcycle))]]
    old.dat <- cbind(match.old.years,match.old.cycle,match.old.subcycle) #make matrix
    
    #get newest set of the 5-year survey
    match.years <- state.surv$INVYR[sel[which(state.surv$CYCLE[sel]==max.rep.cycle&(state.surv$SUBCYCLE[sel] %in% max.subcycle))]]
    match.cycle <- state.surv$CYCLE[sel[which(state.surv$CYCLE[sel]==max.rep.cycle&(state.surv$SUBCYCLE[sel] %in% max.subcycle))]]
    match.subcycle <- state.surv$SUBCYCLE[sel[which(state.surv$CYCLE[sel]==max.rep.cycle&(state.surv$SUBCYCLE[sel] %in% max.subcycle))]]

    if(sum(diff(match.years))==(length(match.years)-1)){ #if there were no other years with same cycle #
      state.cycle[[s]] <- cbind(rep(s,state.max.subcycle[s]),rbind(old.dat,cbind(match.years,match.cycle,match.subcycle)))
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    } else{ #sometimes there is an older survey w/ same cycle number, but is always only a single instance
      match.years <- match.years[2:length(match.years)] #should fix this, but first return is always an old annual survey, so clip it dumb way for now  
      match.cycle <- match.cycle[2:length(match.cycle)]
      match.subcycle <- match.subcycle[2:length(match.subcycle)]

      state.cycle[[s]] <- cbind(rep(s,state.max.subcycle[s]),rbind(old.dat,cbind(match.years,match.cycle,match.subcycle)))
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    }
      
  } else{ #if the cycle is complete
    #get newest set of the 5-year survey
    match.years <- state.surv$INVYR[sel[which(state.surv$CYCLE[sel]==max.rep.cycle&(state.surv$SUBCYCLE[sel] %in% max.subcycle))]]
    match.cycle <- state.surv$CYCLE[sel[which(state.surv$CYCLE[sel]==max.rep.cycle&(state.surv$SUBCYCLE[sel] %in% max.subcycle))]]
    match.subcycle <- state.surv$SUBCYCLE[sel[which(state.surv$CYCLE[sel]==max.rep.cycle&(state.surv$SUBCYCLE[sel] %in% max.subcycle))]]
    
    if(sum(diff(match.years))==(state.max.subcycle[s]-1)){ #if there were no other years with same cycle #
      state.cycle[[s]] <- cbind(rep(s,state.max.subcycle[s]),match.years,match.cycle,match.subcycle)
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    } else{ #sometimes there is an older survey w/ same cycle number, but is always only a single instance
      match.years <- match.years[2:length(match.years)] #should fix this, but first return is always an old annual survey, so clip it dumb way for now  
      match.cycle <- match.cycle[2:length(match.cycle)]
      match.subcycle <- match.subcycle[2:length(match.subcycle)]
      state.cycle[[s]] <- cbind(rep(s,state.max.subcycle[s]),match.years,match.cycle,match.subcycle)
      colnames(state.cycle[[s]]) <- c("statecd","time","cycle","subcycle")
    }
  }
}

#make list into matrix of state code, year, current cycle, current subcycle for easier manipulation
state.mat <- state.cycle[[1]]
for(i in 2:length(state.cycle)){
  if(length(state.cycle[[i]])>0){
    state.mat <- rbind(state.mat,state.cycle[[i]])
  } else{
    state.mat <- rbind(state.mat,rep(NA,4))
  }
}
state.mat <- state.mat[complete.cases(state.mat),]
colnames(state.mat) <- c("statecd","time","cycle","subcycle")


#lon,lat bounds
lat     <- c(44.9,45.1)   
lon     <- c(-90.1,-89.9) 

lonmin <- min(lon)
lonmax <- max(lon)
latmin <- min(lat)
latmax <- max(lat)

query3 <- paste('SELECT p."MEASYEAR" as time,p."PLOT",p."CYCLE",p."SUBCYCLE",p."STATECD",p."LON",p."LAT",t."DIA"*2.54 as dbh, t."STATUSCD",t."SPCD" as spcd FROM "TREE" t, "PLOT" p WHERE p."CN"=t."PLT_CN" AND p."LON" >= ',lonmin,' AND p."LON" < ',lonmax,' AND p."LAT" >= ',latmin,' AND p."LAT" < ',latmax,' AND t."STATUSCD"=1 AND t."DIA"*2.54 > 5.0', sep='')

css.query3 <- dbSendQuery(con, query3)
surv.all  <- fetch(css.query3, n = -1)


#only select data from most current cycle, subcycle 
survey.states <- unique(surv.all$STATECD)
surv.current  <- matrix()
for(s in 1:length(survey.states)){
  state.code      <- survey.states[s]
  state.year      <- state.mat[which(state.mat[,1]==state.code),2]
  state.cycle     <- state.mat[which(state.mat[,1]==state.code),3]
  state.subcycle  <- state.mat[which(state.mat[,1]==state.code),4]
  
  if(s==1){
    surv.current    <- surv.all[which((surv.all$STATECD==state.code) & (surv.all$time %in% state.year) & 
                                        (surv.all$SUBCYCLE %in% state.subcycle)),]
  } else{
    surv.tmp     <- surv.all[which((surv.all$STATECD==state.code) & (surv.all$time %in% state.year) & 
                                        (surv.all$SUBCYCLE %in% state.subcycle)),]
    surv.current <- rbind(surv.current,surv.tmp)
  }  
}

#do some more filtering 

surv.current <- surv.current[complete.cases(surv.current),]                  #remove rows with NA
#surv.current <- surv.current[surv.current$STATUSCD==1,]                      #live trees only
#surv.current <- surv.current[surv.current$dbh>20.32,]                        #only trees >8 inches
#print(surv.current[1:10,]) #print first 10 lines to make sure stuff looks OK

F2P <- read.csv('FIA_PFT.csv')
P2B <- read.csv('P2B.csv')

C2B <- 2.
agf_bs <- 0.7
hgt_ref <- 1.3

PFT <- vector()
b1Bl <- vector()
b2Bl <- vector()
b1Bs <- vector()
b2Bs <- vector()
b1Ht <- vector()
b2Ht <- vector()
qsw <- vector()


for(i in 1:length(surv.current$spcd)){
  PFT[i] <- F2P$PFT[which(F2P$SPCD == surv.current$spcd[i])]
  b1Bl[i] <- P2B$b1Bl[which(P2B$PFT == PFT[i])]
  b2Bl[i] <- P2B$b2Bl[which(P2B$PFT == PFT[i])]
  b1Bs[i] <- P2B$b1Bs[which(P2B$PFT == PFT[i])]
  b2Bs[i] <- P2B$b2Bs[which(P2B$PFT == PFT[i])]
  b1Ht[i] <- P2B$b1Ht[which(P2B$PFT == PFT[i])]
  b2Ht[i] <- P2B$b2Ht[which(P2B$PFT == PFT[i])]
  qsw[i] <- P2B$qsw[which(P2B$PFT == PFT[i])]
}

#Dead biomass [kg/plant]
Bd <- b1Bs/C2B*surv.current$dbh**b2Bs

#Leaf biomass [kg/plant]
Bl <- b1Bl/C2B*surv.current$dbh**b2Bl

#Approximate height from dbh
ht <- hgt_ref + b1Ht*(1.0 - exp(b2Ht*surv.current$dbh))

#Sapwood biomass [kg/plant]
Bsw <- qsw*ht*Bl

#Above-ground biomass [kg/plant]
Ba <- agf_bs*Bd + Bl + Bsw

surv.current <- cbind(surv.current,PFT,Ba)

print(surv.current[1:10,]) #print first 10 lines to make sure stuff looks OK
row.names(surv.current) <- NULL 

####CALCULATE SPP-LEVEL VARIABLES####
#1. Loop over unique plots and aggregate unique species responses
#uniq.pts <- surv.current[!duplicated(surv.current[,c("LAT","LON")]),6:7]
uniq.pts <- surv.current[!duplicated(surv.current[,"PLOT"]),2]

for(p in 1:length(uniq.pts)){
#  lon.ind  <- which(surv.current$LON==uniq.pts[p,1]&surv.current$LAT==uniq.pts[p,2])
  plot.ind  <- which(surv.current$PLOT==uniq.pts[p])

  #grab plot-level attributes
  lon.tmp  <- rep(surv.current$LON[plot.ind[1]],times=length(unique(surv.current$spcd[plot.ind])))
  lat.tmp  <- rep(surv.current$LAT[plot.ind[1]],times=length(unique(surv.current$spcd[plot.ind])))
  pft.tmp  <- sort(unique(surv.current$PFT[plot.ind]))
  sta.tmp  <- rep(unique(surv.current$STATECD[plot.ind]),times=length(spp.tmp))
  
  dat.tmp <- data.frame(surv.current[plot.ind,])
  
  # Sum the biomass for each PFT within a plot
  Ba.sum  <- tapply(dat.tmp$Ba, dat.tmp$PFT, sum)

  avg.plot <- cbind(statecd=sta.tmp,lon=lon.tmp,lat=lat.tmp,pft=pft.tmp,Ba=Ba.sum)
  
  #store data
  if(p==1){
    all.avg <- avg.plot
  } else{
    all.avg <- rbind(all.avg,avg.plot)
  }
}

