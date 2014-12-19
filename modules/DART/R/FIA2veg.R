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
years    <- c(2000,2005)       #find surveys closest to present year
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

for(r in 1:length(years)){

year <- years[r]

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


# Area of plot [m2]
A <- 3.14*400

#lon,lat bounds
lat     <- c(35.25,35.75)   
lon     <- c(-79.75,-79.25) 

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

PFT <- vector()

for(i in 1:length(surv.current$spcd)){
  PFT[i] <- F2P$PFT[which(F2P$SPCD == surv.current$spcd[i])]
}

surv.current <- cbind(surv.current,PFT)

print(surv.current[1:10,]) #print first 10 lines to make sure stuff looks OK
#row.names(surv.current) <- NULL 

hite <- vector()
bd <- vector()
ba <- vector()
avgRG <- vector()
dens <- vector()

patch <- vector()
cohort <- vector()
dbh <- surv.current$dbh
pft <- surv.current$PFT

# Separate plots
if(r ==1){
     pts <- surv.current[!duplicated(surv.current[,"PLOT"]),2]
     uniq.pts <- pts[order(pts)]
}

for(p in 1:length(uniq.pts)){
  plot.ind <- which(surv.current$PLOT==uniq.pts[p])

  for(j in 1:length(plot.ind)){
   patch[plot.ind[j]] <- p
   cohort[plot.ind[j]] <- j
  }
}

o.na <- order(patch)
patch.na <- patch[o.na]
cohort.na <- cohort[o.na]
dbh.na <- dbh[o.na]
pft.na <- pft[o.na]

o <- order(na.omit(patch.na))
patch <- patch.na[o]
cohort <- cohort.na[o]
dbh <- dbh.na[o]
pft <- pft.na[o]

hite[1:length(dbh)] <- 0.
bd[1:length(dbh)] <- 0.
ba[1:length(dbh)] <- 0.
avgRG[1:length(dbh)] <- -999
dens[1:length(dbh)] <- 1./A
}

css <- cbind(year,patch,cohort,dbh,hite,pft,dens,bd,ba,avgRG)

fname <- 'Dk.2k5.lat35.5lon-79.5.css'

write.csv(css,fname,row.names=FALSE)

site <- 1
p_count <- seq(1,max(patch))
dst <- 1
age <- 70
area <- 1/max(patch)
water <- 0.1
fsc <- 5.5
stsc <- 2.15
stsl <- 2.15
ssc <- 0.003
psc <- 0
msn <- 0.16
fsn <- 1.14

pss <- cbind(site,year,p_count,dst,age,area,water,fsc,stsc,stsl,ssc,psc,msn,fsn)

pname <- 'Dk.2k5.lat35.5lon-79.5.pss'

write.csv(pss,pname,row.names=FALSE)