### glmm of tree-ring data
### reality check against hidden process model
library(tidyr)

setwd("C:/Users/mekevans/Documents/Cdrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")

trunc.yr = 1966

AZ.PIPO <- read.delim("AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

### merge together three diameter columns
AZ.PIPO$DIA <- ifelse(is.na(AZ.PIPO$TREE_DIA), AZ.PIPO$SITETREE_DIA, AZ.PIPO$TREE_DIA) # combine together FIADB diameter measurements for trees and site trees
AZ.PIPO$DIA <- ifelse(is.na(AZ.PIPO$DIA), AZ.PIPO$DBH, AZ.PIPO$DIA) # replace missing data with DBH recorded from core mounts (DBH)

### filter out those cases where DIA is NA...no size information
AZ.PIPO <- AZ.PIPO[!is.na(AZ.PIPO$DIA),] # 793 trees
### filter out cases where covariate data are missing (SICOND and SDI)
AZ.PIPO <- AZ.PIPO[!is.na(AZ.PIPO$COND_SICOND),] # 643 trees...~150 missing SICOND. Justin suggests they may be PJ (will never have SICOND)
AZ.PIPO <- AZ.PIPO[!is.na(AZ.PIPO$SDI),] # 641

### problem: in minority of cases, the difference between MEASYEAR and DateEnd is other than 0 or 1
### filter out those cases
temp1 <- AZ.PIPO[AZ.PIPO$PLOT_MEASYEAR-AZ.PIPO$DateEnd<2,] # 544 trees
temp2 <- temp1[temp1$PLOT_MEASYEAR-temp1$DateEnd>-1,] # no change


temp2$Widths <- as.character(temp2$Widths)
first.start.yr <- min(temp2$DateFirst, na.rm=T) #1719
last.DBH.yr.1 <- max(temp2$T2_MEASYR, na.rm=T) # 2010
last.DBH.yr.2 <- 1900 # number guaranteed to be smaller than last.DBH.yr.1
last.meas.yr <- max(last.DBH.yr.1, last.DBH.yr.2) # 2015
years <- seq(first.start.yr, last.meas.yr) # 1719:2010, length = 292
y.matrix <- matrix(data=NA, nrow=nrow(temp2), ncol=length(years)) #tree ring measurements go in y.matrix
colnames(y.matrix) <- years
for (t in 1:nrow(temp2)) {
  width.string <- temp2$Widths[t]
  width.vector <- as.numeric(unlist(strsplit(x = width.string, split = ",")))
  start.column <- which(years == temp2$DateFirst[t])
  end.column <- which(years == temp2$DateEnd[t])
  width.subset <- (end.column - start.column) + 1 # how long should the vectors of TR measurements be?
  width.vector <- width.vector[1:width.subset] # truncate widths vector to this length (get rid of extra zeros at the end)
  width.vector <- width.vector*0.1*2 # convert to cm and multiply by 2 to turn radial increment into diameter increment (be careful about what units Tellervo spits out)
  y.matrix[t, c(start.column:end.column)] <- width.vector # put that vector in y.matrix at the right start year:end year
}


index.last.start <- which(years==trunc.yr) # which(years==1966) # returns 248
y.small <- y.matrix[,index.last.start:ncol(y.matrix)]
z.small <- z.matrix[,index.last.start:ncol(z.matrix)]
years.small <- years[index.last.start:ncol(y.matrix)]

z0 <- matrix(data=NA, nrow=nrow(y.small), ncol=ncol(y.small))

DIA.T1 <- vector(mode="numeric", length=nrow(temp2))
ave.ring <- vector(mode="numeric", length=nrow(temp2))
for (t in 1:nrow(temp2)) {
  ### shrink tree backward: subtract the cumulative tree-ring-derived diameter increments (in y.matrix) from DIA
  ifelse(!is.na(temp2$DIA[t]), DIA.T1[t]<-temp2$DIA[t]*2.54, DIA.T1[t]<-NA) # extract time 1 DBH (in some cases, the only DBH measurement)
  # extract tree-ring data from trunc.yr:end series
  end.col <- which(years==temp2$DateEnd[t])
  temp.growth <- y.matrix[t,(index.last.start+1):end.col] # which(years==1966) # returns 248
  # add rep(ave.ring) to any NA's at the beginning of the tree-ring time series
  ave.ring[t] <- mean(temp.growth, na.rm=T)
  temp.growth[is.na(temp.growth)]<-ave.ring[t]
  temp.growth2 <- c(-rev(cumsum(rev(temp.growth))),0) # add a zero at the end of this so that z0 in MEASYR = DIA
  z0[t,1:length(temp.growth2)] <- DIA.T1[t] + temp.growth2 # note that this is one year off where DateEnd = MEASYEAR-1
  
  ### grow tree forward: find short-term average ring-width per tree and add cumulative from DIA to year 2010
  ave.growth <- rep(ave.ring[t], times=(last.meas.yr-temp2$DateEnd[t]))
  z0[t, (length(temp.growth2)+1):length(years.small)] <- DIA.T1[t] + cumsum(ave.growth)
}


PLOT <- paste0(temp2$County, temp2$Plot) # should maybe use an underscore to avoid mistakes...but would jags choke?
SICOND <- temp2$COND_SICOND
SDI <- temp2$SDI

PRISM.years <- seq(from=1895, to=2015) #length = 121
index.start.climate <- which(PRISM.years == trunc.yr)
index.end.climate <- index.start.climate+(last.meas.yr-trunc.yr)
PRISM.ncol <- (last.meas.yr-trunc.yr)+1 

# first, build an object that has the PRISM strings for both trees with and without cores
climate.names <- c("PPTJan", "PPTFeb", "PPTMar", "PPTApr", "PPTMay", "PPTJun", "PPTJul", "PPTAug", "PPTSep", "PPTOct", "PPTNov", "PPTDec",
                   #                   "TMINJan", "TMINFeb", "TMINMar", "TMINApr", "TMINMay", "TMINJun", "TMINJul", "TMINAug", "TMINSep", "TMINOct", "TMINNov", "TMINDec",
                   "TMAXJan", "TMAXFeb", "TMAXMar", "TMAXApr", "TMAXMay", "TMAXJun", "TMAXJul", "TMAXAug", "TMAXSep", "TMAXOct", "TMAXNov", "TMAXDec")
PRISM.strings <- temp2[climate.names]

# which climate variables should be taken from year t and which from year t-1?
yrt.clim.var <- c("PPTJan", "PPTFeb", "PPTMar", "PPTApr", "PPTMay", "PPTJun", "PPTJul", "PPTAug", "TMAXJan", "TMAXFeb", "TMAXMar", "TMAXApr", "TMAXMay", "TMAXJun", "TMAXJul", "TMAXAug")
yrt_1.clim.var <- c("PPTSep", "PPTOct", "PPTNov", "PPTDec", "TMAXSep", "TMAXOct", "TMAXNov", "TMAXDec")
names.clim.var <- c(yrt.clim.var, yrt_1.clim.var)

time_data <- list() # make empty list; this list will have 12 items (climate variables), each with rows as trees and years as columns

counter <- 1
for (i in yrt.clim.var) { ## this loop deals with the climate variables taken from year t, i.e., current Jan-Aug
  tmp.matrix <- matrix(nrow = nrow(PRISM.strings), ncol = PRISM.ncol) # should have 544 + 14,155 rows
  for (j in 1:nrow(PRISM.strings)) {
    tmp <- as.numeric(unlist(strsplit(PRISM.strings[j,i], split = ",")))
    tmp <- tmp[index.start.climate:index.end.climate]# subset tmp to only contain years of interest (trunc.yr-2015)
    tmp.matrix[j,] <- tmp
  }
  # Put the matrix into the list
  time_data[[counter]] <- tmp.matrix # use 1 here instead of counter for a single climate variable
  counter <- counter + 1
}

for (i in yrt_1.clim.var) { ## this loop deals with the climate variables taken from year t-1, i.e., previous Sept-Dec
  tmp.matrix <- matrix(nrow = nrow(PRISM.strings), ncol = PRISM.ncol)
  for (j in 1:nrow(PRISM.strings)) {
    tmp <- as.numeric(unlist(strsplit(PRISM.strings[j,i], split = ",")))
    tmp <- tmp[(index.start.climate-1):(index.end.climate-1)]# subset tmp to only contain years of interest (1971-2009)
    tmp.matrix[j,] <- tmp
  }
  time_data[[counter]] <- tmp.matrix # use 2 here instead of counter for a single climate variable
  counter <- counter + 1
}

# Assign the climate variable names to the list (named list)
names(time_data) <- names.clim.var

# calculate winter precip and add to the list time_data (rows are trees, columns are years)
wintP.wateryr <- (time_data$PPTSep + time_data$PPTOct + time_data$PPTNov + time_data$PPTDec + time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar + time_data$PPTApr + time_data$PPTMay + time_data$PPTJun + time_data$PPTJul + time_data$PPTAug)
wintP.NM <- (time_data$PPTNov + time_data$PPTDec + time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar)
wintP.JJ <- (time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar + time_data$PPTApr + time_data$PPTMay + time_data$PPTJun + time_data$PPTJul)
time_data$wintP.wateryr <- wintP.wateryr
time_data$wintP.NM <- wintP.NM
time_data$wintP.JJ <- wintP.JJ
# seasonal Tmax variables
tmax.JanA <- (time_data$TMAXJan + time_data$TMAXFeb + time_data$TMAXMar + time_data$TMAXApr + time_data$TMAXMay + time_data$TMAXJun + time_data$TMAXJul + time_data$TMAXAug)/8
tmax.MJul <- (time_data$TMAXMay + time_data$TMAXJun + time_data$TMAXJul)/3
time_data$tmax.JanA <- tmax.JanA
time_data$tmax.MJul <- tmax.MJul

y.long <- gather(data = as.data.frame(y.small), key = year, value = y.small.value)
z.long <- gather(data = as.data.frame(z0), key = year, value = z0.value)
wintP.long <- gather(data = as.data.frame(time_data$wintP.JJ), key = year, value = wintP.JJ.value)

glmm.data <- data.frame(tree = rep(x = c(1:nrow(temp2)), times = length(years.small)), 
                        year = rep(trunc.yr:last.meas.yr, each = nrow(temp2)),
                        incr = y.long$y.small.value,
                        dbh = z.long$z0.value,
                        sdi = rep(SDI, length(years.small)),
                        si = rep(SICOND, length(years.small)),
                        wintP = wintP.long$wintP.JJ.value)
