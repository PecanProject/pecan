#### code to make data object for JAGS
#### from flat file AZ PIPO database

buildJAGSdataobject <- function(temp2, Tree2Tree=NULL, trunc.yr = 1976, rnd.subset = 100, standardize.cov = TRUE){

# helper function
# for standardizing covariates (from K. Holsinger)
standardize.vector <- function(x){
  x.bar <- mean(x, na.rm = TRUE)
  s.d. <- sd(x, na.rm = TRUE)
  return((x-x.bar)/s.d.)
}
  
# take a random subset of the Tree2Tree rows
  if(!is.null(Tree2Tree)){
    Tree2Tree <- Tree2Tree[sample(1:nrow(Tree2Tree), rnd.subset, replace=F),]  
  }


### get tree-ring measurements into a tree*year matrix
  
# first we deal with the tree with cores  
temp2$Widths <- as.character(temp2$Widths)
first.start.yr <- min(temp2$DateFirst, na.rm=T) #1719
last.DBH.yr.1 <- max(temp2$T2_MEASYR, na.rm=T) # 2010
last.DBH.yr.2 <- 1900 # number guaranteed to be smaller than last.DBH.yr.1
if(!is.null(Tree2Tree)){
  last.DBH.yr.2 <- max(Tree2Tree$T2_MEASYR, na.rm=T) # 2015
}
last.meas.yr <- max(last.DBH.yr.1, last.DBH.yr.2) # 2015
years <- seq(first.start.yr, last.meas.yr) # 1719:2015, length = 297
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

# trees without cores (tree-to-tree data, DBH remeasurements)
# this means just putting in the appropriate number of empty rows (NAs)
if(!is.null(Tree2Tree)){
  y.matrix.2 <- matrix(data=NA, nrow=nrow(Tree2Tree), ncol=length(years))
  
  y.matrix <- rbind(y.matrix, y.matrix.2)  
}


### get DBH measurements into a parallel tree*year matrix (mostly NA's, max #data points per tree = 2)

# trees with cores
z.matrix <- matrix(data=NA, nrow=nrow(temp2), ncol=length(years))
colnames(z.matrix) <- years
for (t in 1:nrow(temp2)) {
  # extract DBH (DIA) value if and only if it is not NA
  ifelse(!is.na(temp2$DIA[t]), DIA.T1<-temp2$DIA[t], DIA.T1<-NA) # extract time 1 DBH (in some cases, the only DBH measurement)
  YR.T1 <- temp2$PLOT_MEASYEAR[t] # associated measurement year
  z.matrix[t, which(colnames(z.matrix)==YR.T1)] <- DIA.T1 # put the DBH data in the right place (tree, year)
  
  ifelse(!is.na(temp2$T2_DIA[t]), DIA.T2<-temp2$T2_DIA[t], DIA.T2<-NA)  # time 2 DBH (only cases where there are two DBH measurements)
  YR.T2 <- temp2$T2_MEASYR[t] # associated measurement year
  z.matrix[t, which(colnames(z.matrix)==YR.T2)] <- DIA.T2
  
  ### eventually, use pith information: assign DBH=0 to pith date year-1 
#  ifelse(AZ.PIPO$PITH[t] != NA, z.matrix[t, which(colnames(z.matrix)==AZ.PIPO$PITH[t]-1)]<-0, z.matrix[t, which(colnames(z.matrix)==AZ.PIPO$PITH[t]-1)]<-NA)
}

# trees without cores (tree-to-tree data, DBH remeasurements)
if(!is.null(Tree2Tree)){
  z.matrix.2 <- matrix(data=NA, nrow=nrow(Tree2Tree), ncol=length(years))
  colnames(z.matrix.2) <- years
  for (t in 1:nrow(Tree2Tree)) { # each tree
    # extract DBH (DIA) value if and only if it is not NA
    ifelse(!is.na(Tree2Tree$T1_DIA[t]), DIA.T1<-Tree2Tree$T1_DIA[t], DIA.T1<-NA) # extract time 1 DBH (in some cases, the only DBH measurement)
    YR.T1 <- Tree2Tree$T1_MEASYR[t] # associated measurement year
    z.matrix.2[t, which(colnames(z.matrix.2)==YR.T1)] <- DIA.T1 # put the DBH data in the right place (tree, year)
    
    ifelse(!is.na(Tree2Tree$T2_DIA[t]), DIA.T2<-Tree2Tree$T2_DIA[t], DIA.T2<-NA)  # time 2 DBH (only cases where there are two DBH measurements)
    YR.T2 <- Tree2Tree$T2_MEASYR[t] # associated measurement year
    z.matrix.2[t, which(colnames(z.matrix.2)==YR.T2)] <- DIA.T2
  }
  
  # rbind the data for the trees with (~544) and without (~14,155) cores together
  z.matrix <- rbind(z.matrix, z.matrix.2)
}

### convert DBH measurements to cm (multiply by 2.54)
z.matrix <- z.matrix*2.54

### this is the line that restricts the analysis to the years trunc.yr:2015
index.last.start <- which(years==trunc.yr) # which(years==1966) # returns 248
y.small <- y.matrix[,index.last.start:ncol(y.matrix)]
z.small <- z.matrix[,index.last.start:ncol(z.matrix)]
years.small <- years[index.last.start:ncol(y.matrix)]

### covariate data

### RANDOM EFFECTS

### plot rnd effect (currently implemented at indiv level)
PLOT <- paste0(temp2$County, temp2$Plot) # should maybe use an underscore to avoid mistakes...but would jags choke?
if(!is.null(Tree2Tree)){
  PLOT2 <- paste0(Tree2Tree$COUNTYCD, Tree2Tree$T1_PLOT)
  PLOT <- c(PLOT, PLOT2)
}

### FIXED EFFECTS

### tree-level
### CR, CCLCD
#cov.data <- cbind(temp2$CR, temp2$CCLCD)
#colnames(cov.data) <- c("CR", "CCLCD")## give them column names
### should eventually use height ratio, or other variable that Justin says is more informative

### plot-level
### ELEV

### condition-level
### SICOND  
SICOND <- temp2$COND_SICOND
if(!is.null(Tree2Tree)){
  SICOND2 <- Tree2Tree$SICOND
  SICOND <- c(SICOND, SICOND2)
}
if(standardize.cov==TRUE){
SICOND <- standardize.vector(SICOND)
}

### SLOPE
#SLOPE <- temp2$COND_SLOPE # ranges as a high as 360
#if(!is.null(Tree2Tree)){
#  SLOPE2 <- Tree2Tree$SLOPE # max value = 78...different units??
#  SLOPE <- c(SLOPE, SLOPE2)
#}

### ASPECT ### 
#STAGE2 <- temp2$COND_SLOPE*cos(temp2$COND_ASPECT)
#if(!is.null(Tree2Tree)){
#  STAGE2.2 <- Tree2Tree$SLOPE*cos(Tree2Tree$ASPECT)
#  STAGE2 <- c(STAGE2, STAGE2.2)
#}

#STAGE3 <- temp2$COND_SLOPE*sin(temp2$COND_ASPECT)
#if(!is.null(Tree2Tree)){
#  STAGE3.2 <- Tree2Tree$SLOPE*sin(Tree2Tree$ASPECT)
#  STAGE3 <- c(STAGE3, STAGE3.2)
#}

### STDAGE
### SDI ## eventually should calculate <relative> SDI...observed SDI relative to maxSDI for dominant spp on plot (PIPO)
SDI <- temp2$SDI
if(!is.null(Tree2Tree)){
  SDI2 <- Tree2Tree$SDIc
  SDI <- c(SDI, SDI2)
}
if(standardize.cov == TRUE){
SDI <- standardize.vector(SDI)
}
### BA ## SDI and BA are tightly correlated, can't use both
cov.data <- data.frame(PLOT=PLOT, SICOND=SICOND, SDI=SDI)
#cov.data <- cbind(cov.data, SICOND, SDI)


### plot- and year-specific covariates
### i.e., 36 PRISM data matrices (tree*year)...one for each month*3 variables (Tmax, Tmin, ppt)
### just gonna do 24 climate variables (Tmax and Ppt)

PRISM.years <- seq(from=1895, to=2015) #length = 121
index.start.climate <- which(PRISM.years == trunc.yr)
index.end.climate <- index.start.climate+(last.meas.yr-trunc.yr)
PRISM.ncol <- (last.meas.yr-trunc.yr)+1 

# first, build an object that has the PRISM strings for both trees with and without cores
climate.names <- c("PPTJan", "PPTFeb", "PPTMar", "PPTApr", "PPTMay", "PPTJun", "PPTJul", "PPTAug", "PPTSep", "PPTOct", "PPTNov", "PPTDec",
#                   "TMINJan", "TMINFeb", "TMINMar", "TMINApr", "TMINMay", "TMINJun", "TMINJul", "TMINAug", "TMINSep", "TMINOct", "TMINNov", "TMINDec",
                   "TMAXJan", "TMAXFeb", "TMAXMar", "TMAXApr", "TMAXMay", "TMAXJun", "TMAXJul", "TMAXAug", "TMAXSep", "TMAXOct", "TMAXNov", "TMAXDec")
PRISM.strings <- temp2[climate.names]
if(!is.null(Tree2Tree)){
  PRISM.strings2 <- Tree2Tree[climate.names]
  PRISM.strings <- rbind(PRISM.strings, PRISM.strings2)
}

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
wintP.NovAug <- (time_data$PPTNov + time_data$PPTDec + time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar + time_data$PPTApr + time_data$PPTMay + time_data$PPTJun + time_data$PPTJul + time_data$PPTAug)
wintP.wateryr <- (time_data$PPTSep + time_data$PPTOct + time_data$PPTNov + time_data$PPTDec + time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar + time_data$PPTApr + time_data$PPTMay + time_data$PPTJun + time_data$PPTJul + time_data$PPTAug)
wintP.NM <- (time_data$PPTNov + time_data$PPTDec + time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar)
wintP.JJ <- (time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar + time_data$PPTApr + time_data$PPTMay + time_data$PPTJun + time_data$PPTJul)
time_data$wintP.NovAug <- wintP.NovAug
time_data$wintP.wateryr <- wintP.wateryr
time_data$wintP.NM <- wintP.NM
time_data$wintP.JJ <- wintP.JJ
# seasonal Tmax variables
tmax.fallspr <- (time_data$TMAXAug + time_data$TMAXSep + time_data$TMAXOct + time_data$TMAXMay + time_data$TMAXJun + time_data$TMAXJul)/6
tmax.JanA <- (time_data$TMAXJan + time_data$TMAXFeb + time_data$TMAXMar + time_data$TMAXApr + time_data$TMAXMay + time_data$TMAXJun + time_data$TMAXJul + time_data$TMAXAug)/8
tmax.MJul <- (time_data$TMAXMay + time_data$TMAXJun + time_data$TMAXJul)/3
time_data$tmax.fallspr <- tmax.fallspr
time_data$tmax.JanA <- tmax.JanA
time_data$tmax.MJul <- tmax.MJul

# standardize climate data
if(standardize.cov == TRUE){
for(c in 1:length(time_data)){
  time_data[[c]] <- standardize.vector(time_data[[c]])
  } 
}

## build data object for JAGS
data = list(y = y.small, 
            z = z.small,
            ni = nrow(y.small), nt = ncol(y.small), 
            x_ic = 1, tau_ic = 1e-04,
            a_dbh = 16, r_dbh = 8, 
            a_inc = 0.001, r_inc = 1, 
            a_add = 1, r_add = 1, 
            time = years.small)

## state variable initial condition
## must define an initial (biologically-reasonable) value for DBH for MCMC chains to start at (every tree, every year)
z0 <- matrix(data=NA, nrow=nrow(y.small), ncol=ncol(y.small))

# first deal with the trees with cores
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

  ### note that some of these values (z0) are negative,
  ### some trees were small at their final size, and pith dates are as late as 1972
  ### replace negative values by the growth series implied by growing from zero to DIA
#  ifelse(z0[t,1]<0, 
#         z0[t,1:length(temp.growth)] <- cumsum(rep(DIA.T1[t]/length(temp.growth), length(temp.growth))), 
#         z0[t,1:length(temp.growth)] <- DIA.T1[t] + temp.growth2)
}

# now deal with trees without cores
if(!is.null(Tree2Tree)){
  global.ave.inc <- mean(ave.ring) # 0.363... ~0.18 cm for average ring
  for (t in 1:nrow(Tree2Tree)) {
    # shrink tree backwards from T1 DBH, using global ave diameter increment derived from tree rings
    first.DBH <- Tree2Tree$T1_DIA[t]*2.54 # extract time 1 DBH
    temp.growth <- rep(global.ave.inc, times=(Tree2Tree$T1_MEASYR[t]-trunc.yr))
    temp.growth2 <- c(-rev(cumsum(rev(temp.growth))),0) # add a zero at the end of this so that z0 in MEASYR = DIA
    z0[(nrow(temp2)+t),1:length(temp.growth2)] <- first.DBH + temp.growth2
    
    ### grow tree forward from T1_DIA to year 2015
    temp.forward <- rep(global.ave.inc, times=(last.meas.yr-Tree2Tree$T1_MEASYR[t]))
    z0[(nrow(temp2)+t), (length(temp.growth2)+1):length(years.small)] <- first.DBH + cumsum(temp.forward)
  }
}

colnames(z0) <- years.small

return.list <- list(data=data,
                    z0=z0,
                    cov.data=cov.data,
                    time_data=time_data)
return(return.list)
}