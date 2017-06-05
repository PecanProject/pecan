#### code to make data object for JAGS
#### from flat file AZ PIPO database

buildJAGSdataobject <- function(temp2){

### get tree-ring measurements into a tree*year matrix
temp2$Widths <- as.character(temp2$Widths)
first.start.yr <- min(temp2$DateBegin, na.rm=T)
last.meas.yr <- max(temp2$T2_MEASYR, na.rm=T)
years <- seq(first.start.yr, last.meas.yr)
y.matrix <- matrix(data=NA, nrow=nrow(temp2), ncol=length(years))
colnames(y.matrix) <- years
for (t in 1:nrow(temp2)) {
  width.string <- temp2$Widths[t]
  width.vector <- as.integer(unlist(strsplit(x = width.string, split = ",")))
  start.column <- which(years == temp2$DateBegin[t])
  end.column <- which(years == temp2$DateEnd[t])
  width.subset <- (end.column - start.column) + 1 # how long should the vectors of TR measurements be?
  width.vector <- width.vector[1:width.subset] # truncate widths vector to this length (get rid of extra zeros at the end)
  width.vector <- width.vector*0.0001*2 # convert micrometers to cm and multiply by 2 to turn radial increment into diameter increment
  y.matrix[t, c(start.column:end.column)] <- width.vector # put that vector in y.matrix at the right start year:end year
}

### get DBH measurements into a parallel tree*year matrix (mostly NA's, max #data points per tree = 2)
z.matrix <- matrix(data=NA, nrow=nrow(temp2), ncol=length(years))
colnames(z.matrix) <- years
for (t in 1:nrow(temp2)) {
  # extract DBH (DIA) value if and only if it is not NA
  ifelse(!is.na(temp2$DIA[t]), DIA.T1<-temp2$DIA[t], DIA.T1<-NA) # extract time 1 DBH (in some cases, the only DBH measurement)
  YR.T1 <- temp2$DateEnd[t]+1 # associated measurement year
  z.matrix[t, which(colnames(z.matrix)==YR.T1)] <- DIA.T1 # put the DBH data in the right place (tree, year)
  
  ifelse(!is.na(temp2$T2_DIA[t]), DIA.T2<-temp2$T2_DIA[t], DIA.T2<-NA)  # time 2 DBH (only cases where there are two DBH measurements)
  YR.T2 <- temp2$T2_MEASYR[t] # associated measurement year
  z.matrix[t, which(colnames(z.matrix)==YR.T2)] <- DIA.T2
  
  ### eventually, use pith information: assign DBH=0 to pith date year-1 
#  ifelse(AZ.PIPO$PITH[t] != NA, z.matrix[t, which(colnames(z.matrix)==AZ.PIPO$PITH[t]-1)]<-0, z.matrix[t, which(colnames(z.matrix)==AZ.PIPO$PITH[t]-1)]<-NA)
}

### convert DBH measurements to cm (multiply by 2.54)
z.matrix <- z.matrix*2.54

### for the moment, we'll restrict the analysis to the years 1970:2010
# currently, 1970 is the last DateBegin among all the cores...
last.start.yr <- max(temp2$DateBegin)
index.last.start <- which(years==last.start.yr) # which(years==1970) # returns 186
y.small <- y.matrix[,index.last.start:ncol(y.matrix)]
z.small <- z.matrix[,index.last.start:ncol(z.matrix)]
years.small <- years[index.last.start:ncol(y.matrix)]

### covariate data

### RANDOM EFFECTS

### plot rnd effect (currently implemented at indiv level)
PLOT <- paste0(temp2$CountyNo, temp2$PlotNo)

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
SICOND <- temp2$SICOND
### SLOPE
### ASPECT ### what's the scale for FIA's aspect data? do they need to be converted to N-S spectrum?
### STDAGE
### SDI ## eventually should calculate <relative> SDI...observed SDI relative to maxSDI for dominant spp on plot (PIPO)
SDI <- temp2$SDI
### BA ## SDI and BA are tightly correlated, can't use both
cov.data <- data.frame(PLOT=PLOT, SICOND=SICOND, SDI=SDI)
#cov.data <- cbind(cov.data, SICOND, SDI)


### plot- and year-specific covariates
### i.e., 36 PRISM data matrices (tree*year)...one for each month*3 variables (Tmax, Tmin, ppt)
### just gonna do 24 climate variables (Tmax and Ppt)
PRISM.years <- seq(from=1895, to=2015)
index.start.climate <- which(PRISM.years == last.start.yr)
index.end.climate <- index.start.climate+(last.meas.yr-last.start.yr)
PRISM.ncol <- (last.meas.yr-last.start.yr)+1 

# get climate variable names
#yrt.clim.var <- colnames(AZ.PIPO[110]) # just tmax_Jun; climate data from year t
#yrt_1.clim.var <- c(colnames(AZ.PIPO[140])) # just ppt_Dec; climate data from year t-1

yrt.clim.var <- colnames(AZ.PIPO[105:112]) # Jan-Aug tmax columns
yrt.clim.var <- c(yrt.clim.var, colnames(AZ.PIPO[129:136])) # add Jan-Aug ppt
yrt_1.clim.var <- c(colnames(AZ.PIPO[113:116])) # Sept-Dec tmax
yrt_1.clim.var <- c(yrt_1.clim.var, colnames(AZ.PIPO[137:140])) # add Sept-Dec ppt

names.clim.var <- c(yrt.clim.var, yrt_1.clim.var)
time_data <- list() # make empty list

counter <- 1
for (i in yrt.clim.var) { ## this loop deals with the climate variables taken from year t, i.e., current Jan-Aug
    tmp.matrix <- matrix(nrow = nrow(temp2), ncol = PRISM.ncol)
  for (j in 1:nrow(temp2)) {
    tmp <- as.numeric(unlist(strsplit(temp2[j,i], split = ",")))
    tmp <- tmp[index.start.climate:index.end.climate]# subset tmp to only contain years of interest (1972-2010)
    tmp.matrix[j,] <- tmp
  }
  # Put the matrix into the list
  time_data[[counter]] <- tmp.matrix # use 1 here instead of counter for a single climate variable
  counter <- counter + 1
}

for (i in yrt_1.clim.var) { ## this loop deals with the climate variables taken from year t-1, i.e., previous Sept-Dec
  tmp.matrix <- matrix(nrow = nrow(temp2), ncol = PRISM.ncol)
  for (j in 1:nrow(temp2)) {
    tmp <- as.numeric(unlist(strsplit(temp2[j,i], split = ",")))
    tmp <- tmp[(index.start.climate-1):(index.end.climate-1)]# subset tmp to only contain years of interest (1971-2009)
    tmp.matrix[j,] <- tmp
  }
  # Put the matrix into the list
  time_data[[counter]] <- tmp.matrix # use 2 here instead of counter for a single climate variable
    counter <- counter + 1
}

# Assign the climate variable names to the list (named list)
names(time_data) <- names.clim.var


## build data object for JAGS
n <- nrow(y.small)
data = list(y = y.small[1:n, ], 
            z = z.small[1:n, ],
            ni = nrow(y.small), nt = ncol(y.small), 
            x_ic = 1, tau_ic = 1e-04,
            a_dbh = 16, r_dbh = 8, 
            a_inc = 0.001, r_inc = 1, 
            a_add = 1, r_add = 1, 
            time = years.small)

## state variable initial condition
####### issues:
### 1. NA's at the end of the tree-ring series...need to use the available measurements (and trees were cored in different years)
### 2. z0 is not the same for all trees...DateBegin varies from 1785 to 1972, so we'll truncate to 1972 for the moment
DIA.T1 <- vector(mode="numeric", length=nrow(temp2))
ave.ring <- vector(mode="numeric", length=nrow(temp2))
z0 <- matrix(data=NA, nrow=nrow(y.small), ncol=ncol(y.small))
for (t in 1:nrow(temp2)) {
  ### shrink tree backward: subtract the cumulative tree-ring-derived diameter increments (in y.matrix) from DIA
  ifelse(!is.na(temp2$DIA[t]), DIA.T1[t]<-temp2$DIA[t]*2.54, DIA.T1[t]<-NA) # extract time 1 DBH (in some cases, the only DBH measurement)
  # extract tree-ring data from year 1972:end series
  end.col <- which(years==temp2$DateEnd[t])
  temp.growth <- y.matrix[t,index.last.start:end.col] # which(years==1972) # returns 188
  temp.growth2 <- -rev(cumsum(rev(temp.growth)))
  z0[t,1:length(temp.growth)] <- DIA.T1[t] + temp.growth2 # note that this is one year off where DateEnd = MEASYEAR-1

  ### grow tree forward: find average ring-width per tree and add cumulative from DIA to year 2010
  ave.ring[t] <- mean(temp.growth)
  ave.growth <- rep(ave.ring[t], times=(2010-temp2$DateEnd[t]))
  z0[t, (length(temp.growth)+1):length(years.small)] <- DIA.T1[t] + cumsum(ave.growth)

  ### note that some of these values (z0) are negative,
  ### some trees were small at their final size, and pith dates are as late as 1972
  ### replace negative values by the growth series implied by growing from zero to DIA
  ifelse(z0[t,1]<0, 
         z0[t,1:length(temp.growth)] <- cumsum(rep(DIA.T1[t]/length(temp.growth), length(temp.growth))), 
         z0[t,1:length(temp.growth)] <- DIA.T1[t] + temp.growth2)
  }


return.list <- list(data=data,
                    z0=z0,
                    cov.data=cov.data,
                    time_data=time_data)
return(return.list)
}