#### code to make data object for JAGS
#### from flat file AZ PIPO database

buildJAGSdataobject <- function(temp2, Tree2Tree=NULL, stage.2 = FALSE, forecast =FALSE, YEARDISTURBED = FALSE,trunc.yr = 1976, rnd.subset = 100, standardize.cov = TRUE){
  
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
    
    if(stage.2 == TRUE){ # if we are extracting for the 2nd stage model use only those with dbh measurements
      y.matrix <-  y.matrix.2
    }
    
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
    
    
    if(stage.2 == TRUE){ # if we are extracting for the 2nd stage model use only those with dbh measurements
      z.matrix <-  z.matrix.2
    }
    
    
  }
  
  ### convert DBH measurements to cm (multiply by 2.54)
  z.matrix <- z.matrix*2.54
  #z.matrix.2 <- z.matrix*2.54 # only the trees without cores
  
  ### this is the line that restricts the analysis to the years trunc.yr:2015
  index.last.start <- which(years==trunc.yr) # which(years==1966) # returns 248
  y.small <- y.matrix[,index.last.start:ncol(y.matrix)]
  z.small <- z.matrix[,index.last.start:ncol(z.matrix)]
  years.small <- years[index.last.start:ncol(y.matrix)]
  
  startyr<-rep(NA,nrow(z.small))
  endyr<-rep(NA,nrow(z.small))
  for(i in 1:nrow(z.small)){
    startyr[i]<-which(complete.cases(z.small[i,]))[1]
    endyr[i]<-ifelse(!is.na(which(complete.cases(z.small[i,]))[2]),which(complete.cases(z.small[i,]))[2],45)
  }
  
  startyr[1:nrow(y.small)]<-1
  endyr[1:nrow(y.small)]<-45
  sum(complete.cases(startyr))
  
  ### covariate data
  
  ### plot- and year-specific covariates
  ### i.e., 36 PRISM data matrices (tree*year)...one for each month*3 variables (Tmax, Tmin, ppt)
  ### just gonna do 24 climate variables (Tmax and Ppt)
  
  PRISM.years <- seq(from=1895, to=2010) #length = 121
  index.start.climate <- which(PRISM.years == trunc.yr)
  index.end.climate <- index.start.climate+(last.meas.yr-trunc.yr)
  PRISM.ncol <- (last.meas.yr-trunc.yr)+1 
  
  # first, build an object that has the PRISM strings for both trees with and without cores
  climate.names <- c("PPTJan", "PPTFeb", "PPTMar", "PPTApr", "PPTMay", "PPTJun", "PPTJul", "PPTAug", "PPTSep", "PPTOct", "PPTNov", "PPTDec",
                     #                   "TMINJan", "TMINFeb", "TMINMar", "TMINApr", "TMINMay", "TMINJun", "TMINJul", "TMINAug", "TMINSep", "TMINOct", "TMINNov", "TMINJan",
                     "TMAXJan", "TMAXFeb", "TMAXMar", "TMAXApr", "TMAXMay", "TMAXJun", "TMAXJul", "TMAXAug", "TMAXSep", "TMAXOct", "TMAXNov", "TMAXDec")
  PRISM.strings <- temp2[climate.names]
  if(!is.null(Tree2Tree)){
    PRISM.strings2 <- Tree2Tree[climate.names]
    PRISM.strings <- rbind(PRISM.strings, PRISM.strings2)
    if(stage.2 == TRUE){
      PRISM.strings <- PRISM.strings2
    }
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
      tmp <- as.numeric(unlist(strsplit(as.character(as.character(PRISM.strings[j,i])), split = ",")))
      tmp <- tmp[index.start.climate:index.end.climate]# subset tmp to only contain years of interest (trunc.yr-2015)
      tmp.matrix[j,] <- tmp
    }
    # Put the matrix into the list
    time_data[[counter]] <- tmp.matrix # use 1 here instead of counter for a single climate variable
    counter <- counter + 1
  }
  
  for (i in yrt_1.clim.var) { ## this loop deals with the climate variables taken from year t-1, i.e., previous Sept-Jan
    tmp.matrix <- matrix(nrow = nrow(PRISM.strings), ncol = PRISM.ncol)
    for (j in 1:nrow(PRISM.strings)) {
      tmp <- as.numeric(unlist(strsplit(as.character(PRISM.strings[j,i]), split = ",")))
      tmp <- tmp[(index.start.climate-1):(index.end.climate-1)]# subset tmp to only contain years of interest (1971-2009)
      tmp.matrix[j,] <- tmp
    }
    time_data[[counter]] <- tmp.matrix # use 2 here instead of counter for a single climate variable
    counter <- counter + 1
  }
  
  
  
  
  
  # Assign the climate variable names to the list (named list)
  names(time_data) <- names.clim.var
  
  time_data$PPT
  # if we are forecasting to validate the dbh measurments years 2010 - 2018, we need to add the new climate data:
  if(forecast == TRUE){
    # need to read in the additional data
    if(stage.2 == FALSE){
      new.ppt <- readRDS("data/ppt_for_validation_2010_2018.rds")
      new.tmax <- readRDS("data/tmax_for_validation_2010_2018.rds")
      
      
      new.ppt <- new.ppt[!duplicated(new.ppt),]
      
      
      # reorder to have columns of PPTJAN with columns from 2010:2018:
      
      ppt.red <- new.ppt %>% filter (month %in% 1)   %>% select(keynew, year, month, value)
      PPTJan.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      PPTJan.n <- PPTJan.n[PPTJan.n$keynew %in% temp2$keynew, ]
      
      
      ppt.red <- new.ppt %>% filter (month %in% 2)   %>% select(keynew, year, month, value)
      PPTFeb.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 3)   %>% select(keynew, year, month, value)
      PPTMar.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 4)   %>% select(keynew, year, month, value)
      PPTApr.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 5)   %>% select(keynew, year, month, value)
      PPTMay.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 6)   %>% select(keynew, year, month, value)
      PPTJun.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 7)   %>% select(keynew, year, month, value)
      PPTJul.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 8)   %>% select(keynew, year, month, value)
      PPTAug.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 9)   %>% select(keynew, year, month, value)
      PPTSep.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 10)   %>% select(keynew, year, month, value)
      PPTOct.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 11)   %>% select(keynew, year, month, value)
      PPTNov.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 12)   %>% select(keynew, year, month, value)
      PPTDec.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      time_data2<- list()
      
      time_data2$PPTJan<- PPTJan.n[match(temp2$keynew, PPTJan.n$keynew),]
      time_data2$PPTFeb<- PPTFeb.n[match(temp2$keynew, PPTFeb.n$keynew),]
      time_data2$PPTMar<- PPTMar.n[match(temp2$keynew, PPTMar.n$keynew),]
      time_data2$PPTApr<- PPTApr.n[match(temp2$keynew, PPTApr.n$keynew),]
      time_data2$PPTMay<- PPTMay.n[match(temp2$keynew, PPTMay.n$keynew),]
      time_data2$PPTJun<- PPTJun.n[match(temp2$keynew, PPTJun.n$keynew),]
      time_data2$PPTJul<- PPTJul.n[match(temp2$keynew, PPTJul.n$keynew),]
      time_data2$PPTAug<- PPTAug.n[match(temp2$keynew, PPTAug.n$keynew),]
      time_data2$PPTSep<- PPTSep.n[match(temp2$keynew, PPTSep.n$keynew),]
      time_data2$PPTOct<- PPTOct.n[match(temp2$keynew, PPTOct.n$keynew),]
      time_data2$PPTNov<- PPTNov.n[match(temp2$keynew, PPTNov.n$keynew),]
      time_data2$PPTDec<- PPTDec.n[match(temp2$keynew, PPTDec.n$keynew),]
      
      
      #-------------for Tmax --------
      
      new.tmax <- new.tmax[!duplicated(new.tmax),]
      
      
      # reorder to have columns of TMAXJAN with columns from 2010:2018:
      tmax.red <- new.tmax %>% filter (month %in% 1)   %>% select(keynew, year, month, value)
      TMAXJan.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      TMAXJan.n <- TMAXJan.n[TMAXJan.n$keynew %in% temp2$keynew, ]
      
      
      tmax.red <- new.tmax %>% filter (month %in% 2)   %>% select(keynew, year, month, value)
      TMAXFeb.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 3)   %>% select(keynew, year, month, value)
      TMAXMar.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 4)   %>% select(keynew, year, month, value)
      TMAXApr.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 5)   %>% select(keynew, year, month, value)
      TMAXMay.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 6)   %>% select(keynew, year, month, value)
      TMAXJun.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 7)   %>% select(keynew, year, month, value)
      TMAXJul.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 8)   %>% select(keynew, year, month, value)
      TMAXAug.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 9)   %>% select(keynew, year, month, value)
      TMAXSep.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 10)   %>% select(keynew, year, month, value)
      TMAXOct.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 11)   %>% select(keynew, year, month, value)
      TMAXNov.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 12)   %>% select(keynew, year, month, value)
      TMAXDec.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      # need to reorder the dataframe by the keynew in 
      
      
      
      time_data2$TMAXJan<- TMAXJan.n[match(temp2$keynew, TMAXJan.n$keynew),]
      time_data2$TMAXFeb<- TMAXFeb.n[match(temp2$keynew, TMAXFeb.n$keynew),]
      time_data2$TMAXMar<- TMAXMar.n[match(temp2$keynew, TMAXMar.n$keynew),]
      time_data2$TMAXApr<- TMAXApr.n[match(temp2$keynew, TMAXApr.n$keynew),]
      time_data2$TMAXMay<- TMAXMay.n[match(temp2$keynew, TMAXMay.n$keynew),]
      time_data2$TMAXJun<- TMAXJun.n[match(temp2$keynew, TMAXJun.n$keynew),]
      time_data2$TMAXJul<- TMAXJul.n[match(temp2$keynew, TMAXJul.n$keynew),]
      time_data2$TMAXAug<- TMAXAug.n[match(temp2$keynew, TMAXAug.n$keynew),]
      time_data2$TMAXSep<- TMAXSep.n[match(temp2$keynew, TMAXSep.n$keynew),]
      time_data2$TMAXOct<- TMAXOct.n[match(temp2$keynew, TMAXOct.n$keynew),]
      time_data2$TMAXNov<- TMAXNov.n[match(temp2$keynew, TMAXNov.n$keynew),]
      time_data2$TMAXDec<- TMAXDec.n[match(temp2$keynew, TMAXDec.n$keynew),]
      
    }else{
      # need to do a separate climate matching for tree2tree dataset because it does not have a keynew
      new.ppt <- readRDS("data/ppt_tree2tree_for_validation_2010_2018.rds")
      new.tmax <- readRDS("data/tmax_tree2tree_for_validation_2010_2018.rds")
      #myppt <- getURL('https://de.cyverse.org/dl/d/9E59BCB7-98C2-44F0-9E25-C162BD106465/ppt_tree2tree_for_validation_2010_2018.rds', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
      
      #newppt <- readRDS(textConnection(myppt))
      
      #mytmax <- getURL('http://de.cyverse.org/dl/d/9BB18028-0BE2-4591-8CC5-47CC40D36649/tmax_tree2tree_for_validation_2010_2018.rds', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
      
      #new.tmax <- readRDS(textConnection(mytmax))
      
      new.ppt <- new.ppt[!duplicated(new.ppt),]
      
      # create a keynew:
      new.ppt$keynew <- paste0(new.ppt$T1_PLOT, "_", new.ppt$T1_SUBP, "_" , new.ppt$T1_TREE, "_", new.ppt$T1_COUNTY)
      Tree2Tree$keynew <- paste0(Tree2Tree$T1_PLOT, "_", Tree2Tree$T1_SUBP, "_" , Tree2Tree$T1_TREE, "_", Tree2Tree$COUNTYCD)
      
      #Tree2Tree$keynew %in% new.ppt$keynew
      # reorder to have columns of PPTJAN with columns from 2010:2018:
      
      ppt.red <- new.ppt %>% filter (month %in% 1)   %>% select(keynew, year, month, value)
      PPTJan.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      PPTJan.n <- PPTJan.n[PPTJan.n$keynew %in% Tree2Tree$keynew, ]
      
      
      ppt.red <- new.ppt %>% filter (month %in% 2)   %>% select(keynew, year, month, value)
      PPTFeb.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 3)   %>% select(keynew, year, month, value)
      PPTMar.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 4)   %>% select(keynew, year, month, value)
      PPTApr.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 5)   %>% select(keynew, year, month, value)
      PPTMay.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 6)   %>% select(keynew, year, month, value)
      PPTJun.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 7)   %>% select(keynew, year, month, value)
      PPTJul.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 8)   %>% select(keynew, year, month, value)
      PPTAug.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 9)   %>% select(keynew, year, month, value)
      PPTSep.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 10)   %>% select(keynew, year, month, value)
      PPTOct.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 11)   %>% select(keynew, year, month, value)
      PPTNov.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      ppt.red <- new.ppt %>% filter (month %in% 12)   %>% select(keynew, year, month, value)
      PPTDec.n <- reshape2::dcast(ppt.red, formula = keynew ~ year, mean )
      
      time_data2<- list()
      
      time_data2$PPTJan<- PPTJan.n[match(Tree2Tree$keynew, PPTJan.n$keynew),]
      time_data2$PPTFeb<- PPTFeb.n[match(Tree2Tree$keynew, PPTFeb.n$keynew),]
      time_data2$PPTMar<- PPTMar.n[match(Tree2Tree$keynew, PPTMar.n$keynew),]
      time_data2$PPTApr<- PPTApr.n[match(Tree2Tree$keynew, PPTApr.n$keynew),]
      time_data2$PPTMay<- PPTMay.n[match(Tree2Tree$keynew, PPTMay.n$keynew),]
      time_data2$PPTJun<- PPTJun.n[match(Tree2Tree$keynew, PPTJun.n$keynew),]
      time_data2$PPTJul<- PPTJul.n[match(Tree2Tree$keynew, PPTJul.n$keynew),]
      time_data2$PPTAug<- PPTAug.n[match(Tree2Tree$keynew, PPTAug.n$keynew),]
      time_data2$PPTSep<- PPTSep.n[match(Tree2Tree$keynew, PPTSep.n$keynew),]
      time_data2$PPTOct<- PPTOct.n[match(Tree2Tree$keynew, PPTOct.n$keynew),]
      time_data2$PPTNov<- PPTNov.n[match(Tree2Tree$keynew, PPTNov.n$keynew),]
      time_data2$PPTDec<- PPTDec.n[match(Tree2Tree$keynew, PPTDec.n$keynew),]
      
      #-------------for Tmax --------
      
      new.tmax <- new.tmax[!duplicated(new.tmax),]
      new.tmax$keynew <- paste0(new.tmax$T1_PLOT, "_", new.tmax$T1_SUBP, "_" , new.tmax$T1_TREE, "_", new.tmax$T1_COUNTY)
      
      
      # reorder to have columns of TMAXJAN with columns from 2010:2018:
      tmax.red <- new.tmax %>% filter (month %in% 1)   %>% select(keynew, year, month, value)
      TMAXJan.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      TMAXJan.n <- TMAXJan.n[TMAXJan.n$keynew %in% Tree2Tree$keynew, ]
      
      
      tmax.red <- new.tmax %>% filter (month %in% 2)   %>% select(keynew, year, month, value)
      TMAXFeb.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 3)   %>% select(keynew, year, month, value)
      TMAXMar.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 4)   %>% select(keynew, year, month, value)
      TMAXApr.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 5)   %>% select(keynew, year, month, value)
      TMAXMay.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 6)   %>% select(keynew, year, month, value)
      TMAXJun.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 7)   %>% select(keynew, year, month, value)
      TMAXJul.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 8)   %>% select(keynew, year, month, value)
      TMAXAug.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 9)   %>% select(keynew, year, month, value)
      TMAXSep.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 10)   %>% select(keynew, year, month, value)
      TMAXOct.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 11)   %>% select(keynew, year, month, value)
      TMAXNov.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      tmax.red <- new.tmax %>% filter (month %in% 12)   %>% select(keynew, year, month, value)
      TMAXDec.n <- reshape2::dcast(tmax.red, formula = keynew ~ year, mean )
      
      # need to reorder the dataframe by the keynew in 
      
      
      
      time_data2$TMAXJan<- TMAXJan.n[match(Tree2Tree$keynew, TMAXJan.n$keynew),]
      time_data2$TMAXFeb<- TMAXFeb.n[match(Tree2Tree$keynew, TMAXFeb.n$keynew),]
      time_data2$TMAXMar<- TMAXMar.n[match(Tree2Tree$keynew, TMAXMar.n$keynew),]
      time_data2$TMAXApr<- TMAXApr.n[match(Tree2Tree$keynew, TMAXApr.n$keynew),]
      time_data2$TMAXMay<- TMAXMay.n[match(Tree2Tree$keynew, TMAXMay.n$keynew),]
      time_data2$TMAXJun<- TMAXJun.n[match(Tree2Tree$keynew, TMAXJun.n$keynew),]
      time_data2$TMAXJul<- TMAXJul.n[match(Tree2Tree$keynew, TMAXJul.n$keynew),]
      time_data2$TMAXAug<- TMAXAug.n[match(Tree2Tree$keynew, TMAXAug.n$keynew),]
      time_data2$TMAXSep<- TMAXSep.n[match(Tree2Tree$keynew, TMAXSep.n$keynew),]
      time_data2$TMAXOct<- TMAXOct.n[match(Tree2Tree$keynew, TMAXOct.n$keynew),]
      time_data2$TMAXNov<- TMAXNov.n[match(Tree2Tree$keynew, TMAXNov.n$keynew),]
      time_data2$TMAXDec<- TMAXDec.n[match(Tree2Tree$keynew, TMAXDec.n$keynew),]
      
      
    }
    
    # now need to add the time_data2 to the time_data dataframe
    
    time_data$PPTJan <- cbind(time_data$PPTJan, time_data2$PPTJan[,3:10])
    time_data$PPTFeb <- cbind(time_data$PPTFeb, time_data2$PPTFeb[,3:10])
    time_data$PPTMar <- cbind(time_data$PPTMar, time_data2$PPTMar[,3:10])
    time_data$PPTApr <- cbind(time_data$PPTApr, time_data2$PPTApr[,3:10])
    time_data$PPTMay <- cbind(time_data$PPTMay, time_data2$PPTMay[,3:10])
    time_data$PPTJun <- cbind(time_data$PPTJun, time_data2$PPTJun[,3:10])
    time_data$PPTJul <- cbind(time_data$PPTJul, time_data2$PPTJul[,3:10])
    time_data$PPTAug <- cbind(time_data$PPTAug, time_data2$PPTAug[,3:10])
    time_data$PPTSep <- cbind(time_data$PPTSep, time_data2$PPTSep[,3:10])
    time_data$PPTOct <- cbind(time_data$PPTOct, time_data2$PPTOct[,3:10])
    time_data$PPTNov <- cbind(time_data$PPTNov, time_data2$PPTNov[,3:10])
    time_data$PPTDec <- cbind(time_data$PPTDec, time_data2$PPTDec[,3:10])
    
    # for Tmax:
    time_data$TMAXJan <- cbind(time_data$TMAXJan, time_data2$TMAXJan[,3:10])
    time_data$TMAXFeb <- cbind(time_data$TMAXFeb, time_data2$TMAXFeb[,3:10])
    time_data$TMAXMar <- cbind(time_data$TMAXMar, time_data2$TMAXMar[,3:10])
    time_data$TMAXApr <- cbind(time_data$TMAXApr, time_data2$TMAXApr[,3:10])
    time_data$TMAXMay <- cbind(time_data$TMAXMay, time_data2$TMAXMay[,3:10])
    time_data$TMAXJun <- cbind(time_data$TMAXJun, time_data2$TMAXJun[,3:10])
    time_data$TMAXJul <- cbind(time_data$TMAXJul, time_data2$TMAXJul[,3:10])
    time_data$TMAXAug <- cbind(time_data$TMAXAug, time_data2$TMAXAug[,3:10])
    time_data$TMAXSep <- cbind(time_data$TMAXSep, time_data2$TMAXSep[,3:10])
    time_data$TMAXOct <- cbind(time_data$TMAXOct, time_data2$TMAXOct[,3:10])
    time_data$TMAXNov <- cbind(time_data$TMAXNov, time_data2$TMAXNov[,3:10])
    time_data$TMAXDec <- cbind(time_data$TMAXDec, time_data2$TMAXDec[,3:10])
  }
  
  
  # calculate winter precip and add to the list time_data (rows are trees, columns are years)
  wintP.NovAug <- (time_data$PPTNov + time_data$PPTDec + time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar + time_data$PPTApr + time_data$PPTMay + time_data$PPTJun + time_data$PPTJul + time_data$PPTAug)
  wintP.wateryr <- (time_data$PPTSep + time_data$PPTOct + time_data$PPTNov + time_data$PPTJan + time_data$PPTJan + time_data$PPTFeb + time_data$PPTMar + time_data$PPTApr + time_data$PPTMay + time_data$PPTJun + time_data$PPTJul + time_data$PPTAug)
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
  tmax.AprMayJun <- (time_data$TMAXApr + time_data$TMAXMay + time_data$TMAXJun)/3 # "Arid Foresummer"
  tmax.fall <- (time_data$TMAXSep + time_data$TMAXOct + time_data$TMAXNov)/3 # "Arid post monsoon"
  tmax.monsoon <- (time_data$TMAXJul + time_data$TMAXAug)/2 # "monsoon"
  tmax.wateryr <- (time_data$TMAXSep + time_data$TMAXOct + time_data$TMAXNov + time_data$TMAXDec + time_data$TMAXJan + time_data$TMAXFeb + time_data$TMAXMar + time_data$TMAXApr + time_data$TMAXMay + time_data$TMAXJun + time_data$TMAXJul + time_data$TMAXAug)/12 # Water year mean max temperature
  
  time_data$tmax.fallspr <- tmax.fallspr
  time_data$tmax.JanA <- tmax.JanA
  time_data$tmax.MJul <- tmax.MJul
  time_data$tmax.AprMayJun <- tmax.AprMayJun
  time_data$tmax.fall <- tmax.fall
  time_data$tmax.monsoon <- tmax.monsoon
  time_data$TMAX <- tmax.wateryr
  # save the climate/time data to a file so we can look at the raw values as well:
  saveRDS(time_data, "PRISM_non_scaled.rds")
  
  # standardize climate data
  if(standardize.cov == TRUE){
    for(c in 1:length(time_data)){
      time_data[[c]] <- standardize.vector(as.matrix(time_data[[c]]))
    } 
  }
  
  ### RANDOM EFFECTS
  
  ### plot rnd effect (currently implemented at indiv level)
  PLOT <- paste0(temp2$County, temp2$Plot) # should maybe use an underscore to avoid mistakes...but would jags choke?
  if(!is.null(Tree2Tree)){
    PLOT2 <- paste0(Tree2Tree$COUNTYCD, Tree2Tree$T1_PLOT)
    PLOT <- c(PLOT, PLOT2)
    if(stage.2 ==TRUE){
      PLOT <- PLOT2
    }
    
  }
  
  ### get the FIA DB plot id, if applicable
  T2_FIADB <- temp2$T2_FIADB_PLOT # should maybe use an underscore to avoid mistakes...but would jags choke?
  if(!is.null(Tree2Tree)){
    T2_FIADB2 <- Tree2Tree$T2_FIADB_PLOT
    T2_FIADB  <- c(T2_FIADB, T2_FIADB )
    if(stage.2 ==TRUE){
      T2_FIADB <- T2_FIADB2
    }
    
  }
  
  TREE <- temp2$T1_TRE_CN # should maybe use an underscore to avoid mistakes...but would jags choke?
  if(!is.null(Tree2Tree)){
    TREE2 <- Tree2Tree$T1_TRE_CN
    TREE <- c(TREE, TREE2)
    if(stage.2 ==TRUE){
      TREE <- TREE2
    }
  }
  
  
  ### FIXED EFFECTS
  
  
  
  ### tree-level
  ### CR, CCLCD
  #cov.data <- cbind(temp2$CR, temp2$CCLCD)
  #colnames(cov.data) <- c("CR", "CCLCD")## give them column names
  ### should eventually use height ratio, or other variable that Justin says is more informative
  
  
  ### plot-level
  
  
  #LAT and long
  LAT = temp2$PLOT_LAT
  LON = temp2$PLOT_LON
  
  
  ### ELEV
  ELEV <- temp2$PLOT_ELEV
  if(!is.null(Tree2Tree)){
    ELEV2 <- Tree2Tree$ELEV
    ELEV <- c(ELEV,   ELEV2)
    if(stage.2 ==TRUE){
      ELEV <- ELEV2
    }
  }
  if(standardize.cov==TRUE){
    ELEV <- standardize.vector(ELEV)
  }
  
  
  ### condition-level
  ### SICOND  
  SICOND <- temp2$COND_SICOND
  if(!is.null(Tree2Tree)){
    SICOND2 <- Tree2Tree$SICOND
    SICOND <- c(SICOND, SICOND2)
    
    if(stage.2 ==TRUE){
      SICOND <-  SICOND2
    }
  }
  if(standardize.cov==TRUE){
    SICOND <- standardize.vector(SICOND)
  }
  
  ### SLOPE
  SLOPE <- temp2$COND_SLOPE # ranges as a high as 360
  if(!is.null(Tree2Tree)){
    SLOPE2 <- Tree2Tree$SLOPE # max value = 78...different units??
    SLOPE <- c(SLOPE, SLOPE2)
    if(stage.2 ==TRUE){
      SLOPE <-  SLOPE2
    }
  }
  
  ### ASPECT ### 
  ASPECT <- temp2$COND_ASPECT
  if(!is.null(Tree2Tree)){
    ASPECT.2 <- Tree2Tree$ASPECT
    ASPECT  <- c(ASPECT , ASPECT.2)
    
    if(stage.2 ==TRUE){
      ASPECT <-  ASPECT.2
    }
  }
  
  ## STAGE VARS DERIVED FROM SLOPE & ASPECT ##
  STAGE2 <- temp2$COND_SLOPE*cos(temp2$COND_ASPECT)
  if(!is.null(Tree2Tree)){
    STAGE2.2 <- Tree2Tree$SLOPE*cos(Tree2Tree$ASPECT)
    STAGE2 <- c(STAGE2, STAGE2.2)
    if(stage.2 ==TRUE){
      STAGE2 <-   STAGE2.2
    }
  }
  
  STAGE3 <- temp2$COND_SLOPE*sin(temp2$COND_ASPECT)
  if(!is.null(Tree2Tree)){
    STAGE3.2 <- Tree2Tree$SLOPE*sin(Tree2Tree$ASPECT)
    STAGE3 <- c(STAGE3, STAGE3.2)
    if(stage.2 ==TRUE){
      STAGE3 <-   STAGE3.2
    }
  }
  
  ### STDAGE
  STDAGE <- temp2$COND_STDAGE
  if(!is.null(Tree2Tree)){
    STDAGE2 <- Tree2Tree$STDAGE
    STDAGE <- c(STDAGE, STDAGE2)
    if(stage.2 ==TRUE){
      STDAGE <-   STDAGE2
    }
  }
  
  
  ### SDI ## eventually should calculate <relative> SDI...observed SDI relative to maxSDI for dominant spp on plot (PIPO)
  SDI <- temp2$SDI
  if(!is.null(Tree2Tree)){
    SDI2 <- Tree2Tree$SDIc
    SDI <- c(SDI, SDI2)
    
    if(stage.2 ==TRUE){
      SDI <-   SDI2
    }
  }
  if(standardize.cov == TRUE){
    SDI <- standardize.vector(SDI)
  }
  
  # Pull out treatment and disturbance codes
  # Treatment = 0 ; no treatment
  # Treatment = 10 ; Thinning
  TRTCD1 <- temp2$COND_TRTCD1
  if(!is.null(Tree2Tree)){
    COND_TRTCD12 <- Tree2Tree$TRTCD1
    TRTCD1 <- c(TRTCD1, COND_TRTCD12)
    if(stage.2 ==TRUE){
      TRTCD1 <-   COND_TRTCD12
    }
  }
  if(standardize.cov == TRUE){
    TRTCD1 <- standardize.vector(TRTCD1)
  }
  
  # Disturbance Code 1
  DSTRBCD1 <- temp2$COND_DSTRBCD1
  if(!is.null(Tree2Tree)){
    DSTRBCD12 <- Tree2Tree$DSTRBCD1
    DSTRBCD1 <- c(DSTRBCD1, DSTRBCD12)
    if(stage.2 ==TRUE){
      DSTRBCD1 <-    DSTRBCD12
    }
  }
  
  # Disturbance Year
  DSTRBYR1 <- rep(NA, length(temp2$DBH))
  DSTRBYR1 <- temp2$DSTRBYR1
  if(!is.null(Tree2Tree)){
    DSTRBYR12 <- Tree2Tree$DSTRBYR1
    DSTRBYR1 <- c(DSTRBYR1,DSTRBYR12)
    if(stage.2 ==TRUE){
      DSTRBYR1 <-   DSTRBYR12
    }
  }
  
  
  # if more than 1 disturbance present, there will be DSTRBCD2-3:
  # Disturbance Code 2
  DSTRBCD2 <- temp2$COND_DSTRBCD2
  if(!is.null(Tree2Tree) & "DSTRBCD2" %in% colnames((Tree2Tree))){
    DSTRBCD22 <- Tree2Tree$DSTRBCD2
    DSTRBCD2 <- c(DSTRBCD2, DSTRBCD22)
    if(stage.2 ==TRUE){
      DSTRBCD2 <-   DSTRBCD22
    }
  }
  
  # Disturbance Year
  DSTRBYR2 <- temp2$DSTRBYR2
  if(!is.null(Tree2Tree)){
    DSTRBYR22 <- Tree2Tree$DSTRBYR2
    DSTRBYR2 <- c(DSTRBYR2,DSTRBYR22)
    if(stage.2 ==TRUE){
      DSTRBYR2 <-    DSTRBYR22
    }
  }
  
  # Disturbance Code 3
  DSTRBCD3 <- temp2$COND_DSTRBCD3
  if(!is.null(Tree2Tree) & "DSTRBCD3" %in% colnames((Tree2Tree))){
    DSTRBCD32 <- Tree2Tree$COND_DSTRBCD3
    DSTRBCD3 <- c(DSTRBCD3, DSTRBCD32)
    if(stage.2 ==TRUE){
      DSTRBCD3 <-   DSTRBCD32
    }
  }
  
  # Disturbance Year
  DSTRBYR3 <- temp2$DSTRBYR3
  if(!is.null(Tree2Tree)){
    DSTRBYR32 <- Tree2Tree$DSTRBYR3
    DSTRBYR3 <- c(DSTRBYR3,DSTRBYR32)
    if(stage.2 ==TRUE){
      DSTRBYR3 <-   DSTRBYR32
    }
  }
  
  MAP <- rowMeans(time_data$wintP.wateryr)
  
  if(standardize.cov==TRUE){
    MAP <- standardize.vector(MAP)
  }
  
  MAT <- rowMeans(time_data$TMAX)
  
  if(standardize.cov==TRUE){
    MAT <- standardize.vector(MAT)
  }
  
  
  ### BA ## SDI and BA are tightly correlated, can't use both
  if(is.null(Tree2Tree)){
    cov.data <- data.frame(LAT = LAT, LON = LON, PLOT=PLOT, TREE = TREE ,SICOND=SICOND, SDI=SDI, ELEV = ELEV, SLOPE = SLOPE, ASPECT = ASPECT, STAGE2 = STAGE2, STAGE3 = STAGE3, 
                           STDAGE = STDAGE, TRTCD1 = TRTCD1, DSTRBCD1 = DSTRBCD1,  MAP =MAP, MAT =MAT, T2_FIADB = T2_FIADB )
    
    
    
  }else{
    cov.data <- data.frame(PLOT=PLOT, TREE = TREE ,SICOND=SICOND, SDI=SDI, ELEV = ELEV, SLOPE = SLOPE, ASPECT = ASPECT, STAGE2 = STAGE2, STAGE3 = STAGE3, 
                           STDAGE = STDAGE, #TRTCD1 = TRTCD1, DSTRBCD1 = DSTRBCD1, DSTRBYR1 =DSTRBYR1, DSTRBCD2 = DSTRBCD2, DSTRBYR2 =DSTRBYR2, DSTRBCD3 = DSTRBCD3, DSTRBYR3 =DSTRBYR3,
                           MAP =MAP, MAT =MAT)
  }
  #cov.data <- cbind(cov.data, SICOND, SDI)
  
  
  
  
  ## build data object for JAGS
  data = list(y = y.small, 
              z = z.small,
              ni = nrow(y.small), nt = ncol(y.small), 
              nt2 = endyr, 
              startyr = startyr, startyr2 = startyr+1, endyr = endyr,
              x_ic = 1, tau_ic = 1e-04,
              a_dbh = 16, r_dbh = 8, 
              a_inc = 0.001, r_inc = 1, 
              a_add = 1, r_add = 1, 
              time = years.small)
  
  ## state variable initial condition
  ## must define an initial (biologically-reasonable) value for DBH for MCMC chains to start at (every tree, every year)
  z0 <- matrix(data=NA, nrow=nrow(y.small), ncol=ncol(y.small))
  
  if(stage.2 == FALSE){
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
    
  }
  # now deal with trees without cores
  if(!is.null(Tree2Tree)){
    if(stage.2 == FALSE){
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
      
    }else{
      DIA.T1 <- vector(mode="numeric", length=nrow(temp2))
      ave.ring <- vector(mode="numeric", length=nrow(temp2))
      global.ave.inc <- mean(ave.ring) # 0.363... ~0.18 cm for average ring
      for (t in 1:nrow(Tree2Tree)) {
        # shrink tree backwards from T1 DBH, using global ave diameter increment derived from tree rings
        first.DBH <- Tree2Tree$T1_DIA[t]*2.54 # extract time 1 DBH
        temp.growth <- rep(global.ave.inc, times=(Tree2Tree$T1_MEASYR[t]-trunc.yr))
        temp.growth2 <- c(-rev(cumsum(rev(temp.growth))),0) # add a zero at the end of this so that z0 in MEASYR = DIA
        z0[t,1:length(temp.growth2)] <- first.DBH + temp.growth2
        
        ### grow tree forward from T1_DIA to year 2015
        temp.forward <- rep(global.ave.inc, times=(last.meas.yr-Tree2Tree$T1_MEASYR[t]))
        z0[t, (length(temp.growth2)+1):length(years.small)] <- first.DBH + cumsum(temp.forward)
      }
    }
  }
  
  colnames(z0) <- years.small
  
  return.list <- list(data=data,
                      z0=z0,
                      cov.data=cov.data,
                      time_data=time_data)
  
  
  return(return.list)
}