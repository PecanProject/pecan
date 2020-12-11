# code to get most recent FIA surveys (after 2010) and matach up with our trees to see if we can have "out" of sample validation:
#library(DBI)
library(dplyr)
library(ggplot2)
# note that this script uses temp2 and Tree2Tree.incored.plots dataframes from the Rdriver.R script

#if(!exists("/Users/kah/Documents/docker_pecan/pecan/InWeUS_FIA/AZ_COND.csv")){
# fiadb <- getFIA(states = c("AZ", "UT", "CO", "ID", "WY", "MT"), dir = "InWeUS_FIA", common = FALSE, tables = c("PLOT", "TREE", "COND", "SUBPLOT"), nCores = 1)
#}else{
# fiadb <- readFIA(dir = "InWeUS_FIA")
# #}
# 
# PLOT <- fiadb$PLOT
# SUBPLOT <- fiadb$SUBPLOT
# STATECD <- fiadb$STATECD
# COND <- fiadb$COND
# TREE <- fiadb$TREE
# #TREE.sub <- TREE[TREE$INVYR >= 2010,]
# unique(TREE$INVYR)
# unique(fiadb$PLOT$STATECD)
# 

# read in the non-AZ points:
library(tidyr)
library(ggplot2)
library(dplyr)

setwd("/home/rstudio")
full.clim.data <- read.csv("data/pipo_all_tmean_ppt_v4.csv")
region.rwl <- read.csv("data/trees-rwl-1-31-17.csv") # note that this data is has all RWLS in columsn and a year column
region.ll <- read.csv("data/locs-env-1-31-17.csv")

head(region.ll)


# # not sure if these are actually all matching properly.
# previous.surveys <-  region.ll[ TREE$CN %in% region.ll$ID,]
# 
# TREE.old <- TREE %>% filter(INVYR, )
# TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, TREE$PLT_CN)]
# TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
# TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]
# 
# ggplot(TREE %>% filter(SPCD == 122), aes(PLOT_LON, PLOT_LAT))+geom_point()
# # we should be able to use STATECD, COUNTYCD, PLOT, SUBP, TREE, and SPCD & DIA from region.ll to match with trees from FIADB
# 
# FIA.outside.AZ <- left_join(region.ll[,c("STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD","series", "DIA", "ELEV", "LAT", "LON")], 
#                             TREE, by = c("STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA"))
# 
# FIA.not.orphaned.plots.nona <- FIA.outside.AZ[!is.na(FIA.outside.AZ$INVYR),]
# 
# # now get the repeated survey data for those FIA plots:
# 
# unique(FIA.not.orphaned.plots.nona$PLT_CN %in% TREE$PLT_CN)
# 
# 
# 
# TREE_REMEAS <- subset(TREE, !is.na(PREVDIA))# keep trees that were remeasured (n = 382530)
# 
# ggplot(TREE_REMEAS %>% filter(SPCD == 122), aes(PLOT_LON, PLOT_LAT))+geom_point()
# 
# TREE_REMEAS <- subset(TREE_REMEAS, STATUSCD == 1 | STATUSCD == 2)# trees that either lived or died (n= 357303)
# 
# TREE_REMEAS <- subset(TREE_REMEAS, STATUSCD == 1)# Only keep trees that didn't die (n= 244816)
# 
# # get the previous DIA and INVYR for the previous survey
# TREE_REMEAS$PREV_TRE_CN %in% TREE$CN
# 
# TREE_REMEAS$PREV_PLT_CN <- TREE$PLT_CN[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
# TREE_REMEAS$PREV_CONDID <- TREE$CONDID[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
# TREE_REMEAS$T1_DIA <- TREE$DIA[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
# 
# TREE_REMEAS$T1_INVYR <- TREE$INVYR[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
# TREE_REMEAS$T1_MEASYR <- TREE$MEASYR[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
# TREE_REMEAS$T2_MEASYR <- TREE_REMEAS$MEASYR
# TREE_REMEAS$T2_DIA <- TREE_REMEAS$DIA
# TREE_REMEAS$T2_TRE_CN <- TREE_REMEAS$CN
# TREE_REMEAS$T1_TRE_CN <- TREE_REMEAS$PREV_TRE_CN
# 
# TREE_REMEAS$PREV_PREV_TREE_CN <- TREE$PREV_TRE_CN[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
# 
# 
# 
# 
# # how many cored trees of each species?
# FIA.outside.AZ %>% group_by(SPCD) %>% summarise(n())
# # # A tibble: 4 x 2
# # SPCD `n()`
# # <int> <int>
# #   1   106   415
# # 2   113    56
# # 3   122   981
# # 4   202  1526
# 
# # over 981 PIPO trees cored 
# 
# 
# 
# # of the coredtrees with two meausrements,  how many for each species?
# FIA.not.orphaned.plots.nona %>% group_by(SPCD) %>% summarise(n())
# # SPCD `n()`
# #    106   289
# #    113     8
# #    122   371
# #    202   872
# 
# # 371 pipo trees with at least 2 DBH measurements (outside AZ)
# 
# 
# 
# # there are 16131 remeasured PIPO trees in the FIA database
# TREE_REMEAS %>% filter(SPCD == 122) %>% summarise(n())
# 
# TREE_REMEAS.PIPO <- TREE_REMEAS %>% filter(SPCD == 122)
# 
# unique(TREE_REMEAS.PIPO$PLOT_LON)
# 
# head(TREE_REMEAS.PIPO)
# 
# ggplot()+
#   geom_point(data = TREE_REMEAS %>% filter(SPCD == 122), aes(x = PLOT_LON, y = PLOT_LAT), color = "black")+
#   geom_point(data = FIA.outside.AZ %>% filter(SPCD == 122), aes(x= LON, y = LAT, color = as.character(INVYR)))


#-------------------------------------------------------------------------------------
# Read in AZ data and combine stand level data
#-------------------------------------------------------------------------------------



AZ.PIPO <- read.delim("data/AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

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




# we want to format the rest of the data like we have temp2


### read in function that creates jags objects from above data
source("pecan/modules/data.land/R/BuildJAGSdataobject.R")
#jags.stuff <- buildJAGSdataobject(temp2, Tree2Tree, rnd.subset = 5000, trunc.yr = 1966)
# if you don't have trees without cores, use the following line
# or you wish to not include trees without cores

# note that we need to rescale all of the data once we add the regional values....so we need to set standardize.cov == FALSE

jags.stuff <- buildJAGSdataobject(temp2, rnd.subset = 100, trunc.yr = 1966, forecast = TRUE, standardize.cov = FALSE)
data <- jags.stuff$data
z0 <- jags.stuff$z0
cov.data <- jags.stuff$cov.data
time_data <- jags.stuff$time_data



data$a_dbh <- 512
data$r_dbh <- 256


# try just adding the extra data on here:
# adding together the cov.data is relatively straightforward for PIPO
pipo.ll <- region.ll %>% filter(SPCD == 122)
pipo.ll <- pipo.ll[!is.na(pipo.ll$SICOND),]
pipo.ll <- pipo.ll[!is.na(pipo.ll$DIA),]

cov.data.az <- cov.data
cov.data.az$STATECD <- 4
cov.data.az.sub <- cov.data.az[,c("STATECD","PLOT","TREE","SICOND", "ELEV" , "SLOPE","ASPECT" )]
cov.data.iw.sub <- pipo.ll[,c("STATECD", "PLOT","TREE","SICOND", "ELEV" , "SLOPE","ASPECT" )]
cov.data.az.sub$PLOT <- as.character(cov.data.az.sub$PLOT)
cov.data.iw.sub$PLOT <- as.character(cov.data.iw.sub$PLOT)
cov.data.region <- rbind(cov.data.az.sub, cov.data.iw.sub)

cov.data.full <- cov.data.region

pipo.ll$series

# adding on ring widths to the data$y dataframe:
y <- data$y
# get the pipo only ring widths:
region.rwl.m <- reshape2::melt(region.rwl[,2:length(region.rwl)], id.vars = "Year")
pipo.rwl.m <- region.rwl.m %>% filter(variable %in% pipo.ll$series)

# get the years that match with the years in az dataset:
pipo.subset <- pipo.rwl.m %>% filter(Year %in% colnames(y))

# now rearrange so that each row is a tree and each column is a year from 1966-2010
spread.pipo.mat <- pipo.subset %>% group_by(variable) %>% tidyr::spread(Year, value)
spread.pipo.mat <- data.frame(spread.pipo.mat)
# now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
ordered.pipo.mat<- spread.pipo.mat[order(match(spread.pipo.mat[,"variable"], pipo.ll[,"series"])),]
colnames(ordered.pipo.mat)[2:length(ordered.pipo.mat)] <- colnames(y)
y.new <- rbind(y, ordered.pipo.mat[,2:length(ordered.pipo.mat)])


# adding on DBH values to the data$z dataframe:
z <- data$z
hist(pipo.ll$DIA*2.54)
hist(z)

# we are assuming the DIA is the last year of the tree ring widths:


pipo.rwl.m.nona <- pipo.rwl.m[!is.na(pipo.rwl.m$value),]
last.year <- pipo.rwl.m.nona %>% group_by(variable) %>% summarise(SAMP_YR=max(Year, na.rm = TRUE))

pipo.ll$last_growth <- last.year$SAMP_YR[match(pipo.ll$series, last.year$variable)]
pipo.ll$DBH_cm <- pipo.ll$DIA*2.54
pipo.ll.dia <- pipo.ll %>% select(series, DBH_cm, last_growth)



# now rearrange so that each row is a tree and each column is a year from 1966-2010
spread.dia.mat <- pipo.ll.dia %>% filter(series %in% unique(pipo.ll$series)) %>% group_by(series) %>% tidyr::spread(last_growth, DBH_cm) %>% ungroup()
cols.nams <- names(spread.dia.mat)
spread.dia.mat <- data.frame(spread.dia.mat)
# now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
ordered.dia.mat <- spread.dia.mat[order(match(spread.dia.mat[,"series"], pipo.ll[,"series"])),]
colnames(ordered.dia.mat) <- cols.nams


df <- ordered.dia.mat[,2:length(ordered.dia.mat)]
colnms <- colnames(z)
rownms <- rownames(df)
#rownames(df) <- c("1", "3", "4", "5")

Missing <- setdiff(colnms, names(df))
df[,Missing] <- NA
df <- df[colnms]


z.new <- rbind(z, df)


# now align the climate datasets:
time_data.az <- time_data
head(region.ll)

# read in the larger region climate data:
pipo.clim <- read.csv("data/pipo_all_tmean_ppt_v4.csv")
colnames(pipo.clim)

pipo.clim$wintP.NovAug <- rowSums(pipo.clim[,c("PPT_8", "PPT_9", "PPT_10", "PPT_11")])
pipo.clim$wintP.wateryr <- rowSums(pipo.clim[,c("PPT_1", "PPT_2", "PPT_3", "PPT_4", "PPT_5", "PPT_6", "PPT_7", "PPT_8", "PPT_9", "PPT_10", "PPT_11", "PPT_12")])
pipo.clim$wintP.NM <- rowSums(pipo.clim[,c("PPT_11", "PPT_12", "PPT_1", "PPT_2", "PPT_3")])
pipo.clim$wintP.JJ <- rowSums(pipo.clim[,c("PPT_6", "PPT_7")])

pipo.clim$tmax.fallspr <- rowMeans(pipo.clim[,c( "TMAX_9", "TMAX_10", "TMAX_11", "TMAX_12", "TMAX_1", "TMAX_2", "TMAX_3",  "TMAX_4")])
pipo.clim$tmax.JanA <- rowMeans(pipo.clim[,c("TMAX_1", "TMAX_2", "TMAX_3", "TMAX_4", "TMAX_5", "TMAX_6", "TMAX_7", "TMAX_8")])
pipo.clim$tmax.MJul <- rowMeans(pipo.clim[,c("TMAX_5", "TMAX_6", "TMAX_7")])
pipo.clim$tmax.AprMayJun <- rowMeans(pipo.clim[,c("TMAX_4","TMAX_5", "TMAX_6")])
pipo.clim$tmax.fall <- rowMeans(pipo.clim[,c("TMAX_9","TMAX_10","TMAX_11")])
pipo.clim$tmax.monsoon <- rowMeans(pipo.clim[,c("TMAX_7","TMAX_8", "TMAX_9")])
pipo.clim$TMAX <- rowMeans(pipo.clim[,c("TMAX_1", "TMAX_2", "TMAX_3", "TMAX_4", "TMAX_5", "TMAX_6", "TMAX_7", "TMAX_8", "TMAX_9", "TMAX_10", "TMAX_11", "TMAX_12")])
pipo.clim$TMEAN <- rowMeans(pipo.clim[,c("TMEAN_1", "TMEAN_2", "TMEAN_3", "TMEAN_4", "TMEAN_5", "TMEAN_6", "TMEAN_7", "TMEAN_8", "TMEAN_9", "TMEAN_10", "TMEAN_11", "TMEAN_12")])

require(dplyr)
pipo.clim %>% dplyr::select(lon, lat, name, year, PPT_1, PPT_2, PPT_3, PPT_4, PPT_5, PPT_6, PPT_7, PPT_8, PPT_9, PPT_10, PPT_11, PPT_12,
                            TMAX_1, TMAX_2, TMAX_3, TMAX_4, TMAX_5, TMAX_6, TMAX_7, TMAX_8, TMAX_9, TMAX_10, TMAX_11, TMAX_12,
                            wintP.NovAug , 
                            wintP.wateryr , wintP.NM ,    wintP.JJ,       tmax.fallspr  , tmax.JanA,     
                            tmax.MJul    ,  tmax.AprMayJun ,tmax.fall,      tmax.monsoon,   TMAX, TMEAN   )

#spread.pipo.mat[] 
#test.melt <- reshape2::melt(time_data)
#colnames(test.melt) <- c("TreeID", "YearID", "Value", "Climate")


clim.spread <- test.melt %>% group_by(TreeID, YearID) %>% spread(Climate, Value)
# each climate variable has 45 years (columns) & 544 cores (rows)

# we want to add the pipo.clim values onto those 45 rows
years <- 1966:(1965 + ncol(time_data$PPTJan))

pipo.clim.crop <- pipo.clim %>% filter(year %in% years)
pipo.clim.crop$X <- 1:length(pipo.clim.crop$lon)


get_ordered_climate <- function(x){
  
  
  spread.tmax.fall.spr.mat <- pipo.clim.crop %>% filter(name %in% unique(pipo.ll$series))%>% dplyr::select(lon, lat, name, year, x) %>% group_by(lon, lat, name) %>%  tidyr::spread( year, x, drop = T)
  
  
  # now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
  spread.tmax.fall.spr.mat.ordered <-  spread.tmax.fall.spr.mat[order(match(spread.tmax.fall.spr.mat$name,  unique(pipo.ll[,"series"]))),]
  climate.mat <- spread.tmax.fall.spr.mat.ordered[,4:length(spread.tmax.fall.spr.mat.ordered)]
  climate.mat
}

tmax.fallspr <- get_ordered_climate("tmax.fallspr")
wintP.wateryr <- get_ordered_climate("wintP.wateryr")
tmax.AprMayJun <- get_ordered_climate("tmax.AprMayJun")
tmax.monsoon <- get_ordered_climate("tmax.monsoon")
TMAX <- get_ordered_climate("TMAX")

# make sure column names are the same
names(time_data$tmax.AprMayJun) <- names(tmax.monsoon)
names(time_data$tmax.monsoon) <- names(tmax.monsoon)
names(time_data$tmax.fallspr) <- names(tmax.monsoon)
names(time_data$wintP.wateryr) <- names(tmax.monsoon)
names(time_data$TMAX) <- names(TMAX)

time_data$tmax.monsoon <- rbind(time_data$tmax.monsoon, as.matrix(tmax.monsoon))
time_data$tmax.fallspr <- rbind(time_data$tmax.fallspr, as.matrix(tmax.fallspr))
time_data$wintP.wateryr <- rbind(time_data$wintP.wateryr, as.matrix(wintP.wateryr))
time_data$tmax.AprMayJun <- rbind(time_data$tmax.AprMayJun, as.matrix(tmax.AprMayJun))
time_data$TMAX <- rbind(time_data$TMAX, as.matrix(TMAX))

# use time_data TMAX and wintP.Wayteryr columns to get mean annual temperature and mean annual max T

MAT <- rowMeans(time_data$TMAX)
MAP <- rowMeans(time_data$wintP.wateryr)

cov.data.region$MAT <- MAT
cov.data.region$MAP <- MAP
# scale climate variables


standardize.mat <- function(x){
  x.bar <- mean(as.matrix(x), na.rm = TRUE)
  s.d. <- sd(as.matrix(x), na.rm = TRUE)
  return((x-x.bar)/s.d.)
}

standardize.vector <- function(x){
  x.bar <- mean(x, na.rm = TRUE)
  s.d. <- sd(x, na.rm = TRUE)
  return((x-x.bar)/s.d.)
}

time_data$TMAX.scaled <- standardize.mat(time_data$TMAX)
time_data$tmax.fallspr.scaled <- standardize.mat(time_data$tmax.fallspr)
time_data$wateryr.scaled <- standardize.mat(time_data$wintP.wateryr)


# scale the plot level variables (just SICOND, MAT, and MAP)
cov.data.full.scaled <- cov.data.region %>% mutate_at(standardize.vector, .vars = c("SICOND", "MAT", "MAP"))


# for some reason we have one tree with all NA values for y
y.new.nona <- y.new[!is.na(rowMeans(y.new, na.rm=TRUE)),]

data$y <- y.new
data$z <- z.new
data$time <- 1966:2018
cov.data <- cov.data.full

source("pecan/modules/data.land/R/InventoryGrowthFusion_unif.R") 
cov.data.full.scaled$PLOT.STATE <- as.numeric(as.character(paste0(cov.data.full.scaled$PLOT, cov.data.full.scaled$STATECD)))
# linear model with DBH^2 removed for Precipitation + Tmax and 500 cores
ppt.noX2 <- InventoryGrowthFusion_norm(data=data, cov.data=cov.data.full.scaled, time_data=time_data,
                                       n.iter=40000, z0=z0, scale.state = 30,
                                       n.chunk=100, save.state=TRUE, random="(1|PLOT.STATE[i])",rand.X =FALSE,
                                       fixed = "~ X + X^2 + SICOND + MAP + MAT +  SICOND*X + X*tmax.fallspr.scaled[t] + X*wateryr.scaled[t]",
                                       time_varying = "wateryr.scaled + SICOND*wateryr.scaled[t] + tmax.fallspr.scaled + SICOND*tmax.fallspr.scaled[t]  + tmax.fallspr.scaled[t]*wateryr.scaled[t] + MAP*wateryr.scaled[t] + MAT*wateryr.scaled[t]+ MAP*tmax.fallspr.scaled[t] + MAT*tmax.fallspr.scaled[t] ",
                                       burnin_plot=FALSE, save.jags = "Regional_model_scaled.txt", model.name = "Regional_model_scaled", 
                                       output.folder = "IGF_PIPO_region/", breakearly = FALSE)


