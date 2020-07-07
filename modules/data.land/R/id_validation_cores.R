# Finding additional cores to validate with:
library(ggplot2)
library(tidyverse)
library(rFIA)
library(dplyr)
library(sp)

# read in the metadata
metafia <- read.csv("FIA_inc_data/FIA_meta_all.csv")

# read in the core data that was pulled in 2016:
AZ.PIPO <- read.delim("/Users/kah/Documents/docker_pecan/pecan/FIA_inc_data/AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

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


# lets explore the tree ring data:


### read in function that creates jags objects from above data
source("/Users/kah/Documents/docker_pecan/pecan/modules/data.land/R/BuildJAGSdataobject.R")
#jags.stuff <- buildJAGSdataobject(temp2, Tree2Tree, rnd.subset = 5000, trunc.yr = 1966)
# if you don't have trees without cores, use the following line
# or you wish to not include trees without cores
jags.stuff <- buildJAGSdataobject(temp2, rnd.subset = 100, trunc.yr = 1966)
data <- jags.stuff$data
z0 <- jags.stuff$z0
cov.data <- jags.stuff$cov.data
time_data <- jags.stuff$time_data

data$a_dbh <- 512
data$r_dbh <- 256

test.melt <- reshape2::melt(time_data)
colnames(test.melt) <- c("TreeID", "YearID", "Value", "Climate")


# match with COUNTYCD PLOT SUBP TREE
PIPO.indata <- temp2[,c("PlotNo", "CountyNo", "SubplotNo", "TreeNo")]
colnames(PIPO.indata) <- c("PLOT", "COUNTYCD", "SUBP", "TREE")

metafia$id <- 1:length(metafia$X)

# pipo cores that are in the training set
train.pipo <- merge(PIPO.indata, metafia, by =c("PLOT", "COUNTYCD", "SUBP", "TREE"))
train.pipo <- train.pipo %>% filter(SPCD %in% 122)

# get pipo cores that are not in the training set
test.all <- metafia[!metafia$id %in% train.pipo$id,]


# there appear to be 163 cores that have been run through cofecha and QC
test.pipo %>% filter(SPCD %in% 122) %>% group_by(STATUS) %>% summarise(n())

pipo.cores <- test.pipo %>% filter(SPCD %in% 122)

# make sure we can match pipo cores up to FIA plots (for Lat Long and plot information):

# note that this script uses temp2 and Tree2Tree.incored.plots dataframes from the Rdriver.R script

if(!exists("/Users/kah/Documents/docker_pecan/pecan/recentFIA/AZ_COND.csv")){
  fiadb <- getFIA(states = c("AZ"), dir = "recentFIA", common = FALSE, tables = c("PLOT", "TREE", "COND", "SUBPLOT"), nCores = 1)
}else{
  fiadb <- readFIA(dir = "recentFIA")
}

PLOT <- fiadb$PLOT
SUBPLOT <- fiadb$SUBPLOT
COND <- fiadb$COND
TREE <- fiadb$TREE
TREE.sub <-TREE[TREE$INVYR >= 2010,]
unique(TREE.sub$INVYR)

# Select the tree table attributes to check for new remeasurements
#TREE.new <- TREE %>% select(CN, PLT_CN, STATUSCD, SPCD, SUBP, PREV_TRE_CN, DIA, INVYR)

PIPO <- TREE %>% filter(SPCD %in% "122")
##PIPO %>% filter(CN %in% PIPO$CN[4])
T2_TRE_CN<- unique(temp2$T2_TRE_CN)
T2_TRE_CN<- T2_TRE_CN[!is.na(T2_TRE_CN)]

T1_TRE_CN<- unique(temp2$T1_TRE_CN)
T1_TRE_CN<- T1_TRE_CN[!is.na(T1_TRE_CN)]

TreeNo <- unique(temp2$TreeNo)
remeasured.PIPO <- PIPO[!is.na(PIPO$PREV_TRE_CN),]

# there are 9595 PIPO trees that we appear to have remeasuremetns from:
previous.surveys <-  PIPO[PIPO$CN %in% remeasured.PIPO$PREV_TRE_CN,]
summaries <- PIPO %>% group_by(PLOT, SUBP, TREE, COUNTYCD) %>% summarise(nrecs = n(), 
                                                                         min.invyr = min(INVYR, na.rm = TRUE), 
                                                                         max.invyr = max(INVYR, na.rm = TRUE), 
                                                                         min.DIA = min(DIA), 
                                                                         max.DIA = max(DIA),
                                                                         min.PREVDIA = min (PREVDIA, na.rm = TRUE))

repeat.dbh <- summaries %>% filter(nrecs > 1)


########## note that "T2_FIADB_PLOT" is the "PLOT" to match to the FIA database with!!!
cored <- temp2[, c("PlotNo", "SubplotNo", "TreeNo", "CountyNo", "DBH", "T1_DIA", "T2_DIA", "T1_MEASYR", "T2_MEASYR", "T2_FIADB_PLOT" )] 
colnames(cored)[1:4] <- c("PlotNo", "SUBP", "TREE", "COUNTYCD")
colnames(cored)[10] <- "PLOT"
# using FIADBH to join the plots?
FIADB_PLOT <- temp2[!is.na(temp2$T2_FIADB_PLOT),]$T2_FIADB_PLOT
repeat.dbh %>% filter(PLOT %in% FIADB_PLOT)

# not too many but 
cored.m <- left_join(cored, repeat.dbh, by =c("PLOT", "SUBP", "TREE", "COUNTYCD")) 




# get information from pipo.cores
unique(pipo.cores$STATECD) %in% unique(PIPO$PLOT)
unique(pipo.cores$COUNTYCD) %in% unique(PIPO$COUNTYCD)
unique(pipo.cores$PLOT) %in% unique(PIPO$PLOT)
unique(pipo.cores$SUBP) %in% unique(PIPO$SUBP)
PIPO$SUBP<- as.factor(PIPO$SUBP) 
PIPO$TREE <- as.factor(as.character(PIPO$TREE))


# need to get the xy coords for new plots: in the PLOT fia table. Match up 
ll <- unique(PLOT[,c("PLOT", "COUNTYCD", "LAT", "LON")])

PIPO.ll<- left_join(PIPO, ll, by = c("PLOT", "COUNTYCD"))

not.cored.m <- merge(pipo.cores, PIPO.ll, by =c("PLOT", "SUBP", "TREE", "COUNTYCD", "SPCD")) 



ggplot(not.cored.m, aes( LON, LAT,color = STATUS))+geom_point()

all_states <- map_data("state")
states <- subset(all_states, region %in% c(  "arizona") )
coordinates(states)<- ~long+lat
class(states)
#proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
#mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(states)

png(height = 4, width = 5, units = "in", res = 200, "/Users/kah/Documents/docker_pecan/KH_Notes/CQ_additional_PIPO_cores_for_validation.png")
ggplot(not.cored.m, aes( LON, LAT, color = STATUS))+geom_point()+geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)+theme_bw()+theme(panel.grid = element_blank())#+coord_cartesian(xlim = c(-97,-87), ylim = c(37, 49)) 
dev.off()
# there are 101  CQC cores to work with
not.cored.m %>% group_by(STATUS) %>% summarise(n())




# get core tellervo file names that have been Cofecha checked and QC'd

potential.validation.cores <- pipo.cores %>% filter(STATUS %in% "CQC")

pipo.cores %>% filter(STATUS %in% "CQC") %>% select(FILE)
write.csv(potential.validation.cores, "FIA_inc_data/potential.validation.PIPO.cores.csv")


#-----------------------------------------------
# merge with stand/tree level data:

head(potential.validation.cores)
head(TREE)
pipo.trees <- TREE %>% filter(SPCD == 122)


head(pipo.trees)
unique(TREE[, c("PLOT", "SUBP", "TREE", "COUNTYCD")])
unique(potential.validation.cores[, c("PLOT", "SUBP", "TREE", "COUNTYCD")])

joined.df <- merge(potential.validation.cores, TREE, by =  c("STATECD", "PLOT", "SUBP", "TREE", "COUNTYCD"))

# but we are missing SDI--maybe we can pick cores that fall in plots where we have SDI
head(cov.data)
colnames(cov.data)[1] <- "CC_PLOT"
joined.df$CC_PLOT <- paste0(joined.df$COUNTYCD, joined.df$PLOT)

cov.join <- merge (cov.data, joined.df, by = c("CC_PLOT"))

write.csv(cov.join, "FIA_inc_data/validation_cores_SDI_plot.csv")

