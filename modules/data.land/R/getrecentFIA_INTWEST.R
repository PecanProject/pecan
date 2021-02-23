# code to get most recent FIA surveys (after 2010) and matach up with our trees to see if we can have "out" of sample validation:
#library(DBI)
library(rFIA)
library(dplyr)
library(ggplot2)
# note that this script uses temp2 and Tree2Tree.incored.plots dataframes from the Rdriver.R script

#if(!exists("/Users/kah/Documents/docker_pecan/pecan/InWeUS_FIA/AZ_COND.csv")){
 # fiadb <- getFIA(states = c("AZ", "UT", "CO", "ID", "WY", "MT"), dir = "InWeUS_FIA", common = FALSE, tables = c("PLOT", "TREE", "COND", "SUBPLOT"), nCores = 1)
#}else{
  fiadb <- readFIA(dir = "InWeUS_FIA")
#}

PLOT <- fiadb$PLOT
SUBPLOT <- fiadb$SUBPLOT
STATECD <- fiadb$STATECD
COND <- fiadb$COND
TREE <- fiadb$TREE
#TREE.sub <- TREE[TREE$INVYR >= 2010,]
unique(TREE$INVYR)
unique(fiadb$PLOT$STATECD)


# read in the non-AZ points:
library(tidyr)
library(ggplot2)
library(dplyr)

full.clim.data <- read.csv("/Users/kah/Documents/docker_pecan/pecan/FIA_inc_data/pipo_all_tmean_ppt_v3.csv")
region.rwl <- read.csv("trees-rwl-1-31-17.csv") # note that this data is has all RWLS in columsn and a year column
region.ll <- read.csv("locs-env-1-31-17.csv")

head(region.ll)


# not sure if these are actually all matching properly.
previous.surveys <-  region.ll[ TREE$CN %in% region.ll$ID,]

TREE.old <- TREE %>% filter(INVYR, )
TREE$MEASYR <- PLOT$MEASYEAR[match(TREE$PLT_CN, TREE$PLT_CN)]
TREE$PLOT_LAT <- PLOT$LAT[match(TREE$PLT_CN, PLOT$CN)]
TREE$PLOT_LON <- PLOT$LON[match(TREE$PLT_CN, PLOT$CN)]

ggplot(TREE %>% filter(SPCD == 122), aes(PLOT_LON, PLOT_LAT))+geom_point()
# we should be able to use STATECD, COUNTYCD, PLOT, SUBP, TREE, and SPCD & DIA from region.ll to match with trees from FIADB

FIA.outside.AZ <- left_join(region.ll[,c("STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD","series", "DIA", "ELEV", "LAT", "LON")], 
                           TREE, by = c("STATECD", "COUNTYCD", "PLOT", "SUBP",  "TREE", "SPCD", "DIA"))

FIA.not.orphaned.plots.nona <- FIA.outside.AZ[!is.na(FIA.outside.AZ$INVYR),]

# now get the repeated survey data for those FIA plots:

unique(FIA.not.orphaned.plots.nona$PLT_CN %in% TREE$PLT_CN)



TREE_REMEAS <- subset(TREE, !is.na(PREVDIA))# keep trees that were remeasured (n = 382530)

ggplot(TREE_REMEAS %>% filter(SPCD == 122), aes(PLOT_LON, PLOT_LAT))+geom_point()

TREE_REMEAS <- subset(TREE_REMEAS, STATUSCD == 1 | STATUSCD == 2)# trees that either lived or died (n= 357303)

TREE_REMEAS <- subset(TREE_REMEAS, STATUSCD == 1)# Only keep trees that didn't die (n= 244816)

# get the previous DIA and INVYR for the previous survey
TREE_REMEAS$PREV_TRE_CN %in% TREE$CN

TREE_REMEAS$PREV_PLT_CN <- TREE$PLT_CN[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$PREV_CONDID <- TREE$CONDID[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$T1_DIA <- TREE$DIA[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]

TREE_REMEAS$T1_INVYR <- TREE$INVYR[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$T1_MEASYR <- TREE$MEASYR[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]
TREE_REMEAS$T2_MEASYR <- TREE_REMEAS$MEASYR
TREE_REMEAS$T2_DIA <- TREE_REMEAS$DIA
TREE_REMEAS$T2_TRE_CN <- TREE_REMEAS$CN
TREE_REMEAS$T1_TRE_CN <- TREE_REMEAS$PREV_TRE_CN

TREE_REMEAS$PREV_PREV_TREE_CN <- TREE$PREV_TRE_CN[match(TREE_REMEAS$PREV_TRE_CN, TREE$CN)]




# how many cored trees of each species?
FIA.outside.AZ %>% group_by(SPCD) %>% summarise(n())
# # A tibble: 4 x 2
# SPCD `n()`
# <int> <int>
#   1   106   415
# 2   113    56
# 3   122   981
# 4   202  1526

# over 981 PIPO trees cored 



# of the coredtrees with two meausrements,  how many for each species?
FIA.not.orphaned.plots.nona %>% group_by(SPCD) %>% summarise(n())
# SPCD `n()`
#    106   289
#    113     8
#    122   371
#    202   872

# 371 pipo trees with at least 2 DBH measurements (outside AZ)



# there are 16131 remeasured PIPO trees in the FIA database
TREE_REMEAS %>% filter(SPCD == 122) %>% summarise(n())

TREE_REMEAS.PIPO <- TREE_REMEAS %>% filter(SPCD == 122)

unique(TREE_REMEAS.PIPO$PLOT_LON)

head(TREE_REMEAS.PIPO)

ggplot()+
  geom_point(data = TREE_REMEAS %>% filter(SPCD == 122), aes(x = PLOT_LON, y = PLOT_LAT), color = "black")+
  geom_point(data = FIA.outside.AZ %>% filter(SPCD == 122), aes(x= LON, y = LAT, color = as.character(INVYR)))


#-------------------------------------------------------------------------------------
# Read in AZ data and combine stand level data
#-------------------------------------------------------------------------------------



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


colnames(FIA.not.orphaned.plots.nona)
         

# we want to format the rest of the data like we have temp2


### read in function that creates jags objects from above data
source("/Users/kah/Documents/GrowthFusion/modules/data.land/R/BuildJAGSdataobject.R")
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


# try just adding the extra data on here:
# adding together the cov.data is relatively straightforward for PIPO
pipo.ll <- region.ll %>% filter(SPCD == 122)
cov.data.az <- cov.data
cov.data.az$STATECD <- 4
cov.data.az.sub <- cov.data.az[,c("STATECD","PLOT","TREE","SICOND", "ELEV" , "SLOPE","ASPECT" )]
cov.data.iw.sub <- pipo.ll[,c("STATECD", "PLOT","TREE","SICOND", "ELEV" , "SLOPE","ASPECT" )]
cov.data.full <- rbind(cov.data.az.sub, cov.data.iw.sub)

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
# now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
ordered.pipo.mat<- spread.pipo.mat[order(match(spread.pipo.mat[,"variable"], pipo.ll[,"series"])),]

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
spread.dia.mat <- pipo.ll.dia %>% group_by(series) %>% tidyr::spread(last_growth, DBH_cm)
# now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
ordered.dia.mat <- spread.dia.mat[order(match(spread.dia.mat[,"series"], pipo.ll[,"series"])),]

df <- ordered.dia.mat[,2:length(ordered.dia.mat)]
colnms <- colnames(z)
rownms <- rownames(df)
#rownames(df) <- c("1", "3", "4", "5")

Missing <- setdiff(colnms, names(df))
df[Missing] <- NA
df <- df[colnms]


z.new <- rbind(z, df)


# now align the climate datasets:
time_data.az <- time_data
head(region.ll)

# read in the larger region climate data:
pipo.clim <- read.csv("FIA_inc_data/pipo_all_tmean_ppt_v4.csv")
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

require(dplyr)
pipo.clim %>% dplyr::select(lon, lat, name, year, PPT_1, PPT_2, PPT_3, PPT_4, PPT_5, PPT_6, PPT_7, PPT_8, PPT_9, PPT_10, PPT_11, PPT_12,
                     TMAX_1, TMAX_2, TMAX_3, TMAX_4, TMAX_5, TMAX_6, TMAX_7, TMAX_8, TMAX_9, TMAX_10, TMAX_11, TMAX_12,
                     wintP.NovAug , 
                    wintP.wateryr , wintP.NM ,    wintP.JJ,       tmax.fallspr  , tmax.JanA,     
                     tmax.MJul    ,  tmax.AprMayJun ,tmax.fall,      tmax.monsoon,   TMAX   )

#spread.pipo.mat[] 
test.melt <- melt(time_data)
colnames(test.melt) <- c("TreeID", "YearID", "Value", "Climate")


clim.spread <- test.melt %>% group_by(TreeID, YearID) %>% spread(Climate, Value)
# each climate variable has 45 years (columns) & 544 cores (rows)

# we want to add the pipo.clim values onto those 45 rows
years <- 1966:2010

pipo.clim.crop <- pipo.clim %>% filter(year %in% years)
pipo.clim.crop$X <- 1:length(pipo.clim.crop$lon)


get_ordered_climate <- function(x){
  

    spread.tmax.fall.spr.mat <- pipo.clim.crop %>% dplyr::select(lon, lat, name, year, x) %>% group_by(lon, lat, name) %>%  tidyr::spread( year, x, drop = T)
    
    
    # now rearrange the rows so they are ordered the same as pipo.ll and cov.data.iw.sub
    spread.tmax.fall.spr.mat.ordered <-  spread.tmax.fall.spr.mat[order(match(spread.tmax.fall.spr.mat$name,  pipo.ll[,"series"])),]
    climate.mat <- spread.tmax.fall.spr.mat.ordered[,4:length(spread.tmax.fall.spr.mat.ordered)]
    climate.mat
    }

tmax.fallspr <- get_ordered_climate("tmax.fallspr")
wintP.wateryr <- get_ordered_climate("wintP.wateryr")
tmax.AprMayJun <- get_ordered_climate("tmax.AprMayJun")
tmax.monsoon <- get_ordered_climate("tmax.monsoon")



time_data$tmax.monsoon <- rbind(time_data$tmax.monsoon, as.matrix(tmax.monsoon))
time_data$tmax.fallspr <- rbind(time_data$tmax.fallspr, as.matrix(tmax.fallspr))
time_data$wintP.wateryr <- rbind(time_data$wintP.wateryr, as.matrix(wintP.wateryr))
time_data$tmax.AprMayJun <- rbind(time_data$tmax.AprMayJun, as.matrix(tmax.AprMayJun))



colnames(region.ll)
colnames(temp2)
AZ.ll <- temp2[,c("keynewshort","T1_TRE_CN", "CountyNo", "PlotNo", "SubplotNo", "TreeNo", "DBH","TREE_SPCD",    
                  "T1_DIA",    "PLOT_LAT",   "PLOT_LON",  "PLOT_ELEV" ,  "COND_ASPECT",  
                  "COND_SLOPE",    "COND_SICOND",       "keynew"   )]

colnames(AZ.ll) <- c("X", "CN", "COUNTYCD", "PLOT", "SUBP", "TREE", "DBH", "SPCD","DIA", 
                     "LAT", "LON", "ELEV", "ASPECT", "SLOPE", "SICOND", "series")


region.ll %>% dplyr::select(colnames(AZ.ll))



#-------------------------------------------------------------------------------------
# Read in AZ tree ring & climate data and combine stand level data
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#IW.fia.cored.plots (not arizona here)
#-------------------------------------------------------------------------------------
PIPO.ll <- region.ll %>% filter(SPCD == 122)
# 981
# this left_join should join the TREE table with lat lon and elevation values from FIA.recnet
IW.FIA.plots <- left_join(TREE, 
                          PIPO.ll[,c("STATECD", "COUNTYCD", "PLOT", "SUBP",  "ELEV", "LAT", "LON")], by = c("STATECD", "COUNTYCD", "PLOT", "SUBP"))

IW.FIA <- IW.FIA.plots[!is.na(IW.FIA.plots$LAT),] # remove all the trees with NA for ELEV (ie. all plots that are not in FIA.recent.plots.nona)
unique(IW.FIA$INVYR)

trees.in.cored.plots <- IW.FIA
# these are the old FIA plots 1984-2000?

BA.NSTEMS.all <- trees.in.cored.plots %>% group_by(INVYR, PLOT, SPCD) %>% summarise(BASAL_AREA = sum(DIA, na.rm =TRUE),
                                                                                    NUM_STEMS = n())

BA.NSTEMS.tot.all <- trees.in.cored.plots %>% group_by(INVYR, PLOT) %>% summarise(TOTAL_BASAL_AREA = sum(DIA, na.rm =TRUE),
                                                                                  TOTAL_NUM_STEMS = n())
sum(BA.NSTEMS.tot.all$TOTAL_NUM_STEMS)

BA.STEMS.all <- left_join(BA.NSTEMS.all, BA.NSTEMS.tot.all, by = c("INVYR", "PLOT"))

BA.STEMS.all$PCT_BA <- (BA.STEMS.all$BASAL_AREA / BA.STEMS.all$TOTAL_BASAL_AREA)*100 
BA.STEMS.all$PCT_NSTEMS <- (BA.STEMS.all$NUM_STEMS / BA.STEMS.all$TOTAL_NUM_STEMS)*100 
BA.STEMS.all$IV <- BA.STEMS.all$PCT_BA + BA.STEMS.all$PCT_NSTEMS



# histogram plot of Importance values for PIPO plots with cores
png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_PIPO_IV_by_INVYR_cored_plots_IW_no_AZ.png")
ggplot(BA.STEMS.all%>% filter(SPCD == 122) , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~INVYR)
dev.off()

ggplot(BA.STEMS.all %>% filter(SPCD == 122), aes(IV))+geom_histogram()+xlab("Importance Value")


# read in the table with the spcd crosswalk 
SPCD.tbl <- read.csv("FIA_inc_data/SPCD_table.csv")
BA.STEMS.all <- left_join(BA.STEMS.all, SPCD.tbl[,c("SPCD", "COMMON_NAME", "SCI_NAME")], by = c("SPCD"))

png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_IV_by_SPCD_cored_plots_IW_no_AZ.png")
ggplot(BA.STEMS.all , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~COMMON_NAME)
dev.off()


# probably want to select the most recent INVYR?? 
most.recentFIA.INVYR <- BA.STEMS.all %>% group_by(PLOT, SPCD) %>% summarise(recentINVYR = max(INVYR))
BA.stems.df <- left_join(BA.STEMS.all, most.recentFIA.INVYR, by = c("PLOT", "SPCD"))

BA.stems.df <- BA.stems.df %>% filter(INVYR == recentINVYR)

png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_IV_by_SPCD_cored_plots_recentINVYR_IW_no_AZ.png")
ggplot(BA.stems.df , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~COMMON_NAME)
dev.off()


png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_PIPO_IV_by_INVYR_cored_plots_recentINVYR_IW_no_AZ.png")
ggplot(BA.stems.df %>% filter(SPCD == 122) , aes(IV))+geom_histogram()+xlab("Importance Value")
dev.off()


# make pie charts:
BA.stems.ba.by.spcd<- BA.stems.df %>% group_by(INVYR, PLOT) %>% tidyr::spread(COMMON_NAME, PCT_BA)

pcts<- BA.stems.df %>% group_by(SPCD, COMMON_NAME) %>% summarise(avg.pct.BA = mean(PCT_BA, na.rm= TRUE),
                                                                 avg.pct.nstems = mean(PCT_NSTEMS, na.rm = TRUE))


pcts.m <- reshape2::melt(pcts, id.vars = c("SPCD", "COMMON_NAME"))
pcts.m$ImportanceValue <- ifelse(pcts.m$variable %in% "avg.pct.BA", "Average Basal Area", "Average # of stems")

png(height = 4, width = 6, units = "in", res = 200, "plot_summaries/Mean_IV_barplot_cored_plots_recentINVYR_IW_no_AZ.png")

ggplot(pcts.m, aes(fill=ImportanceValue, y=value, x=COMMON_NAME)) + 
  geom_bar(position="stack", stat="identity")+ylab("Importance Values")+xlab("")+theme_bw(base_size = 10)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

head(pcts, 20)






