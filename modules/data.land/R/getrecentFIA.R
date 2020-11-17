# code to get most recent FIA surveys (after 2010) and matach up with our trees to see if we can have "out" of sample validation:
#library(DBI)
library(rFIA)
library(dplyr)
library(ggplot2)
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
TREE.sub <- TREE[TREE$INVYR >= 2010,]
unique(TREE.sub$INVYR)



# select FIA variable DRYBIO_AG
ag.carbon.plt.spcd <- TREE.sub %>% group_by(PLT_CN, SPCD) %>% 
  summarise(ag.carbon = mean(DRYBIO_AG, na.rm = TRUE)) %>%
  tidyr::spread(SPCD, ag.carbon)

ag.carbon.plt.spcd.long <- TREE.sub %>% group_by(PLT_CN, SPCD) %>% 
  summarise(ag.carbon = sum(CARBON_AG, na.rm = TRUE))

ggplot(ag.carbon.plt.spcd, aes(`122`, `65`))+geom_point()

cor.mat <- cor(ag.carbon.plt.spcd[,2:length(ag.carbon.plt.spcd)],use="pairwise.complete.obs")

no.pipo.corind <- cor.mat[!is.na(cor.mat[,"122"]),]
library(corrplot)
corrplot(no.pipo.corind, method="circle")

cor.pipo <- data.frame(correlations.w.pipo = no.pipo.corind[,"122"], 
              spcd = names(no.pipo.corind[,"122"]))

spcd.df <- data.frame(spcd = unique(cor.pipo$spcd), 
                      common.name = c("white fir", "corkbark fir", "subalpine fir", "Arizona cypress", "alligator juniper", 
                                      "Utah juniper", "Rocky mountain juniper", "oneseed juniper", "Engelmann spruce", "blue spruce",
                                      "two-needle pinyon", "limber pine", "southwestern white pine", "Chihuahua pine", "ponderosa pine", 
                                      "singleleaf pinyon", "border pinyon", "Mexican pinyon pine", "Arizona pinyon pine", "Douglas-fir",
                                      "bigtooth maple", "Arizona elder", "Arizona walnut", "quaking aspen"))

cor.by.spec <- merge(cor.pipo, fia.spcd, by = "spcd")

ggplot(cor.pipo, aes(x = spcd, y = correlations.w.pipo))+geom_bar(stat = "identity")+theme(axis.text.x = element_text(hjust = 1, angle = 45))


ggplot(ag.carbon.plt.spcd, aes(`122`, `15`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `18`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `19`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `51`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `59`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `62`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `63`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `65`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `66`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `69`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `93`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `96`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `106`))+geom_point()
ggplot(ag.carbon.plt.spcd, aes(`122`, `112`))+geom_point()

#--------------------------------------------------------------------------------------
# Select the tree table attributes to check for new remeasurements
#--------------------------------------------------------------------------------------
#TREE.new <- TREE %>% select(CN, PLT_CN, STATUSCD, SPCD, SUBP, PREV_TRE_CN, DIA, INVYR)
setwd("/home/rstudio/pecan/")

### load in the data for trees with increment cores (and 1 or 2 DBH measurements)
AZ.PIPO <- read.delim("FIA_inc_data/AZ_FIA_RWL_PRISM_allinone_04192017.txt", stringsAsFactors = F) ### 820 trees

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

post.2010.validation <- cored.m %>% filter(! is.na(max.DIA))

saveRDS(post.2010.validation, "FIA_inc_data/post2010.core.validation.rds")
# DIAM.df <- post.2010.validation %>% select(PlotNo, PLOT, SUBP, TREE, DBH, T1_DIA, T2_DIA, min.invyr, max.invyr, min.DIA, max.DIA) %>%
#   group_by(PlotNo, PLOT, SUBP, TREE) %>% mutate(dbh.meas = c(DBH, T1_DIA, T2_DIA, min.DIA, max.DIA))
post.2010.validation<- readRDS("FIA_inc_data/post2010.core.validation.rds")
#------------------------------------------------------------
# Now lets get the validation data for the TREE2TREE dataset:
Tree2Tree <- read.csv("FIA_inc_data/Tree2Tree.csv", stringsAsFactors = F)

### limit analysis to those trees with second DBH measurement in =< year 2015
### this is because as of 7/2017, the available PRISM data (KNMI) go only to Dec 2015
Tree2Tree <- Tree2Tree[Tree2Tree$T2_MEASYR<=2015,]

### eliminate those cases without SI (SDI seems to always be there)
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SICOND),]
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SDIc),]

Tree2Tree <- Tree2Tree[Tree2Tree$T2_MEASYR<=2015,]

### eliminate those cases without SI (SDI seems to always be there)
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SICOND),]
Tree2Tree <- Tree2Tree[!is.na(Tree2Tree$SDIc),]


# read in the FIA condition table to get DISTURBYR along with each disturbance code
FIA.COND <- COND
Tree2Tree <- merge(FIA.COND[c("DSTRBYR1", "DSTRBYR2", "DSTRBYR3", "CN", "PLT_CN", "INVYR", "PLOT")], Tree2Tree, by.x = "PLT_CN", by.y = "T1_PLT_CN")
unique(temp2$Plot) %in% unique(Tree2Tree$T1_PLOT)
incPlot.in.dbhPlot <- (unique(temp2$Plot) %in% unique(Tree2Tree$T1_PLOT) == TRUE)
length(incPlot.in.dbhPlot[incPlot.in.dbhPlot ==TRUE])



# or you wish to not include trees without cores
T2T.nodup <- Tree2Tree[!duplicated(Tree2Tree),]

# sample from DBH measurements that only exist in plots that were also cored

Tree2Tree.incored.plots <- Tree2Tree[paste0(Tree2Tree$COUNTYCD, Tree2Tree$PLOT) %in% paste0(newtemp2$CountyNo, newtemp2$PlotNo) ,]



# there are 9595 PIPO trees that we appear to have remeasurements from:
previous.surveys <-  PIPO[PIPO$CN %in% remeasured.PIPO$PREV_TRE_CN,]
summaries <- PIPO %>% group_by(PLOT, SUBP, TREE, COUNTYCD) %>% summarise(nrecs = n(), 
                                                                         min.invyr = min(INVYR, na.rm = TRUE), 
                                                                         max.invyr = max(INVYR, na.rm = TRUE), 
                                                                         min.DIA = min(DIA), 
                                                                         max.DIA = max(DIA),
                                                                         min.PREVDIA = min (PREVDIA, na.rm = TRUE))

repeat.dbh <- summaries %>% filter(nrecs > 1)

# now lets see how many of these are repeated in the tree2tree database:
colnames(Tree2Tree.incored.plots)
train.dbh <- Tree2Tree.incored.plots[, c("T1_PLOT", "T1_SUBP", "T1_TREE", "COUNTYCD", "T1_DIA", "T2_DIA", "T1_MEASYR", "T2_MEASYR", "T2_FIADB_PLOT" )] 
colnames(train.dbh)[1:4] <- c("PlotNo", "SUBP", "TREE", "COUNTYCD")
colnames(train.dbh)[9] <- "PLOT"

# using FIADBH to join the plots?
FIADB_PLOT <- train.dbh[!is.na(train.dbh$T2_FIADB_PLOT),]$T2_FIADB_PLOT
repeat.dbh %>% filter(PLOT %in% FIADB_PLOT)

# not too many but 
train.m <- left_join(train.dbh, repeat.dbh, by =c("PLOT", "SUBP", "TREE", "COUNTYCD")) 

post.2010.validation <- train.m %>% filter(! is.na(max.DIA))
# we have ~ 3840 trees from the tree 2 tree database that have more measurements
saveRDS(post.2010.validation, "FIA_inc_data/post2010.core.validation.tree2tree.rds")



#----------------------------------------------------------------------------------------
# summarise the other trees on the FIA plots:

FIADB_PLOT <- temp2[!is.na(temp2$T2_FIADB_PLOT),]$T2_FIADB_PLOT
trees.in.cored.plots <- TREE %>% filter(PLOT %in% FIADB_PLOT)



# we want the importance value, defined as:
#Importance Value = % Basal Area of focal species + % # of stems of focal species

BA.NSTEMS.all <- trees.in.cored.plots %>% group_by(INVYR, PLOT, SPCD) %>% summarise(BASAL_AREA = sum(DIA, na.rm =TRUE),
                                                                       NUM_STEMS = n())

BA.NSTEMS.tot.all <- trees.in.cored.plots %>% group_by(INVYR, PLOT) %>% summarise(TOTAL_BASAL_AREA = sum(DIA, na.rm =TRUE),
                                                                                    TOTAL_NUM_STEMS = n())


BA.STEMS.all <- left_join(BA.NSTEMS.all, BA.NSTEMS.tot.all, by = c("INVYR", "PLOT"))

BA.STEMS.all$PCT_BA <- (BA.STEMS.all$BASAL_AREA / BA.STEMS.all$TOTAL_BASAL_AREA)*100 
BA.STEMS.all$PCT_NSTEMS <- (BA.STEMS.all$NUM_STEMS / BA.STEMS.all$TOTAL_NUM_STEMS)*100 
BA.STEMS.all$IV <- BA.STEMS.all$PCT_BA + BA.STEMS.all$PCT_NSTEMS



# histogram plot of Importance values for PIPO plots with cores
png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_PIPO_IV_by_INVYR_cored_plots.png")
ggplot(BA.STEMS.all%>% filter(SPCD == 122) , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~INVYR)
dev.off()


ggplot(BA.STEMS.all %>% filter(SPCD == 122), aes(IV))+geom_histogram()+xlab("Importance Value")


# read in the table with the spcd crosswalk 
SPCD.tbl <- read.csv("FIA_inc_data/SPCD_table.csv")
BA.STEMS.all <- left_join(BA.STEMS.all, SPCD.tbl[,c("SPCD", "COMMON_NAME", "SCI_NAME")], by = c("SPCD"))

png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_IV_by_SPCD_cored_plots.png")
ggplot(BA.STEMS.all , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~COMMON_NAME)
dev.off()


# probably want to select the most recent INVYR?? 
most.recentFIA.INVYR <- BA.STEMS.all %>% group_by(PLOT, SPCD) %>% summarise(recentINVYR = max(INVYR))
BA.stems.df <- left_join(BA.STEMS.all, most.recentFIA.INVYR, by = c("PLOT", "SPCD"))

BA.stems.df <- BA.stems.df %>% filter(INVYR == recentINVYR)

png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_IV_by_SPCD_cored_plots_recentINVYR.png")
ggplot(BA.stems.df , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~COMMON_NAME)
dev.off()


png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_PIPO_IV_by_INVYR_cored_plots_recentINVYR.png")
ggplot(BA.stems.df %>% filter(SPCD == 122) , aes(IV))+geom_histogram()+xlab("Importance Value")
dev.off()


# make pie charts:
BA.stems.ba.by.spcd<- BA.stems.df %>% group_by(INVYR, PLOT) %>% tidyr::spread(COMMON_NAME, PCT_BA)

pcts<- BA.stems.df %>% group_by(SPCD, COMMON_NAME) %>% summarise(avg.pct.BA = mean(PCT_BA, na.rm= TRUE),
                                                          avg.pct.nstems = mean(PCT_NSTEMS, na.rm = TRUE))


pcts.m <- reshape2::melt(pcts, id.vars = c("SPCD", "COMMON_NAME"))
pcts.m$ImportanceValue <- ifelse(pcts.m$variable %in% "avg.pct.BA", "Average Basal Area", "Average # of stems")

png(height = 4, width = 6, units = "in", res = 200, "plot_summaries/Mean_IV_barplot_cored_plots_recentINVYR.png")

ggplot(pcts.m, aes(fill=ImportanceValue, y=value, x=COMMON_NAME)) + 
  geom_bar(position="stack", stat="identity")+ylab("Importance Values")+xlab("")+theme_bw(base_size = 10)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
# ------------------------------------------------------------------------------------------   
# do the same for Tree2Tree dataset with just the PIPO trees
# ------------------------------------------------------------------------------------------ 

#summarise the other trees on the FIA plots:
  
  FIADB_PLOT <- train.m[!is.na(train.m$PLOT),]$PLOT
trees.in.Tree2Tree.cored <- TREE %>% filter(PLOT %in% FIADB_PLOT)



# we want the importance value, defined as:
#Importance Value = % Basal Area of focal species + % # of stems of focal species

BA.NSTEMS.t2t <- trees.in.Tree2Tree.cored %>% group_by(INVYR, PLOT, SPCD) %>% summarise(BASAL_AREA = sum(DIA, na.rm =TRUE),
                                                                                        NUM_STEMS = n())

BA.NSTEMS.tot.t2t <- trees.in.Tree2Tree.cored %>% group_by(INVYR, PLOT) %>% summarise(TOTAL_BASAL_AREA = sum(DIA, na.rm =TRUE),
                                                                                      TOTAL_NUM_STEMS = n())


BA.STEMS.t2t <- left_join(BA.NSTEMS.t2t, BA.NSTEMS.tot.t2t, by = c("INVYR", "PLOT"))

BA.STEMS.t2t$PCT_BA <- (BA.STEMS.t2t$BASAL_AREA / BA.STEMS.t2t$TOTAL_BASAL_AREA)*100 
BA.STEMS.t2t$PCT_NSTEMS <- (BA.STEMS.t2t$NUM_STEMS / BA.STEMS.t2t$TOTAL_NUM_STEMS)*100 
BA.STEMS.t2t$IV <- BA.STEMS.t2t$PCT_BA + BA.STEMS.t2t$PCT_NSTEMS



# histogram plot of Importance values for PIPO plots with cores
png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_PIPO_IV_by_INVYR_repeatdbh_plots.png")
ggplot(BA.STEMS.t2t%>% filter(SPCD == 122) , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~INVYR)
dev.off()


ggplot(BA.STEMS.t2t %>% filter(SPCD == 122), aes(IV))+geom_histogram()+xlab("Importance Value")


# read in the table with the spcd crosswalk 
SPCD.tbl <- read.csv("FIA_inc_data/SPCD_table.csv")
BA.STEMS.t2t <- left_join(BA.STEMS.t2t, SPCD.tbl[,c("SPCD", "COMMON_NAME", "SCI_NAME")], by = c("SPCD"))

png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_IV_by_SPCD_repeat_dbh_plots.png")
ggplot(BA.STEMS.t2t , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~COMMON_NAME)
dev.off()


# probably want to select the most recent INVYR?? 
most.recentFIA.INVYR <- BA.STEMS.t2t %>% group_by(PLOT, SPCD) %>% summarise(recentINVYR = max(INVYR))
BA.stems.df <- left_join(BA.STEMS.t2t, most.recentFIA.INVYR, by = c("PLOT", "SPCD"))

BA.stems.df <- BA.stems.df %>% filter(INVYR == recentINVYR)

png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_IV_by_SPCD_repeat_dbh_plots_recentINVYR.png")
ggplot(BA.stems.df , aes(IV))+geom_histogram()+xlab("Importance Value")+facet_wrap(~COMMON_NAME)
dev.off()


png(height = 8, width = 10, units = "in", res = 200, "plot_summaries/histogram_PIPO_IV_by_INVYR_repeat_dbh_plots_recentINVYR.png")
ggplot(BA.stems.df %>% filter(SPCD == 122) , aes(IV))+geom_histogram()+xlab("Importance Value")
dev.off()




#-----------------------------------------------------------------------
# get data from plots with just PIPO--can we scale these plots up?
#-----------------------------------------------------------------------


PLOT.INVYR.all.PIPO <- unique(BA.stems.df %>% filter(IV == 200) %>% select(PLOT))
length(unique(PLOT.INVYR.all.PIPO$PLOT)) # there are 53 plots with 100% PIPO trees in AZ:

#find these plots in the stage 1 + stage 2 results + get biomass 
