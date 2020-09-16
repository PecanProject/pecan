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

post.2010.validation <- cored.m %>% filter(! is.na(max.DIA))

saveRDS(post.2010.validation, "FIA_inc_data/post2010.core.validation.rds")
# DIAM.df <- post.2010.validation %>% select(PlotNo, PLOT, SUBP, TREE, DBH, T1_DIA, T2_DIA, min.invyr, max.invyr, min.DIA, max.DIA) %>%
#   group_by(PlotNo, PLOT, SUBP, TREE) %>% mutate(dbh.meas = c(DBH, T1_DIA, T2_DIA, min.DIA, max.DIA))

#------------------------------------------------------------
# Now lets get the validation data for the TREE2TREE dataset:
colnames(Tree2Tree.incored.plots)


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
