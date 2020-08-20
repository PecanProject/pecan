 # analysis of the other trees in our FIA plots
# are most PIPO trees in releatively homogenous pipo plots, or are they mixed?

library(rFIA)
library(dplyr)
library(here)
library(dplyr)

# note that this script uses temp2 and Tree2Tree.incored.plots dataframes from the Rdriver.R script
if(!exists(paste0(here(), "/recentFIA/AZ_COND.csv"))){
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
head(PLOT)
head(SUBPLOT) # PLT_CN = CN for the PLOT table
head(COND) # PLT_CN = CN for the PLOT table
head(TREE) # PLT_CN = CN for the PLOT table; PREV_TRE_CN is the CN for the tree in a prvious servey, if applicable:

#-----------------Linking plot, cond, subplot and tree tables--------------------
# select the columsn of interest
PLOT <- PLOT %>% select("CN","PLOT", "INVYR", "UNITCD","STATECD", "COUNTYCD","LAT", "LON", "ELEV", "PLOT_STATUS_CD" )
COND <- COND %>% select("CN","PLT_CN", "PLOT", "INVYR", "UNITCD","STATECD", "COUNTYCD", "CONDID", "COND_STATUS_CD", "STDAGE", "SICOND", "SIBASE", 
                        "SLOPE", "ASPECT", "BALIVE" ) # add whatever else we might want!
#also select for subplot

# note that currenly this addes a bunch of cn.y and cn.x onto the dataframe
# should rename or remove for the cond, subp, or tree to cond_cn, subp_cn, tree_cn
# could use data.table or something else
# check the order of this:
# join the condition and PLOT tables
cond.plt <- merge(PLOT, COND, by.x = c("CN","PLOT", "INVYR", "UNITCD","STATECD", "COUNTYCD"), by.y = c("PLT_CN", "PLOT", "INVYR", "UNITCD","STATECD", "COUNTYCD"))

# join the subplot table to this so that we can join trees by subplot too 
subp.plt <- merge(cond.plt, SUBPLOT, by.x = c("CN","PLOT", "INVYR", "UNITCD","STATECD", "COUNTYCD"), by.y = c("PLT_CN", "PLOT", "INVYR", "UNITCD","STATECD", "COUNTYCD"))

# now link up the the tree table using plot cn, plot, subplot, invyr, unit, and state
tree.plt <- merge(subp.plt, TREE, by.x = c("CN","PLOT","SUBP", "INVYR", "UNITCD","STATECD", "COUNTYCD"), by.y = c("PLT_CN", "PLOT", "SUBP","INVYR", "UNITCD","STATECD", "COUNTYCD"))


# so to join the plot level infomration:



# Select the tree table attributes to check for new remeasurements
#TREE.new <- TREE %>% select(CN, PLT_CN, STATUSCD, SPCD, SUBP, PREV_TRE_CN, DIA, INVYR)

PIPO <- TREE %>% filter(SPCD %in% "122")
# all FIA

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

# find all the other trees from the plot:
all.plot.trees <- TREE %>% filter(PLOT %in% FIADB_PLOT)


summary.of.other.trees <- all.plot.trees %>% group_by(PLOT) %>% summarise(n = length(unique(SPCD)),
                                                ntrees = length(unique(TREE)))
ggplot(summary.of.other.trees, aes(n, ntrees))+geom_point()

only.one.spec <- summary.of.other.trees %>% filter(n <2)
all.PIPO <- TREE %>% filter(PLOT %in% only.one.spec$PLOT)
mixed.pipo <- TREE %>% filter(!PLOT %in% only.one.spec$PLOT)
count.spec.by.plot <- mixed.pipo %>% group_by(PLOT, SPCD) %>% summarise(n = n())

count.by.plot <- mixed.pipo %>% group_by(PLOT) %>% summarise(nplot = n())
# caclulate total plot number of trees
count.spec.by.plot2 <- merge( count.spec.by.plot, count.by.plot[,c("PLOT", "nplot")], by = "PLOT")
# calculate the proportion of the plot that pip makes up vs other spcec
count.spec.by.plot2$prop.spec <- (count.spec.by.plot2$n / count.spec.by.plot2$nplot)*100

read.csv
# 52 plots with all PIPO

#4724 with mixed PIPO and another species

