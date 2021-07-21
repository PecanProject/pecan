#Load required libraries
library(PEcAn.allometry)
source('/projectnb/dietzelab/ahelgeso/pecan/modules/allometry/R/query.allom.data.R')
source('/projectnb/dietzelab/ahelgeso/pecan/modules/allometry/R/read.allom.data.R')
#Load in HARV site xml as settings object
outdir <- "/projectnb/dietzelab/ahelgeso/NEON_ic_data/Harvard/allom/"
setwd(outdir)
args = list()
args$settings = "/projectnb/dietzelab/ahelgeso/Site_XMLS/harvard_nosoil.xml"

settings <- PEcAn.settings::read.settings(args$settings)
#set up pfts object from settings
pfts <- settings$pfts
# Create BETY connection
dbparms <- list()
  dbparms$bety$dbname <- "bety"
  dbparms$bety$host <- "psql-pecan.bu.edu"
  dbparms$bety$user <- "bety"
  dbparms$bety$password <- "bety"

bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                            host     = dbparms$bety$host, 
                            user     = dbparms$bety$user, 
                            password = dbparms$bety$password)

con <- bety$con
## Run the Bayesian allometry model
allom.stats = AllomAve(pfts = pfts,ngibbs=500,components=c(3, 18), con=con, outdir = outdir)
## Predict for individual trees
allom.fit = load.allom(getwd())
## Predict for a stand
dbh = filter.TSCA$DBH

stand = allom.predict(allom.fit,dbh = dbh,pft = "TSCA",component = c(3, 18),use = "Bg",interval = "prediction")
#Calculate AGB
AGB = apply(stand,1,sum)
