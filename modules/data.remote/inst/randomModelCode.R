sipnet.out <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/SDA/out/ENS-00001-1000004927/sipnet.2012-07-16.out"
sipnet.out1 <- "/projectnb/dietzelab/hamzed/SDA/ProductionRun/50Sites/SDA_50Sites_1000008768/out/ENS-00001-769/sipnet.out"
text <- readLines(sipnet.out)[-1]
csv <- read.csv(textConnection(text), sep = "")


plot(csv)

site_pft <- read.csv("/projectnb/dietzelab/hamzed/SDA/ProductionRun/50Sites/SDA_50Sites_1000008768/out/ENS-00001-769/sipnet.out")

plot(csv$day,csv$LAI)
plot(csv$day,csv$gpp)

clim.path <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/SDA/run/ENS-00001-646/sipnet.clim"
text.clim <- readLines(clim.path)
csv.clim <- read.csv(textConnection(text.clim), sep="", header = F)
plot(csv.clim$V3, csv.clim$V10)

sipnet.out1 <- "/projectnb/dietzelab/hamzed/SDA/ProductionRun/200Sites/SDA_200Sites_1000008769/out/ENS-00001-622/sipnet.2004-07-16.out"

text1 <- readLines(sipnet.out1)[-1]
csv1 <- read.csv(textConnection(text1), sep = "")
#csv1_1986 <- csv1[csv1$year==1986,]
plot(csv1$day, csv1$nee)

#csv_2001 <- csv[csv$year==2001,]
plot(csv$day, csv$LAI)

param.path <- "/projectnb/dietzelab/dongchen/Multi-site/download_500_sites/SDA/run/ENS-00001-646/sipnet_2001_2002.param"
text.param <- readLines(param.path)
csv.param <- read.csv(textConnection(text.param), sep="", header = F)

#############################################################################################################################
#check on nc file
setwd("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/")
nc.file <- "/projectnb2/dietzelab/dongchen/All_NEON_SDA/NEON42/SDA_testrun/out/ENS-00025-1000004945/2021.nc"
nc <- nc_open(nc.file)
soilMoist <- ncvar_get(nc,'soilMoist')
# AGB <- ncvar_get(nc,"AbvGrndWood")
# LAI <- ncvar_get(nc,"LAI")
# plot(AGB)
# plot(LAI)
h <- nc[["dim"]][["time"]][["vals"]]%%24
time <- PEcAn.utils::cf2datetime(nc[["dim"]][["time"]][["vals"]],nc$dim$time$units)
plot(time)
#############################################################################################################################

#check on the param files
param1.path <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/SDA/run/ENS-00001-1000004927/sipnet.param"
param2.path <- "/projectnb/dietzelab/dongchen/All_NEON_SDA/SDA/run/ENS-00001-646/sipnet.param"
#sipnet 2402
param1 <- readLines(param1.path)
param1 <- read.csv(textConnection(param1), sep="", header = F)
param2 <- readLines(param2.path)
param2 <- read.csv(textConnection(param2), sep="", header = F)