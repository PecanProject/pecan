## Import PROSAIL data file
try(setwd("dev/fortran_data/"), silent = TRUE)
library(data.table)
library(PEcAnRTM)
extra <- 2102:2110

fl <- list.files()
dl <- list()
for(f in fl){
    d <- read.csv(f, header=FALSE)
    d$V11 <- NULL
    dl[[f]] <- do.call(rbind, as.list(t(d)))
}
dl[["brown.dat"]][701:2110] <- 0
dl[["carotenoid.dat"]][201:2110] <- 0
dl[["chlorophyll.dat"]][401:2110] <- 0

dd <- data.frame(dl)[1:2101,]
dt <- data.table(dd)
dt$wl <- 400:2500

setcolorder(dt, c("wl",
                  "refractive.dat",
                  "chlorophyll.dat",
                  "carotenoid.dat",
                  "brown.dat",
                  "water.dat",
                  "drymatter.dat",
                  "energy.dat",
                  "diffuse.dat",
                  "soil1.dat",
                  "soil2.dat"
))

data(dataSpec_p5B)
setnames(dt, c(names(dataSpec_p5B),
               "solar_direct",
               "solar_diffuse",
               "dry_soil",
               "wet_soil"
))

dataSpec_ps5B <- dt
save(dataSpec_ps5B, file="../../data/dataSpec_ps5B.RData")
