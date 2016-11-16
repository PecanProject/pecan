library(devtools)
load_all("~/Projects/pecan/pecan/modules/rtm")

sampfile <- "autosamples.rds"
samps <- readRDS(sampfile)
smcmc <- PEcAn.assim.batch::makeMCMCList(samps$samps.list)

plot(smcmc[,5])
