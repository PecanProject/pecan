## example script for processing tree core data
## and fitting a state space model
setwd("~/Dropbox/Ecological Forecasting/Labs/Lab 8 - Tree Rings/")

## Read tree data
trees <- read.csv("H 2012 Adult Field Data.csv")

## Read tree ring data
rings <- Read_Tuscon("Revised 2/")

combined <- matchInventoryRings(trees,rings)

data <- buildJAGSdata_InventoryRings(combined)

jags.out = InventoryGrowthFusion(data,n.iter=20)

pdf("HF_JAM_treerings.pdf")
InventoryGrowthFusionDiagnostics(jags.out)
dev.off()