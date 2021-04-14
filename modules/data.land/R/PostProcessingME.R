### some tree ring post-processing

library(rjags)
#library(PEcAn.data.land)
jags.comb <- NULL
for(i in 9:12){
  load(paste0("C:/Users/mekevans/Documents/Cdrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New/ppt5000nocores/IGF.0.",i,".RData"))
  if(is.null(jags.comb)){
    jags.comb <- jags.out
  } else {
    for(j in seq_along(jags.out)){
      x.cols <- grep("^x",colnames(jags.out[[j]]))
      if(length(x.cols)>0) jags.out[[j]] <- jags.out[[j]][,-x.cols]
      jags.comb[[j]]  <- rbind(jags.comb[[j]],jags.out[[j]])
    }
  }
}
for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
pdf("IGF.1.burn.pdf")
InventoryGrowthFusionDiagnostics(jags.comb)
dev.off()