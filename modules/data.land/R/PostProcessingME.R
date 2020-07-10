### some tree ring post-processing

library(rjags)
#library(PEcAn.data.land)
jags.comb <- NULL
file.base.name <- "X2_Xscaled_forecasted_2018."
output.base.name <- "X2_Xscaled_forecasted_2018"
stage2 <- TRUE
workingdir <- "/home/rstudio/"
#workingdir <- "/Users/kah/Documents/docker_pecan/pecan"
climate <- "wintP.wateryr"
cov.data = cov.data


jags.comb <- NULL

for(i in 175:200){ # note this model stopped early b/c convergence
  load(paste0(workingdir,"/IGF_PIPO_AZ_mcmc/", file.base.name,i,".RData"))
  new.out <- jags.out 
  
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        jags.comb[[j]] <- new.out[[j]][,x.cols]
      }else{
        jags.comb[[j]] <- new.out[[j]]
      }
      
    }
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
#save(jags.comb,file="IGF.waterYear.PPT.RData")
saveRDS(jags.comb,file=paste0("IGF_xvals_est",output.base.name,".rds"))


# also need to load the data used to run the model:

# check for convergence via gelman-rubin
#gelman.diag(jags.comb)

data$time <- 1966:2018 # should have changed before running model but for now change here
source("pecan/modules/data.land/R/InventoryGrowthFusionDiagnostics_KH.R")
pdf("IGF.X2_Xscaled_forecasted_2018.pred.obs.pdf")
InventoryGrowthFusionDiagnostics2(model.out = jags.comb, getRsq = FALSE)
dev.off()
