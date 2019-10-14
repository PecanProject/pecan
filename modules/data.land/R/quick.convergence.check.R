# Author: Kelly Heilman
# Date Created: 10/14/19
# Quick Convergence Check Function

# this function takes the MCMC outputs saved (each should have ~100 iterations) and plots traceplots and gbr stats
# just need to list the filename base, the last filenumber, and file folder
quick.convergence.check <- function(last.filenum, filename.base, file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/"){
  jags.comb <- NULL
  
  for(i in (last.filenum-25):last.filenum){
    load(paste0(file.folder, filename.base,i,".RData"))
    new.out <- jags.out 
    if(is.null(jags.comb)){
      for(j in seq_along(new.out)){
        x.cols <- grep("^x",colnames(new.out[[j]]))
        new.out[[j]] <- new.out[[j]][,-x.cols]}
      
      jags.comb <- new.out[,-x.cols]
      
    } else {
      for(j in seq_along(new.out)){
        x.cols <- grep("^x",colnames(new.out[[j]]))
        
        if(length(x.cols)>0){ 
          new.out[[j]] <- new.out[[j]][,-x.cols]
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
  
  colnames(jags.comb[[1]])
  
  # plot all the traces for all monitored parameters except for the plot random effects:
  alpha.cols <- grep(pattern = "alpha", colnames(jags.comb[[1]]))
  jags.comb.subset <- list()
  for(j in 1:3){
    jags.comb.subset[[j]] <- jags.comb[[j]][,-alpha.cols]
  }
  
  traceplot(jags.comb.subset)
  gelman.diag(jags.comb.subset)
}

