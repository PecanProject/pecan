# script that looks at all ppt model combinations, checks for convergence and looks at uncertainty parameter values:

# check models for convergence & plot:


library(rjags)
library(PEcAn.data.land)
library(pryr)
library(gridExtra)
library(psych)


# basic function to load and plot converge of the last few saved files (last 2000 iterations)

source("modules/data.land/R/quick.convergence.check.R")

# full PPT model noX2
png(height = 10, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.full.traceplots.png")
par(mfrow =c(4,4))
quick.convergence.check(filename.base = "PPT.noX2.r.10000.", 
                        last.filenum = 100, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
dev.off()

# PPT model no interactions
png(height = 10, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.noX2.noint.traceplots.png")
par(mfrow =c(4,4))
quick.convergence.check(filename.base = "PPT.noX2.noint.5000.", 
                        last.filenum = 123, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
dev.off()


# PPT model no X2 + no X interactions
png(height = 10, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.noX2.noXint.traceplots.png")
par(mfrow =c(4,4))
quick.convergence.check(filename.base = "PPT.noX2.noXint.15000.", 
                        last.filenum = 150, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
dev.off()

# PPT model with only SDI (no SICOND)
png(height = 10, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.noX2.SDIonly.traceplots.png")
par(mfrow =c(4,4))
quick.convergence.check(filename.base = "PPT.noX2.SDIonly.15000.", 
                        last.filenum = 122, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
dev.off()

# PPT model with only SICOND (no SDI)
png(height = 10, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.SICONDonly.traceplots.png")
par(mfrow =c(4,4))
quick.convergence.check(filename.base = "PPT.noX2.SICONDonly.15000.", 
                        last.filenum = 150, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
dev.off()


# PPT + tmax model full
png(height = 15, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.TMAX.full.traceplots.png")
par(mfrow =c(6,4))
quick.convergence.check(filename.base = "PPT.TMAXfs.noX2.25000.", 
                        last.filenum = 250, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
                        #file.folder =  "/Users/kah/Documents/docker_pecan/pecan/IGF_PIPO_AZ_mcmc/")
dev.off()

# okay now read in the last 1000 iterations of each model and compare uncertainties + model parameters

# 1. read in 1000 iterations
# 2. extract all model parameters
# 3. extract all taus

# function that we will use to read in all the files

get.params <- function(filename.base, last.filenum, file.folder = "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/"){
  
  jags.comb <- NULL
  
  for(i in (last.filenum-10):last.filenum){ # assumes that there are 100 iterations in each file so 10 files = 1000 iterations
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
  
  # make sure they are mcmc objects
  for(i in 1:3){
    jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
  }
  jags.comb <- as.mcmc.list(jags.comb)
  
  colnames(jags.comb[[1]])
  
  # get all monitored non-alpha parameters:
  alpha.cols <- grep(pattern = "alpha", colnames(jags.comb[[1]]))
  jags.comb.subset <- list()
  
  for(j in 1:3){
    jags.comb.subset[[j]] <- jags.comb[[j]][,-alpha.cols]
  }
  
  jags.comb.subset
  
  # end function
}
  

# use get.taus to 

# quick.convergence.check(filename.base = "PPT.noX2.r.10000.", 
#                         last.filenum = 100, 
#                         file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")

# PPT model no interactions
PPT.noX2.full <- get.params(filename.base = "PPT.noX2.r.10000.", 
                             last.filenum = 100, 
                             file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")


PPT.noX2.noint <- get.params(filename.base = "PPT.noX2.noint.5000.", 
                        last.filenum = 123, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
# PPT model no X2 + no X interactions
PPT.noX2.noXint <- get.params(filename.base = "PPT.noX2.noXint.15000.", 
                        last.filenum = 150, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")
# PPT model with only SDI (no SICOND)
PPT.noX2.SDIonly <- get.params(filename.base = "PPT.noX2.SDIonly.15000.", 
                        last.filenum = 122, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")

# PPT model with only SICOND (no SDI)
PPT.noX2.SICONDonly <- get.params(filename.base = "PPT.noX2.SICONDonly.15000.", 
                        last.filenum = 150, 
                        file.folder =  "/home/rstudio/pecan/IGF_PIPO_AZ_mcmc/")




tau.list <- list(PPT.noX2.full,PPT.noX2.noint, PPT.noX2.noXint, PPT.noX2.SDIonly, PPT.noX2.SICONDonly)

# create a function to take summaries of all the mcmcs
summarise.mcmc <- function(x){
      list.m <- list()
      for(i in 1:3){
        list.m[[i]] <- reshape2::melt(data.frame(x[[i]]))
      }
      
      df.m <- do.call(rbind, list.m)
      
      
      df.m$variable
      df.summary <- df.m %>% group_by(variable) %>% summarise ( mean = mean(value, na.rm =TRUE), 
                                              ci.low = quantile(value, 0.025, na.rm=TRUE), 
                                              ci.high = quantile(value, 0.975, na.rm =TRUE))
      
      df.summary
}

summaries<- lapply(tau.list, summarise.mcmc)
names(summaries) <- c("PPT.noX2.full", "PPT.noX2.noint", "PPT.noX2.noXint", "PPT.noX2.SDIonly", "PPT.noX2.SICONDonly")
  
  
summaries.df <- do.call(rbind, summaries)
summaries.df$model <- rep(names(summaries), sapply(summaries, nrow))

param.plots <- ggplot(summaries.df, aes(model, mean,color = model))+geom_point()+
  geom_errorbar(aes(x = model, ymin = ci.low, ymax = ci.high, color = model))+ylab("Parameter Estimates")+
  facet_wrap(~variable, scales = "free_y") + theme_bw(base_size = 10)+theme(axis.text.x = element_text(angle = 45, hjust = 1))


png(height = 10, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT_model_noX2_params.png")
param.plots
dev.off()


# plot correlations between variables in all models:

