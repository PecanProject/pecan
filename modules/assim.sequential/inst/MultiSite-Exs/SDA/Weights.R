   library(tidyverse)
   library(mvtnorm)
   library(nimble)
   library(furrr)
   rm(list = ls())
   setwd("/projectnb/dietzelab/hamzed/SDA/ProductionRun/500Sites/Weights")
   load('FORECAST.RData')
   load('ANALYSIS.RData')
   plan(multisession)
   #------------------------------------------------------
   #Loading SDA outputs-----------------------------------
   #------------------------------------------------------
   args <- commandArgs(trailingOnly = TRUE)
   ind <- args[1] %>% as.numeric()
   if(is.na(ind))ind  <- 1
   print(ind)
   
   
   Weights.new <- pmap(list(ANALYSIS[ind],
                            FORECAST[ind],
                            names(FORECAST)[ind]),
                       function(ANALYSIS, FORECAST, Year.applid.weight) {
    
     library(tidyverse)
     library(mvtnorm)
     library(nimble)
      X.original <- FORECAST
      #--- state variables with zeros
      s.v.z <-which(apply(X.original, 2, function(x) any(x==0)))
      
      site.need <- attr(X.original, 'Site')[s.v.z]%>% unique()
      #------------------------------ For each site
      imputed <- site.need %>%
        future_map(possibly(
          function(site.id){
            file.create(file.path("testF", site.id))
            s.v.z.p <- which(attr(X.original, 'Site') == site.id)
            
            X <- X.original[, s.v.z.p]
            #---
            Pf <- cov(X)
            mu.f <- apply(X, 2, mean)
            #--------------------------------------------------------------------------
            #Estimating the weights --
            #--------------------------------------------------------------------------
            PEcAn.assim.sequential:::load_nimble()
            
            intervalX <- matrix(NA, ncol(X), 2)
            rownames(intervalX) <- colnames(X)
            
            intervalX[, 1] <- 0 # lower bound
            intervalX[, 2] <- 300 #upper bound
            
            weight_list <- rep(0, nrow(X))
            wts <- unlist(weight_list)
            #### These vectors are used to categorize data based on censoring from the interval matrix
            x.ind <- x.censored <- matrix(NA, ncol=ncol(X), nrow=nrow(X))
            for(j in seq_along(mu.f)){
              for(n in seq_len(nrow(X))){
                x.ind[n,j] <- as.numeric(X[n,j] > 0)
                x.censored[n,j] <- as.numeric(ifelse(X[n,j] > intervalX[j,2], 0, X[n,j])) #
              }
            }
            
            #The purpose of this step is to impute data for mu.f 
            #where there are zero values so that 
            #mu.f is in 'tobit space' in the full model
            constants.tobit2space <- list(N = nrow(X),
                                          J = length(mu.f))
            
            data.tobit2space <- list(y.ind = x.ind,
                                     y.censored = x.censored,
                                     mu_0 = rep(0,length(mu.f)),
                                     lambda_0 = diag(length(mu.f),
                                                     length(mu.f)+1),
                                     nu_0 = 3,
                                     wts = wts)#some measure of prior obs
            
            inits.tobit2space <- list(pf = cov(X),
                                      muf = colMeans(X))
            
            tobit2space_pred <- nimbleModel(tobit2space.model,
                                            data = data.tobit2space,
                                            constants = constants.tobit2space,
                                            inits = inits.tobit2space,
                                            name = 'space')
            ## Adding X.mod,q,r as data for building model.
            conf_tobit2space <- configureMCMC(tobit2space_pred,
                                              thin = 10,
                                              print=TRUE)
            conf_tobit2space$addMonitors(c("pf", "muf","y.censored")) 
            
            samplerNumberOffset_tobit2space <- length(conf_tobit2space$getSamplers())
            
            for(j in seq_along(mu.f)){
              for(n in seq_len(nrow(X))){
                node <- paste0('y.censored[',n,',',j,']')
                conf_tobit2space$addSampler(node, 'toggle', control=list(type='RW_block'))
              }
            }
            
            #conf_tobit2space$printSamplers()
            
            Rmcmc_tobit2space <- buildMCMC(conf_tobit2space)
            
            Cmodel_tobit2space <- compileNimble(tobit2space_pred)
            Cmcmc_tobit2space <- compileNimble(Rmcmc_tobit2space, project = tobit2space_pred)
            
            for(i in seq_along(X)) {
              valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[i])
            }
            #browser()
            dat.tobit2space <- runMCMC(Cmcmc_tobit2space,
                                       niter = 1000000,
                                       nburnin=400000,
                                       progressBar=TRUE)
            
            iycens <- grep("y.censored",colnames(dat.tobit2space))
            X.new <- matrix(colMeans(dat.tobit2space[,iycens]), nrow(X), ncol(X))
            
            list(list(X.new,
                      X)) %>%
              setNames(site.id)
          }, otherwise = NULL),
          .progress = TRUE)
  
      #------------------------------------------------ Replacing and estimating the X
      imputed <- imputed %>% flatten()
      
      X.new <- imputed %>%
         map(~ .x[[1]]) %>%
         do.call('cbind', .)
      
      ind.rep <- which(attr(X.original, 'Site') %in% names(imputed))
      X.original.clean <- X.original
      X.original.clean[, ind.rep] <- X.new
      
      
      mu.a <- apply(ANALYSIS,2 ,mean)
      Pa <- cov(ANALYSIS)
      
      flux.weights <- dmvnorm(X.original,
                                  mean = mu.a,
                                  sigma = Pa,
                                  log = FALSE)
      
      flux.weights.new <- dmvnorm(X.original.clean,
                                  mean = mu.a,
                                  sigma = Pa,
                                  log = FALSE)

      gc()
      list(x.ori = X.original,
           x.ori.new = X.original.clean,
           weights.new = flux.weights.new,
           weights = flux.weights
           )
   })
         
      saveRDS(Weights.new, file=paste0("W_",ind,".RDS"))
