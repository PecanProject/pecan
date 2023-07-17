##' @title analysis_sda_block
##' @name  analysis_sda_block
##' @author Dongchen Zhang
##' 
##' @param settings  pecan standard multi-site settings list.  
##' @param block.list.all List contains nt empty sub-elements.
##' @param X A matrix contains ensemble forecasts.
##' @param obs.mean List of dataframe of observation means, named with observation datetime.
##' @param obs.cov   List of covariance matrices of state variables , named with observation datetime.
##' @param t time point.
##' @param nt total length of time steps.
##' @param MCMC.args arguments for the MCMC sampling.
##' @details This function will add data and constants into each block that are needed for the MCMC sampling.
##'  
##' @description This function provides the block-based MCMC sampling approach.
##' 
##' @return It returns the `build.block.xy` object and the analysis results.
##' @importFrom dplyr %>%
analysis_sda_block <- function (settings, block.list.all, X, obs.mean, obs.cov, t, nt, MCMC.args) {
  #convert from vector values to block lists.
  if ("try-error" %in% class(try(block.results <- build.block.xy(settings = settings, 
                                                                 block.list.all = block.list.all, 
                                                                 X = X, 
                                                                 obs.mean = obs.mean, 
                                                                 obs.cov = obs.cov, 
                                                                 t = t)))) {
    PEcAn.logger::logger.error("Something wrong within the build.block.xy function.")
  }
  #grab block.list and H from the results.
  block.list.all <- block.results[[1]]
  H <- block.results[[2]]
  Y <- block.results[[3]]
  R <- block.results[[4]]
  
  #update q.
  if ("try-error" %in% class(try(block.list.all <- update_q(block.list.all, t, nt)))) {
    PEcAn.logger::logger.error("Something wrong within the update_q function.")
  }
  
  #add initial conditions.
  if ("try-error" %in% class(try(block.list.all[[t]] <- MCMC_Init(block.list.all[[t]], X)))) {
    PEcAn.logger::logger.error("Something wrong within the MCMC_Init function.")
  }
  
  #update MCMC args.
  block.list.all[[t]] <- block.list.all[[t]] %>% 
    purrr::map(function(l){
      l$MCMC <- MCMC.args
      l
    })
  
  #parallel for loop over each block.
  PEcAn.logger::logger.info(paste0("Running MCMC ", "for ", length(block.list.all[[t]]), " blocks"))
  if ("try-error" %in% class(try(block.list.all[[t]] <- furrr::future_map(block.list.all[[t]], MCMC_block_function, .progress = T)))) {
    PEcAn.logger::logger.error("Something wrong within the MCMC_block_function function.")
  }
  PEcAn.logger::logger.info("Completed!")
  
  #convert from block lists to vector values.
  if ("try-error" %in% class(try(V <- block.2.vector(block.list.all[[t]], X, H)))) {
    PEcAn.logger::logger.error("Something wrong within the block.2.vector function.")
  }
  
  #return values
  return(list(block.list.all = block.list.all,
         mu.f = V$mu.f,
         Pf = V$Pf,
         mu.a = V$mu.a,
         Pa = V$Pa,
         Y = Y,
         R = R))
}

##' @title build.block.xy
##' @name  build.block.xy
##' @author Dongchen Zhang
##' 
##' @param settings  pecan standard multi-site settings list.  
##' @param block.list.all List contains nt empty sub-elements.
##' @param X A matrix contains ensemble forecasts.
##' @param obs.mean List of dataframe of observation means, named with observation datetime.
##' @param obs.cov   List of covariance matrices of state variables , named with observation datetime.
##' @param t time point.
##' @details This function will add data and constants into each block that are needed for the MCMC sampling.
##'  
##' @description This function split long vector and covariance matrix into blocks corresponding to the localization.
##' 
##' @return It returns the `build.block.xy` object with data and constants filled in.
build.block.xy <- function(settings, block.list.all, X, obs.mean, obs.cov, t) {
  #set q.type from settings.
  if (settings$state.data.assimilation$q.type == "vector") {
    q.type <- 1
  } else if (settings$state.data.assimilation$q.type == "wishart") {
    q.type <- 2
  }
  
  #grab basic arguments based on X.
  site.ids <- unique(attributes(X)$Site)
  var.names <- unique(attributes(X)$dimnames[[2]])
  mu.f <- colMeans(X)
  Pf <- stats::cov(X)
  diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5 #fixing det(Pf)==0
  
  #distance calculations and localization
  if (!is.null(settings$state.data.assimilation$Localization.FUN)) {
    Localization.FUN <- get(settings$state.data.assimilation$Localization.FUN)
    site.locs <- settings$run %>% 
      purrr::map('site') %>% 
      purrr::map_dfr(~c(.x[['lon']],.x[['lat']]) %>% as.numeric)%>% 
      t %>%
      `colnames<-`(c("Lon","Lat")) %>%
      `rownames<-`(site.ids)
    #Finding the distance between the sites
    dis.matrix <- sp::spDists(site.locs, longlat = TRUE)
    #turn that into a blocked matrix format
    blocked.dis <- block_matrix(dis.matrix %>% as.numeric(), rep(length(var.names), length(site.ids)))
    Pf <- Localization.FUN(Pf, blocked.dis, settings$state.data.assimilation$scalef %>% as.numeric())
  }
  
  #Handle observation
  Obs.cons <- Construct.R(site.ids, var.names, obs.mean[[t]], obs.cov[[t]])
  Y <- Obs.cons$Y
  R <- Obs.cons$R
  if (length(Y) > 1) {
    PEcAn.logger::logger.info("The zero variances in R and Pf is being replaced by half and one fifth of the minimum variance in those matrices respectively.")
    diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
  }
  #create matrix the describes the support for each observed state variable at time t
  min_max <- settings$state.data.assimilation$state.variables %>% 
    purrr::map(function(state.variable){
      c(as.numeric(state.variable$min_value),
        as.numeric(state.variable$max_value))
    }) %>% unlist() %>% as.vector() %>% 
    matrix(length(settings$state.data.assimilation$state.variables), 2, byrow = T) %>%
    `rownames<-`(var.names)
  #Create y.censored and y.ind
  #describing if the obs are within the defined range.
  y.ind <- y.censored <- c()
  for (i in seq_along(Y)) {
    if (Y[i] > min_max[names(Y[i]), 1]) {
      y.ind[i] = 1; y.censored[i] = Y[i]
    } else {y.ind[i] <- y.censored[i] <- 0}
  }
  #observation number per site
  obs_per_site <- obs.mean[[t]] %>% 
    purrr::map(function(site.obs){length(site.obs)}) %>% 
    unlist()
  #create H
  H <- construct_nimble_H(site.ids = site.ids,
                          var.names = var.names,
                          obs.t = obs.mean[[t]],
                          pft.path = settings[[1]]$run$inputs$pft.site$path,
                          by = "block_pft_var")
  
  #start the blocking process
  #should we consider interactions between sites?
  if(as.logical(settings$state.data.assimilation$by.site)){
    block.list <- vector("list", length(site.ids))
    #loop over sites
    for (i in seq_along(site.ids)) {
      #store which block contains which sites.
      block.list[[i]]$sites.per.block <- i
      block.list[[i]]$site.ids <- site.ids[i]
      block.list[[i]]$t <- t
      
      #fill in mu.f and Pf
      f.start <- (i - 1) * length(var.names) + 1
      f.end <- i * length(var.names)
      block.list[[i]]$data$muf <- mu.f[f.start:f.end]
      block.list[[i]]$data$pf <- Pf[f.start:f.end, f.start:f.end]
      
      #fill in y and r
      y.start <- obs_per_site[i] * (i - 1) + 1
      y.end <- obs_per_site[i] * i
      block.list[[i]]$data$y.censored <- y.censored[y.start:y.end]
      block.list[[i]]$data$r <- solve(R[y.start:y.end, y.start:y.end])
      
      #fill in constants.
      block.h <- Construct.H.multisite(site.ids[i], var.names, obs.mean[[t]])
      block.list[[i]]$constant$H <- which(apply(block.h, 2, sum) == 1)
      block.list[[i]]$constant$N <- length(f.start:f.end)
      block.list[[i]]$constant$YN <- length(y.start:y.end)
      block.list[[i]]$constant$q.type <- q.type
    }
  } else {
    #find networks given TRUE/FALSE matrix representing sites' interactions.
    block.vec <- matrix_network(dis.matrix <= as.numeric(settings$state.data.assimilation$scalef))
    block.list <- vector("list", length(block.vec))
    #loop over sites
    for (i in seq_along(block.vec)) {#i is site index
      #store which block contains which sites.
      ids <- block.vec[[i]]
      block.list[[i]]$sites.per.block <- ids
      block.list[[i]]$site.ids <- site.ids[ids]
      block.list[[i]]$t <- t
      y.ind <- f.ind <- c()
      for (j in seq_along(ids)) {
        f.start <- (ids[j] - 1) * length(var.names) + 1
        f.end <- ids[j] * length(var.names)
        y.start <- obs_per_site[ids[j]] * (ids[j] - 1) + 1
        y.end <- obs_per_site[ids[j]] * ids[j]
        
        f.ind <- c(f.ind, f.start:f.end)
        y.ind <- c(y.ind, y.start:y.end)
      }
      #fill in  mu.f and Pf
      block.list[[i]]$data$muf <- mu.f[f.ind]
      block.list[[i]]$data$pf <- GrabFillMatrix(Pf, f.ind)
      
      #fill in y and R
      block.list[[i]]$data$y.censored <- y.censored[y.ind]
      block.list[[i]]$data$r <- GrabFillMatrix(solve(R), y.ind)
      
      #fill in constants
      block.h <- Construct.H.multisite(site.ids[ids], var.names, obs.mean[[t]])
      block.list[[i]]$constant$H <- which(apply(block.h, 2, sum) == 1)
      block.list[[i]]$constant$N <- length(f.ind)
      block.list[[i]]$constant$YN <- length(y.ind)
      block.list[[i]]$constant$q.type <- q.type
    }
  }
  
  #return values.
  block.list.all[[t]] <- block.list
  return(list(block.list.all = block.list.all, H = H, Y = Y, R = R))
}

##' @title MCMC_Init
##' @name  MCMC_Init
##' @author Dongchen Zhang
##' 
##' @param block.list  lists of blocks generated by the `build.block.xy` function.
##' @param X A matrix contains ensemble forecasts.
##' @details This function helps create initial conditions for the MCMC sampling.
##' 
##' @return It returns the `block.list` object with initial conditions filled in.
MCMC_Init <- function (block.list, X) {
  var.names <- unique(attributes(X)$dimnames[[2]])
  #sample mu.f from X.
  sample.mu.f <- X[sample(seq_along(1:nrow(X)), 1),]
  for (i in seq_along(block.list)) {
    #number of observations.
    num.obs <- length(block.list[[i]]$data$y.censored)
    #loop over each site within each block
    for (j in seq_along(block.list[[i]]$sites.per.block)) {
      #initialize mu.f
      start <- (block.list[[i]]$sites.per.block[j] - 1) * length(var.names) + 1
      end <- (block.list[[i]]$sites.per.block[j]) * length(var.names)
      block.list[[i]]$Inits$X.mod <- c(block.list[[i]]$Inits$X.mod, sample.mu.f[start:end])
      #initialize X
      block.list[[i]]$Inits$X <- block.list[[i]]$Inits$X.mod[block.list[[i]]$constant$H]
      #initialize Xs
      block.list[[i]]$Inits$Xs <- block.list[[i]]$Inits$X.mod[block.list[[i]]$constant$H]
    }
    #initialize q.
    #if we want the vector q.
    if (block.list[[i]]$constant$q.type == 1) {
      for (j in seq_along(block.list[[i]]$data$y.censored)) {
        block.list[[i]]$Inits$q <- c(block.list[[i]]$Inits$q, stats::rgamma(1, shape = block.list[[i]]$data$aq[j], rate = block.list[[i]]$data$bq[j]))
      }
    } else if (block.list[[i]]$constant$q.type == 2) {
      #if we want the wishart Q.
      if ("try-error" %in% class(try(block.list[[i]]$Inits$q <- 
                                     stats::rWishart(1, df = block.list[[i]]$data$bq, Sigma = block.list[[i]]$data$aq)[,,1], silent = T))) {
        block.list[[i]]$Inits$q <- 
          stats::rWishart(1, df = block.list[[i]]$data$bq, Sigma = stats::toeplitz((block.list[[i]]$constant$YN:1)/block.list[[i]]$constant$YN))[,,1]
      }
    }
  }
  #return values.
  return(block.list)
}

##' @title MCMC_block_function
##' @name  MCMC_block_function
##' @author Dongchen Zhang
##' 
##' @param block  each block within the `block.list` lists.
##' 
##' @return It returns the `block` object with analysis results filled in.
MCMC_block_function <- function(block) {
  #build nimble model
  model_pred <- nimble::nimbleModel(GEF.Block.Nimble,
                                    data = block$data,
                                    inits = block$Inits,
                                    constants = block$constant,
                                    name = 'base')
  #configure MCMC
  conf <- nimble::configureMCMC(model_pred, print=FALSE)
  conf$setMonitors(c("X", "X.mod", "q"))
  
  #Handle samplers
  #hear we change the RW_block sampler to the ess sampler 
  #because it has a better performance of MVN sampling
  samplerLists <- conf$getSamplers()
  if (block$constant$q.type == 1) {
    #if we have vector q
    #only X.mod should be sampled with ess sampler.
    X.mod.ind <- which(grepl("X.mod", samplerLists %>% purrr::map(~ .x$target) %>% unlist()))
    samplerLists[[X.mod.ind]]$setName("ess")
  } else if (block$constant$q.type == 2) {
    #if we have wishart q
    #everything should be sampled with ess sampler.
    samplerLists %>% purrr::map(function(l){l$setName("ess")})
  }
  conf$setSamplers(samplerLists)
  
  #compile MCMC
  Rmcmc <- nimble::buildMCMC(conf)
  Cmodel <- nimble::compileNimble(model_pred)
  Cmcmc <- nimble::compileNimble(Rmcmc, project = model_pred, showCompilerOutput = FALSE)
  
  #run MCMC
  dat <- runMCMC(Cmcmc, niter = block$MCMC$niter, nburnin = block$MCMC$nburnin, thin = block$MCMC$nthin, nchains = block$MCMC$nchain)
  
  #update aq, bq, mua, and pa
  M <- colMeans(dat)
  block$update$aq <- block$Inits$q
  if (block$constant$q.type == 1) {
    #if it's a vector q case
    aq <- bq <- rep(NA, length(block$data$y.censored))
    for (i in seq_along(aq)) {
      CHAR <- paste0("[", i, "]")
      aq[i] <- (mean(dat[, paste0("q", CHAR)]))^2/stats::var(dat[, paste0("q", CHAR)])
      bq[i] <- mean(dat[, paste0("q", CHAR)])/stats::var(dat[, paste0("q", CHAR)])
    }
    #update aqq and bqq
    block$aqq[,block$t+1] <- block$aqq[, block$t]
    block$aqq[block$constant$H, block$t+1] <- aq
    block$bqq[,block$t+1] <- block$bqq[, block$t]
    block$bqq[block$constant$H, block$t+1] <- bq
  } else if (block$constant$q.type == 2) {
    #previous updates
    mq <- dat[,  grep("q", colnames(dat))]  # Omega, Precision
    q.bar <- matrix(apply(mq, 2, mean),
                    length(block$constant$H),
                    length(block$constant$H)
    )
    wish.df <- function(Om, X, i, j, col) {
      (Om[i, j]^2 + Om[i, i] * Om[j, j]) / stats::var(X[, col])
    }
    col <- matrix(1:length(block$constant$H) ^ 2,
                  length(block$constant$H),
                  length(block$constant$H))
    WV  <- matrix(0, length(block$constant$H), length(block$constant$H))
    for (i in seq_along(block$constant$H)) {
      for (j in seq_along(block$constant$H)) {
        WV[i, j] <- wish.df(q.bar, X = mq, i = i, j = j, col = col[i, j])
      }
    }
    bq <- mean(WV)
    if (bq < block$constant$YN) {
      bq <- block$constant$YN
    }
    aq <- solve(q.bar) * bq
    block$aqq[,,block$t+1] <- GrabFillMatrix(block$aqq[,,block$t], block$constant$H, aq)
    block$bqq[block$t+1] <- bq
    
    # #if it's a wishart case
    # bq <- block$data$bq
    # aq <- block$data$aq
    # for (i in 1:dim(aq)[1]) {
    #   CHAR <- paste0("[", i, "]")
    #   for (j in 1:dim(aq)[2]) {
    #     aq[i, j] <- M[paste0("q[", i, ", ", j, "]")]/bq
    #   }
    # }
    # #update aqq and bqq
    # block$aqq[,,block$t+1] <- GrabFillMatrix(block$aqq[,,block$t], block$constant$H, aq)
    # block$bqq[block$t+1] <- block$bqq[block$t]
  }
  #update mua and pa; mufa, and pfa
  iX <- grep("X[", colnames(dat), fixed = TRUE)
  iX.mod <- grep("X.mod[", colnames(dat), fixed = TRUE)
  mua <- colMeans(dat[, iX])
  pa <- stats::cov(dat[, iX])
  mufa <- colMeans(dat[, iX.mod])
  pfa <- stats::cov(dat[, iX.mod])
  
  #return values.
  block$update <- list(aq = aq, bq = bq, mua = mua, pa = pa, mufa = mufa, pfa = pfa)
  return(block)
}

##' @title update_q
##' @name  update_q
##' @author Dongchen Zhang
##' 
##' @param block.list.all  each block within the `block.list` lists.
##' @param t time point.
##' @param nt total length of time steps.
##' @param MCMC_dat data frame of MCMC samples, the default it NULL.
##' 
##' @return It returns the `block.list.all` object with initialized/updated Q filled in.
update_q <- function (block.list.all, t, nt, MCMC_dat = NULL) {
  block.list <- block.list.all[[t]]
  #if it's an update.
  if (is.null(MCMC_dat)) {
    #loop over blocks
    if (t == 1) {
      for (i in seq_along(block.list)) {
        nvar <- length(block.list[[i]]$data$muf)
        nobs <- length(block.list[[i]]$data$y.censored)
        if (block.list[[i]]$constant$q.type == 1) {
          #initialize aqq and bqq for nt
          block.list[[i]]$aqq <- array(1, dim = c(nvar, nt))
          block.list[[i]]$bqq <- array(1, dim = c(nvar, nt))
          #update aq and bq based on aqq and bqq
          block.list[[i]]$data$aq <- block.list[[i]]$aqq[block.list[[i]]$constant$H, t]
          block.list[[i]]$data$bq <- block.list[[i]]$bqq[block.list[[i]]$constant$H, t]
        } else if (block.list[[i]]$constant$q.type == 2) {
          #initialize aqq and bqq for nt
          block.list[[i]]$aqq <- array(1, dim = c(nvar, nvar, nt))
          block.list[[i]]$aqq[,,t] <- stats::toeplitz((nvar:1)/nvar)
          block.list[[i]]$bqq <- rep(nobs, nt)
          #update aq and bq based on aqq and bqq
          block.list[[i]]$data$aq <- GrabFillMatrix(block.list[[i]]$aqq[,,t], block.list[[i]]$constant$H)
          block.list[[i]]$data$bq <- block.list[[i]]$bqq[t]
        }
      }
    } else if (t > 1) {
      #if we want to update q from previous SDA runs.
      block.list.pre <- block.list.all[[t - 1]]
      for (i in seq_along(block.list)) {
        nvar <- length(block.list[[i]]$data$muf)
        nobs <- length(block.list[[i]]$data$y.censored)
        if (block.list[[i]]$constant$q.type == 1) {
          #copy previous aqq and bqq to the current t
          block.list[[i]]$aqq <- block.list.pre[[i]]$aqq
          block.list[[i]]$bqq <- block.list.pre[[i]]$bqq
          #update aq and bq
          block.list[[i]]$data$aq <- block.list[[i]]$aqq[block.list[[i]]$constant$H, t]
          block.list[[i]]$data$bq <- block.list[[i]]$bqq[block.list[[i]]$constant$H, t]
        } else if (block.list[[i]]$constant$q.type == 2) {
          #initialize aqq and bqq for nt
          block.list[[i]]$aqq <- block.list.pre[[i]]$aqq
          block.list[[i]]$bqq <- block.list.pre[[i]]$bqq
          #if previous Q is smaller than the actual YN.
          if (block.list.pre[[i]]$bqq[t] <= block.list[[i]]$constant$YN) {
            block.list[[i]]$bqq[t] <- block.list[[i]]$constant$YN
          }
          #update aq and bq based on aqq and bqq
          block.list[[i]]$data$aq <- GrabFillMatrix(block.list[[i]]$aqq[,,t], block.list[[i]]$constant$H)
          block.list[[i]]$data$bq <- block.list[[i]]$bqq[t]
        }
      }
    }
  } else {
    #TODO: Implement the feature that Q can be updated based on the pft types.
  }
  
  #return values.
  block.list.all[[t]] <- block.list
  return(block.list.all)
}

##' @title block.2.vector
##' @name  block.2.vector
##' @author Dongchen Zhang
##' 
##' @param block.list  lists of blocks generated by the `build.block.xy` function.
##' @param X A matrix contains ensemble forecasts.
##' @param H H index created by the `construct_nimble_H` function.
##' 
##' @return It returns a list of analysis results by MCMC sampling.
block.2.vector <- function (block.list, X, H) {
  site.ids <- attributes(X)$Site
  mu.f <- mu.a <- c()
  Pf <- Pa <- matrix(0, length(site.ids), length(site.ids))
  for (L in block.list) {
    ind <- c()
    for (id in L$site.ids) {
      ind <- c(ind, which(site.ids == id))
    }
    #convert mu.f and pf
    mu.a[ind] <- mu.f[ind] <- L$update$mufa
    Pa[ind, ind] <- Pf[ind, ind] <- L$update$pfa
    #convert mu.a and pa
    ind <- intersect(ind, H$H.ind)
    mu.a[ind] <- L$update$mua
    Pa[ind, ind] <- L$update$pa
  }
  return(list(mu.f = mu.f,
              Pf = Pf,
              mu.a = mu.a,
              Pa = Pa))
}