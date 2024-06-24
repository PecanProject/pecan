##' @title analysis_sda_block
##' @name  analysis_sda_block
##' @author Dongchen Zhang
##' 
##' @param settings  pecan standard multi-site settings list.  
##' @param block.list.all Lists of forecast and analysis outputs for each time point of each block. If t=1, we initialize those outputs of each block with NULL from the `sda.enkf.multisite` function.
##' @param X A matrix contains ensemble forecasts with the dimensions of `[ensemble number, site number * number of state variables]`. The columns are matched with the site.ids and state variable names of the inside the `FORECAST` object in the `sda.enkf.multisite` script. 
##' @param obs.mean Lists of date times named by time points, which contains lists of sites named by site ids, which contains observation means for each state variables of each site for each time point. 
##' @param obs.cov   Lists of date times named by time points, which contains lists of sites named by site ids, which contains observation covariances for all state variables of each site for each time point. 
##' @param t time point in format of YYYY-MM-DD.
##' @param nt total length of time steps, corresponding to the `nt` variable in the `sda.enkf.multisite` function.
##' @param MCMC.args arguments for the MCMC sampling, details can be found in the roxygen strucutre for control list in the `sda.enkf.multisite` function.
##' @param block.list.all.pre pre-existed block.list.all object for passing the aqq and bqq to the current SDA run, the default is NULL. Details can be found in the roxygen structure for `pre_enkf_params` of the `sda.enkf.multisite` function
##' @details This function will add data and constants into each block that are needed for the MCMC sampling.
##'  
##' @description This function provides the block-based MCMC sampling approach.
##' 
##' @return It returns the `build.block.xy` object and the analysis results.
##' @importFrom dplyr %>%
analysis_sda_block <- function (settings, block.list.all, X, obs.mean, obs.cov, t, nt, MCMC.args, block.list.all.pre = NULL) {
  #convert from vector values to block lists.
  if ("try-error" %in% class(try(block.results <- build.block.xy(settings = settings, 
                                                                 block.list.all = block.list.all, 
                                                                 X = X, 
                                                                 obs.mean = obs.mean, 
                                                                 obs.cov = obs.cov, 
                                                                 t = t)))) {
    PEcAn.logger::logger.severe("Something wrong within the build.block.xy function.")
    return(0)
  }
  #grab block.list and H from the results.
  block.list.all <- block.results[[1]]
  H <- block.results[[2]]
  Y <- block.results[[3]]
  R <- block.results[[4]]
  
  #update q.
  if ("try-error" %in% class(try(block.list.all <- update_q(block.list.all, t, nt, aqq.Init = as.numeric(settings$state.data.assimilation$aqq.Init),
                                                            bqq.Init = as.numeric(settings$state.data.assimilation$bqq.Init),
                                                            MCMC_dat = NULL,
                                                            block.list.all.pre)))) {
    PEcAn.logger::logger.severe("Something wrong within the update_q function.")
    return(0)
  }
  
  #add initial conditions for the MCMC sampling.
  if ("try-error" %in% class(try(block.list.all[[t]] <- MCMC_Init(block.list.all[[t]], X)))) {
    PEcAn.logger::logger.severe("Something wrong within the MCMC_Init function.")
    return(0)
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
    PEcAn.logger::logger.severe("Something wrong within the MCMC_block_function function.")
    return(0)
  }
  PEcAn.logger::logger.info("Completed!")
  
  #convert from block lists to vector values.
  if ("try-error" %in% class(try(V <- block.2.vector(block.list.all[[t]], X, H)))) {
    PEcAn.logger::logger.severe("Something wrong within the block.2.vector function.")
    return(0)
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
    q.type <- 3
  } else if (settings$state.data.assimilation$q.type == "wishart") {
    q.type <- 4
  }
  #grab basic arguments based on X.
  site.ids <- unique(attributes(X)$Site)
  var.names <- unique(attributes(X)$dimnames[[2]])
  mu.f <- colMeans(X)
  Pf <- stats::cov(X)
  if (length(diag(Pf)[which(diag(Pf)==0)]) > 0) {
    diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5 #fixing det(Pf)==0
    PEcAn.logger::logger.warn("The zero variances in Pf is being replaced by one fifth of the minimum variance in those matrices respectively.")
  }
  #distance calculations and localization
  site.locs <- settings$run %>%
    purrr::map('site') %>%
    purrr::map_dfr(~c(.x[['lon']],.x[['lat']]) %>% as.numeric)%>%
    t %>%
    `colnames<-`(c("Lon","Lat")) %>%
    `rownames<-`(site.ids)
  #Finding the distance between the sites
  dis.matrix <- sp::spDists(site.locs, longlat = TRUE)
  if (!is.null(settings$state.data.assimilation$Localization.FUN)) {
    Localization.FUN <- get(settings$state.data.assimilation$Localization.FUN)
    #turn that into a blocked matrix format
    blocked.dis <- block_matrix(dis.matrix %>% as.numeric(), rep(length(var.names), length(site.ids)))
    Pf <- Localization.FUN(Pf, blocked.dis, settings$state.data.assimilation$scalef %>% as.numeric())
  }
  #Handle observation
  #observation number per site
  #free run special case.
  if (is.null(obs.mean[[t]])) {
    obs_per_site <- rep(0, length(site.ids)) %>% purrr::set_names(site.ids)
  } else {
    obs_per_site <- purrr::map_int(obs.mean[[t]], length)
  }
  #if we do free run or the current obs.mean are all NULL.
  if (as.logical(settings$state.data.assimilation$free.run) | all(is.null(unlist(obs.mean[[t]])))) {
    H <- list(ind = seq_along(rep(var.names, length(site.ids))))
    Y <- rep(NA, length(H$ind))
    R <- diag(1, length(H$ind))
  } else if (!as.logical(settings$state.data.assimilation$free.run) && all(is.null(unlist(obs.mean[[t]])))) {
    PEcAn.logger::logger.error("Please set the settings$state.data.assimilation$free.run as TRUE if you don't have any observations!")
    return(0)
  } else {
    Obs.cons <- Construct.R(site.ids, var.names, obs.mean[[t]], obs.cov[[t]])
    Y <- Obs.cons$Y
    R <- Obs.cons$R
    if (length(Y) > 1) {
      if (length(diag(R)[which(diag(R)==0)]) > 0) {
        diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
        PEcAn.logger::logger.warn("The zero variances in R is being replaced by half of the minimum variance in those matrices respectively.")
      }
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
    #create H
    # if there is any site that has zero observation.
    if (any(obs_per_site == 0)) {
      #name matching between observation names and state variable names.
      f.2.y.ind <- obs.mean[[t]] %>%
        purrr::map(\(x)which(var.names %in% names(x))) %>%
        unlist %>%
        unique
      H <- list(ind = f.2.y.ind %>% purrr::map(function(start){
        seq(start, length(site.ids) * length(var.names), length(var.names))
      }) %>% unlist() %>% sort)
    } else {
      H <- construct_nimble_H(site.ids = site.ids,
                              var.names = var.names,
                              obs.t = obs.mean[[t]],
                              pft.path = settings[[1]]$run$inputs$pft.site$path,
                              by = "block_pft_var")
    }
  }
  #start the blocking process
  #should we consider interactions between sites?
  if(as.numeric(settings$state.data.assimilation$scalef) == 0){
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
      #find indexs for Y.
      y.start <- sum(obs_per_site[1:i]) - obs_per_site[i] + 1
      y.end <- sum(obs_per_site[1:i])
      #fill in y and r
      #if there is no observation for this site.
      if (y.end < y.start) {
        #if every site has zero observation/free run.
        if (max(obs_per_site) == 0) {
          block.list[[i]]$data$y.censored <- rep(NA, length(var.names))
          block.list[[i]]$data$r <- diag(1, length(var.names))
          block.h <- matrix(1, 1, length(var.names))
        } else {
          block.list[[i]]$data$y.censored <- rep(NA, max(obs_per_site))
          block.list[[i]]$data$r <- diag(1, max(obs_per_site))
          block.h <- matrix(1, 1, max(obs_per_site))
        }
      } else {
        block.list[[i]]$data$y.censored <- y.censored[y.start:y.end]
        block.list[[i]]$data$r <- solve(R[y.start:y.end, y.start:y.end])
        block.h <- Construct.H.multisite(site.ids[i], var.names, obs.mean[[t]])
      }
      #fill in constants.
      block.list[[i]]$H <- block.h
      block.list[[i]]$constant$H <- which(apply(block.h, 2, sum) == 1)
      block.list[[i]]$constant$N <- length(f.start:f.end)
      block.list[[i]]$constant$YN <- length(y.start:y.end)
      block.list[[i]]$constant$q.type <- q.type
    }
    names(block.list) <- site.ids
  } else {
    #find networks given TRUE/FALSE matrix representing sites' interactions.
    block.vec <- matrix_network(dis.matrix <= as.numeric(settings$state.data.assimilation$scalef))
    #check if the matrix_network function is working correctly.
    #check if the blocks are calculated correctly.
    if (block.vec %>% 
        purrr::map(function(l){length(l)}) %>%
        unlist %>%
        sum() != length(site.ids)) {
      PEcAn.logger::logger.severe("Block calculation failed, please check the matrix_network function!")
      return(0)
    }
    block.list <- vector("list", length(block.vec))
    #loop over sites
    for (i in seq_along(block.vec)) {#i is site index
      #store which block contains which sites.
      ids <- block.vec[[i]]
      block.list[[i]]$sites.per.block <- ids
      block.list[[i]]$site.ids <- site.ids[ids]
      block.list[[i]]$t <- t
      y.ind <- f.ind <- na.ind <- c()
      r.block <- y.block <- c()
      for (j in seq_along(ids)) {
        f.start <- (ids[j] - 1) * length(var.names) + 1
        f.end <- ids[j] * length(var.names)
        y.start <- sum(obs_per_site[1:ids[j]]) - obs_per_site[ids[j]] + 1
        y.end <- sum(obs_per_site[1:ids[j]])
        f.ind <- c(f.ind, f.start:f.end)
        #if the current site has greater or equal than 1 observation.
        if (y.end >= y.start) {
          # y.ind <- c(y.ind, y.start:y.end)
          y.block <- c(y.block, y.censored[y.start:y.end])
          r.block <- c(r.block, diag(R)[y.start:y.end])
        } else {
          #if the current site has zero observation.
          #if for free run.
          if (max(obs_per_site) == 0) {
            y.block <- c(y.block, rep(NA, length(var.names)))
            r.block <- c(r.block, rep(1, length(var.names)))
          } else {
            y.block <- c(y.block, rep(NA, max(obs_per_site)))
            r.block <- c(r.block, rep(1, max(obs_per_site)))
          }
        }
      }
      #if we have NA for y, we will build H differently.
      if (any(is.na(y.block))) {
        block.h <- matrix(0, 1, length(ids)*length(var.names))
        #if for free run.
        if (is.null(obs.mean[[t]])) {
          f.2.y.ind <- seq_along(var.names)
        } else {
          f.2.y.ind <- obs.mean[[t]] %>%
            purrr::map(\(x)which(var.names %in% names(x))) %>%
            unlist %>%
            unique
        }
        seq.ind <- f.2.y.ind %>% purrr::map(function(start){
          seq(start, dim(block.h)[2], length(var.names))
        }) %>% unlist()
        block.h[1, seq.ind] <- 1
      } else {
        block.h <- Construct.H.multisite(site.ids[ids], var.names, obs.mean[[t]])
      }
      #fill in  mu.f and Pf
      block.list[[i]]$data$muf <- mu.f[f.ind]
      block.list[[i]]$data$pf <- GrabFillMatrix(Pf, f.ind)
      #fill in y and R
      block.list[[i]]$data$y.censored <- y.block
      if (length(r.block)  == 1) {
        block.list[[i]]$data$r <- 1/r.block
      } else {
        block.list[[i]]$data$r <- solve(diag(r.block))
      }
      block.list[[i]]$H <- block.h
      block.list[[i]]$constant$H <- which(apply(block.h, 2, sum) == 1)
      block.list[[i]]$constant$N <- length(f.ind)
      block.list[[i]]$constant$YN <- length(y.block)
      block.list[[i]]$constant$q.type <- q.type
    }
  }
  #if it's Wishart Q, we need to replace any NA Y with corresponding muf, and r with Pf.
  #also, if length of observation is 1, the Wishart Q is not suitable for the MCMC.
  #we will then need to change the Q type to 3, which is the vector Q.
  if (q.type == 4) {
    for (i in seq_along(block.list)) {
      #check length.
      if (block.list[[i]]$constant$YN == 1) {
        block.list[[i]]$constant$q.type <- 3
        next
      }
      # #check NAs.
      # na.ind <- which(is.na(block.list[[i]]$data$y.censored))
      # if (length(na.ind) > 0) {
      #     block.list[[i]]$constant$YN <- block.list[[i]]$constant$YN - length(na.ind)
      #     block.list[[i]]$constant$H <- block.list[[i]]$constant$H[-na.ind]
      #     block.list[[i]]$data$y.censored <- block.list[[i]]$data$y.censored[-na.ind]
      #     block.list[[i]]$data$r <- diag(diag(block.list[[i]]$data$r)[-na.ind])
      # }
      # na.site.ind <- which(obs_per_site[block.list[[i]]$site.ids] == 0)
      # na.ind <- which(is.na(block.list[[i]]$data$y.censored))
      # if (length(na.site.ind) > 0) {
      #   site.inds <- block.list[[i]]$sites.per.block[na.site.ind]
      #   y.2.muf.ind <- f.2.y.ind %>% purrr::map(function(start){
      #     seq(start, length(mu.f), length(var.names))[site.inds]
      #   }) %>% unlist() %>% sort()
      #   block.list[[i]]$data$y.censored[na.ind] <- mu.f[y.2.muf.ind]
      #   block.list[[i]]$data$r[na.ind, na.ind] <- Pf[y.2.muf.ind, y.2.muf.ind]
      # }
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
  sample.mu.f <- colMeans(X)
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
      block.list[[i]]$Inits$X <- block.list[[i]]$data$y.censored
      #initialize Xs
      block.list[[i]]$Inits$Xs <- block.list[[i]]$Inits$X.mod[block.list[[i]]$constant$H]
    }
    #initialize q.
    #if we want the vector q.
    if (block.list[[i]]$constant$q.type == 3) {
      for (j in seq_along(block.list[[i]]$data$y.censored)) {
        block.list[[i]]$Inits$q <- c(block.list[[i]]$Inits$q, stats::rgamma(1, shape = block.list[[i]]$data$aq[j], rate = block.list[[i]]$data$bq[j]))
      }
    } else if (block.list[[i]]$constant$q.type == 4) {
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
  #TODO: harmonize the MCMC code between block-based and general analysis functions to reduce the complexity of code.
  model_pred <- nimble::nimbleModel(GEF.MultiSite.Nimble,
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
  samplerNumberOffset <- length(samplerLists)
  if (block$constant$q.type == 4) {
    #if we have wishart q
    #everything should be sampled with ess sampler.
    samplerLists %>% purrr::map(function(l){l$setName("ess")})
  }
  conf$setSamplers(samplerLists)
  
  #add Pf as propCov in the control list of the X.mod nodes.
  X.mod.ind <- which(grepl("X.mod", samplerLists %>% purrr::map(~ .x$target) %>% unlist()))
  conf$removeSampler(samplerLists[[X.mod.ind]]$target)
  conf$addSampler(target = samplerLists[[X.mod.ind]]$target, type = "ess",
                  control = list(propCov= block$data$pf, adaptScaleOnly = TRUE,
                                 latents = "X", pfOptimizeNparticles = TRUE))

  #add toggle Y sampler.
  for (i in 1:block$constant$YN) {
    conf$addSampler(paste0("y.censored[", i, "]"), 'toggle', control=list(type='RW'))
  }
  conf$printSamplers()
  #compile MCMC
  Rmcmc <- nimble::buildMCMC(conf)
  Cmodel <- nimble::compileNimble(model_pred)
  Cmcmc <- nimble::compileNimble(Rmcmc, project = model_pred, showCompilerOutput = FALSE)
  
  #if we don't have any NA in the Y.
  if (!any(is.na(block$data$y.censored))) {
    #add toggle Y sampler.
    for(i in 1:block$constant$YN) {
      valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 0)
    }
  }
  
  #run MCMC
  dat <- runMCMC(Cmcmc, niter = block$MCMC$niter, nburnin = block$MCMC$nburnin, thin = block$MCMC$nthin, nchains = block$MCMC$nchain)
  #update aq, bq, mua, and pa
  M <- colMeans(dat)
  block$update$aq <- block$Inits$q
  if (block$constant$q.type == 3) {
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
  } else if (block$constant$q.type == 4) {
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
  }
  #update mua and pa; mufa, and pfa
  iX <- grep("X[", colnames(dat), fixed = TRUE)
  iX.mod <- grep("X.mod[", colnames(dat), fixed = TRUE)
  if (length(iX) == 1) {
    mua <- mean(dat[, iX])
    pa <- stats::var(dat[, iX])
  } else {
    mua <- colMeans(dat[, iX])
    pa <- stats::cov(dat[, iX])
  }
  
  if (length(iX.mod) == 1) {
    mufa <- mean(dat[, iX.mod])
    pfa <- stats::var(dat[, iX.mod])
  } else {
    mufa <- colMeans(dat[, iX.mod])
    pfa <- stats::cov(dat[, iX.mod])
  }
  
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
##' @param aqq.Init the initial values of aqq, the default is NULL.
##' @param bqq.Init the initial values of bqq, the default is NULL.
##' @param MCMC_dat data frame of MCMC samples, the default it NULL.
##' @param block.list.all.pre pre-existed block.list.all object for passing the aqq and bqq to the current SDA run, the default is NULL.
##' 
##' @return It returns the `block.list.all` object with initialized/updated Q filled in.
update_q <- function (block.list.all, t, nt, aqq.Init = NULL, bqq.Init = NULL, MCMC_dat = NULL, block.list.all.pre = NULL) {
  block.list <- block.list.all[[t]]
  #if it's an update.
  if (is.null(MCMC_dat)) {
    #loop over blocks
    if (t == 1) {
      for (i in seq_along(block.list)) {
        nvar <- length(block.list[[i]]$data$muf)
        nobs <- length(block.list[[i]]$data$y.censored)
        if (block.list[[i]]$constant$q.type == 3) {
          #initialize aqq and bqq for nt
          if (!is.null(aqq.Init) && !is.null(bqq.Init)) {
            block.list[[i]]$aqq <- array(aqq.Init, dim = c(nvar, nt + 1))
            block.list[[i]]$bqq <- array(bqq.Init, dim = c(nvar, nt + 1))
          } else {
            block.list[[i]]$aqq <- array(1, dim = c(nvar, nt + 1))
            block.list[[i]]$bqq <- array(1, dim = c(nvar, nt + 1))
          }
          #update aq and bq based on aqq and bqq
          block.list[[i]]$data$aq <- block.list[[i]]$aqq[block.list[[i]]$constant$H, t]
          block.list[[i]]$data$bq <- block.list[[i]]$bqq[block.list[[i]]$constant$H, t]
        } else if (block.list[[i]]$constant$q.type == 4) {
          #initialize aqq and bqq for nt
          block.list[[i]]$aqq <- array(1, dim = c(nvar, nvar, nt + 1))
          block.list[[i]]$aqq[,,t] <- stats::toeplitz((nvar:1)/nvar)
          block.list[[i]]$bqq <- rep(nobs, nt + 1)
          #update aq and bq based on aqq and bqq
          block.list[[i]]$data$aq <- GrabFillMatrix(block.list[[i]]$aqq[,,t], block.list[[i]]$constant$H)
          block.list[[i]]$data$bq <- block.list[[i]]$bqq[t]
        }
      }
    } else if (t > 1) {
      if (!is.null(block.list.all.pre)) {
        block.list.pre <- block.list.all.pre[[t - 1]]
      } else {
        #if we want to update q from previous SDA runs.
        block.list.pre <- block.list.all[[t - 1]]
      }
      for (i in seq_along(block.list)) {
        nvar <- length(block.list[[i]]$data$muf)
        nobs <- length(block.list[[i]]$data$y.censored)
        if (block.list[[i]]$constant$q.type == 3) {
          #copy previous aqq and bqq to the current t
          block.list[[i]]$aqq <- block.list.pre[[i]]$aqq
          block.list[[i]]$bqq <- block.list.pre[[i]]$bqq
          #update aq and bq
          block.list[[i]]$data$aq <- block.list[[i]]$aqq[block.list[[i]]$constant$H, t]
          block.list[[i]]$data$bq <- block.list[[i]]$bqq[block.list[[i]]$constant$H, t]
        } else if (block.list[[i]]$constant$q.type == 4) {
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