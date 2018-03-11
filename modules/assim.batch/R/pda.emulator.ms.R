##' Paramater Data Assimilation using emulator on multiple sites in three modes: local, global, hierarchical
##' First draft, not complete yet
##'
##' @param multi.settings = a pecan multi-settings list
##'
##' @return settings
##'
##' @author Istem Fer
##' @export
pda.emulator.ms <- function(multi.settings) {
  
  ## -------------------------------------- Initialization --------------------------------------------------- 
 
  # check mode 
  pda.mode <- unique(sapply(multi.settings$assim.batch,`[[`, "mode"))
  
  if(pda.mode == "local"){
    local <- TRUE
    global <- hierarchical <- FALSE
  }else if(pda.mode == "global"){
    global <- TRUE
    local <- hierarchical <- FALSE
  }else if(pda.mode == "hierarchical"){
    hieararchical <- TRUE
    local <- global <- FALSE
  }else{
    local <- global <- hierarchical <- TRUE
  }
  
  # how many sites
  nsites <- length(multi.settings)

  
  # lists to collect emulators and run MCMC per site later
  gp.stack      <- vector("list", nsites) 
  SS.stack      <- vector("list", nsites) 
  #nstack       <- vector("list", nsites) 
  
  ## -------------------------------------- Local runs and calibration ------------------------------------------
  
  for(s in seq_along(multi.settings)){ # site runs - loop begin
    
    settings <- multi.settings[[s]]
    # NOTE: local flag is not used currently, prepearation for future use
    # if this flag is FALSE, pda.emulator will not fit GP and run MCMC, 
    # but will run LHC ensembles, calculate SS and return settings list with saved SS paths etc.
    # this requires some re-arrangement in pda.emulator, 
    # for now we will always run site-level calibration
    settings <- pda.emulator(settings, local = local)
    multi.settings[[s]] <- settings
  } # site runs - loop end
  
  #PEcAn.settings::write.settings(multi.settings, outputfile='pecan.PDAMS.xml')
  
  ## -------------------------------- Prepare for Global and Hierarchical ----------------------------------------- 
  
  
  # we need some objects that are common to all calibrations
  obj_names <- c("init.list", "rng", "jmp.list", "prior.fn.all", "prior.ind.all", "llik.fn", 
    "settings", "prior.ind.all.ns", "sf", "prior.list", "n.param.orig", "pname", "prior.ind.orig",
    "hyper.pars")
  
  need_obj <- load_pda_history(workdir = multi.settings$outdir,  
                   ensemble.id = multi.settings[[1]]$assim.batch$ensemble.id, 
                   objects = obj_names)
  
  init.list        <- need_obj$init.list
  rng              <- need_obj$rng
  jmp.list         <- need_obj$jmp.list
  prior.list       <- need_obj$prior.list
  prior.fn.all     <- need_obj$prior.fn.all
  prior.ind.all    <- need_obj$prior.ind.all
  prior.ind.all.ns <- need_obj$prior.ind.all.ns
  llik.fn          <- need_obj$llik.fn
  tmp.settings     <- need_obj$settings
  sf               <- need_obj$sf
  n.param.orig     <- need_obj$n.param.orig
  prior.ind.orig   <- need_obj$prior.ind.orig
  pname            <- need_obj$pname
  hyper.pars       <- need_obj$hyper.pars
  
  resume.list <- vector("list", multi.settings[[1]]$assim.batch$chain)
  
  # collect GPs and SSs
  for(s in seq_along(multi.settings)){
    
    load(multi.settings[[s]]$assim.batch$emulator.path)
    load(multi.settings[[s]]$assim.batch$ss.path)
    gp.stack[[s]] <- gp
    SS.stack[[s]] <- SS
    remove(gp, SS)
    
  }

  ## Open database connection
  if (multi.settings$database$bety$write) {
    con <- try(db.open(multi.settings$database$bety), silent = TRUE)
    if (is(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(db.close(con))
    }
  } else {
    con <- NULL
  }
  
  ## Get the workflow id
  if ("workflow" %in% names(tmp.settings)) {
    workflow.id <- tmp.settings$workflow$id
  } else {
    workflow.id <- -1
  }
  

  ## -------------------------------------- Global calibration -------------------------------------------------- 
  if(global){ # global - if begin
    
    ## Get an ensemble id for global calibration
    tmp.settings$assim.batch$ensemble.id <- pda.create.ensemble(tmp.settings, con, workflow.id)
    
    gp <- unlist(gp.stack, recursive = FALSE)

    # start the clock
    ptm.start <- proc.time()
    
    # prepare for parallelization
    dcores <- parallel::detectCores() - 1
    ncores <- min(max(dcores, 1), multi.settings[[1]]$assim.batch$chain)
    # 
    logger.setOutputFile(file.path(multi.settings$outdir, "pda.log"))
    # 
    cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(multi.settings$outdir, "pda.log"))
    
    ## Sample posterior from emulator
    mcmc.out <- parallel::parLapply(cl, 1:multi.settings[[1]]$assim.batch$chain, function(chain) {
      mcmc.GP(gp          = gp, ## Emulator(s)
              x0          = init.list[[chain]],     ## Initial conditions
              nmcmc       = 100000,       ## Number of reps
              rng         = rng,       ## range
              format      = "lin",      ## "lin"ear vs "log" of LogLikelihood 
              mix         = "joint",     ## Jump "each" dimension independently or update them "joint"ly
              jmp0        = jmp.list[[chain]],  ## Initial jump size
              ar.target   = 0.3,   ## Target acceptance rate
              priors      = prior.fn.all$dprior[prior.ind.all], ## priors
              settings    = tmp.settings, # this is just for checking llik functions downstream
              run.block   = TRUE,  
              n.of.obs    = NULL, # need this for Gaussian likelihoods, keep it NULL for now
              llik.fn     = llik.fn,
              hyper.pars  = hyper.pars,
              resume.list = resume.list[[chain]]
      )
    })
    
    parallel::stopCluster(cl)
    
    # Stop the clock
    ptm.finish <- proc.time() - ptm.start
    logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(settings$assim.batch$iter), " iterations."))
    
    mcmc.samp.list <- sf.samp.list <- list()
    
    for (c in seq_len(tmp.settings$assim.batch$chain)) {
      
      m <- matrix(NA, nrow =  nrow(mcmc.out[[c]]$mcmc.samp), ncol = length(prior.ind.all.ns))
      
      if(!is.null(sf)){
        sfm <- matrix(NA, nrow =  nrow(mcmc.out[[c]]$mcmc.samp), ncol = length(sf))
        # give colnames but the order can change, we'll overwrite anyway
        colnames(sfm) <- paste0(sf, "_SF")
      }
      ## Set the prior functions back to work with actual parameter range
      
      prior.all <- do.call("rbind", prior.list)
      prior.fn.all <- pda.define.prior.fn(prior.all)
      
      # retrieve rownames separately to get rid of var_name* structures
      prior.all.rownames <- unlist(sapply(prior.list, rownames))
      
      sc <- 1
      for (i in seq_along(prior.ind.all.ns)) {
        sf.check <- prior.all.rownames[prior.ind.all.ns][i]
        idx <- grep(sf.check, rownames(prior.all)[prior.ind.all])
        if(any(grepl(sf.check, sf))){
          
          m[, i] <- eval(prior.fn.all$qprior[prior.ind.all.ns][[i]],
                         list(p = mcmc.out[[c]]$mcmc.samp[, idx]))
          if(sc <= length(sf)){
            sfm[, sc] <- mcmc.out[[c]]$mcmc.samp[, idx]
            colnames(sfm)[sc] <- paste0(sf.check, "_SF")
            sc <- sc + 1
          }
          
        }else{
          m[, i] <- mcmc.out[[c]]$mcmc.samp[, idx]
        }
      }
      
      colnames(m) <- prior.all.rownames[prior.ind.all.ns]
      mcmc.samp.list[[c]] <- m
      
      if(!is.null(sf)){
        sf.samp.list[[c]] <- sfm
      }
      
      resume.list[[c]] <- mcmc.out[[c]]$chain.res
    }
    
    # Separate each PFT's parameter samples (and bias term) to their own list
    mcmc.param.list <- list()
    ind <- 0
    for (i in seq_along(n.param.orig)) {
      mcmc.param.list[[i]] <- lapply(mcmc.samp.list, function(x) x[, (ind + 1):(ind + n.param.orig[i]), drop = FALSE])
      ind <- ind + n.param.orig[i]
    }
    
    # Collect non-model parameters in their own list
    if(length(mcmc.param.list) > length(tmp.settings$pfts)) { 
      # means bias parameter was at least one bias param in the emulator
      # it will be the last list in mcmc.param.list
      # there will always be at least one tau for bias
      for(c in seq_len(tmp.settings$assim.batch$chain)){
        mcmc.param.list[[length(mcmc.param.list)]][[c]] <- cbind( mcmc.param.list[[length(mcmc.param.list)]][[c]],
                                                                  mcmc.out[[c]]$mcmc.par)
      }
      
    } else if (ncol(mcmc.out[[1]]$mcmc.par) != 0){
      # means no bias param but there are still other params, e.g. Gaussian
      mcmc.param.list[[length(mcmc.param.list)+1]] <- list()
      for(c in seq_len(tmp.settings$assim.batch$chain)){
        mcmc.param.list[[length(mcmc.param.list)]][[c]] <- mcmc.out[[c]]$mcmc.par
      }
    }
    
    tmp.settings <- pda.postprocess(tmp.settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = "_global")
    
  } # global - if end
  
  ## -------------------------------------- Hierarchical MCMC ------------------------------------------ 
  if(hierarchical){ # hierarchical - if begin
    
    ## Get an ensemble id for hierarchical calibration
    tmp.settings$assim.batch$ensemble.id <- pda.create.ensemble(tmp.settings, con, workflow.id)
    
    ## Transform values from non-normal distributions to standard Normal
    ## it won't do anything if all priors are already normal
    norm_transform <- norm_transform_priors(prior.list, prior.fn.all, prior.ind.all, SS.stack, init.list, jmp.list)
    if(!norm_transform$normF){ # means SS values are transformed
      ## get new SS.stack with transformed values
      SS.stack <- norm_transform$normSS
      
      ## re-fit GP on new param space
      for(i in seq_along(SS.stack)){
        GPmodel <- lapply(SS.stack[[i]], function(x) mlegp::mlegp(X = x[, -ncol(x), drop = FALSE], Z = x[, ncol(x), drop = FALSE], nugget = 0, nugget.known = 1, verbose = 0))
        gp.stack[[i]] <- GPmodel
      }
      
      ## re-define rng
      rng <- norm_transform$rng
      
      ## get new init.list and jmp.list
      init.list <- norm_transform$init
      jmp.list  <- norm_transform$jmp

      
    }
    

    ########### hierarchical MCMC function with Gibbs ##############
    
    
    hier.mcmc <- function(settings, gp.stack, nstack, nmcmc, rng, 
                          mu0, jmp0, nparam, nsites){

      pos.check <- sapply(settings$assim.batch$inputs, `[[`, "ss.positive")
      
      if(length(unlist(pos.check)) == 0){
        # if not passed from settings assume none
        pos.check <- rep(FALSE, length(settings$assim.batch$inputs))
      }else if(length(unlist(pos.check)) != length(settings$assim.batch$inputs)){
        # maybe one provided, but others are forgotten
        # check which ones are provided in settings
        from.settings <- sapply(seq_along(pos.check), function(x) !is.null(pos.check[[x]]))
        tmp.check <- rep(FALSE, length(settings$assim.batch$inputs))
        # replace those with the values provided in the settings
        tmp.check[from.settings] <- as.logical(unlist(pos.check))
        pos.check <- tmp.check
      }else{
        pos.check <- as.logical(pos.check)
      }
      
      ################################################################
      #
      #      mu_site    : site level parameters (nsite x nparam)
      #      tau_site   : site level precision (nsite x nsite)
      #      mu_global  : global parameters (nparam)
      #      tau_global : global precision matrix (nparam x nparam)
      #
      ################################################################
      
      
      
      ###### (hierarchical) global mu priors
      #
      #      mu_f    : prior mean vector
      #      P_f     : prior covariance matrix
      #      P_f_inv : prior precision matrix
      #
      #      mu_global ~ MVN (mu_f, P_f)
      #

      mu_f    <- unlist(mu0)
      P_f     <- diag(jmp0)
      P_f_inv <- solve(P_f) 
      
      # # initialize mu_global (nparam)
      repeat{
        mu_global <- mvtnorm::rmvnorm(1, mu_f, P_f)
        check.that <- (mu_global > apply(rng,1:2,max)[, 1] & mu_global < apply(rng,1:2,min)[, 2])
        if(all(check.that)) break
      }
      
      ######  (hierarchical) global tau priors
      #
      #      tau_df    : Wishart degrees of freedom
      #      tau_V     : Wishart scale matrix
      #
      #      tau_global ~ W (tau_df, tau_scale)
      #      sigma_global <- solve(tau_global)
      #
      
      tau_df <- nsites + nparam + 1
      tau_V  <- diag(1, nparam)
      V_inv  <- solve(tau_V)  # will be used in gibbs updating
      
      # initialize tau_global (nparam x nparam)
      tau_global   <- rWishart(1, tau_df, tau_V)[,,1]
      
      
      # initialize jcov.arr (jump variances per site)
      jcov.arr <-  array(NA_real_, c(nparam, nparam, nsites))
      for(j in seq_len(nsites)) jcov.arr[,,j] <- diag(jmp0)
      
      
      # initialize mu_site (nsite x nparam)
      mu_site_curr <- matrix(NA_real_, nrow = nsites, ncol= nparam)
      mu_site_new  <- matrix(NA_real_, nrow = nsites, ncol= n.param)
      for(ns in 1:nsites){
        repeat{
          mu_site_curr[ns,] <- mvtnorm::rmvnorm(1, mu_global, jcov.arr[,,ns]) # site mean
          check.that <- (mu_site_curr[ns,] > rng[, 1, ns] & mu_site_curr[ns,] < rng[, 2, ns])
          if(all(check.that)) break
        }
      }
      
      # values for each site will be accepted/rejected in themselves
      currSS    <- sapply(seq_len(nsites), function(v) PEcAn.emulator::get_ss(gp.stack[[v]], mu_site_curr[v,], pos.check))
      # force it to be nvar x nsites matrix
      currSS <- matrix(currSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
      currllp   <- lapply(seq_len(nsites), function(v) PEcAn.assim.batch::pda.calc.llik.par(settings, nstack[[v]], currSS[,v]))
      
      # storage
      mu_site_samp <-  array(NA_real_, c(nmcmc, nparam, nsites))
      # tau_site_samp <- array(NA_real_, c(nmcmc, nsites, nsites))
      mu_global_samp  <-  matrix(NA_real_, nrow = nmcmc, ncol= nparam)
      tau_global_samp <-  array(NA_real_, c(nmcmc, nparam, nparam))
      
      musite.accept.count    <- rep(0, nsites)

      ########################## Start MCMC ########################
      
      for(g in seq_len(nmcmc)){
        
        # jump adaptation step
        if ((g > 2) && ((g - 1) %% settings$assim.batch$jump$adapt == 0)) {
          
          # update site level jvars
          params.recent <- mu_site_samp[(g - settings$assim.batch$jump$adapt):(g - 1), , ]
          #colnames(params.recent) <- names(x0)
          jcov.list <- lapply(seq_len(nsites), function(v) pda.adjust.jumps.bs(settings, jcov.arr[,,v], accept.count[v], params.recent[,,v]))
          jcov.arr  <- abind::abind(jcov.list, along=3)
          musite.accept.count <- rep(0, nsites)  # Reset counter

        }
        
        
        
        ########################################
        # update tau_global | mu_global, mu_site
        #
        # tau_global ~ W(tau_df, tau_sigma)
        # 
        # tau_global   : error precision matrix
        
        # sum of pairwise deviation products
        pairwise_deviation <- apply(mu_site_curr, 1, function(r) r - mu_global)
        sum_term <- pairwise_deviation %*% t(pairwise_deviation)
        
        tau_sigma <- solve(V_inv + sum_term)
        
        # update tau
        tau_global <- rWishart(1, df = tau_df, Sigma = tau_sigma)[,,1] # site precision
        sigma_global <- solve(tau_global) # site covariance, new prior sigma to be used below for prior prob. calc.
         

        
        ########################################
        # update mu_global | mu_site, tau_global
        #
        # mu_global ~ MVN(global_mu, global_Sigma)
        #
        # mu_global     : global parameters
        # global_mu     : precision weighted average between the data (mu_site) and prior mean (mu_f)
        # global_Sigma  : sum of mu_site and mu_f precision
        

        global_Sigma <- solve(P_f + (nsites * sigma_global))
        
        global_mu <- global_Sigma %*% ((sigma_global %*% colSums(mu_site_curr)) + (P_f %*% mu_f))

        mu_global <- mvtnorm::rmvnorm(1, global_mu, global_Sigma) # new prior mu to be used below for prior prob. calc.

          
        
        # site level M-H
        ########################################
        
        # propose mu_site 
        
        for(ns in seq_len(nsites)){
          repeat{ # make sure to stay in emulator boundaries, otherwise it confuses adaptation
            mu_site_new[ns,] <- mvtnorm::rmvnorm(1, mu_site_curr[ns,], jcov.arr[,,ns])
            check.that <- (mu_site_new[ns,] > rng[, 1, ns] & mu_site_new[ns,] < rng[, 2, ns])
            if(all(check.that)) break
          }
        }
        
        
        # re-predict current SS
        currSS    <- sapply(seq_len(nsites), function(v) get_ss(gp.stack[[v]], mu_site_curr[v,], pos.check))
        currSS <- matrix(currSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
        
        # calculate posterior
        currLL    <- sapply(seq_len(nsites), function(v) pda.calc.llik(currSS[,v], llik.fn, currllp[[v]]))
        # use new priors for calculating prior probability
        currPrior <- mvtnorm::dmvnorm(mu_site_curr, mu_global, sigma_global, log = TRUE)
        currPost  <- currLL + currPrior
        
        
        # predict new SS
        newSS <- sapply(seq_len(nsites), function(v) get_ss(gp.stack[[v]], mu_site_new[v,], pos.check))
        newSS <- matrix(newSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
        
        # calculate posterior
        newllp   <- lapply(seq_len(nsites), function(v) pda.calc.llik.par(settings, nstack[[v]], newSS[,v]))
        newLL    <- sapply(seq_len(nsites), function(v) pda.calc.llik(newSS[,v], llik.fn, newllp[[v]]))
        # use new priors for calculating prior probability
        newPrior <- dmvnorm(mu_site_new, mu_global, sigma_global, log = TRUE)
        newPost  <- newLL + newPrior
        
        ar <- is.accepted(currPost, newPost)
        mu_site_curr[ar, ] <- mu_site_new[ar, ]
        musite.accept.count <- musite.accept.count + ar
        
        
        mu_site_samp[g, , seq_len(nsites)] <- t(mu_site_curr)[,seq_len(nsites)]
        # tau_site_samp <-
        mu_global_samp[g,]   <- mu_global  # 100% acceptance for gibbs
        tau_global_samp[g,,] <- tau_global # 100% acceptance for gibbs
        
        if(g %% 500 == 0) PEcAn.logger::logger.info(g, "of", nmcmc, "iterations")
      }
      
      return(list(mu_site_samp = mu_site_samp, mu_global_samp = mu_global_samp, tau_global_samp = tau_global_samp,
                  musite.accept.count = musite.accept.count))
    } # hier.mcmc
    
    # start the clock
    ptm.start <- proc.time()
    
    # prepare for parallelization
    dcores <- parallel::detectCores() - 1
    ncores <- min(max(dcores, 1), settings$assim.batch$chain)
     
    logger.setOutputFile(file.path(settings$outdir, "pda.log"))
    
    cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(settings$outdir, "pda.log"))
    
    
    ## Sample posterior from emulator
    mcmc.out <- parallel::parLapply(cl, seq_len(tmp.settings$assim.batch$chain), function(chain) {
      hier.mcmc(settings = tmp.settings, gp.stack = gp.stack, nstack = NULL, 
                nmcmc = tmp.settings$assim.batch$iter, rng = rng,
                mu0 = init.list[[chain]], jmp0 = jmp.list[[chain]], 
                nparam = length(prior.ind.all), nsites = nsites)
    })
    
    parallel::stopCluster(cl)
    
    # Stop the clock
    ptm.finish <- proc.time() - ptm.start
    logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(tmp.settings$assim.batch$iter), " iterations."))
    
    # transform samples from std normal to prior quantiles
    mcmc.out2 <- back_transform_posteriors(prior.list, prior.fn.all, prior.ind.all, mcmc.out)
    
    # Collect global params in their own list and postprocess
    mcmc.param.list <- pda.sort.params(mcmc.out2, sub.sample = "mu_global_samp", ns = NULL, prior.all, prior.ind.all.ns, sf, n.param.orig, prior.list, prior.fn.all)
    tmp.settings <- pda.postprocess(tmp.settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = "_hierarchical")
    
    # Collect site-level params in their own list and postprocess
    for(ns in seq_len(nsites)){
      mcmc.param.list <- pda.sort.params(mcmc.out2, sub.sample = "mu_site_samp", ns = ns, prior.all, prior.ind.all.ns, sf, n.param.orig, prior.list, prior.fn.all)
      settings <- pda.postprocess(tmp.settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = paste0("_hierarchical_SL",ns))
    }
  } # hierarchical - if end
  
}

