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
  
  if(pda.mode == "individual"){
    individual <- TRUE
    joint <- hierarchical <- FALSE
  }else if(pda.mode == "joint"){
    joint <- TRUE
    individual <- hierarchical <- FALSE
  }else if(pda.mode == "hierarchical"){
    hieararchical <- TRUE
    individual <- joint <- FALSE
  }else{
    individual <- joint <- hierarchical <- TRUE
  }
  
  # how many sites
  nsites <- length(multi.settings)

  
  # lists to collect emulators and run MCMC per site later
  gp.stack      <- vector("list", nsites) 
  SS.stack      <- vector("list", nsites) 
  #nstack       <- vector("list", nsites) 
  
  ## -------------------------------------- Individual runs and calibration ------------------------------------------
  
  if(individual){
    # NOTE: individual flag -mode functionality in general- is not currently in use, 
    # preparation for future use, the idea is to skip site-level fitting if we've already done it
    # if this flag is FALSE, pda.emulator will not fit GP and run MCMC, 
    # but this requires some re-arrangement in pda.emulator function 
    # for now we will always run site-level calibration
    
    # Open the tunnel (might not need)
    PEcAn.remote::open_tunnel(multi.settings[[1]]$host$name,
                              user = multi.settings[[1]]$host$user,
                              tunnel_dir = dirname(multi.settings[[1]]$host$tunnel))
    
    # Until a check function is implemented, run a predefined number of emulator rounds
    n_rounds <- ifelse(is.null(multi.settings[[1]]$assim.batch$n_rounds), 3, as.numeric(multi.settings[[1]]$assim.batch$n_rounds))
    PEcAn.logger::logger.info(n_rounds, " individual PDA rounds will be run per site. Please wait.")
    repeat{
     
      # number of sites will probably get big real quick, so some multi-site PDA runs should be run on the cluster
      # unfortunately pda.emulator function was not fully designed for remote runs, so first we need to prepare a few things it needs
      # (1) all sites should be running the same knots
      # (2) all sites will use the same prior.list
      # (3) the format information for the assimilated data (pda.emulator needs DB connection to query it if it's not externally provided)
      # (4) ensemble ids (they provide unique trackers for emulator functions)
      multi_site_objects <- return_multi_site_objects(multi.settings) 
      
      emulator_jobs <- rep(NA, length(multi.settings))
      for(ms in seq_along(multi.settings)){
        
        # Sync to remote
        subfile <- prepare_pda_remote(multi.settings[[ms]], site = ms, multi_site_objects)
        
        # Submit emulator scripts
        tmp <- PEcAn.remote::remote.execute.cmd(multi.settings[[ms]]$host, paste0("qsub ", subfile))
        emulator_jobs[ms] <- as.numeric( sub("\\D*(\\d+).*", "\\1", tmp))
      }
      
      # listen
      repeat{
        PEcAn.logger::logger.info("Multi-site calibration running. Please wait.")
        Sys.sleep(180)
        check_all_sites <- sapply(emulator_jobs, qsub_run_finished,  multi.settings[[1]]$host, multi.settings[[1]]$host$qstat)
        if(all(check_all_sites)) break
      }
      
      # Sync from remote
      multi.settings <- sync_pda_remote(multi.settings, multi_site_objects$ensembleidlist)
    
      
      # continue or stop
      r_counter <- as.numeric(multi.settings[[1]]$assim.batch$round_counter)
      # write multi.settings with individual-pda info
      PEcAn.settings::write.settings(multi.settings, outputfile = paste0('pecan.PDA_MS', r_counter, '.xml'))
      PEcAn.logger::logger.info("Round", r_counter, "finished.")
      if(r_counter == n_rounds) break 
    }

    # Close the tunnel
    PEcAn.remote::kill.tunnel(settings)

  }
  

  

  ## -------------------------------- Prepare for Joint and Hierarchical ----------------------------------------- 
  
  
  # we need some objects that are common to all calibrations
  obj_names <- c("init.list", "rng", "jmp.list", "prior.fn.all", "prior.ind.all", "llik.fn", 
    "settings", "prior.ind.all.ns", "sf", "prior.list", "n.param.orig", "pname", "prior.ind.orig",
    "hyper.pars", "resume.list")
  
  need_obj <- load_pda_history(workdir = multi.settings$outdir,  
                   ensemble.id = multi.settings[[1]]$assim.batch$ensemble.id, 
                   objects = obj_names)
  
  init.list        <- need_obj$init.list
  rng_orig         <- need_obj$rng
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
  nparam           <- length(prior.ind.all)
  prior.all        <- do.call("rbind", prior.list)
  
  resume.list <- vector("list", multi.settings[[1]]$assim.batch$chain)
  
  # collect GPs and SSs
  for(s in seq_along(multi.settings)){
    
    load(file.path(multi.settings[[s]]$outdir, 
                   basename(multi.settings[[s]]$assim.batch$emulator.path)))
    load(file.path(multi.settings[[s]]$outdir,
                   basename(multi.settings[[s]]$assim.batch$ss.path)))
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
  
  ## remote hack for now
  ## currently site-level PDA runs on remote but joint and hierarchical runs locally
  ## this will change soon (before this PR is finalized)
  ## but I'm still developing the code so for now let's change the paths back to local
  for(i in seq_along(tmp.settings$pfts)){
    tmp.settings$pfts[[i]]$outdir <- file.path(tmp.settings$outdir, "pft", basename(tmp.settings$pfts[[i]]$outdir))
  }
  tmp.settings$modeloutdir <- file.path(tmp.settings$outdir, basename(tmp.settings$modeloutdir))

  ## -------------------------------------- Joint calibration -------------------------------------------------- 
  if(joint){ # joint - if begin
    
    ## Get an ensemble id for global calibration
    tmp.settings$assim.batch$ensemble.id <- pda.create.ensemble(tmp.settings, con, workflow.id)
    
    ## history restart
    hbc.restart.file <- file.path(tmp.settings$outdir,paste0("history.joint",
                                                           tmp.settings$assim.batch$ensemble.id, ".Rdata"))
    
    current.step <- "BEG OF JOINT MCMC"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    
    gp <- unlist(gp.stack, recursive = FALSE)

    # start the clock
    ptm.start <- proc.time()
    
    # prepare for parallelization (over chains)
    dcores <- parallel::detectCores() - 1
    ncores <- min(max(dcores, 1), multi.settings[[1]]$assim.batch$chain)
     
    logger.setOutputFile(file.path(multi.settings$outdir, "pda.log"))
     
    cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(multi.settings$outdir, "pda.log"))
    
    ## Sample posterior from emulator
    mcmc.out <- parallel::parLapply(cl, 1:multi.settings[[1]]$assim.batch$chain, function(chain) {
      mcmc.GP(gp          = gp, ## Emulator(s)
              x0          = init.list[[chain]],     ## Initial conditions
              nmcmc       = as.numeric(multi.settings[[1]]$assim.batch$iter),  ## Number of iters
              rng         = rng_orig,       ## range
              format      = "lin",      ## "lin"ear vs "log" of LogLikelihood 
              mix         = "joint",     ## Jump "each" dimension independently or update them "joint"ly
              jmp0        = jmp.list[[chain]],  ## Initial jump size
              ar.target   = multi.settings[[1]]$assim.batch$jump$ar.target,   ## Target acceptance rate
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
    logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(tmp.settings$assim.batch$iter), " iterations."))
    
    current.step <- "END OF JOINT MCMC"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    
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
    
    tmp.settings <- pda.postprocess(tmp.settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = "_joint")
    
    current.step <- "JOINT - END"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    
  } # joint - if end
  
  ## -------------------------------------- Hierarchical MCMC ------------------------------------------ 
  if(hierarchical){ # hierarchical - if begin
    
    ## Get an ensemble id for hierarchical calibration
    tmp.settings$assim.batch$ensemble.id <- pda.create.ensemble(tmp.settings, con, workflow.id)
    
    ## history restart
    hbc.restart.file <- file.path(tmp.settings$outdir,paste0("history.hier",
                                                           tmp.settings$assim.batch$ensemble.id, ".Rdata"))

    
    ## Transform values from non-normal distributions to standard Normal
    ## it won't do anything if all priors are already normal
    ## edit: actually hierarchical sampling may be assuming standard normal, test for this later
    norm_transform <- norm_transform_priors(prior.list, prior.fn.all, prior.ind.all, SS.stack, init.list, jmp.list)
    if(!norm_transform$normF){ # means SS values are transformed
      
      ## Previously emulator was refitted on the standard normal domain
      ## Instead I now use original emulators, but switch back and forth between domains
      
      ## range limits on standard normal domain
      rng_stdn <- norm_transform$rng[,,1] #all same, maybe return just one from norm_transform_priors
      
      ## get new init.list and jmp.list
      init.list <- norm_transform$init
      
    }
    
    ## proposing starting points from knots
    mu_site_init <- list()
    jump_init    <- list()
    if(nrow(SS.stack[[1]][[1]]) > nsites*tmp.settings$assim.batch$chain){
      # sample without replacement
      sampind <- sample(seq_len(nrow(SS.stack[[1]][[1]])), nsites*tmp.settings$assim.batch$chain)
    }else{
      # this would hardly happen, usually we have a lot more knots than nsites*tmp.settings$assim.batch$chain
      # but to make this less error-prone sample with replacement if we have fewer, combinations should still be different enough
      sampind <- sample(seq_len(nrow(SS.stack[[1]][[1]])), nsites*tmp.settings$assim.batch$chain, replace = TRUE)
    }
    
    for(i in seq_len(tmp.settings$assim.batch$chain)){
      mu_site_init[[i]] <- SS.stack[[1]][[1]][sampind[((i-1) * nsites + 1):((i-1) * nsites + nsites)], 1:nparam]
      jump_init[[i]]    <- need_obj$resume.list[[i]]$jump
    }

    current.step <- "HIERARCHICAL MCMC PREP"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    

    ########### hierarchical MCMC function with Gibbs ##############
    
    
    hier.mcmc <- function(settings, gp.stack, nstack, nmcmc, rng_stdn, rng_orig,
                          mu0, jmp0, mu_site_init, nparam, nsites, prior.fn.all, prior.ind.all){

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
      #      mu_global_mean      : prior mean vector
      #      mu_global_sigma     : prior covariance matrix
      #      mu_global_tau       : prior precision matrix
      #
      #      mu_global ~ MVN (mu_global_mean, mu_global_tau)
      
      ### these are all in the STANDARD NORMAL DOMAIN
      # we want MVN for mu_global for conjugacy
      # stdandard normal to avoid singularity (param units may differ on orders of magnitudes)
      
      mu_global_mean   <- as.matrix(rep(0, nparam))
      mu_global_sigma  <- diag(1, nparam)
      mu_global_tau    <- solve(mu_global_sigma) 
      
      
      ## initialize mu_global (nparam)
      mu_global <- as.matrix(unlist(mu0))
      
      ######  (hierarchical) global tau priors
      #
      #      tau_global_df        : Wishart degrees of freedom
      #      tau_global_sigma     : Wishart scale matrix
      #
      #      tau_global ~ W (tau_global_df, tau_global_sigma)
      #      sigma_global <- solve(tau_global)
      #
      
      tau_global_df     <- nparam # the least informative choice
      tau_global_sigma  <- diag(1, nparam)
      tau_global_tau    <- solve(tau_global_sigma)  # will be used in gibbs updating
      
      # initialize tau_global (nparam x nparam)
      tau_global   <- rWishart(1, tau_global_df, tau_global_sigma)[,,1]
      sigma_global <- solve(tau_global)
      
      # initialize jcov.arr (jump variances per site)
      jcov.arr <-  array(NA_real_, c(nparam, nparam, nsites))
      for(j in seq_len(nsites)) jcov.arr[,,j] <- jmp0
      
      # prepare mu_site (nsite x nparam)
      mu_site_new  <- matrix(NA_real_, nrow = nsites, ncol= nparam)
      mu_site_new_stdn  <- matrix(NA_real_, nrow = nsites, ncol= nparam)
      
      mu_site_curr <- mu_site_init
      
      # values for each site will be accepted/rejected in themselves
      currSS    <- sapply(seq_len(nsites), function(v) PEcAn.emulator::get_ss(gp.stack[[v]], mu_site_curr[v,], pos.check))
      # force it to be nvar x nsites matrix
      currSS <- matrix(currSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
      currllp   <- lapply(seq_len(nsites), function(v) PEcAn.assim.batch::pda.calc.llik.par(settings, nstack[[v]], currSS[,v]))
      
      # storage
      mu_site_samp    <-  array(NA_real_, c(nmcmc, nparam, nsites))
      mu_global_samp  <-  matrix(NA_real_, nrow = nmcmc, ncol= nparam)
      tau_global_samp <-  array(NA_real_, c(nmcmc, nparam, nparam))
      
      musite.accept.count    <- rep(0, nsites)
      

      ########################## Start MCMC ########################
      
      for(g in 1:nmcmc){
        
         # jump adaptation step
         if ((g > 2) && ((g - 1) %% settings$assim.batch$jump$adapt == 0)) {
         
           # update site level jvars
           params.recent <- mu_site_samp[(g - settings$assim.batch$jump$adapt):(g - 1), , ]
           #colnames(params.recent) <- names(x0)
           jcov.list <- lapply(seq_len(nsites), function(v) pda.adjust.jumps.bs(settings, jcov.arr[,,v], musite.accept.count[v], params.recent[,,v]))
           jcov.arr  <- abind::abind(jcov.list, along=3)
           musite.accept.count <- rep(0, nsites)  # Reset counter
         
         }
         
        
        ########################################
        # gibbs update tau_global | mu_global, mu_site
        #
        # W(tau_global | mu_global, mu_site) ~ MVN( mu_site | mu_global, tau_global) * W(tau_global | tau_df, tau_V)
        # 
        #
        # using MVN-Wishart conjugacy
        # prior hyperparameters:     tau_global_df, tau_global_sigma
        # posterior hyperparameters: tau_global_df_gibbs, tau_global_sigma_gibbs
        #
        # update:
        # tau_global ~ W(tau_global_df_gibbs, tau_global_sigma_gibbs)
        
        tau_global_df_gibbs <- tau_global_df + nsites
        
        # transform from original domain to standard normal
        mu_site_curr_stdn <- sapply(seq_len(nparam), function(x){
          orig.quantiles <- eval(prior.fn.all$pprior[[prior.ind.all[x]]], list(q = mu_site_curr[,x]))
          norm.vals   <- qnorm(orig.quantiles)
          return(norm.vals)
        })
        
        # sum of pairwise deviation products
        pairwise_deviation <- apply(mu_site_curr_stdn, 1, function(r) r - t(mu_global))
        sum_term <- pairwise_deviation %*% t(pairwise_deviation)
        
        tau_global_sigma_gibbs <- solve(tau_global_tau + sum_term)
        
        # update tau
        tau_global <- rWishart(1, df = tau_global_df_gibbs, Sigma = tau_global_sigma_gibbs)[,,1] # across-site precision
        sigma_global <- solve(tau_global) # across-site covariance, to be used below
        
        
        ########################################
        # update mu_global | mu_site, tau_global
        #
        # MVN(mu_global | mu_site, tau_global) ~ MVN( mu_site | mu_global, tau_global) * W(tau_global | tau_df, tau_V)
        #
        # mu_global ~ MVN(global_mu, global_Sigma)
        #
        # mu_global     : global parameters
        # global_mu     : precision weighted average between the data (mu_site) and prior mean (mu_f)
        # global_Sigma  : sum of mu_site and mu_f precision      
        #
        # Dietze, 2017, Eqn 13.6
        # mu_global ~ MVN(solve((nsites * sigma_global) + P_f_inv)) * ((nsites * sigma_global) + P_f_inv * mu_f),  
        #                 solve((nsites * sigma_global) + P_f_inv))
        
        # prior hyperparameters      : mu_global_mean, mu_global_sigma
        # posterior hyperparameters  : mu_global_mean_gibbs, mu_global_sigma_gibbs
        #
        # update:
        # mu_global ~ MVN(mu_global_mean_gibbs, mu_global_sigma_gibbs)

        # calculate mu_global_sigma_gibbs from prior hyperparameters and tau_global
        mu_global_sigma_gibbs <- solve(mu_global_tau + nsites * tau_global)
        
        # Jensen's inequality: take the mean of mu_site_curr, then transform
        mu_site_bar     <- apply(mu_site_curr, 2, mean)
        mu_site_bar_std <- sapply(seq_len(nparam), function(x){
          prior.quantiles <- eval(prior.fn.all$pprior[[prior.ind.all[x]]], list(q = mu_site_bar[x]))
          norm.vals    <- qnorm(prior.quantiles)
          return(norm.vals)
        })
        mu_site_bar_std <- as.matrix(mu_site_bar_std)
        
        # calculate mu_global_mean_gibbs from prior hyperparameters, mu_site_means and tau_global
        mu_global_mean_gibbs <- mu_global_sigma_gibbs %*% (mu_global_tau %*% mu_global_mean + (tau_global * nsites) %*% mu_site_bar_std)
        
        # update mu_global
        mu_global <- mvtnorm::rmvnorm(1, mu_global_mean_gibbs, mu_global_sigma_gibbs) # new prior mu to be used below for prior prob. calc.
        
        
        # site level M-H
        ########################################
        
        # # propose new mu_site on standard normal domain
        # for(ns in seq_len(nsites)){
        #   repeat{ # make sure to stay in emulator boundaries, otherwise it confuses adaptation
        #     mu_site_new_stdn[ns,] <- mvtnorm::rmvnorm(1, mu_global,  sigma_global)
        #     check.that <- (mu_site_new_stdn[ns,] > rng_stdn[, 1] & mu_site_new_stdn[ns, ] < rng_stdn[, 2])
        #     if(all(check.that)) break
        #   }
        # }
        
        # propose new site parameter vectors
         for(ns in seq_len(nsites)){
           repeat{ # make sure to stay in emulator boundaries, otherwise it confuses adaptation
             mu_site_new[ns,] <- mvtnorm::rmvnorm(1, mu_site_curr[ns,], jcov.arr[,,ns])
             check.that <- (mu_site_new[ns,] > rng_orig[, 1] & mu_site_new[ns, ] < rng_orig[, 2])
             if(all(check.that)) break
           }
         }
        # # transform back to original domain
        # mu_site_new <- sapply(seq_len(nparam), function(x){
        #   norm.quantiles   <- pnorm(mu_site_new_stdn[,x])
        #   orig.vals <- eval(prior.fn.all$qprior[[prior.ind.all[x]]], list(p = norm.quantiles))
        #   return(orig.vals)
        # })
        
         mu_site_new_stdn <- sapply(seq_len(nparam), function(x){
           orig.quantiles <- eval(prior.fn.all$pprior[[prior.ind.all[x]]], list(q = mu_site_new[,x]))
           norm.vals   <- qnorm(orig.quantiles)
           return(norm.vals)
         })
        
        # re-predict current SS
        currSS <- sapply(seq_len(nsites), function(v) get_ss(gp.stack[[v]], mu_site_curr[v,], pos.check))
        currSS <- matrix(currSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
        
        # calculate posterior
        currLL    <- sapply(seq_len(nsites), function(v) pda.calc.llik(currSS[,v], llik.fn, currllp[[v]]))
        # use new priors for calculating prior probability
        currPrior <- dmvnorm(mu_site_curr_stdn, mu_global, sigma_global, log = TRUE)
        #currPrior <- unlist(lapply(seq_len(nsites), function(v) mvtnorm::dmvnorm(mu_site_curr_stdn[v,], mu_global, sigma_global, log = TRUE)))
        currPost  <- currLL + currPrior
        
        # predict new SS
        newSS <- sapply(seq_len(nsites), function(v) get_ss(gp.stack[[v]], mu_site_new[v,], pos.check))
        newSS <- matrix(newSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
        
        # calculate posterior
        newllp   <- lapply(seq_len(nsites), function(v) pda.calc.llik.par(settings, nstack[[v]], newSS[,v]))
        newLL    <- sapply(seq_len(nsites), function(v) pda.calc.llik(newSS[,v], llik.fn, newllp[[v]]))
        # use new priors for calculating prior probability
        newPrior <- dmvnorm(mu_site_new_stdn, mu_global, sigma_global, log = TRUE)
        #newPrior <- unlist(lapply(seq_len(nsites), function(v) mvtnorm::dmvnorm(mu_site_new_stdn[v,], mu_s, s_sigma[[v]], log = TRUE)))
        newPost  <- newLL + newPrior
        
        ar <- is.accepted(currPost, newPost)
        mu_site_curr[ar, ] <- mu_site_new[ar, ]
        mu_site_curr_stdn[ar, ] <- mu_site_new_stdn[ar, ]
        musite.accept.count <- musite.accept.count + ar
        
        mu_site_samp[g, , seq_len(nsites)] <- t(mu_site_curr)[,seq_len(nsites)]
        mu_global_samp[g,]       <- mu_global  # 100% acceptance for gibbs
        tau_global_samp[g, , ]   <- tau_global # 100% acceptance for gibbs
        
        if(g %% 500 == 0) PEcAn.logger::logger.info(g, "of", nmcmc, "iterations")
      }
      
      return(list(mu_site_samp = mu_site_samp, mu_global_samp = mu_global_samp, tau_global_samp = tau_global_samp,
                  musite.accept.count = musite.accept.count))
    } # hier.mcmc
    
    # start the clock
    ptm.start <- proc.time()
    
    # prepare for parallelization
    dcores <- parallel::detectCores() - 1
    ncores <- min(max(dcores, 1), tmp.settings$assim.batch$chain)
     
    logger.setOutputFile(file.path(tmp.settings$outdir, "pda.log"))
    
    cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(tmp.settings$outdir, "pda.log"))
    
    
    ## Sample posterior from emulator
    mcmc.out <- parallel::parLapply(cl, seq_len(tmp.settings$assim.batch$chain), function(chain) {
      hier.mcmc(settings      = tmp.settings, 
                gp.stack      = gp.stack, 
                nstack        = NULL, 
                nmcmc         = tmp.settings$assim.batch$iter, 
                rng_stdn      = rng_stdn, 
                rng_orig      = rng_orig,
                mu0           = init.list[[chain]], 
                jmp0          = jump_init[[chain]], 
                mu_site_init  = mu_site_init[[chain]],
                nparam        = length(prior.ind.all), 
                nsites        = nsites, 
                prior.fn.all  = prior.fn.all, 
                prior.ind.all = prior.ind.all)
    })
    
    parallel::stopCluster(cl)
    
    # Stop the clock
    ptm.finish <- proc.time() - ptm.start
    logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(tmp.settings$assim.batch$iter), " iterations."))
    
    current.step <- "HIERARCHICAL MCMC END"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    
    # transform samples from std normal to prior quantiles
    mcmc.out2 <- back_transform_posteriors(prior.all, prior.fn.all, prior.ind.all, mcmc.out)
    
    # Collect global params in their own list and postprocess
    mcmc.param.list <- pda.sort.params(mcmc.out2, sub.sample = "mu_global_samp", ns = NULL, prior.all, prior.ind.all.ns, sf, n.param.orig, prior.list, prior.fn.all)
    # processing these just for further analysis later, but con=NULL because these samples shouldn't be used for new runs later
    tmp.settings <- pda.postprocess(tmp.settings, con = NULL, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = "_hierarchical_mean")
    
    mcmc.param.list <- pda.sort.params(mcmc.out2, sub.sample = "hierarchical_samp", ns = NULL, prior.all, prior.ind.all.ns, sf, n.param.orig, prior.list, prior.fn.all)
    tmp.settings <- pda.postprocess(tmp.settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = "_hierarchical")
    
    # Collect site-level params in their own list and postprocess
    for(ns in seq_len(nsites)){
      mcmc.param.list <- pda.sort.params(mcmc.out, sub.sample = "mu_site_samp", ns = ns, prior.all, prior.ind.all.ns, sf, n.param.orig, prior.list, prior.fn.all)
      settings <- pda.postprocess(tmp.settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = paste0("_hierarchical_SL",ns))
    }
    
    current.step <- "HIERARCHICAL - END"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    
  } # hierarchical - if end
  
}

