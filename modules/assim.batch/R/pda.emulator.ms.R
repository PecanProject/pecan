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
    n_rounds <- ifelse(is.null(multi.settings[[1]]$assim.batch$n_rounds), 5, as.numeric(multi.settings[[1]]$assim.batch$n_rounds))
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
        Sys.sleep(300)
        check_all_sites <- sapply(emulator_jobs, PEcAn.remote::qsub_run_finished,  multi.settings[[1]]$host, multi.settings[[1]]$host$qstat)
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
  
  SS <- NULL # will be loaded from history objects next loop,  but R CMD check can't see that
  
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
    con <- try(PEcAn.DB::db.open(multi.settings$database$bety), silent = TRUE)
    if (methods::is(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(PEcAn.DB::db.close(con))
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
  ## this will change soon (?!)
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
     
    PEcAn.logger::logger.setOutputFile(file.path(multi.settings$outdir, "pda.log"))
     
    cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(multi.settings$outdir, "pda.log"))
    
    ## Sample posterior from emulator
    mcmc.out <- parallel::parLapply(cl, 1:multi.settings[[1]]$assim.batch$chain, function(chain) {
      PEcAn.emulator::mcmc.GP(gp          = gp, ## Emulator(s)
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
    PEcAn.logger::logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(tmp.settings$assim.batch$iter), " iterations."))
    
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

    
    
    ## proposing starting points from knots
    mu_site_init <- list()
    jump_init    <- list()

    # sample without replacement
    sampind <- sample(seq_len(nrow(SS.stack[[1]][[1]])), tmp.settings$assim.batch$chain)
  
    for(i in seq_len(tmp.settings$assim.batch$chain)){
      mu_site_init[[i]] <- SS.stack[[1]][[1]][sampind[i], 1:nparam]
      jump_init[[i]]    <- need_obj$resume.list[[i]]$jump
    }

    current.step <- "HIERARCHICAL MCMC PREP"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    
    
    # start the clock
    ptm.start <- proc.time()
    
    # prepare for parallelization
    dcores <- parallel::detectCores() - 1
    ncores <- min(max(dcores, 1), tmp.settings$assim.batch$chain)
     
    PEcAn.logger::logger.setOutputFile(file.path(tmp.settings$outdir, "pda.log"))
    
    cl <- parallel::makeCluster(ncores, type="FORK", outfile = file.path(tmp.settings$outdir, "pda.log"))
    
    
    ## Sample posterior from emulator
    mcmc.out <- parallel::parLapply(cl, seq_len(tmp.settings$assim.batch$chain), function(chain) {
      hier.mcmc(settings      = tmp.settings, 
                gp.stack      = gp.stack, 
                nmcmc         = tmp.settings$assim.batch$iter * 3, # need to run chains longer than indv
                rng_orig      = rng_orig,
                jmp0          = jump_init[[chain]], 
                mu_site_init  = mu_site_init[[chain]],
                nparam        = length(prior.ind.all), 
                nsites        = nsites, 
                llik.fn       = llik.fn,
                prior.fn.all  = prior.fn.all, 
                prior.ind.all = prior.ind.all)
    })
    
    parallel::stopCluster(cl)
    
    # Stop the clock
    ptm.finish <- proc.time() - ptm.start
    PEcAn.logger::logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(tmp.settings$assim.batch$iter), " iterations."))
    
    current.step <- "HIERARCHICAL MCMC END"
    save(list = ls(all.names = TRUE),envir=environment(),file=hbc.restart.file)
    
    # generate hierarhical posteriors
    mcmc.out <- generate_hierpost(mcmc.out,  prior.fn.all, prior.ind.all)
    
    # Collect global params in their own list and postprocess
    mcmc.param.list <- pda.sort.params(mcmc.out, sub.sample = "mu_global_samp", ns = NULL, prior.all, prior.ind.all.ns, sf, n.param.orig, prior.list, prior.fn.all)
    # processing these just for further analysis later, but con=NULL because these samples shouldn't be used for new runs later
    tmp.settings <- pda.postprocess(tmp.settings, con = NULL, mcmc.param.list, pname, prior.list, prior.ind.orig, sffx = "_hierarchical_mean")
    
    mcmc.param.list <- pda.sort.params(mcmc.out, sub.sample = "hierarchical_samp", ns = NULL, prior.all, prior.ind.all.ns, sf, n.param.orig, prior.list, prior.fn.all)
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

