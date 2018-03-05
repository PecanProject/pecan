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
  SS.stack      <- vector("list", nsites) 
  gp.stack      <- vector("list", nsites) 
  prior.stack   <- vector("list", nsites) 
  nstack        <- vector("list", nsites) 
  
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
                   ensemble.id = multi.settings[[8]]$assim.batch$ensemble.id, 
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

  ## Open database connection
  if (settings$database$bety$write) {
    con <- try(db.open(settings$database$bety), silent = TRUE)
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
  
  ## Create an ensemble id
  tmp.settings$assim.batch$ensemble.id <- pda.create.ensemble(tmp.settings, con, workflow.id)
  
  
  ## -------------------------------------- Global calibration -------------------------------------------------- 
  if(global){ # global - if begin
    
    # collect GPs
    for(s in seq_along(multi.settings)){

      load(multi.settings[[s]]$assim.batch$emulator.path)
      gp.stack[[s]] <- gp
      remove(gp)
      
    }
    
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
    
    tmp.settings <- pda.postprocess(tmp.settings, con, mcmc.param.list, pname, prior.list, prior.ind.orig)
    
  } # global - if end
  
}