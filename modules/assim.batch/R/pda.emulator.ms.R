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
  need_obj <- load_pda_history(workdir = multi.settings$outdir,  
                   ensemble.id = multi.settings[[8]]$assim.batch$ensemble.id, 
                   objects = c("init.list", "rng", "jmp.list", "prior.fn.all", "prior.ind.all", "llik.fn", "settings"))
  
  init.list     <- need_obj$init.list
  rng           <- need_obj$rng
  jmp.list      <- need_obj$jmp.list
  prior.fn.all  <- need_obj$prior.fn.all
  prior.ind.all <- need_obj$prior.ind.all
  llik.fn       <- need_obj$llik.fn
  tmp.settings  <- need_obj$settings
  
  
  resume.list <- vector("list", multi.settings[[1]]$assim.batch$chain)

  
  
  
  ## -------------------------------------- Global calibration -------------------------------------------------- 
  if(global){ # global - if begin
    
    # collect SS matrices
    for(s in seq_along(multi.settings)){
      load(multi.settings[[s]]$assim.batch$ss.path)
      SS.stack[[s]] <- SS
      remove(SS)
    }
    
    # double check indices and dimensions for multiple variables later
    SS <- lapply(seq_along(SS.stack[[1]]), 
           function(l) matrix(sapply(SS.stack,`[[`,l), ncol = ncol(SS.stack[[1]][[l]]), byrow = FALSE))
    
    # pass colnames using the first SS.stack (all should be the same)
    for(c in seq_along(SS)){
      colnames(SS[[c]]) <- colnames(SS.stack[[1]][[c]]) 
    }
    
    ## Fit emulator on SS from multiple sites ##
    
    # start the clock
    ptm.start <- proc.time()
    
    # prepare for parallelization
    dcores <- parallel::detectCores() - 1
    ncores <- min(max(dcores, 1), length(SS))
    
    cl <- parallel::makeCluster(ncores, type="FORK")
    
    ## Parallel fit for GPs
    GPmodel <- parallel::parLapply(cl, SS, function(x) mlegp::mlegp(X = x[, -ncol(x), drop = FALSE], Z = x[, ncol(x), drop = FALSE], nugget = 0, nugget.known = 1, verbose = 0))
    # GPmodel <- lapply(SS, function(x) mlegp::mlegp(X = x[, -ncol(x), drop = FALSE], Z = x[, ncol(x), drop = FALSE], nugget = 0, nugget.known = 1, verbose = 0))
    
    
    parallel::stopCluster(cl)
    
    # Stop the clock
    ptm.finish <- proc.time() - ptm.start
    PEcAn.logger::logger.info(paste0("GP fitting took ", paste0(round(ptm.finish[3])), " seconds."))
    
    
    gp <- GPmodel
    
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
              resume.list = resume.list[[chain]]
      )
    })
    
    parallel::stopCluster(cl)
    
    # Stop the clock
    ptm.finish <- proc.time() - ptm.start
    logger.info(paste0("Emulator MCMC took ", paste0(round(ptm.finish[3])), " seconds for ", paste0(settings$assim.batch$iter), " iterations."))
    
    
  } # global - if end
  
}