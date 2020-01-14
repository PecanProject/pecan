##' Paramater Data Assimilation using MCMC
##'
##' Brute-force, only to be used on simple models
##'
##' @title Paramater Data Assimilation using MCMC
##' @param settings = a pecan settings list
##' @param params.id id of pars
##' @param param.names names of pars
##' @param prior.id ids of priors
##' @param chain how many chains
##' @param iter how many iterations
##' @param adapt adaptation intervals
##' @param adj.min to be used in adjustment
##' @param ar.target acceptance rate target
##' @param jvar jump variance
##' @param n.knot number of knots requested
##'
##'
##' @return nothing. Diagnostic plots, MCMC samples, and posterior distributions
##'  are saved as files and db records.
##'
##' @author Mike Dietze
##' @author Ryan Kelly
##' @export
pda.mcmc <- function(settings, params.id = NULL, param.names = NULL, prior.id = NULL,
                     chain = NULL, iter = NULL, adapt = NULL, adj.min = NULL,
                     ar.target = NULL, jvar = NULL, n.knot = NULL) {
  
  ## this bit of code is useful for defining the variables passed to this function if you are
  ## debugging
  if (FALSE) {
    params.id <- param.names <- prior.id <- chain <- iter <- NULL
    n.knot <- adapt <- adj.min <- ar.target <- jvar <- NULL
  }
  
  ## -------------------------------------- Setup ------------------------------------- ## Handle
  ## settings
  settings <- pda.settings(settings = settings, params.id = params.id, param.names = param.names, 
                           prior.id = prior.id, chain = chain, iter = iter, adapt = adapt, adj.min = adj.min, ar.target = ar.target, 
                           jvar = jvar, n.knot = n.knot)
  
  
  ## will be used to check if multiplicative Gaussian is requested
  any.mgauss <- sapply(settings$assim.batch$inputs, `[[`, "likelihood")
  
  ## Open database connection
  if (settings$database$bety$write) {
    con <- try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
    if (inherits(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(PEcAn.DB::db.close(con), add = TRUE)
    }
  } else {
    con <- NULL
  }
  
  bety <- dplyr::src_postgres(dbname = settings$database$bety$dbname,
                       host = settings$database$bety$host, 
                       user = settings$database$bety$user, 
                       password = settings$database$bety$password)
  
  ## Load priors
  temp        <- pda.load.priors(settings, bety$con)
  prior.list  <- temp$prior
  settings    <- temp$settings
  pname       <- lapply(prior.list, rownames)
  n.param.all <- sapply(prior.list, nrow)
  
  ## Load data to assimilate against
  inputs  <- load.pda.data(settings, bety)
  n.input <- length(inputs)
  
  # get hyper parameters if any
  hyper.pars <- return_hyperpars(settings$assim.batch, inputs)
  
  ## Set model-specific functions
  do.call("require", list(paste0("PEcAn.", settings$model$type)))
  my.write.config <- paste0("write.config.", settings$model$type)
  if (!exists(my.write.config)) {
    PEcAn.logger::logger.severe(paste(my.write.config, 
                        "does not exist. Please make sure that the PEcAn interface is loaded for", 
                        settings$model$type))
  }
  
  ## Select parameters to constrain
  prior.ind <- lapply(seq_along(settings$pfts), 
                      function(x) which(pname[[x]] %in% settings$assim.batch$param.names[[x]]))
  n.param <- sapply(prior.ind, length)
  
  ## Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  
  ## Create an ensemble id
  settings$assim.batch$ensemble.id <- pda.create.ensemble(settings, con, workflow.id)
  
  ## Set up likelihood functions
  llik.fn <- pda.define.llik.fn(settings)
  
  ## Set prior distribution functions (d___, q___, r___, and multivariate versions)
  prior.all     <- do.call("rbind", prior.list)
  prior.fn.all  <- pda.define.prior.fn(prior.all)
  prior.ind.all <- which(unlist(pname) %in% unlist(settings$assim.batch$param.names))
  pname.all     <- unlist(pname)
  
  ## ----------------------------------- MCMC Setup ----------------------------------- ##
  
  mcmc.list <- jvar.list <- llpar.list <- list()
  
  for (chain in 1:settings$assim.batch$chain) {
    params.list <- pda.init.params(settings, chain, pname.all, sum(n.param.all))
    
    ## Initialize empty params matrix (concatenated to params from a previous PDA, if provided)
    params <- params.list$params
    start  <- params.list$start
    finish <- params.list$finish
    
    iter.flag <- 1
    
    if(!is.null(settings$assim.batch$extension) & !is.null(params.list$llpars)){
      llpars     <- params.list$llpars
      llparnames <- sapply(strsplit(colnames(llpars), "\\."), `[[`, 1)
      bias       <- llpars[ ,llparnames == "bias"]
      nobias     <- llpars[ ,llparnames != "bias"]
      all.bias   <- bias[length(bias)]
      parl       <- nobias[length(nobias)]
      LLpar      <- matrix(NA, ncol= ncol(llpars), nrow = (finish-start)+1,
                           dimnames = list(NULL, colnames(llpars)))
      LLpar <- rbind(llpars, LLpar)
      par.flag   <- TRUE
      iter.flag  <- 0
    }
    
    ## Set initial conditions
    if (start == 1) {
      parm <- sapply(prior.fn.all$qprior, eval, list(p = 0.5))
    } else {
      parm <- params[start - 1, ]
    }
    names(parm) <- pname.all
    LL.old <- prior.old <- -Inf
    
    ## Jump distribution setup
    accept.rate <- numeric(sum(n.param))  ## Create acceptance rate vector of 0's (one zero per parameter)
    
    # Default jump variances.  default to 0.1 * 90% prior CI
    if (!is.null(settings$assim.batch$extension)) {
      load(settings$assim.batch$jvar.path)  # loads params
      jmp.vars <- jvar.list[[chain]]
    } else {
      jmp.vars <- sapply(prior.fn.all$qprior, 
                         function(x) 0.1 * diff(eval(x, list(p = c(0.05, 0.95)))))[prior.ind.all]
    }
    
    ## Create dir for diagnostic output
    if (!is.null(settings$assim.batch$diag.plot.iter)) {
      dir.create(file.path(settings$outdir,
                           paste0("diag.pda", settings$assim.batch$ensemble.id)), 
                 showWarnings = FALSE, recursive = TRUE)
    }
    
    ## save updated settings XML. Will be overwritten at end, but useful in case of crash
    XML::saveXML(
      PEcAn.settings::listToXml(settings, "pecan"),
      file = file.path(
        settings$outdir,
        paste0("pecan.pda", settings$assim.batch$ensemble.id, ".xml")))

    ## --------------------------------- Main MCMC loop --------------------------------- ##
    for (i in start:finish) {
      PEcAn.logger::logger.info(paste("Data assimilation MCMC iteration", i, "of", finish))
      
      ## Adjust Jump distribution
      if (i%%settings$assim.batch$jump$adapt < 1) {
        jmp.vars <- pda.adjust.jumps(settings, 
                                     jmp.vars,
                                     accept.rate,
                                     pnames = pname.all[prior.ind.all])
        accept.rate <- numeric(sum(n.param))
        
        # # Save updated settings XML. Will be overwritten at end, but useful in case of crash
        # saveXML(listToXml(settings, 'pecan'), file=file.path(settings$outdir, paste0('pecan.pda',
        # settings$assim.batch$ensemble.id, '.xml')))
      }
      
      for (j in seq_len(sum(n.param))) {
        pstar <- parm
        
        ## Propose parameter values
        if (i > 1) {
          pnew <- stats::rnorm(1, parm[prior.ind.all[j]], jmp.vars[j])
          pstar[prior.ind.all[j]] <- pnew
        }
        
        ## Check that value falls within the prior
        prior.star <- prior.fn.all$dmvprior(pstar)
        
        # Convert pstar to a list of 1-row data frame
        if (is.null(dim(pstar))) {
          pnames <- names(pstar)
          run.params <- as.data.frame(matrix(pstar, nrow = 1))
          names(run.params) <- pnames
        }
        run.params <- list(run.params)
        
        if (is.finite(prior.star)) {
          ## Set up run and write run configs
          run.id <- pda.init.run(settings,
                                 con,
                                 my.write.config, 
                                 workflow.id, 
                                 run.params, 
                                 n = 1, 
                                 run.names = paste0("MCMC_chain.", chain, "_iteration.", i, "_variable.", j))
          
          ## Start model run
          PEcAn.remote::start.model.runs(settings, settings$database$bety$write)
          
          ## Read model outputs
          align.return <- pda.get.model.output(settings, run.id, bety, inputs)
          model.out <- align.return$model.out
          if(!is.na(model.out)){
            inputs <- align.return$inputs
          } 
          
          # do this once
          if(i==1){
            inputs <- pda.neff.calc(inputs)
          }
          
          # retrieve n
          n.of.obs <- sapply(inputs,`[[`, "n") 
          names(n.of.obs) <- sapply(model.out,names)
          
          # handle bias parameters if multiplicative Gaussian is listed in the likelihoods
          if(any(unlist(any.mgauss) == "multipGauss")) {
            isbias <- which(unlist(any.mgauss) == "multipGauss")
            # testing now
            nbias <- 1
            bias.list <- return.bias(isbias, list(model.out), inputs, prior.list, nbias)
            bias.terms <- bias.list$bias.params
          } else {
            bias.terms <- NULL
          }
          
          if(!is.null(bias.terms)){
            all.bias <- lapply(bias.terms, function(n) n[1,])
            all.bias <- do.call("rbind", all.bias)
          } else {
            all.bias <- NULL
          }
          
          ## calculate error statistics      
          pda.errors <- pda.calc.error(settings, con, model_out = model.out, run.id, inputs, all.bias)
          llik.par <- pda.calc.llik.par(settings, n = n.of.obs, 
                                        error.stats = unlist(pda.errors),
                                        hyper.pars)
          # store llik-par
          parl <- unlist(sapply(llik.par, `[[` , "par"))
          if(!is.null(parl) & iter.flag == 1 & is.null(all.bias)) {
            LLpar <- matrix(NA, ncol= length(parl), nrow = finish, dimnames = list(NULL, names(parl)))
            par.flag <- TRUE
            iter.flag <- 0
          } else if(!is.null(parl) & iter.flag == 1 & !is.null(all.bias)) {
            LLpar <- matrix(NA, ncol= length(parl) + nrow(all.bias), nrow = finish, 
                            dimnames = list(NULL, c(rownames(all.bias), names(parl))))
            par.flag <- TRUE
            iter.flag <- 0
          } else if(iter.flag == 1){
            par.flag <- FALSE
            iter.flag <- 0
          }
          
          ## Calculate likelihood
          LL.new <- pda.calc.llik(pda.errors = unlist(pda.errors), llik.fn, llik.par)
          
          ## Accept or reject step
          a <- LL.new - LL.old + prior.star - prior.old
          if (is.na(a)) {
            a <- -Inf  # Can occur if LL.new == -Inf (due to model crash) and LL.old == -Inf (first run)
          }
          
          if (a > log(stats::runif(1))) {
            LL.old <- LL.new
            prior.old <- prior.star
            parm <- pstar
            accept.rate[j] <- accept.rate[j] + 1
          }
        }  ## end if(is.finite(prior.star))
      }  ## end loop over variables
      
      ## Diagnostic figure
      if (!is.null(settings$assim.batch$diag.plot.iter) && is.finite(prior.star) && 
          (i == start | 
           i == finish |
           (i%%settings$assim.batch$diag.plot.iter == 0))) {
        grDevices::pdf(
          file.path(
            settings$outdir,
            paste0("diag.pda", settings$assim.batch$ensemble.id),
            paste0("data.vs.model_", gsub(" ", "0", sprintf("%5.0f", i)), ".pdf")))

        NEEo      <- inputs[[1]]$obs
        NEEm      <- model.out[[1]]
        NEE.resid <- NEEm - NEEo

        graphics::par(mfrow = c(1, 2))
        graphics::plot(NEEo)
        graphics::points(NEEm, col = 2, cex = 0.5)
        graphics::legend("topleft", col = c(1, 2), pch = 1,
                         legend = c("data", "model"))
        graphics::hist(NEE.resid, 100,
                       main = paste0("LLik: ", round(LL.new, 1)))
        grDevices::dev.off()
      }
      
      ## Store output
      params[i, ] <- parm
      if(!is.null(parl) & is.null(all.bias)){
        LLpar[i, ]  <- parl
      } else if (!is.null(parl) & !is.null(all.bias)){
        LLpar[i, ]  <- c(all.bias, parl)
      }
      
      ## Add to temp file (overwrite when i=1, append thereafter) 
      ## cat(c(parm,'\n'), file=filename.mcmc.temp, sep='\t', append=(i != 1))
    }
    
    mcmc.list[[chain]] <- params
    jvar.list[[chain]] <- jmp.vars
    if(par.flag){
      llpar.list[[chain]] <- LLpar
    }
  }  # end of chain-loop
  
  settings$assim.batch$mcmc.path <- file.path(settings$outdir, 
                                              paste0("mcmc.list.pda", 
                                                     settings$assim.batch$ensemble.id, 
                                                     ".Rdata"))
  save(mcmc.list, file = settings$assim.batch$mcmc.path)
  
  # subset to params of interst only
  mcmc.list <- lapply(mcmc.list, "[", , prior.ind.all, drop = FALSE)
  
  # Separate each PFT's parameter samples to their own list
  mcmc.param.list <- list()
  ind <- 0
  for (i in seq_along(settings$pfts)) {
    mcmc.param.list[[i]] <- lapply(mcmc.list, 
                                   function(x) x[, (ind + 1):(ind + n.param[i]), drop = FALSE])
    ind <- ind + n.param[i]
  }

  if (par.flag){
    mcmc.param.list[[length(mcmc.param.list)+1]] <- list()
    prior.list[[length(prior.list)+1]] <- list()
    for(c in seq_len(settings$assim.batch$chain)){
      mcmc.param.list[[length(mcmc.param.list)]][[c]] <- llpar.list[[c]]
    }
    
    settings$assim.batch$llpar.path <- file.path(settings$outdir, 
                                                 paste0("llpar.pda",
                                                        settings$assim.batch$ensemble.id, 
                                                        ".Rdata"))
    save(llpar.list, file = settings$assim.batch$llpar.path)
  }
  
  ## ------------------------------------ Clean up ------------------------------------
  ## Save outputs to plots, files, and db
  settings$assim.batch$jvar.path <- file.path(settings$outdir,
                                              paste0("jvar.pda", settings$assim.batch$ensemble.id, ".Rdata"))
  save(jvar.list, file = settings$assim.batch$jvar.path)
  
  settings <- pda.postprocess(settings, con, mcmc.param.list, pname, prior.list, prior.ind)
  
  ## close database connection
  if (!is.null(con)) {
    PEcAn.DB::db.close(con)
  }
  
  ## Output an updated settings list
  return(settings)
  
} # pda.mcmc
