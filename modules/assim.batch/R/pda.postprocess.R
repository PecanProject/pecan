##' Postprocessing for PDA Results
##'
##' @title Postprocessing for PDA Results
##' @param settings PEcAn settings list
##' @param con DB connection
##' @param mcmc.param.list output of PDA MCMC
##' @param pname parameter names
##' @param prior prior list
##' @param prior.ind indices of targeted parameters
##' @param sffx suffix to the output files, e.g. "hierarchical"
##'
##' @return PEcAn settings list, updated with <params.id> pointing to the new params file.
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.postprocess <- function(settings, con, mcmc.param.list, pname, prior, prior.ind, sffx = NULL) {
  
  # prepare for non-model params
  if(length(mcmc.param.list) > length(settings$pfts)){  
    # create a subfolder under pft folder for non-model parameters
    par.file.name <- file.path(settings$outdir, paste0("pft/parameters"))
    dir.create(par.file.name, showWarnings = FALSE, recursive = TRUE)
    
    # parameters are in the last list, increase length(prior.ind) accordingly
    # only bias params will be thrown into emulator though
    # TODO: save posteriors for likelihood parameters in the database?
    par.names <- colnames(mcmc.param.list[[length(mcmc.param.list)]][[1]])
    extr.bias <- sapply(strsplit(par.names, "\\."), `[[`, 1)
    bias.ind <- which(extr.bias == "bias")
    
    prior.ind[[length(prior.ind)+1]] <- bias.ind
    pname[[length(pname)+1]] <- par.names[bias.ind]
  } else{
    par.file.name <- NULL
  }

  params.subset <- pda.plot.params(settings, mcmc.param.list, prior.ind, par.file.name, sffx)
  
  for (i in seq_along(settings$pfts)) {
    
    ## Save params
    filename.mcmc <- file.path(settings$pfts[[i]]$outdir, 
                                paste0("mcmc.pda.", 
                                      settings$pfts[[i]]$name, 
                                      "_", 
                                      settings$assim.batch$ensemble.id, 
                                      sffx, ".Rdata"))

    params.pft <- params.subset[[i]]
    save(params.pft, file = filename.mcmc)
    
    if(!is.null(con)){
      ## create a new Posteriors DB entry
      pft.id <- PEcAn.DB::db.query(paste0("SELECT pfts.id FROM pfts, modeltypes WHERE pfts.name='",
                                          settings$pfts[[i]]$name, 
                                          "' and pfts.modeltype_id=modeltypes.id and modeltypes.name='", 
                                          settings$model$type, "'"), 
                                   con)[["id"]]
      
      
      posteriorid <-  PEcAn.DB::db.query(paste0("INSERT INTO posteriors (pft_id) VALUES (",
                                                pft.id, ") RETURNING id"), con)
      
      
      PEcAn.logger::logger.info(paste0("--- Posteriorid for ", settings$pfts[[i]]$name, " is ", posteriorid, " ---"))
      settings$pfts[[i]]$posteriorid <- posteriorid
    }
    
    ## save named distributions
    ## *** TODO: Generalize for multiple PFTS
    post.distns <- PEcAn.MA::approx.posterior(trait.mcmc = params.subset[[i]], 
                                    priors = prior[[i]], 
                                    outdir = settings$pfts[[i]]$outdir, 
                                    filename.flag = paste0(".pda.", settings$pfts[[i]]$name, "_", 
                                                           settings$assim.batch$ensemble.id, sffx))
    filename <- file.path(settings$pfts[[i]]$outdir, 
                          paste0("post.distns.pda.", settings$pfts[[i]]$name, "_", 
                                 settings$assim.batch$ensemble.id, sffx, ".Rdata"))
    save(post.distns, file = filename)
    
    if(!is.null(con)){
      PEcAn.DB::dbfile.insert(dirname(filename), basename(filename), "Posterior", posteriorid, con)
    }

    
    # Symlink to post.distns.Rdata (no ensemble.id identifier)
    if (file.exists(file.path(dirname(filename), "post.distns.Rdata"))) {
      file.remove(file.path(dirname(filename), "post.distns.Rdata"))
    }
    file.symlink(filename, file.path(dirname(filename), "post.distns.Rdata"))
    
    ## coerce parameter output into the same format as trait.mcmc
    pname <- rownames(post.distns)
    trait.mcmc <- list()
    for (v in seq_along(prior.ind[[i]])) {
      beta.o <- array(params.subset[[i]][[v]], c(length(params.subset[[i]][[v]]), 1))
      colnames(beta.o) <- "beta.o"
      if (pname[prior.ind[[i]][v]] %in% names(trait.mcmc)) {
        trait.mcmc[[pname[prior.ind[[i]][v]]]] <- coda::mcmc.list(coda::as.mcmc(beta.o))
      } else {
        k <- length(trait.mcmc) + 1
        trait.mcmc[[k]] <- coda::mcmc.list(coda::as.mcmc(beta.o))
        names(trait.mcmc)[k] <- pname[prior.ind[[i]]][v]
      }
    }
    
    ## save updated parameter distributions as trait.mcmc so that they can be read by the ensemble code
    ## *** TODO: Generalize for multiple PFTS
    filename <- file.path(settings$pfts[[i]]$outdir,
                          paste0("trait.mcmc.pda.", 
                                 settings$pfts[[i]]$name, 
                                 "_", settings$assim.batch$ensemble.id, 
                                 sffx, ".Rdata"))
    save(trait.mcmc, file = filename)
    
    if(!is.null(con)){
      PEcAn.DB::dbfile.insert(dirname(filename), basename(filename), "Posterior", posteriorid, con)
    }
    
  }  #end of loop over PFTs
  
  ## save updated settings XML
  XML::saveXML(
    PEcAn.settings::listToXml(settings, "pecan"),
    file = file.path(
      dirname(settings$modeloutdir),
      paste0("pecan.pda", settings$assim.batch$ensemble.id, ".xml")))

  return(settings)
} # pda.postprocess


##' Plot PDA Parameter Diagnostics
##'
##' @title Plot PDA Parameter Diagnostics
##' @param settings PEcAn settings list
##' @param mcmc.param.list MCMC param list to be sorted
##' @param prior.ind indices of the targeted parameters
##' @param par.file.name output file name
##' @param sffx suffix to the output file names
##'
##' @return Nothing. Plot is generated and saved to PDF.
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.plot.params <- function(settings, mcmc.param.list, prior.ind, par.file.name = NULL, sffx) {
  
  params.subset <- list()
  
  # flag for gelman.plot
  enough.iter <- TRUE
  
  for (i in seq_along(prior.ind)) {
    params.subset[[i]] <- coda::as.mcmc.list(lapply(mcmc.param.list[[i]], coda::mcmc))
    
    burnin <- getBurnin(params.subset[[i]], method = "gelman.plot")
    
    # rare, but this can happen; better to throw an error than continue, as it might lead
    # mis-interpretation of posteriors otherwise
    if (burnin == nrow(params.subset[[i]][[1]])) {
      PEcAn.logger::logger.severe(paste0("*** Burn-in is the same as the length of the chain, please run a longer chain ***"))
    }
    
    params.subset[[i]] <- stats::window(params.subset[[i]], start = max(burnin, na.rm = TRUE))
    
    # chek number of iterations left after throwing the burnin, gelman.plot requires > 50
    if (nrow(params.subset[[i]][[1]]) < 50) {
      PEcAn.logger::logger.info(paste0("*** Not enough iterations in the chain after removing burn-in, skipping gelman.plot ***"))
      enough.iter <- FALSE
    }
    
    if(i <= length(settings$pfts)){
      grDevices::pdf(file.path(settings$pfts[[i]]$outdir,
                    paste0("mcmc.diagnostics.pda.",
                           settings$pfts[[i]]$name,
                           "_", settings$assim.batch$ensemble.id,
                           sffx, ".pdf")))
    } else {
      grDevices::pdf(file.path(par.file.name,
                    paste0("mcmc.diagnostics.pda.par_",
                           settings$assim.batch$ensemble.id,
                           sffx, ".pdf")))
    }

    graphics::layout(matrix(c(1, 2, 3, 4, 5, 6), ncol = 2, byrow = TRUE))

    graphics::plot(params.subset[[i]], auto.layout = FALSE)

    dm <- do.call("rbind", params.subset[[i]])
    
    if (length(prior.ind[[i]]) > 1) {
      correlationPlot(dm)
    }
    
    if (length(params.subset[[i]]) > 1 & enough.iter) {
      coda::gelman.plot(params.subset[[i]], auto.layout = FALSE, autoburnin = FALSE)
    }
    
    graphics::layout(1)
    grDevices::dev.off()
    
    # Write out convergence diagnostics to a txt file
    if(i <= length(settings$pfts)){
      filename.mcmc.temp <- file.path(settings$pfts[[i]]$outdir,
                                      paste0("mcmc.diagnostics.pda.", 
                                             settings$pfts[[i]]$name, "_", 
                                             settings$assim.batch$ensemble.id, 
                                             sffx, ".txt"))
    } else {
      filename.mcmc.temp <- file.path(par.file.name, 
                                      paste0("mcmc.diagnostics.pda.par_", 
                                             settings$assim.batch$ensemble.id, sffx, ".txt"))
    }

    
    cat("Summary statistics\n", file = filename.mcmc.temp)
    utils::capture.output(summary(params.subset[[i]]), file = filename.mcmc.temp, append = TRUE)
    cat("\n\n\n", file = filename.mcmc.temp, append = TRUE)
    
    if (length(prior.ind[[i]]) > 1) {
      cat("Covariance matrix :\n", file = filename.mcmc.temp, append = TRUE)
      utils::capture.output(stats::cov(dm), file = filename.mcmc.temp, append = TRUE)
      cat("\n\n\n", file = filename.mcmc.temp, append = TRUE)
    }
    
    if (length(prior.ind[[i]]) > 1) {
      cat("Correlation matrix :\n", file = filename.mcmc.temp, append = TRUE)
      utils::capture.output(stats::cor(dm), file = filename.mcmc.temp, append = TRUE)
      cat("\n\n\n", file = filename.mcmc.temp, append = TRUE)
    }
    
    if (length(params.subset[[i]]) > 1) {
      cat("Gelman and Rubin convergence diagnostics\n", file = filename.mcmc.temp, append = TRUE)
      utils::capture.output(coda::gelman.diag(params.subset[[i]], autoburnin = FALSE), file = filename.mcmc.temp,
                     append = TRUE)
    }
    
  }  # end of for-loop over prior.ind
  
  # convert mcmc.list to list of matrices
  params.subset.list <- list()
  for (i in seq_along(params.subset)) {
    params.subset.list[[i]] <- do.call("rbind", params.subset[[i]])
  } 
  # reformat each sublist such that params have their own list and return
  return(lapply(seq_along(params.subset.list), function(x) as.list(data.frame(params.subset.list[[x]]))))
  
} # pda.plot.params


##' Function to write posterior distributions of the scaling factors
##' @param sf.samp.list scaling factor MCMC samples
##' @param sf.prior scaling factor prior 
##' @param sf.samp.filename scaling factor posterior output file name
##' @export
write_sf_posterior <- function(sf.samp.list, sf.prior, sf.samp.filename){
  
  sf.samp <- coda::as.mcmc.list(lapply(sf.samp.list, coda::mcmc))
  
  # saving this before discarding burnin, because in resampling we want to keep the samples together
  save(sf.samp, file = sf.samp.filename)
  
  burnin <- getBurnin(sf.samp, method = "gelman.plot")
  
  sf.samp <- stats::window(sf.samp, start = max(burnin, na.rm = TRUE))
  
  # convert mcmc.list to list of matrices
  sf.subset.list <- list()
  sf.subset.list[[1]] <- as.data.frame(do.call("rbind", sf.samp))

  filename.flag <- paste0("_", basename(sf.samp.filename))
  
  sf.post.distns <- PEcAn.MA::approx.posterior(trait.mcmc = sf.subset.list[[1]], priors = sf.prior,
                                               outdir = dirname(sf.samp.filename),
                                               filename.flag = filename.flag)
  
  
  return(sf.post.distns)
  
} # write_sf_posterior


##' Function to sort Hierarchical MCMC samples
##' @param mcmc.out MCMC samples
##' @param sub.sample which subsample to return
##' @param ns site number
##' @param prior.all prior dataframe
##' @param prior.ind.all.ns indices of targeted parameters on the prior.all dataframe
##' @param sf scaling factor if used
##' @param n.param.orig original indices of parameters on the prior.list
##' @param prior.list list of prior dataframes
##' @param prior.fn.all prior functions
##' @export
pda.sort.params <- function(mcmc.out, sub.sample = "mu_global_samp", ns = NULL, prior.all, prior.ind.all.ns, 
                            sf = NULL, n.param.orig, prior.list, prior.fn.all){
  
  mcmc.samp.list <- list()
  
  for (c in seq_along(mcmc.out)) {
    
    m <- matrix(NA, nrow =  nrow(mcmc.out[[c]][[sub.sample]]), ncol = length(prior.ind.all.ns))
    
    # TODO: make this sf compatible for multi site
    if(!is.null(sf)){
      sfm <- matrix(NA, nrow =  nrow(mcmc.out[[c]][[sub.sample]]), ncol = length(sf))
    }
    
    # TODO: get back to this when scaling factor is used
    # # retrieve rownames separately to get rid of var_name* structures
    prior.all.rownames <- unlist(sapply(prior.list, rownames))
    
    sc <- 1
    for (i in seq_along(prior.ind.all.ns)) {
      sf.check <- prior.all.rownames[prior.ind.all.ns][i]
      idx <- grep(sf.check, rownames(prior.all)[prior.ind.all.ns]) ## it used to be prior.ind.all, check if this was a typo
      if(any(grepl(sf.check, sf))){
        
        m[, i] <- eval(prior.fn.all$qprior[prior.ind.all.ns][[i]],
                       list(p = mcmc.out[[c]][[sub.sample]][, idx]))
        
        
        if(sc <= length(sf)){
          sfm[, sc] <- mcmc.out[[c]][[sub.sample]][, idx]
          sc <- sc + 1
        }
        
      }else{
        
        if(is.null(ns)){
          m[, i] <- mcmc.out[[c]][[sub.sample]][, idx]
        }else{
          m[, i] <- mcmc.out[[c]][[sub.sample]][, idx, ns]
        }
        

      }
    }
    
    colnames(m) <- prior.all.rownames[prior.ind.all.ns]
    mcmc.samp.list[[c]] <- m
    
    if(!is.null(sf)){
      colnames(sfm) <- paste0(sf, "_SF")
      sf.samp.list[[c]] <- sfm
    }
    
  }
  
  # Separate each PFT's parameter samples (and bias term) to their own list
  mcmc.param.list <- list()
  ind <- 0
  for (i in seq_along(n.param.orig)) {
    mcmc.param.list[[i]] <- lapply(mcmc.samp.list, function(x) x[, (ind + 1):(ind + n.param.orig[i]), drop = FALSE])
    ind <- ind + n.param.orig[i]
  }
  
  return(mcmc.param.list)
} # pda.sort.params
