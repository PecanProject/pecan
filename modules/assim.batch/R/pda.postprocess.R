##' Postprocessing for PDA Results
##'
##' @title Postprocessing for PDA Results
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return PEcAn settings list, updated with <params.id> pointing to the new params file.
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.postprocess <- function(settings, con, mcmc.param.list, pname, prior, prior.ind) {
  
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

  params.subset <- pda.plot.params(settings, mcmc.param.list, prior.ind, par.file.name)
  
  for (i in seq_along(settings$pfts)) {
    
    ## Save params
    filename.mcmc <- file.path(settings$pfts[[i]]$outdir, 
                                paste0("mcmc.pda.", 
                                      settings$pfts[[i]]$name, 
                                      "_", 
                                      settings$assim.batch$ensemble.id, 
                                      ".Rdata"))

    params.pft <- params.subset[[i]]
    save(params.pft, file = filename.mcmc)
    

    ## create a new Posteriors DB entry
    pft.id <- db.query(paste0("SELECT pfts.id FROM pfts, modeltypes WHERE pfts.name='",
                              settings$pfts[[i]]$name, 
                              "' and pfts.modeltype_id=modeltypes.id and modeltypes.name='", 
                              settings$model$type, "'"), 
                         con)[["id"]]


    posteriorid <-  db.query(paste0("INSERT INTO posteriors (pft_id) VALUES (",
                    pft.id, ") RETURNING id"), con)
    
    
    PEcAn.logger::logger.info(paste0("--- Posteriorid for ", settings$pfts[[i]]$name, " is ", posteriorid, " ---"))
    settings$pfts[[i]]$posteriorid <- posteriorid
    
    ## save named distributions
    ## *** TODO: Generalize for multiple PFTS
    post.distns <- PEcAn.MA::approx.posterior(trait.mcmc = params.subset[[i]], 
                                    priors = prior[[i]], 
                                    outdir = settings$pfts[[i]]$outdir, 
                                    filename.flag = paste0(".pda.", settings$pfts[[i]]$name, "_", settings$assim.batch$ensemble.id))
    filename <- file.path(settings$pfts[[i]]$outdir, 
                          paste0("post.distns.pda.", settings$pfts[[i]]$name, "_", settings$assim.batch$ensemble.id, ".Rdata"))
    save(post.distns, file = filename)
    dbfile.insert(dirname(filename), basename(filename), "Posterior", posteriorid, con)
    
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
        trait.mcmc[[pname[prior.ind[[i]][v]]]] <- mcmc.list(as.mcmc(beta.o))
      } else {
        k <- length(trait.mcmc) + 1
        trait.mcmc[[k]] <- mcmc.list(as.mcmc(beta.o))
        names(trait.mcmc)[k] <- pname[prior.ind[[i]]][v]
      }
    }
    
    ## save updated parameter distributions as trait.mcmc so that they can be read by the ensemble code
    ## *** TODO: Generalize for multiple PFTS
    filename <- file.path(settings$pfts[[i]]$outdir,
                          paste0("trait.mcmc.pda.", 
                                 settings$pfts[[i]]$name, 
                                 "_", settings$assim.batch$ensemble.id, 
                                 ".Rdata"))
    save(trait.mcmc, file = filename)
    dbfile.insert(dirname(filename), basename(filename), "Posterior", posteriorid, con)
  }  #end of loop over PFTs
  
  ## save updated settings XML
  saveXML(PEcAn.settings::listToXml(settings, "pecan"), file = file.path(settings$outdir, paste0("pecan.pda", settings$assim.batch$ensemble.id,
                                                                                 ".xml")))
  
  return(settings)
} # pda.postprocess


##' Plot PDA Parameter Diagnostics
##'
##' @title Plot PDA Parameter Diagnostics
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return Nothing. Plot is generated and saved to PDF.
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.plot.params <- function(settings, mcmc.param.list, prior.ind, par.file.name = NULL) {
  
  params.subset <- list()
  
  # flag for gelman.plot
  enough.iter <- TRUE
  
  for (i in seq_along(prior.ind)) {
    params.subset[[i]] <- as.mcmc.list(lapply(mcmc.param.list[[i]], mcmc))
    
    burnin <- getBurnin(params.subset[[i]], method = "gelman.plot")
    
    # rare, but this can happen; better to throw an error than continue, as it might lead
    # mis-interpretation of posteriors otherwise
    if (burnin == nrow(params.subset[[i]][[1]])) {
      PEcAn.logger::logger.severe(paste0("*** Burn-in is the same as the length of the chain, please run a longer chain ***"))
    }
    
    params.subset[[i]] <- window(params.subset[[i]], start = max(burnin, na.rm = TRUE))
    
    # chek number of iterations left after throwing the burnin, gelman.plot requires > 50
    if (nrow(params.subset[[i]][[1]]) < 50) {
      PEcAn.logger::logger.info(paste0("*** Not enough iterations in the chain after removing burn-in, skipping gelman.plot ***"))
      enough.iter <- FALSE
    }
    
    if(i <= length(settings$pfts)){
      pdf(file.path(settings$pfts[[i]]$outdir, 
                    paste0("mcmc.diagnostics.pda.", 
                           settings$pfts[[i]]$name, 
                           "_", settings$assim.batch$ensemble.id,
                           ".pdf")))
    } else {
      pdf(file.path(par.file.name, 
                    paste0("mcmc.diagnostics.pda.par_", 
                           settings$assim.batch$ensemble.id,
                           ".pdf")))
    }

    layout(matrix(c(1, 2, 3, 4, 5, 6), ncol = 2, byrow = TRUE))
    
    plot(params.subset[[i]], auto.layout = FALSE)
    
    dm <- do.call("rbind", params.subset[[i]])
    
    if (length(prior.ind[[i]]) > 1) {
      correlationPlot(dm)
    }
    
    if (length(params.subset[[i]]) > 1 & enough.iter) {
      coda::gelman.plot(params.subset[[i]], auto.layout = FALSE, autoburnin = FALSE)
    }
    
    layout(1)
    dev.off()
    
    # Write out convergence diagnostics to a txt file
    if(i <= length(settings$pfts)){
      filename.mcmc.temp <- file.path(settings$pfts[[i]]$outdir,
                                      paste0("mcmc.diagnostics.pda.", 
                                             settings$pfts[[i]]$name, "_", 
                                             settings$assim.batch$ensemble.id, 
                                             ".txt"))
    } else {
      filename.mcmc.temp <- file.path(par.file.name, 
                                      paste0("mcmc.diagnostics.pda.par_", 
                                             settings$assim.batch$ensemble.id,".txt"))
    }

    
    cat("Summary statistics\n", file = filename.mcmc.temp)
    capture.output(summary(params.subset[[i]]), file = filename.mcmc.temp, append = TRUE)
    cat("\n\n\n", file = filename.mcmc.temp, append = TRUE)
    
    if (length(prior.ind[[i]]) > 1) {
      cat("Covariance matrix :\n", file = filename.mcmc.temp, append = TRUE)
      capture.output(cov(dm), file = filename.mcmc.temp, append = TRUE)
      cat("\n\n\n", file = filename.mcmc.temp, append = TRUE)
    }
    
    if (length(prior.ind[[i]]) > 1) {
      cat("Correlation matrix :\n", file = filename.mcmc.temp, append = TRUE)
      capture.output(cor(dm), file = filename.mcmc.temp, append = TRUE)
      cat("\n\n\n", file = filename.mcmc.temp, append = TRUE)
    }
    
    if (length(params.subset[[i]]) > 1) {
      cat("Gelman and Rubin convergence diagnostics\n", file = filename.mcmc.temp, append = TRUE)
      capture.output(coda::gelman.diag(params.subset[[i]], autoburnin = FALSE), file = filename.mcmc.temp, 
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
##' @export
write_sf_posterior <- function(sf.samp.list, sf.prior, sf.samp.filename){
  
  sf.samp <- as.mcmc.list(lapply(sf.samp.list, mcmc))
  
  burnin <- getBurnin(sf.samp, method = "gelman.plot")
  
  sf.samp <- window(sf.samp, start = max(burnin, na.rm = TRUE))
  
  # convert mcmc.list to list of matrices
  sf.subset.list <- list()
  sf.subset.list[[1]] <- as.data.frame(do.call("rbind", sf.samp))

  filename.flag <- basename(sf.samp.filename)
  
  sf.post.distns <- PEcAn.MA::approx.posterior(trait.mcmc = sf.subset.list[[1]], priors = sf.prior,
                                               outdir = dirname(sf.filename),
                                               filename.flag = filename.flag)
  
  save(sf.subset.list, file = sf.samp.filename)
  
  return(sf.post.distns)
  
} # write_sf_posterior
