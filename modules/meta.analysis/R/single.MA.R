#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##' Individual Meta-analysis
##'
##'  Individual meta-analysis for a specific trait and PFT is run by the function
##' single.MA. This will allow power analysis  to run repeated MA outside of the
##' full loop over traits and PFTs. 
##' @title Single MA
##' @param data 
##' @param j.chains number of chains in meta-analysis
##' @param j.iter  number of mcmc samples
##' @param tauA prior on variance parameters
##' @param tauB  prior on variance parameters
##' @param prior 
##' @param jag.model.file file to which model will be written 
##' @param overdispersed 
##' @export
##' @return jags.out, an mcmc.object with results of meta-analysis
##' @author David LeBauer, Michael C. Dietze
single.MA <- function(data, j.chains, j.iter, tauA, tauB, prior,
                      jag.model.file, overdispersed=TRUE){
  ## Convert R distributions to JAGS distributions
  jagsprior <- r2bugs.distributions(prior)
  jagsprior <- jagsprior[, c('distn', 'parama', 'paramb', 'n')]
  colnames(jagsprior) <- c("distn", "a", "b", "n")
  colnames(prior)     <- c("distn", "a", "b", "n")

    
    # determine what factors to include in meta-analysis
    model.parms <- list(ghs  = length(unique(data$ghs)),
                        site = length(unique(data$site)),
                        trt  = length(unique(data$trt)))
    # define regression model
    reg.parms   <- list(ghs  = 'beta.ghs[ghs[k]]', #beta.o will be included by default
                        site = 'beta.site[site[k]]',
                        trt  = 'beta.trt[trt[k]]')
    if(sum(model.parms>1)==0) {
      reg.model <- ''
    } else {
      reg.model <- paste('+', reg.parms[model.parms > 1], collapse = " ")
    }
    
    ## generate list of parameters for jags to follow and produce mcmc output for
    vars <- c( 'beta.o', 'sd.y') 
    for (x in c('ghs', 'site', 'trt')) {
      if(model.parms[[x]] == 1) {
        data <- data[, which(names(data) != x)]
      } else {
        data <- data
        if(x!='ghs') {
          vars <- c(vars, paste('sd.', x, sep = ''))
        }
        # m <- min(model.parms[[x]], 5)
        m <- model.parms[[x]]
        for (i in 1:m) {
          if(i == 1 && x == 'site') {
            vars <- c(vars, 'beta.site[1]')
          }
          if (i > 1) {
            vars <- c(vars, paste('beta.', x, '[', i, ']', sep=''))
          }
        }
      }
    }

    
    ### Import defaul JAGS model file
    modelfile = system.file("ma.model.template.bug", package="PEcAn.MA")
    
    ### Write JAGS bug file based on user settings and default bug file
    #write.ma.model (modelfile = paste(settings$pecanDir,'rscripts/ma.model.template.bug',sep=""),
    write.ma.model (modelfile = modelfile,
                    outfile = jag.model.file,
                    reg.model = reg.model,
                    jagsprior$distn, jagsprior$a, jagsprior$b,
                    n     = length ( data$Y ),
                    trt.n = model.parms[['trt']],
                    site.n= model.parms[['site']],
                    ghs.n = model.parms[['ghs']],
                    tauA  = tauA,
                    tauB  = tauB)

    if(overdispersed == TRUE){
      ## overdispersed chains
      j.inits <- function(chain) list("beta.o" = do.call(paste('q',prior$dist,sep=''),
                                        list(chain * 1/(j.chains + 1), prior$a, prior$b)),
                                      .RNG.seed = chain,
                                      .RNG.name = "base::Mersenne-Twister")
    } else if (overdispersed == FALSE) {
      ## chains fixed at data mean - used if above code does not converge,
      ## invalidates assumptions about convergence, e.g. Gelman-Rubin diagnostic
      j.inits <- function(chain) list("beta.o" = mean(data$Y))
    }
    
    j.model   <- jags.model (file = jag.model.file,
                             data = data,
#                             n.adapt = 100, #will burn in below
                             inits = j.inits,
                             n.chains = j.chains)


    jags.out   <- coda.samples ( model = j.model,
                                variable.names = vars,
                                n.iter = j.iter,
                                thin = max(c(2,j.iter/(5000*2))))

    return(jags.out)
  }
