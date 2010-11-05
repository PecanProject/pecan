pecan.ma <- function(trait.data, priors, j.iter){
  ## Meta-analysis for each trait
  mcmc.object <- list() #  initialize output list of mcmc objects for each trait
  mcmc.mat    <- list()
  
  ## Set inputs for jags.model()
  j.chains <- 4
  j.adapt  <- 500
  j.thin   <- 100    # thinning interval for mcmc monitors


  ## log the mcmc chain parameters
  sink(file = 'out/meta-analysis.log', split = TRUE)
  cat(paste( 'Each meta-analysis will be run with: \n',
            j.iter, ' total iterations,\n',
            j.chains, ' chains, \n',
            'a burnin of ', j.adapt, ' samples,\n',
            'a thinning interval of ', j.thin,
            ', \nthus the total number of samples will be ', j.chains*(j.iter-j.adapt)/j.thin,'\n', sep = '')
      )
  
  for(trait.name in names(trait.data)) {
    prior.name <- ifelse(trait.name != 'Vcmax', trait.name, 'Vm0')
    prior <- priors[prior.name, c('distn', 'parama', 'paramb', 'n')]
    colnames(prior) <- c("distn", "a", "b", "n")
    writeLines(paste('starting meta-analysis for', trait.name))

    data <- trait.data[[trait.name]]
    data <- data[order(data$site,data$trt),]#not sure why, but required for JAGS model

    #print out some data summaries to check
    print(paste('prior for ', trait.name, ':',
                prior[1], '(',prior[2], ', ', prior[3], ')', sep = ''))
    writeLines(paste('data max:', max(data$Y), '\ndata min:', min(data$Y), '\nmean:', signif(mean(data$Y),3), '\nn:', length(data$Y)))
    writeLines('stem plot of data points')
    writeLines(paste(stem(data$Y)))
    if(FALSE %in% is.na(data$obs.prec)){
      writeLines('stem plot of SD:')
      writeLines(paste(stem(1/(data$n*data$obs.prec^2))))
    } else {
      writeLines(paste('no estimates of SD for', trait.name))
    }
    print(data)
    #todo? could add internal check to make sure data contains Y, n, trt, site, trt, obs.prec

    # determine what factors to include in meta-analysis
    model.parms <- list(ghs  = length(unique(data$ghs)),
                        site = length(unique(data$site)),
                        trt  = length(unique(data$trt)))
    # define regression model
    reg.parms   <- list(ghs  = 'beta.ghs[ghs[k]]', #beta.o will be included by default
                        site = 'beta.site[site[k]]',
                        trt  = 'beta.trt[trt[k]]')
    reg.model <- paste('+', reg.parms[model.parms > 1], collapse = " ")
    if (model.parms[['ghs']] >1) data$ghs = data$ghs + 1 #avoid index beta.ghs[0]
    
    ## parameters for jags to follow
    vars <- c( 'beta.o', 'sd.y') 
    
    for (x in c('ghs', 'site', 'trt')) {
      if(model.parms[[x]] == 1) {
        data <- data[, -which(names(data) == x)]
      } else {
        data <- data
        vars <- c(vars, paste('sd.', x, sep = ''))
        m <- min(model.parms[[x]], 5)
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

    
    jag.model.file <-  paste( trait.name, ".model.bug",sep="")  # file to store model
    write.ma.model (modelfile = 'rscripts/ma.model.template.bug',
                    outfile = jag.model.file,
                    reg.model = reg.model,
                    prior$distn, prior$a, prior$b,
                    length ( data$Y ),
                    model.parms[['trt']],
                    model.parms[['site']],
                    model.parms[['ghs']])

    j.model    <- jags.model ( file = jag.model.file,
                              data = data,
                              n.adapt = j.adapt,
                              n.chains = j.chains)
    
    mcmc.object[[prior.name]] <- coda.samples ( model = j.model,
                                               variable.names = vars,
                                               n.iter = j.iter,
                                               thin = j.thin)
    
  }
  sink()
  return(mcmc.object)
}
