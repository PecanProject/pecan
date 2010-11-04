pecan.ma <- function(trait.data, priors, j.iter){
  ## Meta-analysis for each trait
  mcmc.object <- list() #  initialize output list of mcmc objects for each trait
  mcmc.mat    <- list()
  
  ## Set inputs for jags.model()
  j.chains <- 4
  j.adapt  <- 500
  j.thin   <- 100    # thinning interval for mcmc monitors


  ## log the mcmc chain parameters 
  cat(paste( 'Each meta-analysis will be run with: \n',
            j.iter, ' total iterations,\n',
            j.chains, ' chains, \n',
            'a burnin of ', j.adapt, ' samples,\n',
            'a thinning interval of ', j.thin,
            ', \nso the total number of samples will be ', j.chains*(j.iter-j.adapt)/j.thin, sep = '')
      )
  
  for(trait.name in names(trait.data)) {
    prior.name <- ifelse(trait.name != 'Vcmax', trait.name, 'Vm0')
    prior <- priors[prior.name, c('distn', 'parama', 'paramb', 'n')]
    colnames(prior) <- c("distn", "a", "b", "n")
    writeLines(paste('starting meta-analysis for', trait.name))

    data <- trait.data[[trait.name]]
    data <- data[order(data$site,data$trt),]#not sure why, but required for JAGS model
    print(paste('prior:', prior[1], '(',prior[2], ', ', prior[3], ')', sep = ''))
    writeLines(paste('data max:', max(data$Y), '\ndata min:', min(data$Y), '\nmean:', signif(mean(data$Y),3), '\nn:', length(data$Y)))
    writeLines('stem plot of data points')
    writeLines(paste(stem(data$Y)))
    print(trait.name)
    print(data$obs.prec)
    if(FALSE %in% is.na(data$obs.prec)){
      writeLines('stem plot of SD:')
      writeLines(paste(stem(1/(data$n*data$obs.prec^2))))
    } else {
      writeLines(paste('no estimates of SD for', trait.name))
    }
    
    print(data)

    #determine what factors to include in meta-analysis
    model.parms <- list(mean = length(data$Y),
                        ghs  = length(unique(data$ghs)),
                        site = length(unique(data$site)),
                        trt  = length(unique(data$trt)))

    ## parameters for jags to follow
    vars <- c( 'beta.o', 'sd.y') 
    
    for (x in c('ghs','site', 'trt')) {
      if(model.parms[[x]] == 1) {
        data <- data[, -which(names(data) == x)]
      } else {
        data <- data
        vars <- c(vars, paste('sd.', x, sep = ''))
        m <- min(model.parms[[x]], 5)
        for (i in 1:m) {
          if(!i == 1 && x == 'trt') {
            vars <- c(vars, paste('b.', x, '[', i, ']', sep=''))
          }
        }
      }
    }

    
    jag.model.file <-  paste( trait.name, ".model.bug",sep="")  # file to store model
    write.ma.model (ma.model, jag.model.file,
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
                                               variable.names = j.vars,
                                               n.iter = j.iter,
                                               thin = j.thin)
    
  }
  return(mcmc.object)
}
