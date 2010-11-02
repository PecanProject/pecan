pecan.ma <- function(trait.data, priors, j.iter){
  ## Meta-analysis for each trait
  mcmc.object <- list() #  initialize output list of mcmc objects for each trait
  mcmc.mat    <- list()
  
  ## Set inputs for jags.model()
  j.chains <- 4
  j.adapt  <- 500
  j.thin   <- 100    # thinning interval for mcmc monitors
       
  ## set variables to follow in mcmc, defined in model (below
  ##       global mean: beta.o
  ##         global SD: thetaSD
  ##   within study SD: ySD 
  vars.noghs     <- c( 'beta.o','thetaSD', 'ySD', 'trtSD')
  vars.ghs <- c( 'beta.o','thetaSD', 'ySD', 'trtSD', 'ghsSD', 'b.ghs[2]')

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
    writeLines(paste('prior:', prior['distn'], '(',prior['parama'], ', ', prior['paramb'], ')', sep = ''))
    writeLines(paste('data max:', max(data$Y), '\ndata min:', min(data$Y), '\nmean:', signif(mean(data$Y),3), '\nn:', length(data$Y)))
    writeLines('stem plot of data points')
    writeLines(paste(stem(data$Y)))
    writeLines('stem plot of SD:')
    writeLines(paste(stem(1/(data$n*data$obs.prec^2))))
    writeLines(paste(data))

    if (!1 %in% data$ghs) {
      jag.model <- model1
      data <- data[,-which(names(data) == 'ghs')]
      j.vars <- vars.noghs
    } else {
      jag.model <-modelg
      if(0 %in% data$ghs) data$ghs <- data$ghs + 1
      j.vars <- vars.ghs
    }
    nsite <- length(unique(data$site))
    m <- min(nsite, 5)
    for ( i in 1:m){
      j.vars <- c(j.vars, paste('b.site[',i,']',sep=''))
    }
    ntrt <- length(unique(data$trt))
    m <- max(ntrt, 5)
    if (ntrt > 1) {
      m <- min(ntrt, 5)
      for ( i in 2:m){
        j.vars <- c(j.vars, paste('b.trt[',i,']',sep=''))
      }
    }
  
    jag.model.file <-  paste( trait.name, ".model.bug",sep="")  # file to store model
    write.ma.model ( jag.model, jag.model.file,
                    prior$distn, prior$a, prior$b,
                    length ( data$Y ),
                    length(unique(data$trt)),
                    length(unique(data$site)))

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
