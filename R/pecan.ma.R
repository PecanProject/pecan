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
  j.vars     <- c( 'beta.o','thetaSD', 'ySD', 'trtSD', 'b.trt', 'b.site')
  j.vars.ghs <- c( 'beta.o','thetaSD', 'ySD', 'trtSD', 'ghsSD', 'b.ghs', 'b.site', 'b.trt')

  ## log the mcmc chain parameters 
  cat(paste( 'Each meta-analysis will be run with: \n',
              j.iter, ' total iterations,\n',
              j.chains, ' chains, \n',
              'a burnin of ', j.adapt, ' samples,\n',
              'a thinning interval of ', j.thin,
              ', \nso the total number of samples will be ', j.chains*(j.iter-j.adapt)/j.thin,
              '\nand the parameters ',vecpaste(j.vars), ' will be sampled\n', sep = '')
              )
  
  for(trait.name in names(trait.data)) {
    prior.name <- ifelse(trait.name != 'Vcmax', trait.name, 'Vm0')
    prior <- priors[prior.name, c('distn', 'parama', 'paramb', 'n')]
    colnames(prior) <- c("distn", "a", "b", "n")

    data <- trait.data[[trait.name]]
    data <- data[order(data$site,data$trt),]#not sure why, but required for JAGS model
    if (!1 %in% data$ghs) {
      jag.model <- model1
      data <- data[,-which(names(data) == 'ghs')]
      j.vars <- j.vars
    } else {
      jag.model <-modelg
      if(0 %in% data$ghs) data$ghs <- data$ghs + 1
      j.vars <- j.vars.ghs
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
