pecan.ma <- function(trait.data, trvec, prvec, pft, n.iter){
  ## Meta-analysis for each trait
  mcmc.object <- list() #  initialize output list of mcmc objects for each trait
  mcmc.mat    <- list()
  
  ## Set inputs for jags.model()
  j.chains <- 4
  j.adapt  <- 500
  j.thin   <- 25    # thinning interval for mcmc monitors
       
  ## set variables to follow in mcmc, defined in model (below
  ##       global mean: beta.o
  ##         global SD: thetaSD
  ##   within study SD: ySD 
  j.vars   <- c( 'beta.o','thetaSD', 'ySD')

  ## log the mcmc chain parameters 
  print(paste( 'Each meta-analysis will be run with ',
              n.iter, ' total iterations, ',
              j.chains, ' chains, ',
              'a burnin of ', j.adapt, ' samples, ',
              'and a thinning interval of ', j.thin,
              ', so the total number of samples will be ', j.chains*(n.iter-j.adapt)/j.thin,
              ' and the parameters ',vecpaste(j.vars), ' will be sampled', sep = '')
              )
  
  ## DB Connection to BETYdb
  con <- query.bety.con()

  for (i.tr in seq(trvec)) {
    tr.name <- trvec[i.tr]
    pr.name <- prvec[i.tr]
    
    
}
