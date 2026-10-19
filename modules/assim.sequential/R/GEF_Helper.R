#' tobit_model_censored
#'
#' @param settings (list) pecan standard settings list.
#' @param X (numeric) A matrix contains ensemble forecasts (ensembles * variables).
#' @param var.names (character) variable names.
#' @param mu.f (numeric) forecast mean values.
#' @param Pf (numeric) forecast covariance matrix.
#' @param t (numeric) timestep. If t=1, initial values are imputed for zero values in mu.f
#'
#' @return list with updated mu.f, pf, X, and indication of which y values are censored
#' @export
#'
#' @examples
tobit_model_censored <- function(settings, X, var.names, mu.f, Pf, t) {
  intervalX <- matrix(NA, ncol(X), 2)
  rownames(intervalX) <- colnames(X)
  outdir     <- settings$modeloutdir
  #TO DO: Not working for fcomp
  for (i in 1:length(var.names)) {
    intervalX[which(startsWith(rownames(intervalX),
                               var.names[i])), ] <-
      matrix(c(
        as.numeric(
          settings$state.data.assimilation$state.variables[[i]]$min_value
        ),
        as.numeric(
          settings$state.data.assimilation$state.variables[[i]]$max_value
        )
      ),
      length(which(startsWith(
        rownames(intervalX),
        var.names[i]
      ))), 2, byrow = TRUE)
    
  }
  #### These vectors are used to categorize data based on censoring from the interval matrix
  x.ind <-
    x.censored <- matrix(NA, ncol = ncol(X), nrow = nrow(X))
  for (j in seq_along(mu.f)) {
    for (n in seq_len(nrow(X))) {
      x.ind[n, j] <- as.numeric(X[n, j] > 0)
      x.censored[n, j] <-
        as.numeric(ifelse(X[n, j] > intervalX[j, 2], 0, X[n, j])) #
    }
  }
  
  if (t == 1) {
    #The purpose of this step is to impute data for mu.f
    #where there are zero values so that
    #mu.f is in 'tobit space' in the full model
    constants.tobit2space = list(N = nrow(X),
                                 J = length(mu.f))
    
    data.tobit2space = list(
      y.ind = x.ind,
      y.censored = x.censored,
      mu_0 = rep(0, length(mu.f)),
      lambda_0 = diag(10, length(mu.f)),
      nu_0 = 3
    )#some measure of prior obs
    
    inits.tobit2space <<-
      list(pf = Pf, muf = colMeans(X)) #pf = cov(X)
    #set.seed(0)
    #ptm <- proc.time()
    tobit2space_pred <<-
      nimbleModel(
        tobit2space.model,
        data = data.tobit2space,
        constants = constants.tobit2space,
        inits = inits.tobit2space,
        name = 'space'
      )
    ## Adding X.mod,q,r as data for building model.
    conf_tobit2space <<-
      configureMCMC(tobit2space_pred, thin = 10, print = TRUE)
    conf_tobit2space$addMonitors(c("pf", "muf", "y.censored"))
    ## important!
    ## this is needed for correct indexing later
    samplerNumberOffset_tobit2space <<-
      length(conf_tobit2space$getSamplers())
    
    for (j in seq_along(mu.f)) {
      for (n in seq_len(nrow(X))) {
        node <- paste0('y.censored[', n, ',', j, ']')
        conf_tobit2space$addSampler(node, 'toggle', control = list(type =
                                                                     'RW'))
      }
    }
    
    #conf_tobit2space$printSamplers()
    
    Rmcmc_tobit2space <<- buildMCMC(conf_tobit2space)
    
    Cmodel_tobit2space <<- compileNimble(tobit2space_pred)
    Cmcmc_tobit2space <<-
      compileNimble(Rmcmc_tobit2space, project = tobit2space_pred)
    
    for (i in seq_along(X)) {
      ## ironically, here we have to "toggle" the value of y.ind[i]
      ## this specifies that when y.ind[i] = 1,
      ## indicator variable is set to 0, which specifies *not* to sample
      valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space +
                                                                          i]],
                                    'toggle',
                                    1 - x.ind[i])
    }
    
  } else{
    Cmodel_tobit2space$y.ind <- x.ind
    Cmodel_tobit2space$y.censored <- x.censored
    
    inits.tobit2space = list(pf = Pf, muf = colMeans(X))
    Cmodel_tobit2space$setInits(inits.tobit2space)
    
    for (i in seq_along(X)) {
      valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space +
                                                                          i]],
                                    'toggle',
                                    1 - x.ind[i])
    }
    
  }
  
  dat.tobit2space <-
    runMCMC(Cmcmc_tobit2space,
            niter = 50000,
            progressBar = TRUE)
  
  ## update parameters
  mu.f <-
    colMeans(dat.tobit2space[, grep("muf", colnames(dat.tobit2space))])
  Pf <-
    matrix(colMeans(dat.tobit2space[, grep("pf", colnames(dat.tobit2space))]), ncol(X), ncol(X))
  
  
  
  iycens <- grep("y.censored", colnames(dat.tobit2space))
  X.new <-
    matrix(colMeans(dat.tobit2space[, iycens]), nrow(X), ncol(X))
  
  return(list(mu.f = mu.f,
              Pf=Pf,
              iycens=iycens,
              X.new=X.new
  ))
  
}