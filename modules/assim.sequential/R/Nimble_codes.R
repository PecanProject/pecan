##' @title load_nimble
##' @name  load_nimble
##' @author Ann Raiho, Hamze Dokoohaki
##'
##' @description This functions is internally used to register a series of nimble functions inside GEF analysis function.
##'
#' @import nimble
#' @param X state var
#'
#' @export
y_star_create <-  nimbleFunction(
  run = function(X = double(1)) {
    returnType(double(1))
    
    y_star <- X
    
    return(y_star)
  }
)

#' Additive Log Ratio transform
#' @param y state var
#' @export
alr <-  nimbleFunction(
  run = function(y = double(1)) {
    returnType(double(1))
    
    y[y < .00001] <- .000001
    
    y_out <- log(y[1:(length(y) - 1)] / y[length(y)])
    
    return(y_out)
  }
)

#' inverse of ALR transform
#' @param alr state var
#' @export
inv.alr <-  nimbleFunction(
  run = function(alr = double(1)) {
    returnType(double(1))
    
    y = exp(c(alr, 0)) / sum(exp(c(alr, 0)))
    
    return(y)
  }
)

#' random weighted multivariate normal
#' @param n sample size
#' @param mean mean
#' @param prec precision
#' @param wt weight
#' @export
rwtmnorm <- nimbleFunction(
  run = function(n = integer(0),
                 mean = double(1),
                 prec = double(2),
                 wt = double(0)) {
    returnType(double(1))
    if (n != 1)
      nimPrint("rwtmnorm only allows n = 1; using n = 1.")
    Prob <-
      rmnorm_chol(n = 1, mean, chol(prec), prec_param = TRUE) * wt
    return(Prob)
  }
)

#' weighted multivariate normal density
#' @param x random variable
#' @param mean mean
#' @param prec precision
#' @param wt weight
#' @param log log
#' @export
dwtmnorm <- nimbleFunction(
  run = function(x = double(1),
                 mean = double(1),
                 prec = double(2),
                 wt = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))
    
    logProb <-
      dmnorm_chol(
        x = x,
        mean = mean,
        cholesky = chol(prec),
        prec_param = TRUE,
        log = TRUE
      ) * wt
    
    if (log) {
      return((logProb))
    } else {
      return((exp(logProb)))
    }
  }
)

registerDistributions(list(dwtmnorm = list(
  BUGSdist = "dwtmnorm(mean, prec, wt)",
  types = c(
    'value = double(1)',
    'mean = double(1)',
    'prec = double(2)',
    'wt = double(0)'
  )
)))

#tobit2space.model------------------------------------------------------------------------------------------------
#' Fit tobit prior to ensemble members
#' @format TBD
#' @export
tobit2space.model <- nimbleCode({
  for (i in 1:N) {
    y.censored[i, 1:J] ~ dwtmnorm(mean = muf[1:J],
                                  prec = pf[1:J, 1:J],
                                  wt = wts[i]) #
    for (j in 1:J) {
      y.ind[i, j] ~ dinterval(y.censored[i, j], 0)
    }
  }
  
  muf[1:J] ~ dmnorm(mean = mu_0[1:J], prec = Sigma_0[1:J, 1:J])
  pf[1:J, 1:J] ~ dwish(S = lambda_0[1:J, 1:J], df = nu_0)
  
})

#tobit.model--This does the GEF ----------------------------------------------------
#' TWEnF
#' @export
#' @format TBD
tobit.model <-  nimbleCode({
  q[1:N, 1:N]  ~ dwish(R = aq[1:N, 1:N], df = bq) ## aq and bq are estimated over time
  Q[1:N, 1:N] <- inverse(q[1:N, 1:N])
  X.mod[1:N] ~ dmnorm(muf[1:N], prec = pf[1:N, 1:N]) ## Model Forecast ##muf and pf are assigned from ensembles
  
  ## add process error
  X[1:N]  ~ dmnorm(X.mod[1:N], prec = q[1:N, 1:N])
  
  #observation operator
  
  if (direct_TRUE) {
    y_star[X_direct_start:X_direct_end] <-
      y_star_create(X[X_direct_start:X_direct_end])
  } else{
    
  }
  
  if (fcomp_TRUE) {
    y_star[X_fcomp_start:X_fcomp_end] <-
      alr(X[X_fcomp_model_start:X_fcomp_model_end])
  }  else{
    
  }
  
  if (pft2total_TRUE) {
    y_star[X_pft2total_start] <-
      sum(X[X_pft2total_model_start:X_pft2total_model_end])
  }  else{
    
  }
  
  #likelihood
  y.censored[1:YN] ~ dmnorm(y_star[1:YN], prec = r[1:YN, 1:YN])
  for (i in 1:YN) {
    y.ind[i] ~ dinterval(y.censored[i], 0)
  }
  
  
})

#tobit.model--This does the GEF for multi Site -------------------------------------
#' multisite TWEnF
#' @format TBD
#' @export
GEF.MultiSite.Nimble <-  nimbleCode({
  # X model
  X.mod[1:N] ~ dmnorm(mean = muf[1:N], cov = pf[1:N, 1:N])
  if (q.type == 1 | q.type == 2) {
    if (q.type == 1) {#single Q
      # Sorting out qs
      qq ~ dgamma(aq, bq) ## aq and bq are estimated over time
      q[1:YN, 1:YN] <- qq * diag(YN)
    } else if (q.type == 2) {#site Q
      # Sorting out qs
      q[1:YN, 1:YN] ~ dwish(R = aq[1:YN, 1:YN], df = bq) ## aq and bq are estimated over time
    }

    for (i in 1:nH) {
      tmpX[i]  <- X.mod[H[i]]
      Xs[i] <- tmpX[i]
    }
    ## add process error to x model but just for the state variables that we have data and H knows who
    X[1:YN]  ~ dmnorm(Xs[1:YN], prec = q[1:YN, 1:YN])

    ## Likelihood
    y.censored[1:YN] ~ dmnorm(X[1:YN], prec = r[1:YN, 1:YN])
    
    # #puting the ones that they don't have q in Xall - They come from X.model
    # # If I don't have data on then then their q is not identifiable, so we use the same Xs as Xmodel
    if(nNotH > 0){
      for (j in 1:nNotH) {
        tmpXmod[j]  <- X.mod[NotH[j]]
        Xall[NotH[j]] <- tmpXmod[j]
      }
    }
  } else if (q.type == 3) {#Vector Q
    for (i in 1:YN) {
      #sample Q.
      q[i] ~ dgamma(shape = aq[i], rate = bq[i])
      if (length(H) == 1) {
        X[i]  ~ dnorm(X.mod[H], sd = 1/sqrt(q[i]))
        #likelihood
        y.censored[i] ~ dnorm(X[i], sd = 1/sqrt(r[i]))
      } else {
        #sample latent variable X.
        X[i]  ~ dnorm(X.mod[H[i]], sd = 1/sqrt(q[i]))
        #likelihood
        y.censored[i] ~ dnorm(X[i], sd = 1/sqrt(r[i, i]))
      }
    }
  } else if (q.type == 4) {#Wishart Q
    #if it's a Wishart Q.
    #sample Q.
    q[1:YN, 1:YN] ~ dwishart(R = aq[1:YN, 1:YN], df = bq)
    #sample latent variable X.
    for (i in 1:YN) {
      Xs[i] <- X.mod[H[i]]
    }
    X[1:YN] ~ dmnorm(Xs[1:YN], prec = q[1:YN, 1:YN])
    #likelihood
    y.censored[1:YN] ~ dmnorm(X[1:YN], prec = r[1:YN, 1:YN])
  }
})

#sampler_toggle------------------------------------------------------------------------------------------------
#' sampler toggling
#' @export
#' @param model model
#' @param mvSaved copied to
#' @param target thing being targetted
#' @param control unused
sampler_toggle <- nimbleFunction(
  contains = sampler_BASE,
  setup = function(model, mvSaved, target, control) {
    type <- control$type
    nested_sampler_name <- paste0('sampler_', type)
    control_new <- nimbleOptions('MCMCcontrolDefaultList')
    control_new[[names(control)]] <- control
    nested_sampler_list <- nimbleFunctionList(sampler_BASE)
    nested_sampler_list[[1]] <-
      do.call(nested_sampler_name,
              list(model, mvSaved, target, control_new))
    toggle <- 1
  },
  run = function() {
    if (toggle == 1)
      nested_sampler_list[[1]]$run()
  },
  methods = list(
    reset = function()
      nested_sampler_list[[1]]$reset()
  )
)

#' Weighted conjugate wishart
#' @param model model
#' @param mvSaved copied to
#' @param target thing being targetted
#' @param control unused
#' @export
conj_wt_wishart_sampler <-  nimbleFunction(
  contains = sampler_BASE,
  setup = function(model, mvSaved, target, control) {
    targetAsScalar <-
      model$expandNodeNames(target, returnScalarComponents = TRUE)
    d <- sqrt(length(targetAsScalar))
    
    dep_dmnorm_nodeNames <-
      model$getDependencies(target, self = F, includeData = T)
    N_dep_dmnorm <- length(dep_dmnorm_nodeNames)
    
    dep_dmnorm_nodeSize <- d #ragged problem refered to on github?
    
    calcNodes <- model$getDependencies(target)
    
    dep_dmnorm_values <-
      array(0, c(N_dep_dmnorm, dep_dmnorm_nodeSize))
    dep_dmnorm_mean <-
      array(0, c(N_dep_dmnorm, dep_dmnorm_nodeSize))
    dep_dmnorm_wt <- numeric(N_dep_dmnorm)
    
  },
  run = function() {
    #Find Prior Values
    prior_R <- model$getParam(target[1], "R")
    prior_df <- model$getParam(target[1], "df")
    
    #Loop over multivariate normal
    for (iDep in 1:N_dep_dmnorm) {
      dep_dmnorm_values[iDep, 1:dep_dmnorm_nodeSize] <<-
        model$getParam(dep_dmnorm_nodeNames[iDep],
                       "value")
      dep_dmnorm_mean[iDep, 1:dep_dmnorm_nodeSize] <<-
        model$getParam(dep_dmnorm_nodeNames[iDep],
                       "mean")
      dep_dmnorm_wt[iDep] <<-
        model$getParam(dep_dmnorm_nodeNames[iDep],
                       "wt")
      
    }
    
    #Calculate contribution parameters for wishart based on multivariate normal
    contribution_R <<- nimArray(0, dim = nimC(d, d))
    contribution_df <<- 0
    for (iDep in 1:N_dep_dmnorm) {
      tmp_diff <<-
        sqrt(dep_dmnorm_wt[iDep]) * asRow(dep_dmnorm_values[iDep, 1:d] - dep_dmnorm_mean[iDep, 1:d])
      
      contribution_R <<- contribution_R + t(tmp_diff) %*% tmp_diff
      
      contribution_df <<- contribution_df + dep_dmnorm_wt[iDep]
    }
    
    #Draw a new value based on prior and contribution parameters
    newValue <-
      rwish_chol(
        1,
        cholesky = chol(prior_R + contribution_R),
        df = prior_df + contribution_df,
        scale_param = 0
      )
    model[[target]] <<- newValue
    
    #Calculate probability
    calculate(model, calcNodes)
    nimCopy(
      from = model,
      to = mvSaved,
      row = 1,
      nodes = calcNodes,
      logProb = TRUE
    )
  },
  methods = list(
    reset = function () {
    }
  )
)

GEF_singleobs_nimble <-  nimbleCode({
  
  # Sorting out qs
  qq ~ dgamma(aq, bq) ## aq and bq are estimated over time
  q[1, 1] <- qq * diag(YN)
  
  # # X model
  X.mod[1:N] ~ dmnorm(mean = muf[1:N], cov = pf[1:N, 1:N])
  # # got rid of for loop no need when nH = 1
  Xs[1] <- X.mod[H]
  
  ## add process error to x model but just for the state variables that we have data and H knows who
  #changed model from dmnorm to dnorm to accomodate when only assimilating 1 obs
  X[1]  ~ dnorm(Xs[1], q[1, 1])
  
  ## Likelihood
  #changed model from dmnorm to dnorm to accomodate when only assimilating 1 obs
  y.censored[1] ~ dnorm(X[1], r[1, 1])
  
  #puting the ones that they don't have q in Xall - They come from X.model
  # If I don't have data on then then their q is not identifiable, so we use the same Xs as Xmodel
  for (j in 1:nNotH) {
    tmpXmod[j]  <- X.mod[NotH[j]]
    Xall[NotH[j]] <- tmpXmod[j]
  }
  # # # #These are the one that they have data and their q can be estimated
  #got rid of for loop no need when nH = 1
  Xall[H]  <- X[1]
  
  y.ind[1] ~ dinterval(y.censored[1], 0)
  
  
})