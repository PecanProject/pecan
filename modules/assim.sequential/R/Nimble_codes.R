##' @title load_nimble
##' @name  load_nimble
##' @author Ann Raiho, Hamze Dokoohaki
##' 
##' @description This functions is internally used to register a series of nimble functions inside GEF analysis function.
##' 
#' @import nimble
load_nimble <- function(){
    #y_star_create--------------------------------------------------------------------
  y_star_create <<-  nimbleFunction(
    run = function(X = double(1)) {
      returnType(double(1))
      
      y_star <- X
      
      return(y_star)
    })
  
  y_star_create_Fcomp <<-  nimbleFunction(
    run = function(X = double(1)) {
      returnType(double(1))
      
      X_use <- X
      X_use[X_use<0] <- 0
      y_star <- X_use/sum(X_use)
      
      return(y_star)
    })
  #tobit2space.model------------------------------------------------------------------
  tobit2space.model <<- nimbleCode({
    for(i in 1:N){
      y.censored[i,1:J] ~ dmnorm(muf[1:J], cov = pf[1:J,1:J])
      for(j in 1:J){
        y.ind[i,j] ~ dinterval(y.censored[i,j], 0)
      }
    }
    
    muf[1:J] ~ dmnorm(mean = mu_0[1:J], cov = pf[1:J,1:J])
    
    Sigma[1:J,1:J] <- lambda_0[1:J,1:J]/nu_0
    pf[1:J,1:J] ~ dinvwish(S = Sigma[1:J,1:J], df = J)
    
  })
  
  #tobit.model--This does the GEF ----------------------------------------------------
  tobit.model <<- nimbleCode({ 

    q[1:N,1:N]  ~ dwish(R = aq[1:N,1:N], df = bq) ## aq and bq are estimated over time
    Q[1:N,1:N] <- inverse(q[1:N,1:N])
    X.mod[1:N] ~ dmnorm(muf[1:N], prec = pf[1:N,1:N]) ## Model Forecast ##muf and pf are assigned from ensembles
    
    ## add process error
    X[1:N]  ~ dmnorm(X.mod[1:N], prec = q[1:N,1:N])
    
    #observation operator
    y_star[1:YN] <- y_star_create(X[1:YN])
    
    ## Analysis
    y.censored[1:YN] ~ dmnorm(y_star[1:YN], prec = r[1:YN,1:YN]) 
    
    #don't flag y.censored as data, y.censored in inits
    #remove y.censored samplers and only assign univariate samplers on NAs
    
    for(i in 1:YN){
      y.ind[i] ~ dinterval(y.censored[i], 0)
    }
    
  })
  
  #tobit.model--This does the GEF for multi Site -------------------------------------
  GEF.MultiSite.Nimble <<- nimbleCode({ 
    # Sorting out qs
    q[1:YN,1:YN]  ~ dwish(R = aq[1:YN,1:YN], df = bq) ## aq and bq are estimated over time
    
    Q[1:YN,1:YN] <- inverse(q[1:YN,1:YN])
    
    # X model  
    X.mod[1:N] ~ dmnorm(mean = muf[1:N], cov = pf[1:N, 1:N])
    
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
    for (j in 1:nNotH) {
      tmpXmod[j]  <- X.mod[NotH[j]]
      Xall[NotH[j]] <- tmpXmod[j]
    }
    #These are the one that they have data and their q can be estimated.
    for (i in 1:nH) {
      tmpXH[i]  <- X[i]
      Xall[H[i]] <- tmpXH[i]
    }
    
    for (i in 1:YN) {
      y.ind[i] ~ dinterval(y.censored[i], 0)
    }
    
  })
  
  #sampler_toggle------------------------------------------------------------------------------------------------
  sampler_toggle <<- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
      type <- control$type
      nested_sampler_name <- paste0('sampler_', type)
      control_new <- nimbleOptions('MCMCcontrolDefaultList')
      control_new[[names(control)]] <- control
      nested_sampler_list <- nimbleFunctionList(sampler_BASE)
      nested_sampler_list[[1]] <- do.call(nested_sampler_name, list(model, mvSaved, target, control_new))
      toggle <- 1
    },
    run = function() {
      if(toggle == 1)
        nested_sampler_list[[1]]$run()
    },
    methods = list(
      reset = function()
        nested_sampler_list[[1]]$reset()
    )
  )
}
