##' @name InventoryGrowthFusion
##' @title InventoryGrowthFusion
##' @description this code fuses forest inventory data with tree growth data (tree ring or dendrometer band)
##' for the same plots. Code is a rewrite of Clark et al 2007 Ecol Appl into JAGS
##' 
##' @param data  list of data inputs
##' @param random = whether or not to include random effects
##' @note Requires JAGS
##' @return an mcmc.list object
##' @export
InventoryGrowthFusion <- function(data, cov.data=NULL,time_data = NULL,n.iter=5000, random = TRUE, fixed = NULL,time_varying=NULL, burnin_plot = FALSE) {
  library(rjags)
  
  burnin.variables <- c("tau_add", "tau_dbh", "tau_inc", "mu")
  out.variables <- c("x", "tau_add", "tau_dbh", "tau_inc", "mu")
  
  TreeDataFusionMV <- "
model{

  ### Loop over all individuals
  for(i in 1:ni){
  
  #### Data Model: DBH
  for(t in 1:nt){
  z[i,t] ~ dnorm(x[i,t],tau_dbh)
  }
  
  #### Data Model: growth
  for(t in 2:nt){
  inc[i,t] <- x[i,t]-x[i,t-1]
  y[i,t] ~ dnorm(inc[i,t],tau_inc)
  }
  
  #### Process Model
  for(t in 2:nt){
  Dnew[i,t] <- x[i,t-1] + mu ##PROCESS
  x[i,t]~dnorm(Dnew[i,t],tau_add)
  }
  
#RANDOM ## individual effects
#RANDOM ind[i] ~ dnorm(0,tau_ind)  

  ## initial condition
  x[i,1] ~ dnorm(x_ic,tau_ic)
  }  ## end loop over individuals

#RANDOM ## year effects
#RANDOM for(t in 1:nt){
#RANDOM year[t] ~ dnorm(0,tau_yr)
#RANDOM }
  
  #### Priors
  tau_dbh ~ dgamma(a_dbh,r_dbh)
  tau_inc ~ dgamma(a_inc,r_inc)
  tau_add ~ dgamma(a_add,r_add)
#RANDOM tau_ind ~ dgamma(1,0.1)
#RANDOM tau_yr  ~ dgamma(1,0.1)
  mu ~ dnorm(0.5,0.5)
## FIXED EFFECTS BETAS
## TIME VARYING BETAS
 }"
  
  Pformula <- NULL
  ## RANDOM EFFECTS
  if (random) {
    TreeDataFusionMV <- gsub(pattern = "#RANDOM", " ", TreeDataFusionMV)
    Pformula <- "+ ind[i] + year[t]"
    burnin.variables <- c(burnin.variables, "tau_ind", "tau_yr")
    out.variables <- c(out.variables, "tau_ind", "tau_yr", "ind", "year")
  }

  ## Design matrix
  if (is.null(fixed)) {
    Xf <- NULL
  } else {
    if (is.null(cov.data)) {
      print("formula provided but covariate data is absent:", fixed)
    }
    ## check if there's a tilda in the formula
    if (length(grep("~", fixed)) == 0) {
      fixed <- paste("~", fixed)
    }
    ## build design matrix from formula
    Xf      <- with(cov.data, model.matrix(formula(fixed)))
    Xf.cols <- colnames(Xf)
    Xf.cols <- Xf.cols[Xf.cols != "(Intercept)"]
    Xf      <- as.matrix(Xf[, Xf.cols])
    colnames(Xf) <- Xf.cols
    ##Center the covariate data
    Xf.center <- apply(Xf, 2, mean, na.rm = TRUE)
    Xf      <- t(t(Xf) - Xf.center)
  }
  
  ## build formula in JAGS syntax
  if (!is.null(Xf)) {
    Xf.names <- gsub(" ", "_", colnames(Xf))  ## JAGS doesn't like spaces in variable names
    ## append to process model formula
    Pformula <- paste(Pformula,
                      paste0("+ beta", Xf.names, "*Xf[rep[i],", seq_along(Xf), "]", collapse = " "))
    ## create priors
    Xf.priors <- paste0("     beta", Xf.names, "~dnorm(0,0.001)", collapse = "\n")
    TreeDataFusionMV <- sub(pattern = "## FIXED EFFECTS BETAS", Xf.priors, TreeDataFusionMV)
    ## update variables for JAGS to track
    data[["Xf"]] <- Xf
    out.variables <- c(out.variables, paste0("beta", Xf.names))
  }
 
  if(FALSE){
    ## DEVEL TESTING FOR TIME VARYING
    time_varying <- "TminJuly + PrecipDec + TminJuly*PrecipDec"
    time_data <- list(TminJuly = matrix(0,4,4),PrecipDec = matrix(1,4,4))
  }
  
  ## Time-varying covariates
  if(!is.null(time_varying)){
    if (is.null(time_data)) {
      print("time_varying formula provided but time_data is absent:", time_varying)
    }
    
    ## parse equation into variable names
    t_vars <- gsub(" ","",unlist(strsplit(time_varying,"+",fixed=TRUE))) ## split on +, remove whitespace
    ## check for interaction terms
    it_vars <- grep(pattern = "*",x=t_vars,fixed = TRUE)
      ## need to deal with interactions with fixed variables
      ## will get really nasty if interactions are with catagorical variables
      ## need to create new data matrices on the fly
    
    ## loop over variables
    for(j in seq_along(t_vars)){
      tvar <- t_vars[j]
      
      ## grab from the list of data matrices
      dtmp <- time_data[[tvar]]
      
      ## insert data into JAGS inputs
      data[[length(data)+1]] <- dtmp
      names(data)[length(data)] <- tvar
    
      ## append to process model formula
      Pformula <- paste(Pformula,
                        paste0("+ beta", tvar, "*",tvar,"[i,t]"))
    
      ## add to list of varibles JAGS is tracking
      out.variables <- c(out.variables, paste0("beta", tvar))
    }
    ## build prior
    Xt.priors <- paste0("     beta", t_vars, "~dnorm(0,0.001)", collapse = "\n")
    TreeDataFusionMV <- sub(pattern = "## TIME VARYING BETAS", Xt.priors, TreeDataFusionMV)
    
  } ## END time varying covariates
   
  
  ## insert process model into JAGS template
  if (!is.null(Pformula)) {
    TreeDataFusionMV <- sub(pattern = "##PROCESS", Pformula, TreeDataFusionMV)
  }
  
  ## state variable initial condition
  z0 <- t(apply(data$y, 1, function(y) {
    -rev(cumsum(rev(y)))
  })) + data$z[, ncol(data$z)]
  
  ## JAGS initial conditions
  nchain <- 3
  init   <- list()
  for (i in seq_len(nchain)) {
    y.samp <- sample(data$y, length(data$y), replace = TRUE)
    init[[i]] <- list(x = z0, 
                      tau_add = runif(1, 1, 5) / var(diff(y.samp), na.rm = TRUE),
                      tau_dbh = 1, 
                      tau_inc = 1500,
                      tau_ind = 50, 
                      tau_yr = 100, 
                      ind = rep(0, data$ni),  
                      year = rep(0, data$nt))
  }
  
  ## compile JAGS model
  j.model <- jags.model(file = textConnection(TreeDataFusionMV), data = data, inits = init, n.chains = 3)
  ## burn-in
  jags.out <- coda.samples(model = j.model, 
                           variable.names = burnin.variables, 
                           n.iter = min(n.iter, 2000))
  if (burnin_plot) {
    plot(jags.out)
  }
  
  ## run MCMC
  coda.samples(model = j.model, variable.names = out.variables, n.iter = n.iter)
} # InventoryGrowthFusion
