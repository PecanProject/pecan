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
InventoryGrowthFusion <- function(data, cov.data=NULL,time_data = NULL,n.iter=5000, random = NULL, fixed = NULL,time_varying=NULL, burnin_plot = FALSE,save.jags="IGF.txt") {
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
  
  ## initial condition
  x[i,1] ~ dnorm(x_ic,tau_ic)
  }  ## end loop over individuals

## RANDOM_EFFECTS
  
  #### Priors
  tau_dbh ~ dgamma(a_dbh,r_dbh)
  tau_inc ~ dgamma(a_inc,r_inc)
  tau_add ~ dgamma(a_add,r_add)
  mu ~ dnorm(0.5,0.5)
## FIXED EFFECTS BETAS
## ENDOGENOUS BETAS
## TIME VARYING BETAS
## RANDOM EFFECT TAUS
 }"
  
  Pformula <- NULL
  ## RANDOM EFFECTS
  if (!is.null(random)) {
    Rpriors <- NULL
    Reffects <- NULL
    ## parse random effects
    r_vars <- gsub(" ","",unlist(strsplit(random,"+",fixed=TRUE))) ## split on +, remove whitespace
    for(i in seq_along(r_vars)){
      ## special case: individidual
      if(r_vars[i] == "i"){
        r_var   <- "i"
        counter <- ""
        index   <- "i"
        nr <- nrow(cov.data)
      } else if(r_vars[i] == "i"){
        r_var   <- "t"
        counter <- ""
        index   <- "t"
        nr <- ncol(cov.data)
      } else {
        index <- counter <- nr <- NA
        r_var <- gsub("(","",gsub(")","",r_vars[i],fixed = TRUE),fixed="TRUE")
        r_var <- strsplit(r_var,"|",fixed=TRUE)[[1]]
        fix   <- r_var[1]
        ## check for nested effects
        r_var <- strsplit(gsub("\\",":",r_var[2],fixed=TRUE),":",fixed = TRUE)[[1]]
        for(j in seq_along(length(r_var))){
          if(j>1)print("WARNING: not actually nesting random effects at this time")            ## HACK: to get started, not actually nesting
          ## parse
          j_var      <- strsplit(r_var[j],"[",fixed = TRUE)[[1]]
          index[j]   <- gsub("]","",j_var[2],fixed=TRUE)
          counter[j] <- j_var[1]
          r_var[j]   <- j_var[1]
          ## add variable to data
          if(!(r_var[j] %in% names(data))){
            data[[length(data)+1]] <- as.numeric(as.factor(as.character(cov.data[,r_var[j]]))) ## multiple conversions to eliminate gaps
            names(data)[length(data)] <- r_var[j]
          }
          nr[j] <- max(as.numeric(data[[r_var[j]]]))
        }
        index <- paste0("[",index,"]")
      }
      ## create formula
      Pformula <- paste(Pformula,
                        paste0("+ alpha_", r_var,"[",counter,index,"]"))
      ## create random effect
      for(j in seq_along(nr)){
        Reffects <- paste(Reffects,paste0("for(k in 1:",nr[j],"){\n"),
                                 paste0("   alpha_",r_var[j],"[k] ~ dnorm(0,tau_",r_var[j],")\n}\n"))
      }
      ## create priors
      Rpriors <- paste(Rpriors,paste0("tau_",r_var," ~ dgamma(1,0.1)\n",collapse = " "))
      ## track
      burnin.variables <- c(burnin.variables, paste0("tau_", r_var))
      out.variables <- c(out.variables, paste0("tau_", r_var), paste0("alpha_",r_var))
      
    }
    ## Substitute into code
    TreeDataFusionMV <- sub(pattern = "## RANDOM EFFECT TAUS", Rpriors, TreeDataFusionMV)
    TreeDataFusionMV <- gsub(pattern = "## RANDOM_EFFECTS", Reffects, TreeDataFusionMV)
  }

  if(FALSE){
    ## DEV TESTING FOR X, polynomial X, and X interactions
    fixed <- "X + X^3 + X*bob + bob + dia + X*Tmin[t]"
  }
  ## Design matrix
  if (is.null(fixed)) {
    Xf <- NULL
  } else {
    ## check for covariate data (note: will falsely fail if only effect is X)
    if (is.null(cov.data)) {
      print("formula provided but covariate data is absent:", fixed)
    } else {
      cov.data <- as.data.frame(cov.data)
    }
    ## check if there's a tilda in the formula
    if (length(grep("~", fixed)) == 0) {
      fixed <- paste("~", fixed)
    }
    
    ## First deal with endogenous terms (X and X*cov interactions)
    fixedX <- sub("~","",fixed, fixed=TRUE)
    lm.terms <- gsub("[[:space:]]", "", strsplit(fixedX,split = "+",fixed=TRUE)[[1]])  ## split on + and remove whitespace
    X.terms <- strsplit(lm.terms,split = c("^"),fixed = TRUE)
    X.terms <- sapply(X.terms,function(str){unlist(strsplit(str,,split="*",fixed=TRUE))})
    X.terms <- which(sapply(X.terms,function(x){any(toupper(x) == "X")}))
    if(length(X.terms) > 0){
      ## rebuild fixed without X.terms
      fixed <- paste("~",paste(lm.terms[-X.terms],collapse = " + "))  
      
      ## isolate terms with X
      X.terms <- lm.terms[X.terms]
      Xpriors <- NULL
      for(i in seq_along(X.terms)){
        
        myBeta <- NULL
        Xformula <- NULL
        if(length(grep("*",X.terms[i],fixed = TRUE)) == 1){  ## INTERACTION
          
          covX <- strsplit(X.terms[i],"*",fixed=TRUE)[[1]] 
          covX <- covX[-which(toupper(covX)=="X")] ## remove X from terms
            
            ##is covariate fixed or time varying?
            tvar <-  length(grep("[t]",covX,fixed=TRUE)) > 0           
            if(tvar){
              covX <- sub("[t]","",covX,fixed = TRUE)
              if(!(covX %in% names(data))){
                ## add cov variables to data object
                data[[covX]] <- time_varying[[covX]]
              }
              covX <- paste0(covX,"[i,t]")
            } else {
              ## variable is fixed
              if(covX %in% colnames(cov.data)){ ## covariate present
                if(!(covX %in% names(data))){
                  ## add cov variables to data object
                  data[[covX]] <- cov.data[,covX]
                }
              } else {
                ## covariate absent
                print("covariate absent from covariate data:", covX)
              }
              
            } ## end fixed or time varying
            
            myBeta <- paste0("betaX_",covX)
            Xformula <- paste0(myBeta,"*x[i,t-1]*",covX,"[i]")

        } else if(length(grep("^",X.terms[i],fixed=TRUE))==1){  ## POLYNOMIAL
          powX <- strsplit(X.terms[i],"^",fixed=TRUE)[[1]] 
          powX <- powX[-which(toupper(powX)=="X")] ## remove X from terms
          myBeta <- paste0("betaX",powX)
          Xformula <- paste0(myBeta,"*x[i,t-1]^",powX)
          
        } else {  ## JUST X
          myBeta <- "betaX"
          Xformula <- paste0(myBeta,"*x[i,t-1]")
        }
        
        ## add variables to Pformula
        Pformula <- paste(Pformula,"+",Xformula)

        ## add priors
        Xpriors <- paste(Xpriors,"     ",myBeta,"~dnorm(0,0.001)\n")
          
        ## add to out.variables
        out.variables <- c(out.variables, myBeta)
        
      }

      ## create priors
      TreeDataFusionMV <- sub(pattern = "## ENDOGENOUS BETAS", Xpriors, TreeDataFusionMV)
      
    }  ## end processing of X terms

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
                      paste0("+ beta", Xf.names, "*Xf[rep[i],", seq_along(Xf.names), "]", collapse = " "))
    ## create 'rep' variable if not defined
    if(is.null(data$rep)){
      data$rep <- seq_len(nrow(Xf))
    }
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
    Xt.priors <- ""
    
    ## parse equation into variable names
    t_vars <- gsub(" ","",unlist(strsplit(time_varying,"+",fixed=TRUE))) ## split on +, remove whitespace
    ## check for interaction terms
    it_vars <- t_vars[grep(pattern = "*",x=t_vars,fixed = TRUE)]
    t_vars <- t_vars[!(tvars == it_vars)]
    
      ## need to deal with interactions with fixed variables
      ## will get really nasty if interactions are with catagorical variables
      ## need to create new data matrices on the fly
    
    for(i in seq_along(it_vars)){

      ##is covariate fixed or time varying?
      covX <- strsplit(it_vars[i],"*",fixed=TRUE)[[1]] 
      tvar    <- length(grep("[t]",covX[1],fixed=TRUE)) > 0
      tvar[2] <- length(grep("[t]",covX[2],fixed=TRUE)) > 0
      myBeta <- "beta_"
      for(j in 1:2){
        if(j == 2) myBeta <- paste0(myBeta,"_")
        if(tvar[j]){
          covX[j] <- sub("[t]","",covX[j],fixed = TRUE)
          if(!(covX[j] %in% names(data))){
            ## add cov variables to data object
            data[[covX[j]]] <- time_varying[[covX[j]]]
          }
          myBeta <- paste0(myBeta,covX[j])
          covX[j] <- paste0(covX[j],"[i,t]")
        } else {
          ## variable is fixed
          if(!(covX[j] %in% names(data))){
            ## add cov variables to data object
            data[[covX[j]]] <- cov.data[,covX[j]]
          }
          myBeta <- paste0(myBeta,covX[j])
          covX[j] <- paste0(covX[j],"[i]")
        } ## end fixed or time varying
        
      } ## end building beta
      
      ## append to process model formula
      Pformula <- paste(Pformula,
                        paste0(myBeta,"*",covX[1],"*",covX[2]))
      
      ## priors
      Xt.priors <- paste0(Xt.priors,
                          "    ",myBeta,"~dnorm(0,0.001)\n")
    }  ## end time-varying interaction terms
    
    
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
    Xt.priors <- paste0(Xt.priors,
                        paste0("     beta", t_vars, "~dnorm(0,0.001)", collapse = "\n")
                       )
    TreeDataFusionMV <- sub(pattern = "## TIME VARYING BETAS", Xt.priors, TreeDataFusionMV)
    
  } ## END time varying covariates
   
  
  ## insert process model into JAGS template
  if (!is.null(Pformula)) {
    TreeDataFusionMV <- sub(pattern = "##PROCESS", Pformula, TreeDataFusionMV)
  }
  
  ## Save script
  if(!is.null(save.jags)){
    cat(TreeDataFusionMV,file=save.jags)
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

