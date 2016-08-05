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
InventoryGrowthFusion <- function(data,n.iter,random=TRUE){
  require(rjags)
  
  ## Build IC for DBH matrix, data$z
  foo <- data$z
  #subract increment from DBH to extrapolate DBH backward
  for(g in 1:nrow(data$z)){
    gbar <- mean(data$y[g,],na.rm=TRUE)
    if(is.na(gbar)){gbar <- mean(data$y,na.rm=TRUE)}
    for(f in rev(2:ncol(data$y))){
      if(is.na(foo[g,f-1])){  ## not NA = censused that year
        if(is.na(data$y[g,f])){  
          foo[g,f-1]<- foo[g,f] - gbar  ## beyond start of core
        } else {
          foo[g,f-1]<- foo[g,f] - data$y[g,f]
        }
      }
    }
  }
  foo[foo < 0 & is.na(data$y)] <- NA #0
#  pith = apply(foo,1,function(x){rev(which(x == 0))[1]}) + 1
  pith = apply(foo,1,function(x){rev(which(is.na(x)))[1]}) + 1
  data$hit_pith = which(!is.na(pith))
  data$nhp = length(data$hit_pith)
  data$no_pith = which(is.na(pith))
  data$nnp = length(data$no_pith)
  pith[data$no_pith] = 1
  data$pith = pith

  
  data$y = log(data$y)
  data$z = log(data$z)
  
  burnin.variables = c("tau_add","tau_dbh","tau_inc","mu")
  out.variables = c("x","tau_add","tau_dbh","tau_inc","mu")
  
  TreeDataFusionMV = "
model{

  ### Loop over all individuals
  for(i in 1:ni){
  
  #### Data Model: DBH
  for(t in pith[i]:nt){
   lx[i,t] <- log(x[i,t])
   z[i,t] ~ dnorm(lx[i,t],tau_dbh)
  }
  
  #### Data Model: growth
  for(t in (pith[i]+1):nt){
   inc[i,t] <- log(x[i,t]-x[i,t-1])
   y[i,t] ~ dnorm(inc[i,t],tau_inc)
  }
  
  #### Process Model
  for(t in (pith[i]+1):nt){
   Dnew[i,t] <- log(x[i,t-1]) + mu ##PROCESS
   lnx[i,t]~dnorm(Dnew[i,t],tau_add)
   x[i,t] <- exp(lnx[i,t])
  }
  
#RANDOM ## individual effects
#RANDOM ind[i] ~ dnorm(0,tau_ind)  

  }  ## end loop over individuals

  ## initial condition
  for(i in 1:nnp){
    x[no_pith[i],1] ~ dnorm(x_ic,tau_ic)
  }
  for(i in 1:nhp){
    x[hit_pith[i],pith[hit_pith[i]]] ~ dnorm(x_ic,tau_ic)
  }

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
  mu ~ dnorm(0,0.5)
 }"

  Pformula = NULL
  ## RANDOM EFFECTS
  if(random == TRUE){
    TreeDataFusionMV = gsub(pattern="#RANDOM"," ",TreeDataFusionMV)
    Pformula = "+ ind[i] + year[t]"
    burnin.variables = c(burnin.variables,"tau_ind","tau_yr")
    out.variables = c(out.variables,"tau_ind","tau_yr","ind","year")
  }

  if(!is.null(Pformula)) TreeDataFusionMV = sub(pattern="##PROCESS",Pformula,TreeDataFusionMV)

  ## state variable initial condition
  z0 = t(apply(data$y,1,function(y){-rev(cumsum(rev(y)))})) + data$z[,ncol(data$z)] 
  
  ## JAGS initial conditions
  nchain = 3
  init <- list()
  for(i in 1:nchain){
    y.samp = sample(data$y,length(data$y),replace=TRUE)
    init[[i]] <- list(lnx = log(foo),tau_add=runif(1,1,5)/var(diff(y.samp),na.rm=TRUE),
                      tau_dbh=1,tau_inc=1500,tau_ind=50,tau_yr=100,ind=rep(0,data$ni),year=rep(0,data$nt))
  }
  
  ## compile JAGS model
  j.model   <- jags.model (file = textConnection(TreeDataFusionMV),
                           data = data,
                           inits = init,
                           n.chains = 3)
  ## burn-in
  jags.out   <- coda.samples (model = j.model,
                              variable.names = burnin.variables,
                              n.iter = min(n.iter,2000))
  plot(jags.out)
  
  ## run MCMC
  jags.out   <- coda.samples (model = j.model,
                              variable.names = out.variables,
                              n.iter = n.iter)
  
  return(jags.out)
}