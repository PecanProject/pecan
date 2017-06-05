#### Clark et al. 2007 fusion of tree-ring and DBH data
#### analysis of Arizona PIPO FIA cores + DBH

### prep data files into jags objects
setwd("C:/Users/mekevans/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")

#AZ.PIPO <- read.delim("AZ_core_site_trees_cond_matches_allinone_new_10282016.txt") # 447 trees (rows)
AZ.PIPO <- read.table("AZ_core_site_trees_cond_matches_allinone_new_10282016.txt", header = T, sep = "\t", stringsAsFactors = F) 

### filter out those cases where DIA is NA...no size information
AZ.PIPO <- AZ.PIPO[!is.na(AZ.PIPO$DIA),] # 367 trees
### problem: in minority of cases, the difference between MEASYEAR and DateEnd is other than 0 or 1
### filter out those cases
temp1 <- AZ.PIPO[AZ.PIPO$MEASYEAR-AZ.PIPO$DateEnd<2,] # 329 trees
temp2 <- temp1[temp1$MEASYEAR-temp1$DateEnd>-1,] # 326 trees

source("BuildJAGSdataobject.R")
jags.stuff <- buildJAGSdataobject(temp2)
data <- jags.stuff$data
z0 <- jags.stuff$z0


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
InventoryGrowthFusion <- function(data, n.iter, random = TRUE, burnin_plot = FALSE) {
  library(rjags)

  # define which variables to monitor  
  burnin.variables <- c("tau_add", "tau_dbh", "tau_inc", "mu")
  out.variables <- c("x", "tau_add", "tau_dbh", "tau_inc", "mu")
  # start text object that will be manipulated (to build different linear models, swap in/out covariates)
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
}"

  Pformula <- NULL
  ## RANDOM EFFECTS
  if (random) {
    TreeDataFusionMV <- gsub(pattern = "#RANDOM", " ", TreeDataFusionMV)
    Pformula <- "+ ind[i] + year[t]"
    burnin.variables <- c(burnin.variables, "tau_ind", "tau_yr")
    out.variables <- c(out.variables, "tau_ind", "tau_yr", "ind", "year")
  }
  
  if (!is.null(Pformula)) {
    TreeDataFusionMV <- sub(pattern = "##PROCESS", Pformula, TreeDataFusionMV)
  }

  ### initial conditions
  ## state variable initial condition (can't use the Dietze version bc of structure of AZ PIPO data)
  ## z0 is built outside of this functions in Evans custom data-processing function (buildJAGSdataobject.R)
#  z0 <- t(apply(data$y, 1, function(y) {
#    -rev(cumsum(rev(y)))
#  })) + data$z[, ncol(data$z)]
  
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

out1 <- InventoryGrowthFusion(data=data, n.iter=2000)

