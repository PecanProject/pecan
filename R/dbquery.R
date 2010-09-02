##PECAn batchjobs module 1:
##meta-analysis of plant traits

## This has no copyright. All rights belong to BP
## Authors: David LeBauer, Mike Dietze
## Affiliation: Energy Biosciences Institute, University of Illinois at Urbana Champaign

## Author Comments: Prepared for use in Cyberintegrator workflow tool dbquery.r

## Description

## File Overview:
## 1. Inputs
## 2. Packages: RMySQL, rjags, XML
## 3. DB connections
## 4. Define Functions (model, write.model.loop)
## 5. Run Meta-analysis in loop for each trait
## 5.1 set trait name
## 5.2 Query biofuel.TraitData for each. sp x trait combination
## 5.3 Transform Data
## 5.4 Clean up data
## 5.5 Calculate obs.prec
## 5.6 Query biofuel.Prior for trait prior
## 5.7 Write model with write.model.loop(prior)
## 5.8 Gelman-Rubin Stat to ensure convergence
## 5.9 Plot mcmc
## 6 write ED2IN and config files
## 7 output ED2IN and config files

## List of Variables
## pft: level at which posterior is being generated
## species: component species for which trait data is used in Meta-analysis 

library(PECAn)

##log will gather diagnostic information throughout the workflow
log <-list()


trstr <- vecpaste(traits)

prstr <- gsub('Vcmax', 'Vm0', trstr)

## 1.3 Set inputs for jags.model()
j.chains <- 4
j.adapt  <- 500
##  1.4 Set inputs for coda.samples() from "rjags"
j.iter   <- length # number of iterations for JAGS
                                        # set to 100 for testing, then increase
j.thin   <- 25    # thinning interval for mcmc monitors
##  1.5 Calculate and log # of mcmc samples
mcmc.samples <- j.chains*(j.iter-j.adapt)/j.thin 
log[['jagparms']] <- list(j.chains = j.chains, j.adapt = j.adapt, j.iter = j.iter, j.thin = j.thin, mcmc.samples = mcmc.samples)

##  1.6 set variables to follow in mcmc, defined in model (below
##       global mean: beta.o
##         global SD: thetaSD
##   within study SD: ySD 
j.vars   <- c( 'beta.o','thetaSD', 'ySD')
##  1.7 Number of runs in ensemble
n <- 500
##  2 Required Packages
require(RMySQL)
require(rjags)
require(XML)
require(MASS)
require(ggplot2)
##  3 DB Connection to biofuel
dvr <- dbDriver ("MySQL")#Specifies DB driver being used
con <- dbConnect(dvr, group  = "biofuel" )
                                        #group is defined in $HOME/.my.cnf in square parens [ ]
                                        # used to specify database name, username, and  password
                                        # Contents of .my.cnf (insert username and password where indicated)
                                        # [client]
                                        # user = <username>
                                        # password = <password>
                                        # host = localhost
                                        # [biofuel]
                                        # database = biofuel

##  5 Meta-analysis for each trait
mcmc.object <- list() #  initialize output list of mcmc objects for each trait
mcmc.mat    <- list()

##  5.0 Check each trait to see if db contains data
qd   <- paste("SELECT DISTINCT TraitVarID FROM TraitData WHERE USDASymbol in (", spstr,") AND TraitVarID in(", trstr,");", sep = "")
q    <- dbSendQuery(con, qd)
.vars <- NA
.vars <- fetch ( q, n = -1 )
.varID <- gsub(' ', '', .vars$TraitVarID)
.v <- traits %in% .varID
for (p in seq(along = traits)) {
  if ( .v[p]) log[[traits[p]]] <- "data FOUND, will run MA"
  if (!.v[p]) log[[traits[p]]] <- "data NOT FOUND, will sample from prior"
}

## traits used to query traits with data

dbtrvec  <- traits[.v]
dbprvec  <- gsub('Vcmax', 'Vm0', traits[!.v])

## prior traits will be used in meta-analysis for traits w/ data
## or to query priors for traits w/o data
qd <- paste("SELECT Priors.PriorID, PriorPFT, VarID, PriorDistn, PriorParamA, PriorParamB, PriorN from Priors, Priors_has_PFT where Priors.PriorID = Priors_has_PFT.PriorID and Priors_has_PFT.pftID in ('", pft, "') and VarID in (", prstr, ");", sep = "")
q    <- dbSendQuery(con, qd)
priors <- NA
priors <- fetch ( q, n = -1 )
rownames(priors) <- priors$VarID

.v <- dbprvec %in% priors$VarID
for (p in seq(along = dbprvec)) {
  if (.v[p]) log[['priors']][p] <- paste ( traits[p], "prior found in database Priors table")
  if  (!.v[p]) log[['priors']][p] <-  paste ("!!db ERROR!! more than one ", traits[p], "prior found in database")
}

pdf (paste(pft, "post.pdf", sep = ""))
for (i in dbtrvec) { 
  name <- i
  pname <- ifelse(name != 'Vcmax', i, 'Vm0')
  
  ## 5.2 Query trait data from biofuel DB and assign results to data.frame "data"
  
  
  ## 5.3 Data transforms etc.
  ## 5.3.1 Convert Vcmax to Vm0 reference at 15C/288.15K
  if (name != 'Vcmax') {
    qd <- paste("SELECT SiteID, TrtID, TraitMean, TraitStatName, TraitStat, TraitN FROM TraitData WHERE USDASymbol in (", spstr,") AND TraitVarID ='", name,"' ;", sep = "")
    q    <- dbSendQuery(con, qd)
    data <- NA
    data <- fetch ( q, n = -1 )
  } else if (name == 'Vcmax') {
    #qd   <- paste("SELECT SiteID, TrtID, TraitMean, TraitStatName, TraitStat, TraitN, TraitCovLevel FROM TraitData LEFT JOIN TraitData_has_Covariate USING (TraitDataID) WHERE TraitCovID = 'temp' AND USDASymbol in (", spstr,") AND TraitVarID ='", name,"' ;", sep = "")
    qd <- paste("SELECT SiteID, TrtID, TraitMean, TraitStatName, TraitStat, TraitN, tdhc1.TraitCovLevel AS 'temp', tdhc2.TraitCovLevel AS 'canopy_layer' FROM TraitData LEFT JOIN TraitData_has_Covariate AS tdhc1 USING (TraitDataID) LEFT JOIN TraitData_has_Covariate AS tdhc2 USING (TraitDataID) WHERE tdhc1.TraitCovID = 'temp' and tdhc2.TraitCovID = 'canopy_layer' and (tdhc2.TraitCovLevel >= .8 or tdhc2.TraitCovLevel IS NULL) and tdhc2.TraitCovLevel IS NOT NULL and MONTH(TraitDate) between 4 and 7 and USDASymbol in (", spstr, ") and TraitVarID = 'Vcmax'; ", sep = "")
    q    <- dbSendQuery(con, qd)
    data <- NA
    data <- fetch ( q, n = -1 )
    data$TraitMean <- data$TraitMean * exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))
    .l <- c("SE", "SD")
    data$TraitStat[data$TraitStatName %in% .l] <- data$TraitStat * exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))
    .s <- "MSE"
    data$TraitStat[!data$TraitStatName %in% .s] <- data$TraitStat * exp (3000 * ( 1 / 288.15 - 1 / (273.15 + data$temp)))^2
  }
    data <- data[,1:6] #drop covariates

  ## 5.3.2 Assigning unique sequential integers to site and trt 
  data$SiteID[which(is.na(data$SiteID))] <- 0 # combine all unidentified sites
  site.i <- unique(data$SiteID)
  site.seq <- seq(site.i)
  data$site <- NA
  
  for (ii in site.seq) {
    data$site[data$SiteID == site.i[ii]] <- ii
  }
  data$trt <- 1
  
  for (ii in site.seq) {
    data$trt[which(is.na(data$TrtID))] <- 0
                                        #temp set unidentified treatments to 0, change at end
    l.a <- length(unique(data$TrtID[data$site == ii]))
    if (l.a >= 2) {
      trt.i <- unique(data$TrtID[data$site == ii])
      trt.seq <- seq(trt.i)
      for (jj in trt.seq) {
        if (jj == 1) {
          data$trt[data$TrtID == trt.i[jj]] <- 1
        } else {
          data$trt[data$TrtID == trt.i[jj]]  <- max(data$trt) + 1
        }
      }
    }
  }
  data$trt[data$trt == 0] <- max(data$trt) + 1
  ## add single id to all unidentified trts
  
  
  
  ## 5.3.4 Transformation of misc Stats to SE
  ## transform SD to SE
  sdi <- which(data$TraitStatName == "SD")
  data$TraitStat[sdi] <- data$TraitStat[sdi] / sqrt(data$TraitN[sdi])
  data$TraitStatName[sdi] <- "SE"
  ## transform MSE to SE
  msei <- which (data$TraitStatName == "MSE")
  data$TraitStat[msei] <- sqrt (data$TraitStat[msei])
  data$TraitStatName[msei] <- "SE"
  ## need to deal with LSD
  lsdi <- which(data$TraitStatName == "LSD")
  if (sum(lsdi) > 0 ) data[lsdi,c(4,5)] <- NA
  
  ## make sure there is at least one measure of variance
  is.var <- sum(as.numeric(is.na(data$TraitStat)))
  if(is.var == 0) log[['variance']] <- paste("no measure of variance for",name)
  ## check to make sure that all stats have been transformed to SE 
  statnames <- unique(data$TraitStatName[!is.na(data$TraitStatName)])
  if ( length(statnames) == 0) statnames <- "SE" 
  
  if (statnames == "SE"){
    log[['trait_stats']][name] <- paste (name, "trait stats accounted for")
  } else {
    log[['trait_stats']][name] <- paste("ERROR! ", statnames[statnames != "SE"], " statistics not transformed")
  }
  
  
  ## 5.4 Clean up data
  data <- data[,c(3, 5, 6, 7, 8)]                    # drop trait stat name column
  colnames(data) <- c("Y", "se", "n", "site","trt")  # rename cols
  data$stdev <- NA                                   # create col for sd
  data$obs.prec <- NA                                # create col for obs.prec
  
  ## 5.5 Calculate obs.prec
  ##  for leaf%N, transform mean and sd, assume leaf is 48%C
  j <- 1
  data$stdev <- sqrt(data$n) * data$se # sd for all other traits
  data$obs.prec <- 1 / sqrt(data$stdev)
  
  prior <- priors[pname, c('PriorDistn', 'PriorParamA', 'PriorParamB', 'PriorN')]
  colnames(prior) <- c("distn", "a", "b", "n")
  ## 5.7 Write MA model with trait-specific prior
  jag.model.file <-  paste( name, ".model.bug",sep="")  # file to store model
  write.ma.model ( ma.model, jag.model.file,
                    prior$distn, prior$a, prior$b,
                    length ( data$Y ), length(unique(data$trt)), length(unique(data$site)))

  j.data     <- data[,c(1,3,4,5,7)] #Y,n,site,trt, obs.prec
  j.data$n[is.na(j.data$n)] <- 1
  j.model    <- jags.model ( file = jag.model.file,
                            data = j.data,
                            n.adapt = j.adapt,
                            n.chains = j.chains)
  mcmc.object <- coda.samples ( model = j.model,
                               variable.names = j.vars,
                               n.iter = j.iter,
                               thin = j.thin)
  
  ## 5.8 G-R diagnostics to ensure convergence
  gd<-gelman.diag(mcmc.object)
  mpsrf<-round(gd$mpsrf,digits=4)
  if(mpsrf<1.1){
    note <-  paste ("JAGS model converged for", pft, pname,
                    "GD MPSRF = ",mpsrf, sep=" ")
  } else {
    note <- paste ("JAGS model did not converge for", pft, pname,
                   "GD MPSRF = ",mpsrf, sep=" ")
  }
  log[['gelman_diagnostic']][name] <- note
  
  ## 5.9 make simple diagnostic plots
  plot(mcmc.object[,1], trace = FALSE, density = TRUE, main = paste('posterior pdf of beta.o for', pft, name))
  mtext(text = note, line = 3)
  

  ## 5.10 data storage

  mcmc.mat[[pname]] <- as.matrix(mcmc.object)
}
dev.off()

samp.n <- dim(mcmc.mat[[1]])[1]

for (pname in traits) {
  if (pname == 'Vcmax') pname <- 'Vm0'
  prior <- priors[pname, c('PriorDistn', 'PriorParamA', 'PriorParamB')]
  colnames (prior) <- c("distn", "a", "b")
  priorsamp <- eval ( parse ( text = paste("r",prior$dist, "(", samp.n , ", ", prior$a, ", ",prior$b, ")",sep = "")))  
  if (pname %in% dbprvec) {
    mcmc.mat[[pname]] <- matrix (priorsamp, nrow = samp.n, ncol = 1)
  }
}


## 5.11 Convert variables with different units in DB and ED
## 5.11.1 convert leaf width in mm to leaf width in m
mcmc.mat[['leaf_width']]   <- 1/1000 * mcmc.mat[['leaf_width']]
priors['leaf_width', "PriorParamA"] <-  priors['leaf_width', "PriorParamA"] - log(1000)

## 5.11.2 leafN --> c2n_leaf
mcmc.mat[['c2n_leaf']] <- 48/mcmc.mat[['leafN']]
mcmc.mat <- mcmc.mat[-which(names(mcmc.mat)=='leafN')]

.d <- priors["leafN", c("PriorDistn", "PriorParamA", "PriorParamB")]
priors <- priors[-which(rownames(priors) == "leafN"),]
c2n_leaf <- eval (parse (text = paste("48/r", .d[1],"(100000", ", ", .d[2], ", ", .d[3], ")", sep = "")))
.parms <- signif(fitdistr(c2n_leaf, "lognormal")$estimate, 2)
priors["c2n_leaf", c("PriorDistn", "PriorParamA", "PriorParamB")] <- c("lnorm", .parms)

## 5.11.3 root maint resp = 50% of total root resp 
mcmc.mat[['root_respiration_factor']] <- 0.5 * mcmc.mat[['root_respiration_factor']]
priors['root_respiration_factor', "PriorParamA"] <- as.numeric(priors['root_respiration_factor', "PriorParamA"]) - log(2)


.d <- list(c("Vcmax", "Vm0"), c("leafN", "c2n_leaf"))
for (i in 1:2) dbprvec <- gsub(.d[[i]][1], .d[[i]][2], dbprvec)
for (i in 1:2) dbtrvec <- gsub(.d[[i]][1], .d[[i]][2], dbtrvec)

save(mcmc.mat, pft, priors, dbprvec, dbtrvec, log, file='out.dbquery.Rdata')
save(priors, traits, pft, file = 'dbquery.priors.Rdata')

## 05/2010: plot priors does not work in R1.7 on cluster or forecast
##source('plot.priors.R')
