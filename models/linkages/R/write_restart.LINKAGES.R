##' @title write_restart.LINKAGES
##' @name  write_restart.LINKAGES
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @param outdir      output directory
##' @param runid       run ID
##' @param start.time,stop.time year that is being read
##' @param settings    PEcAn settings object
##' @param new.state    analysis vector
##' @param RENAME      flag to either rename output file or not
##' @param new.params updated parameter values to write.
##    Format is named list with each entry matching a PFT
##' @param inputs passed on to `write.config.LINKAGES()`
##' 
##' @description Write restart files for LINKAGES
##' 
##' @return NONE
##' @export
##' 
write_restart.LINKAGES <- function(outdir, runid, start.time, stop.time,
                                   settings, new.state, 
                                   RENAME = TRUE, new.params, inputs) {
  
  ### TO DO : needs to be vectorized to improve SDA speed for runs that are longer than 50 years
  ### TO DO : distance matrix needs fixing
  
  ### Removing negative numbers because biomass can't be negative ###
  new.state[new.state < 0] <- 0
  
  names.keep <- names(new.state)
  new.state <- as.matrix(new.state)
  names(new.state) <- names.keep
  
  new.state.save <- new.state
  
  if(any(grep('Fcomp',names.keep))){
    new.state <- new.state.save[grep("Fcomp", names(new.state.save))]
    new.state.other <- new.state.save[grep("Fcomp", names(new.state.save), invert = TRUE)]
  }
  
  if(any(grep('AGB.pft',names.keep))){
    new.state <- new.state.save[grep("AGB.pft", names(new.state.save))]
    new.state.other <- new.state.save[grep("AGB.pft", names(new.state.save), invert = TRUE)]
  }
  
  variables <- names(new.state)
  ### Going to need to change this... ### Get some expert opinion
  N <- length(new.state)
  distance.matrix <- matrix(1, N, N)
  for (i in seq_len(N)) {
    distance.matrix[i, ] <- sample(c(seq(0, N-1, 1)), size = N)
    if(which(distance.matrix[i,]==0)!=i){
      distance.matrix[i,which(distance.matrix[i,]==0)] <- distance.matrix[i,i]
      distance.matrix[i,i] <- 0
    } 
  }
  #diag(distance.matrix) <- 0
  
  if(FALSE){
    distance.matrix <- rbind(c(0, 1, 4, 3, 2, 6, 5, 8, 7, 9, 10, 11, 12, 13, 14), 
                             c(5, 0, 3, 4, 8, 1, 2, 7, 6, 9, 10, 11, 12, 13, 14), 
                             c(5, 3, 0, 1, 8, 4, 2, 7, 6, 9, 10, 11, 12, 13, 14), 
                             c(6, 2, 1, 0, 8, 4, 3, 7, 5, 9, 10, 11, 12, 13, 14), 
                             c(2, 7, 5, 4, 0, 8, 6, 1, 3, 9, 10, 11, 12, 13, 14), 
                             c(6, 1, 3, 4, 8, 0, 2, 7, 5, 9, 10, 11, 12, 13, 14), 
                             c(5, 3, 1, 2, 8, 6, 0, 7, 4, 9, 10, 11, 12, 13, 14), 
                             c(3, 6, 4, 5, 1, 7, 8, 0, 2, 9, 10, 11, 12, 13, 14), 
                             c(1, 5, 3, 2, 7, 6, 4, 8, 0, 9, 10, 11, 12, 13, 14), 
                             c(3, 6, 4, 5, 1, 7, 8, 9, 2, 0, 10, 11, 12, 13, 14), 
                             c(3, 6, 4, 5, 1, 7, 8, 10, 2, 9, 0, 11, 12, 13, 14), 
                             c(3, 6, 4, 5, 1, 7, 8, 11, 2, 9, 10, 0, 12, 13, 14), 
                             c(3, 6, 4, 5, 1, 7, 8, 12, 2, 9, 10, 11, 0, 13, 14), 
                             c(3, 6, 4, 5, 1, 7, 8, 13, 2, 9, 10, 11, 12, 0, 14), 
                             c(3, 6, 4, 5, 1, 7, 8, 14, 2, 9, 10, 11, 12, 13, 0))
    
  }
  #distance.matrix <- rbind(c(0,3,1,2), c(3,0,2,1), c(1,2,0,3), c(2,1,3,0))
  
  ## HACK
  spp.params.default <- utils::read.csv(system.file("spp_matrix.csv", package = "linkages"))  #default spp.params
  nspec <- length(settings$pfts)
  spp.params.save <- numeric(nspec)
  for (i in seq_len(nspec)) {
    spp.params.save[i] <- which(spp.params.default[, 1] %in% settings$pfts[i]$pft$name)
  }
  
  spp.params <- spp.params.default[spp.params.save, ]
  biomass_spp_params <- function(new.params, default.params, pft) {
    if ("SLTA" %in% names(new.params[[as.character(pft)]])) {
      slta <- new.params[[as.character(pft)]]$SLTA
    } else {
      slta <- default.params[default.params$Spp_Name == pft, ]$SLTA
    }
    if ("SLTB" %in% names(new.params[[as.character(pft)]])) {
      sltb <- new.params[[as.character(pft)]]$SLTB
    } else {
      sltb <- default.params[default.params$Spp_Name == pft, ]$SLTB
    }
    if ("SLA" %in% names(new.params[[as.character(pft)]])) {
      sla_use <- (1/new.params[[as.character(pft)]]$SLA)*1000
      sla_use[sla_use>5000] <- stats::rnorm(1,4000,100)
      fwt <- sla_use#(1 / new.params[[as.character(pft)]]$SLA) * 10000
    } else {
      fwt <- default.params[default.params$Spp_Name == pft, ]$FWT
    }
    if ("FRT" %in% names(new.params[[as.character(pft)]])) {
      frt <- new.params[[as.character(pft)]]$FRT
    } else {
      frt <- default.params[default.params$Spp_Name == pft, ]$FRT
    }
    return(list(slta = slta, sltb = sltb, fwt = fwt, frt = frt))
  } # biomass_spp_params
  
  biomass_function <- function(dbh, spp.biomass.params) {
    # kg/tree
    0.1193 * dbh ^ 2.393 + 
      ((spp.biomass.params$slta + spp.biomass.params$sltb * dbh) / 2) ^ 2 * 
      3.14 * spp.biomass.params$fwt * spp.biomass.params$frt * 0.001
  } # biomass_function
  
  merit <- function(dbh, b_obs, spp.biomass.params) {
    (b_obs - biomass_function(dbh, spp.biomass.params)) ^ 2
  } # merit
  
  ## HACK
  
  # skip ensemble member if no file availible
  outfile <- file.path(outdir, runid, "linkages.out.Rdata")
  if (!file.exists(outfile)) {
    outfile <- file.path(outdir, runid, paste0(start.time, "linkages.out.Rdata"))
    if (!file.exists(outfile)) {
      PEcAn.logger::logger.severe(paste0("missing outfile ens #", runid))
    }
  }
  print(paste0("runid = ", runid))
  
  # load output
  load(outfile)
  
  ntrees <- ntrees.kill[, ncol(ntrees.kill), 1]  # number of trees
  
  if(sum(ntrees)==0) {
    #reloads spin up if theres nothing in the output file
    print('No survivors. Reusing spinup.')
    load(file.path(outdir, runid,list.files(file.path(outdir, runid))[grep(list.files(file.path(outdir, runid)),pattern='linkages')][1]))
    ntrees <- ntrees.kill[, ncol(ntrees.kill), 1]  # number of trees
    
  }
  
  nspec  <- length(settings$pfts)
  ncohrt <- ncohrt
  tyl    <- tyl
  C.mat  <- C.mat
  
  nogro  <- as.vector(nogro.save[, ncol(nogro.save), 1])  ## no growth indicator
  ksprt  <- matrix(0, 1, nspec)  ## kill sprout indicator ## LOOK INTO THIS
  iage   <- as.vector(iage.save[, ncol(iage.save), 1])  # individual age
  
  dbh    <- as.vector(dbh.save[, ncol(dbh.save), 1])
  
  n.index <- c(rep(1, ntrees[1]))
  for (i in 2:length(settings$pfts)) {
    n.index <- c(n.index, rep(i, ntrees[i]))
  }
  
  if(max(dbh) < 20){ # if all trees are small than large trees are 95th percentile otherwise trees bigger than 5 cm
    large.trees <- which(dbh >= (max(dbh) / 1.05))
  }else{
    large.trees <- which(dbh >= 20)
  }
  
  large.trees <- which(dbh > 0)
  
  for (s in seq_along(settings$pfts)) {
    ntrees[s] <- length(which(n.index[large.trees] == s))
  }
  
  n.index <- n.index[large.trees]
  
  dbh <- dbh[large.trees]
  iage <- iage[large.trees]
  nogro <- nogro[large.trees]
  
  new.ntrees <- numeric(length(settings$pfts))
  
  print(paste0("ntrees (large trees) =", ntrees))  #these are the large trees
  
  ##### This takes the average individual biomass of each species from the model and computes how many
  ##### individuals you should keep to match the biomass estimated from the data.  Still have to correct
  ##### for the total species biomass in the next step.
  
  ind.biomass <- numeric(sum(ntrees))
  
  # calculate biomass of each individual
  for (j in seq_len(sum(ntrees))) {
    # slta <- spp.params$SLTA[n.index[j]] sltb <- spp.params$SLTB[n.index[j]] fwt <-
    # spp.params$FWT[n.index[j]] frt <- spp.params$FRT[n.index[j]]
    pft <- spp.params$Spp_Name[n.index[j]]
    spp.biomass.params <- biomass_spp_params(new.params = new.params, 
                                             default.params = spp.params.default, 
                                             pft = pft)
    ind.biomass[j] <- biomass_function(dbh[j], spp.biomass.params) * (1 / 833) * 0.48  # changing units to be kgC/m^2
  }
  
  data2 <- data.frame(ind.biomass = ind.biomass,
                      n.index = n.index)
  mean.biomass.spp <- stats::aggregate(ind.biomass ~ n.index, mean, data = data2)   # calculate mean individual biomass for each species
  #browser()
  # calculate number of individuals needed to match new.state
  for (s in seq_along(settings$pfts)) {
    
    if (ntrees[s] > 0) {
      fix_adjust <- new.state[s]/mean.biomass.spp[mean.biomass.spp[, 1] == s, 2]  # number of individuals needed to agree with new.state      
    } else {
      for (r in 1:(length(settings$pfts) - 1)) {
        s.select <- which(distance.matrix[s, ] == r)  # select a new spp. to clone from
        if (ntrees[s.select] > 0) {
          break
        }
      }
      fix_adjust <- new.state[s] / mean.biomass.spp[mean.biomass.spp[, 1] == s.select, 2]
    }
    new.ntrees[s] <- as.numeric(ceiling(fix_adjust-.01))  #new number of ind. of each species
    if(new.ntrees[s]>200&!is.na(new.ntrees[s])){
      new.ntrees[s] = sample(size = 1, x = 50:150)
    } 
    print(s)
  }
  
  #making sure to stick with density dependence rules in linkages (< 198 trees per 800/m^2)
  #someday we could think about estimating this parameter from data
  if(sum(new.ntrees,na.rm = T) > 198) new.ntrees <- round((new.ntrees / sum(new.ntrees)) * stats::runif(1,195,198))
  
  print(paste0("new.ntrees =", new.ntrees))
  
  new.n.index <- c(rep(1, new.ntrees[1]))
  for (i in 2:length(settings$pfts)) {
    new.n.index <- c(new.n.index, rep(i, new.ntrees[i]))
  }
  
  n.ind <- 200
  
  dbh.temp <- numeric(n.ind)
  iage.temp <- numeric(n.ind)
  nogro.temp <- numeric(n.ind)
  
  # sample from individuals to construct new states
  for (s in seq_len(nspec)) {
    if (new.ntrees[s] == 0) {
      next
    }
    if (new.ntrees[s] <= ntrees[s]) {
      # new are less than the old of the same spp.  print('new are less than the old of the same spp.')
      select <- sample(size = new.ntrees[s], x = which(n.index == s), replace = FALSE)
    } else {
      if (new.ntrees[s] > ntrees[s] & ntrees[s] >= 1) {
        # new are greater than the old of the same spp. and there are old trees to clone print('new are
        # greater than the old of the same spp. and there are old trees of same spp. to clone')
        select <- c(which(n.index == s), 
                    sample(size = (new.ntrees[s] - ntrees[s]), x = which(n.index == s), replace = TRUE))
      } else {
        # print(paste0('clone needed for spp. ',s))
        for (r in 1:(length(settings$pfts) - 1)) {
          s.select <- which(distance.matrix[s, ] == r)  #select a new spp. to clone from
          # print(paste0('r =',r))
          if (ntrees[s.select] > 0) {
            break
          }
        }
        # print(s.select)
        select <- sample(size = as.numeric(new.ntrees[s]), 
                         x = which(n.index == s.select), 
                         replace = TRUE)
      }
    }
    dbh.temp[which(new.n.index == s)] <- dbh[select]
    iage.temp[which(new.n.index == s)] <- iage[select]
    nogro.temp[which(new.n.index == s)] <- nogro[select]
  }
  
  # fix dbh of sampled individuals to match new.state
  nl <- 1  ## individual counter
  b_calc <- numeric(length(settings$pfts))  #biomass of sampled trees
  b_calc1 <- numeric(length(settings$pfts))  #biomass of sampled trees
  bcorr <- numeric(length(settings$pfts))  #biomass correction factor to new.state
  b_obs <- numeric(sum(new.ntrees))
  for (s in seq_len(nspec)) {
    if (new.ntrees[s] == 0) {
      next
    }
    nu <- nl + new.ntrees[s] - 1
    pft <- unique(spp.params$Spp_Name[new.n.index[nl:nu]])
    spp.biomass.params <- biomass_spp_params(new.params = new.params, 
                                             default.params = spp.params.default, 
                                             pft = pft)
    b_calc[s] <- sum(biomass_function(dbh.temp[nl:nu], 
                                      spp.biomass.params = spp.biomass.params)) * (1 / 833) * 0.48  # changing units to be kgC/m^2
    
    bcorr[s] <- new.state[s] / b_calc[s] #calculate biomass correction
    
    if (length(pft) > 1) {
      stop("error too many pfts assigned")
    }
    
    b_obs[nl:nu] <- biomass_function(dbh.temp[nl:nu], 
                                     spp.biomass.params = spp.biomass.params) * as.numeric(bcorr[s])
    bMax <- 200
    for (j in nl:nu) {
      dbh.temp[j] <- stats::optimize(
        merit,
        c(1, bMax),
        b_obs = b_obs[j],
        spp.biomass.params = spp.biomass.params)$minimum
    }

    b_calc1[s] <- sum(biomass_function(dbh.temp[nl:nu],
                                       spp.biomass.params = spp.biomass.params)) * (1 / 833) * 0.48
    nl <- nu + 1
  }
  
  dbh <- dbh.temp
  iage <- iage.temp
  nogro <- nogro.temp  # numeric(200)#hack
  
  #nogro[nogro < 1] <- 1
  
  ntrees <- new.ntrees
  
  # print(dbh[1:ntrees[1]])
  
  # translate agb to dbh
  
  #dbh_spp[s] <- optimize(merit, c(0,200))$minimum bcorr = new.state[i,] /
  # agb.pft[,ncol(agb.pft),1] *(bcorr[s]/ntrees[s]) dbh.temp1[j] <- optimize(merit,
  # c(0,200))$minimum
  
  # for(n in 1:nspec){ slta <- spp.params$SLTA[n] sltb <- spp.params$SLTB[n] fwt <-
  # spp.params$FWT[n] frt <- spp.params$FRT[n] if (agb.pft[n,ncol(agb.pft),1]==0 &
  # new.state[i,n]>0){ abg.pft.temp <- sum(distance.matrix[,n]%*%t(agb.pft[n,ncol(agb.pft),1]))
  # ntrees.temp <- sum(distance.matrix[,n]%*%t(t(as.matrix(ntrees)))) dbh.temp <-
  # dbh[sum(ntrees[1:n])-1] for(j in 1:ntrees.temp){ b_obs <-
  # biomass_function(dbh[j],slta=slta,sltb=sltb,fwt=fwt,frt=frt)*bcorr[n] dbh.temp[j] <-
  # optimize(merit, c(0,200),b_obs=b_obs)$minimum } } nu <- nl + ntrees[n] - 1 nl <- nu + 1 }
  
  ##### SOIL
  if ("TotSoilCarb" %in% names(new.state.other)) {
    leaf.sum <- sum(tyl[1:12]) * 0.48
    if(new.state.other["TotSoilCarb"] > 1000) new.state.other["TotSoilCarb"] = stats::rnorm(1,1000,10)
    soil.org.mat <- new.state.other["TotSoilCarb"] - leaf.sum
    soil.corr <- soil.org.mat / (sum(C.mat[C.mat[1:ncohrt, 5], 1]) * 0.48)
    #if(soil.corr > 1) soil.corr <- 1
    C.mat[C.mat[1:ncohrt, 5], 1] <- C.mat[C.mat[1:ncohrt, 5], 1] * as.numeric(soil.corr)
    C.mat[is.na(C.mat[1:ncohrt,1]),1] <- 0
    C.mat[C.mat[1:ncohrt,1] < 0,1] <- 0
  }
  
  if (RENAME) {
    file.rename(file.path(settings$rundir, runid, "linkages.restart.Rdata"), 
                file.path(settings$rundir, runid, paste0(start.time, "linkages.restart.Rdata")))  # save original output
  }
  restart.file <- file.path(settings$rundir, runid, "linkages.restart.Rdata")
  sprintf("%s", restart.file)
  
  
  save(dbh, tyl, ntrees, nogro, ksprt, iage, C.mat, ncohrt, file = restart.file)
  
  # make a new settings with the right years min start date and end date - fail in informative way

  settings$run$start.date <- paste0(
    formatC(lubridate::year(start.time + 1), width = 4, format = "d", flag = "0"),
    "/01/01")
  settings$run$end.date <- paste0(
    formatC(lubridate::year(stop.time), width = 4, format = "d", flag = "0"),
    "/12/31")

  do.call(write.config.LINKAGES,
          args = list(trait.values = new.params, settings = settings, run.id = runid,
                      restart = TRUE, spinup = FALSE, inputs = inputs))

  # save original output
  if (RENAME) {
    file.rename(file.path(outdir, runid, "linkages.out.Rdata"), 
                file.path(outdir, runid, paste0(start.time, "linkages.out.Rdata")))
  }
} # write_restart.LINKAGES
