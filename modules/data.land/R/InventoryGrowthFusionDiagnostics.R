### first, some helper functions...

##' @name parse.MatrixNames
##' @title parse.MatrixNames
##' @author Michael Dietze
##' @param w mcmc object containing matrix outputs
##' @param pre prefix (variable name) for the matrix variable to be extracted
##' @param numeric boolean, whether to coerce class to numeric
##' @return matrix
##' @export
parse.MatrixNames <- function(w, pre = "x", numeric = FALSE) {
  w <- sub(pre, "", w)
  w <- sub("[", "", w, fixed = TRUE)
  w <- sub("]", "", w, fixed = TRUE)
  w <- matrix(unlist(strsplit(w, ",")), nrow = length(w), byrow = TRUE)
  if (numeric) {
    class(w) <- "numeric"
  }
  colnames(w) <- c("row", "col")
  return(as.data.frame(w))
} # parse.MatrixNames

#' plots a confidence interval around an x-y plot (e.g. a timeseries)
#' 
#' @param x Vector defining CI center
#' @param ylo Vector defining bottom of CI envelope
#' @param yhi Vector defining top of CI envelope
#' @export 
#' @author Michael Dietze, David LeBauer
ciEnvelope <- function(x, ylo, yhi, ...) {
  m   <- rbind(x, ylo, yhi)
  nas <- which(apply(is.na(m), 2, sum) > 0)
  if (length(nas) > 0) {
    ## break overall dataset into complete blocks
    sub.m <- list()
    for (i in seq_along(nas)) {
      if (i == 1) {
        if (nas[i] > 1) {
          sub.m[[i]] <- m[, 1:(nas[i] - 1)]
        }
      } else {
        if (nas[i] > (nas[i - 1] + 1)) {
          ## if NAs are not consecutive
          sub.m[[i]] <- m[, (nas[i - 1] + 1):(nas[i] - 1)]
        }
      }
    }
  } else {
    sub.m <- list(m = m)
  }
  for (i in seq_along(sub.m)) {
    x <- sub.m[[i]]["x", ]
    ylo <- sub.m[[i]]["ylo", ]
    yhi <- sub.m[[i]]["yhi", ]
    polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi), ylo[1])), border = NA, ...)
  }
} # ciEnvelope


### modified InventoryGrowthFusionDiagostics function
### tailored for AZ PIPO data

##' @name InventoryGrowthFusionDiagnostics
##' @title InventoryGrowthFusionDiagnostics
##' @param jags.out output mcmc.list from InventoryGrowthFusion
##' @param combined  data output from matchInventoryRings
##' @author Michael Dietze
##' @export 
InventoryGrowthFusionDiagnostics <- function(model.out, combined) {
  
  #### Diagnostic plots
  
  ### DBH par(mfrow=c(3,2))
  layout(matrix(1:8, 4, 2, byrow = TRUE))
  out      <- as.matrix(model.out) ### LOADS MCMC OUTPUT INTO OBJECT "OUT"
  x.cols   <- which(substr(colnames(out), 1, 1) == "x") # grab the state variable columns
  ci       <- apply(out[, x.cols], 2, quantile, c(0.025, 0.5, 0.975))
  ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
  
  smp <- sample.int(data$ni, min(8, data$ni)) # select a random sample of 8 trees to plot
  for (i in smp) {
    sel <- which(ci.names$row == i)
    rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
    
    plot(data$time, ci[2, sel], type = "n", 
         ylim = range(rng), ylab = "DBH (cm)", main = i)
    ciEnvelope(data$time, ci[1, sel], ci[3, sel], col = "lightBlue")
    points(data$time, data$z[i, ], pch = "+", cex = 1.5)
    # lines(data$time,z0[i,],lty=2)
    
    ## growth
    sel      <- which(ci.names$row == i)
    inc.mcmc <- apply(out[, x.cols[sel]], 1, diff)
    inc.ci   <- apply(inc.mcmc, 1, quantile, c(0.025, 0.5, 0.975)) * 5
    # inc.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
    
    plot(data$time[-1], inc.ci[2, ], type = "n", 
         ylim = range(inc.ci, na.rm = TRUE), ylab = "Increment (mm)")
    ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
    points(data$time, data$y[i, ] * 5, pch = "+", cex = 1.5, type = "b", lty = 2)
  }
  
  if (FALSE) {
    ## check a DBH
    plot(out[, which(colnames(out) == "x[3,31]")])
    abline(h = z[3, 31], col = 2, lwd = 2)
    hist(out[, which(colnames(out) == "x[3,31]")])
    abline(v = z[3, 31], col = 2, lwd = 2)
  }
  
  ## process model (mu...average growth)
  ## AZ PIPO version will have more than mu in vars...needs to be modified here
  #vars <- (1:ncol(out))[-c(which(substr(colnames(out), 1, 1) == "x"), grep("tau", colnames(out)), 
  #                         grep("year", colnames(out)), grep("ind", colnames(out)))]
  vars <- (1:ncol(out))[-c(which(substr(colnames(out), 1, 1) == "x"), 
                           grep("tau", colnames(out)),
                           grep("year", colnames(out)), 
                           grep("ind", colnames(out)),
                           grep("alpha",colnames(out)),
                           grep("deviance",colnames(out)))]  
  
  par(mfrow = c(1, 1))
  for (i in vars) {
    hist(out[, i], main = colnames(out)[i])
    abline(v=0,lwd=3)
  }
  if (length(vars) > 1 & length(vars) < 10) {
    pairs(out[, vars])
  }
  
  if("deviance" %in% colnames(out)){
    hist(out[,"deviance"])
    vars <- c(vars,which(colnames(out)=="deviance"))
  }
  
  
  ## rebuild coda for just vars
  var.out <- as.mcmc.list(lapply(model.out,function(x){ x[,vars]}))
  
  ## convergence
  gelman.diag(var.out)
  
  #### Diagnostic plots
  plot(var.out)
  
  ### raftery
  raftery.diag(var.out)
  
  #  mu <- out[,c(grep("mu", colnames(out)))]
  
  #  ppt.betas <- out[,c(grep("betappt", colnames(out)))]
  #  par(mfrow = c(4, 3))
  #  for (curr.month in month.abb) {
  #    curr.beta <- grep(pattern = curr.month, x = colnames(ppt.betas))
  #    hist(ppt.betas[, curr.beta], main = colnames(ppt.betas)[curr.beta])
  #  }
  
  #  tmax.betas <- out[,c(grep("betatmax", colnames(out)))]
  #  par(mfrow = c(4, 3))
  #  for (curr.month in month.abb) {
  #    curr.beta <- grep(pattern = curr.month, x = colnames(tmax.betas))
  #    hist(tmax.betas[, curr.beta], main = colnames(tmax.betas)[curr.beta])
  #  }
  
  #  wintP.JJ.beta <- out[,"betawintP.JJ"]
  #  tmax.JanA.beta <- out[,"betatmax.JanA"]
  #  hist(wintP.JJ.beta, main = "winter P (Jan-Jul)")
  #  hist(tmax.JanA.beta, main = "tmax (Jan-Aug)")
  
  par(mfrow = c(2, 2))
  #  for (i in 1:2){ # SDI, SI
  #    hist(betas[,i], main = colnames(betas)[i])
  #  }   
  
  SDI.beta <- out[,c(grep("betaSDI", colnames(out)))]
  SICOND.beta <- out[,c(grep("betaSICOND", colnames(out)))]
  X.beta <- out[,c(grep("betaX$", colnames(out)))]
  X2.beta <- out[,c(grep("betaX2", colnames(out)))]
  
  hist(SDI.beta, main = "stand density index")
  hist(SICOND.beta, main = "site index")  
  hist(X.beta, main = "tree size")
  hist(X2.beta, main = "quadratic tree size")  
  
  ### interaction effects
  X.SDI.int <- out[,"betaX_SDI"]
  X.SI.int <- out[,"betaX_SICOND"]  
  X.wintP.int <- out[,"betaX_wintP.JJ"]
  SDI.wintP.int <- out[,"beta_SDI_wintP.JJ"]
  hist(X.SDI.int, main = "size*stand density")
  hist(X.SI.int, main = "size*site index")
  hist(X.wintP.int, main = "size*wintP")
  hist(SDI.wintP.int, main = "SDI*wintP")
  
  ## Standard Deviations layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
  par(mfrow = c(2, 3))
  prec <- out[, grep("tau", colnames(out))]
  for (i in seq_along(prec)) {
    hist(1 / sqrt(prec[, i]), main = colnames(prec)[i])
  }
  cor(prec)
  # pairs(prec)
  
  
  par(mfrow = c(1, 1))
  ### YEAR
  year.cols <- grep("year", colnames(out))
  if (length(year.cols > 0)) {
    ci.yr <- apply(out[, year.cols], 2, quantile, c(0.025, 0.5, 0.975))
    plot(data$time, ci.yr[2, ], type = "n", 
         ylim = range(ci.yr, na.rm = TRUE), ylab = "Year Effect")
    ciEnvelope(data$time, ci.yr[1, ], ci.yr[3, ], col = "lightBlue")
    lines(data$time, ci.yr[2, ], lty = 1, lwd = 2)
    abline(h = 0, lty = 2)
  }
  
  ### INDIV
  ind.cols <- which(substr(colnames(out), 1, 3) == "ind")
  if (length(ind.cols) > 0) {
    boxplot(out[, ind.cols], horizontal = TRUE, outline = FALSE, col = as.factor(combined$PLOT))
    abline(v = 0, lty = 2)
    tapply(apply(out[, ind.cols], 2, mean), combined$PLOT, mean)
    table(combined$PLOT)
    
    spp <- combined$SPP
    # boxplot(out[order(spp),ind.cols],horizontal=TRUE,outline=FALSE,col=spp[order(spp)])
    boxplot(out[, ind.cols], horizontal = TRUE, outline = FALSE, col = spp)
    abline(v = 0, lty = 2)
    spp.code <- levels(spp)[table(spp) > 0]
    legend("bottomright", legend = rev(spp.code), col = rev(which(table(spp) > 0)), lwd = 4)
    tapply(apply(out[, ind.cols], 2, mean), combined$SPP, mean)
  }
} # InventoryGrowthFusionDiagnostics

########### NEXT STEPS: ############
#what explains the year effects? climate
#what explains the individual effects? size, species, canopy position, plot -> landscape