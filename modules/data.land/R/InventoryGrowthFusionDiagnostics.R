##' @name InventoryGrowthFusionDiagnostics
##' @title InventoryGrowthFusionDiagnostics
##' @param jags.out output mcmc.list from InventoryGrowthFusion
##' @param combined  data output from matchInventoryRings
##' @author Michael Dietze
##' @export 
InventoryGrowthFusionDiagnostics <- function(jags.out, combined=NULL) {
  
  out      <- as.matrix(jags.out)
  x.cols   <- which(substr(colnames(out), 1, 1) == "x")
  if(length(x.cols) > 0){
    ci       <- apply(out[, x.cols], 2, stats::quantile, c(0.025, 0.5, 0.975))
    ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
    
    ### DBH par(mfrow=c(3,2))
    if(length(x.cols) > 0){
      graphics::layout(matrix(1:8, 4, 2, byrow = TRUE))
      ci       <- apply(out[, x.cols], 2, stats::quantile, c(0.025, 0.5, 0.975))
      ci.names <- parse.MatrixNames(colnames(ci), numeric = TRUE)
      
      smp <- sample.int(data$ni, min(8, data$ni))
      for (i in smp) {
        sel <- which(ci.names$row == i)
        rng <- c(range(ci[, sel], na.rm = TRUE), range(data$z[i, ], na.rm = TRUE))
        
        plot(data$time, ci[2, sel], type = "n", 
             ylim = range(rng), ylab = "DBH (cm)", main = i)
        PEcAn.visualization::ciEnvelope(data$time, ci[1, sel], ci[3, sel], col = "lightBlue")
        graphics::points(data$time, data$z[i, ], pch = "+", cex = 1.5)
        # lines(data$time,z0[i,],lty=2)
        
        ## growth
        sel      <- which(ci.names$row == i)
        inc.mcmc <- apply(out[, x.cols[sel]], 1, diff)
        inc.ci   <- apply(inc.mcmc, 1, stats::quantile, c(0.025, 0.5, 0.975)) * 5
        # inc.names = parse.MatrixNames(colnames(ci),numeric=TRUE)
        
        plot(data$time[-1], inc.ci[2, ], type = "n", 
             ylim = range(inc.ci, na.rm = TRUE), ylab = "Ring Increment (mm)")
        PEcAn.visualization::ciEnvelope(data$time[-1], inc.ci[1, ], inc.ci[3, ], col = "lightBlue")
        graphics::points(data$time, data$y[i, ] * 5, pch = "+", cex = 1.5, type = "b", lty = 2)
      }
    }
  }
  
  if (FALSE) {
    ## check a DBH
    plot(out[, which(colnames(out) == "x[3,31]")])
    graphics::abline(h = z[3, 31], col = 2, lwd = 2)
    graphics::hist(out[, which(colnames(out) == "x[3,31]")])
    graphics::abline(v = z[3, 31], col = 2, lwd = 2)
  }
  
  ## process model
  vars <- (1:ncol(out))[-c(which(substr(colnames(out), 1, 1) == "x"), 
                           grep("tau", colnames(out)),
                           grep("year", colnames(out)), 
                           grep("ind", colnames(out)),
                           grep("alpha",colnames(out)),
                           grep("deviance",colnames(out)))]
  
  graphics::par(mfrow = c(1, 1))
  for (i in vars) {
    graphics::hist(out[, i], main = colnames(out)[i])
    graphics::abline(v=0,lwd=3)
  }
  if (length(vars) > 1 && length(vars) < 10) {
    graphics::pairs(out[, vars])
  }
  
  if("deviance" %in% colnames(out)){
    graphics::hist(out[,"deviance"])
    vars <- c(vars,which(colnames(out)=="deviance"))
  }
  
  
  ## rebuild coda for just vars
  var.out <- coda::as.mcmc.list(lapply(jags.out,function(x){ x[,vars]}))
  
  ## convergence
  coda::gelman.diag(var.out)
  
  #### Diagnostic plots
  plot(var.out)
  
  if("deviance" %in% colnames(out)){
    graphics::hist(out[,"deviance"])
    vars <- c(vars,which(colnames(out)=="deviance"))
  }
  
  ## rebuild coda for just vars
  var.out <- coda::as.mcmc.list(lapply(jags.out,function(x){ x[,vars]}))
  
  ## convergence
  coda::gelman.diag(var.out)
  
  #### Diagnostic plots
  plot(var.out)
  
  
  ## Standard Deviations layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
  graphics::par(mfrow = c(2, 3))
  prec <- out[, grep("tau", colnames(out))]
  for (i in seq_along(colnames(prec))) {
    graphics::hist(1 / sqrt(prec[, i]), main = colnames(prec)[i])
  }
  stats::cor(prec)
  # pairs(prec)
  
  ### alpha
  graphics::par(mfrow = c(1, 1))
  alpha.cols <- grep("alpha", colnames(out))
  if (length(alpha.cols) > 0) {
    alpha.ord <- 1:length(alpha.cols)
    ci.alpha <- apply(out[, alpha.cols], 2, stats::quantile, c(0.025, 0.5, 0.975))
    plot(alpha.ord, ci.alpha[2, ], type = "n", 
         ylim = range(ci.alpha, na.rm = TRUE), ylab = "Random Effects")
    PEcAn.visualization::ciEnvelope(alpha.ord, ci.alpha[1, ], ci.alpha[3, ], col = "lightBlue")
    graphics::lines(alpha.ord, ci.alpha[2, ], lty = 1, lwd = 2)
    graphics::abline(h = 0, lty = 2)
  }
  
  graphics::par(mfrow = c(1, 1))
  ### alpha
  alpha.cols <- grep("alpha", colnames(out))
  if (length(alpha.cols) > 0) {
    alpha.ord <- 1:length(alpha.cols)
    ci.alpha <- apply(out[, alpha.cols], 2, stats::quantile, c(0.025, 0.5, 0.975))
    plot(alpha.ord, ci.alpha[2, ], type = "n", 
         ylim = range(ci.alpha, na.rm = TRUE), ylab = "Random Effects")
    PEcAn.visualization::ciEnvelope(alpha.ord, ci.alpha[1, ], ci.alpha[3, ], col = "lightBlue")
    graphics::lines(alpha.ord, ci.alpha[2, ], lty = 1, lwd = 2)
    graphics::abline(h = 0, lty = 2)
  }
  
  ### YEAR
  year.cols <- grep("year", colnames(out))
  if (length(year.cols > 0)) {
    ci.yr <- apply(out[, year.cols], 2, stats::quantile, c(0.025, 0.5, 0.975))
    plot(data$time, ci.yr[2, ], type = "n", 
         ylim = range(ci.yr, na.rm = TRUE), ylab = "Year Effect")
    PEcAn.visualization::ciEnvelope(data$time, ci.yr[1, ], ci.yr[3, ], col = "lightBlue")
    graphics::lines(data$time, ci.yr[2, ], lty = 1, lwd = 2)
    graphics::abline(h = 0, lty = 2)
  }
  
  ### INDIV
  ind.cols <- which(substr(colnames(out), 1, 3) == "ind")
  if (length(ind.cols) > 0 & !is.null(combined)) {
    graphics::boxplot(out[, ind.cols], horizontal = TRUE, outline = FALSE, col = as.factor(combined$PLOT))
    graphics::abline(v = 0, lty = 2)
    tapply(apply(out[, ind.cols], 2, mean), combined$PLOT, mean)
    table(combined$PLOT)
    
    spp <- combined$SPP
    # boxplot(out[order(spp),ind.cols],horizontal=TRUE,outline=FALSE,col=spp[order(spp)])
    graphics::boxplot(out[, ind.cols], horizontal = TRUE, outline = FALSE, col = spp)
    graphics::abline(v = 0, lty = 2)
    spp.code <- levels(spp)[table(spp) > 0]
    graphics::legend("bottomright", legend = rev(spp.code), col = rev(which(table(spp) > 0)), lwd = 4)
    tapply(apply(out[, ind.cols], 2, mean), combined$SPP, mean)
  }
} # InventoryGrowthFusionDiagnostics

########### NEXT STEPS: ############
#what explains the year effects? climate
#what explains the individual effects? size, species, canopy position, plot -> landscape
