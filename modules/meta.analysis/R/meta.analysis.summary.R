##' Generate summary statistics and diagnostics for PEcAn meta.analysis
##'
##' @param mcmc.object JAGS mcmc output from \code{\link{pecan.ma}}
##' @param pft plant functional type
##' @param outdir output directory
##' @param threshold Gelman-Rubin convergence diagnostic (MGPRF)
##' default = 1.2 following Bolker 2008 Ecological Models and Data in R
##' @param gg produce extra diagnostic plots using the "ggmcmc" package? Caution: very slow!
##' @export
##'
##' @examples
##' \dontrun{
##' summary <- pecan.ma.summary(
##'  trait.mcmc,
##'  settings$pfts$pft,
##'  settings$outdir,
##'  settings$meta.analysis$threshold)
##' }
##' @author David LeBauer, Shawn Serbin
pecan.ma.summary <- function(mcmc.object, pft, outdir, threshold = 1.2, gg = FALSE) {
  fail <- rep(FALSE, length(mcmc.object))
  names(fail) <- names(mcmc.object)
  not.converged <- data.frame()
  
  sink(file = file.path(outdir, "meta-analysis.log"), append = TRUE, split = TRUE)
  for (trait in names(mcmc.object)) {
    
    if (gg) {
      if (!requireNamespace("ggmcmc", quietly = TRUE)) {
        PEcAn.logger::logger.severe(
          "Can't find package 'ggmcmc',",
          "needed by `PEcAn.MA::meta.analysis.summary()` when `gg = TRUE`.",
          "Please install it and try again.")
      }
    }
    ## new diagnostic plots. But very slow & !any(grepl('^gg', dir(outdir)))){
    if (gg) {
      if (coda::is.mcmc.list(mcmc.object[[trait]])) {
        ggplot2::theme_set(ggplot2::theme_bw())
        ggmcmc::ggmcmc(ggmcmc::ggs(mcmc.object[[trait]]), 
               plot = c("ggs_density", "ggs_traceplot", "ggs_autocorrelation", "ggs_Rhat", "ggs_geweke"), 
               file.path(outdir, paste0("gg.ma.summaryplots.", trait, ".pdf")))
      }
    }
    
    ## reordering maparms so that beta.o etc not sent to end
    .maparms <- names(mcmc.object[[trait]][1, ][1][[1]])
    .parms   <- c("beta.o", "thetaSD", "trtSD", "ySD")
    maparms  <- .maparms[c(which(.maparms %in% .parms), which(!.maparms %in% .parms))]
    
    ## plots for mcmc diagnosis
    grDevices::pdf(file.path(outdir, paste0("ma.summaryplots.", trait, ".pdf")))

    for (i in maparms) {
      plot(mcmc.object[[trait]][, i],
           trace = FALSE,
           density = TRUE,
           main = paste("summary plots of", i, "for", pft, trait))
      graphics::box(lwd = 2)
      plot(mcmc.object[[trait]][, i], density = FALSE)
      graphics::box(lwd = 2)
      coda::autocorr.plot(mcmc.object[[trait]][, i][1], xlim = c(1, 50))
      graphics::box(lwd = 2)
    }
    lattice::xyplot(mcmc.object[[trait]])
    lattice::densityplot(mcmc.object[[trait]])
    coda::acfplot(mcmc.object[[trait]])
    grDevices::dev.off()

    ## G-R diagnostics to ensure convergence
    gd            <- coda::gelman.diag(mcmc.object[[trait]])
    mpsrf         <- round(gd$mpsrf, digits = 3)
    if (mpsrf < threshold) {
      PEcAn.logger::logger.info(paste("JAGS model converged for", pft, trait,
                        "\nGD MPSRF = ", mpsrf, "\n"))
    } else {
      not.converged <- rbind(not.converged, data.frame(pft = pft, trait = trait, mpsrf = mpsrf))
      PEcAn.logger::logger.info(paste("JAGS model did not converge for", pft, trait, 
                        "\nGD MPSRF = ", mpsrf, "\n"))
      fail[trait] <- TRUE
    }
  } # trait-loop ends
  
  if (any(fail)) {
    PEcAn.logger::logger.warn("JAGS model failed to converge for one or more trait. Discarding samples.")
    for (i in seq_len(nrow(not.converged))) {
      with(not.converged[i, ], PEcAn.logger::logger.info(paste(pft, trait, "MPSRF = ", mpsrf)))
    }
    mcmc.object[fail] <- NULL #discard samples
  }
  sink()
  return(mcmc.object)
} # pecan.ma.summary
