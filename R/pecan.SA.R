pecan.SA <- function(dat, dtheta.q) {
  ## PECAn Sensitivity Analysis

  ## solving var(f) = sum((df/dtheta)^2*var(theta)) + O3
  ## theta are the n trait inputs defined in 'trait' dataframe
  ## f are the model outputs at different quantiles of theta
  load('out/trait.defs.Rdata')
  ## create tables with data for sensitivity analysis
  satables <- list()
  for (runname in c('post', 'prior')) {
    satables[[runname]] <- list()
    for ( outvar in c('agb')) {
      satables[[runname]][[outvar]] <- pecan.SAcalcs(runname, outvar,
                                                     dat, dtheta.q,
                                                     trait.defs, trait.samps)
    }
  }

  return(list(satables=satables,
              transformed.samps = dat))
}
