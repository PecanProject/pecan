pecan.dtheta <- function(samps){
  ##calculate values of:
  ## dtheta_0.35, dtheta_0.5, dtheta_0.65 
  ## var(theta_i)

  traits <- colnames(samps)
  n.traits <- length(traits)

  dtheta <-  matrix(NA, ncol = 5, nrow = n.traits)
  colnames(dtheta) <- c('lcl', 'ucl', 'mean', 'var', 'cv')
  rownames(dtheta) <- traits

  for (tr.i in traits) {
    dtheta[tr.i, ] <- dtheta.q(samps[ ,tr.i])
  }

  return(dtheta)
}
