pecan.dtheta <- function(samps){
  ##calculate values of:
  ## dtheta_0.35, dtheta_0.5, dtheta_0.65 
  ## var(theta_i)

  traits <- colnames(samps)
  n.traits <- length(traits)

  dtheta <-  matrix(NA, ncol = 5, nrow = n.traits)
  colnames(dtheta) <- c('lcl', 'ucl', 'mean', 'var', 'cv')
  rownames(dtheta) = traits

  dtheta.q <- function(x) {
    if (i.tr != 'Vm_low_temp') {
      c(quantile(x,c(0.35,0.5,0.65)), var(x), sqrt(var(x))/mean(x))
    } else {
      c(quantile(x,c(0.35,0.5,0.65)), var(x), sqrt(var(x))/mean(x+273.15))
    }
  }

  for (i.tr in traits) {
    dtheta.q[i.tr, ] <- dtheta.q(samps[ ,i.tr])
  }
  return(dtheta.q)
}
