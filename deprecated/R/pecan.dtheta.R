#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
pecan.dtheta <- function(samps, q){
  ##calculate values of:
  ## dtheta_0.35, dtheta_0.5, dtheta_0.65 
  ## var(theta_i)

  traits <- colnames(samps)
  n.traits <- length(traits)

  dtheta <-  matrix(NA, ncol = 5, nrow = n.traits)
  colnames(dtheta) <- c('lcl', 'ucl', 'mean', 'var', 'cv')
  rownames(dtheta) <- traits

  for (tr.i in traits) {
    dtheta[tr.i, ] <- dtheta.q(samps[ ,tr.i], tr.i, q)
  }

  return(dtheta)
}
