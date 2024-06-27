#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' Adjust LPJ-GUESS individual's density
##' 
##' Very simple function that just scales the density of individuals and the associated C and N pools 
##' by a relative amount
##' 
##' @param individual A nested list which encapsulates an LPJ-GUESS 'Individual' as read from a binary state file
##' @param rel.change A numeric by which to scale the density and C and N pools
##' 
##' 
##' @keywords internal
##' @return the scaled 'individual' (the initial nested list with update values)
##' @author Matthew Forrest
adjust.density.LPJGUESS  <- function(individual, rel.change) {
  
  # the density
  individual$densindiv <- individual$densindiv * rel.change
  #the coupled C and N pools
  individual$cmass_leaf <- individual$cmass_leaf * rel.change
  individual$nmass_leaf <- individual$nmass_leaf * rel.change
  individual$cmass_root <- individual$cmass_root * rel.change
  individual$nmass_root <- individual$nmass_root * rel.change
  individual$cmass_sap <- individual$cmass_sap * rel.change
  individual$nmass_sap <- individual$nmass_sap * rel.change
  individual$cmass_heart <- individual$cmass_heart * rel.change
  individual$nmass_heart <- individual$nmass_heart * rel.change
  # the carbon debt ('retrocative storage' with no N couterpart)
  individual$cmass_debt <- individual$cmass_debt * rel.change
  # labile and long term N storage with no C counterparts
  individual$nstore_longterm <- individual$nstore_longterm * rel.change
  individual$nstore_labile <- individual$nstore_labile * rel.change
  
  return(individual)
  
}