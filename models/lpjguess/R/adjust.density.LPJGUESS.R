
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
  individual$densindiv <- unname(individual$densindiv * rel.change)
  #the coupled C and N pools
  individual$cmass_leaf <- unname(individual$cmass_leaf * rel.change)
  individual$nmass_leaf <- unname(individual$nmass_leaf * rel.change)
  individual$cmass_root <- unname(individual$cmass_root * rel.change)
  individual$nmass_root <- unname(individual$nmass_root * rel.change)
  individual$cmass_sap <- unname(individual$cmass_sap * rel.change)
  individual$nmass_sap <- unname(individual$nmass_sap * rel.change)
  individual$cmass_heart <- unname(individual$cmass_heart * rel.change)
  individual$nmass_heart <- unname(individual$nmass_heart * rel.change)
  # the carbon debt ('retrocative storage' with no N couterpart)
  individual$cmass_debt <- unname(individual$cmass_debt * rel.change)
  # labile and long term N storage with no C counterparts
  individual$nstore_longterm <- unname(individual$nstore_longterm * rel.change)
  individual$nstore_labile <- unname(individual$nstore_labile * rel.change)
  
  return(individual)
  
}