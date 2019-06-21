
#' @keywords internal
adjustDensity.LPJGUESS  <- function(individual, rel.change) {
  
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