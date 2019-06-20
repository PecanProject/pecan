
#' @keywords internal
adjustDensity.LPJGUESS  <- function(individual, rel.change) {
  
  individual$densindiv <- individual$densindiv * rel.change
  individual$cmass_leaf <- individual$cmass_leaf * rel.change
  individual$nmass_leaf <- individual$nmass_leaf * rel.change
  individual$cmass_root <- individual$cmass_root * rel.change
  individual$nmass_root <- individual$nmass_root * rel.change
  individual$cmass_sap <- individual$cmass_sap * rel.change
  individual$nmass_sap <- individual$nmass_sap * rel.change
  individual$cmass_heart <- individual$cmass_heart * rel.change
  individual$nmass_heart <- individual$nmass_heart * rel.change
  individual$cmass_debt <- individual$cmass_debt * rel.change
  
  return(individual)
  
}