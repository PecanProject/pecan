

adjustBiomass <- function(individual, biomass.increment,  sla, wooddens, lifeform, k_latosa, k_allom2, k_allom3){
  
  # dummy input values to the allocation function below
  # note that they are not actually updated by the function, the updated values are in the returned list
  cmass_leaf_inc <- 0
  cmass_root_inc <- 0
  cmass_sap_inc <- 0 
  cmass_debt_inc <- 0
  cmass_heart_inc <- 0
  litter_leaf_inc <- 0
  litter_root_inc <- 0
  exceeds_cmass <- 0
  
  updated.pools <- allocation(
    # vegetation state
    bminc = as.numeric(biomass.increment/individual$densindiv),
    cmass_leaf = as.numeric(individual$cmass_leaf/individual$densindiv),              
    cmass_root = as.numeric(individual$cmass_root/individual$densindiv),
    cmass_sap = as.numeric(individual$cmass_sap/individual$densindiv),
    cmass_debt  = as.numeric(individual$cmass_debt/individual$densindiv),
    cmass_heart = as.numeric(individual$cmass_heart/individual$densindiv),
    ltor = as.numeric(individual$ltor),
    height = as.numeric(individual$height),
    # PFT parameters
    sla = as.numeric(sla),
    wooddens = as.numeric(wooddens),
    lifeform = as.integer(lifeform),
    k_latosa = as.numeric(k_latosa),
    k_allom2 = as.numeric(k_allom2),
    k_allom3 = as.numeric(k_allom3),
    # calculated increments (not actually used, the updated values returned in the updated.pools list)
    cmass_leaf_inc = as.numeric(cmass_leaf_inc),
    cmass_root_inc = as.numeric(cmass_root_inc),
    cmass_sap_inc = as.numeric(cmass_sap_inc),
    cmass_debt_inc = as.numeric(cmass_debt_inc),
    cmass_heart_inc = as.numeric(cmass_heart_inc),
    litter_leaf_inc = as.numeric(litter_leaf_inc),
    litter_root_inc = as.numeric(litter_root_inc),
    exceeds_cmass = as.numeric(exceeds_cmass))
  
  
  # adjust the various associated C (and N) pools based on the results of the allocation call
  
  # leaf
  original.cmass_leaf <- individual$cmass_leaf
  new.cmass_leaf <- individual$cmass_leaf + (updated.pools[["cmass_leaf_inc"]] * individual$densindiv)
  leaf.scaling <- new.cmass_leaf / original.cmass_leaf
  individual$cmass_leaf <- new.cmass_leaf
  individual$nmass_leaf <- individual$nmass_leaf * leaf.scaling
  
  # root
  original.cmass_root <- individual$cmass_root
  new.cmass_root <- individual$cmass_root + (updated.pools[["cmass_root_inc"]] * individual$densindiv)
  root.scaling <- new.cmass_root / original.cmass_root
  individual$cmass_root <- new.cmass_root
  individual$nmass_root <- individual$nmass_root * root.scaling
  
  # sap
  original.cmass_sap <- individual$cmass_sap
  new.cmass_sap <- individual$cmass_sap + (updated.pools[["cmass_sap_inc"]] * individual$densindiv)
  sap.scaling <- new.cmass_sap / original.cmass_sap
  individual$cmass_sap <- new.cmass_sap
  individual$nmass_sap <- individual$nmass_sap * sap.scaling
  
  # heart
  original.cmass_heart <- individual$cmass_heart
  new.cmass_heart <- individual$cmass_heart + (updated.pools[["cmass_heart_inc"]] * individual$densindiv)
  heart.scaling <- new.cmass_heart / original.cmass_heart
  individual$cmass_heart <- new.cmass_heart
  individual$nmass_heart <- individual$nmass_heart * heart.scaling
  
  # debt - note no equivalant N debt
  original.cmass_debt <- individual$cmass_debt
  new.cmass_debt <- individual$cmass_debt + (updated.pools[["cmass_debt_inc"]] * individual$densindiv)
  individual$cmass_debt <- new.cmass_debt
  
  
  # checks
  if(FALSE) {
    
    biomass.final <- individual$cmass_leaf+individual$cmass_root+individual$cmass_heart+individual$cmass_sap-individual$cmass_debt
    if(abs((biomass.final/biomass.total) - 1.1) < 0.001) {
      print("--- okay ---")
    }
    else {
      print("--- not okay ---")
    }
    print(individual$indiv.pft.id)
    print(lifeform[individual$indiv.pft.id+1])
    print(biomass.final/biomass.total)
    print(unlist(updated.pools))
    print("--- end ---")
  }
  
  return(individual)
  
}
        