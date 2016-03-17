sample.IC.LINKAGES <- function(ne,state){
  ## g C * m-2 ground area in wood (above-ground + roots)
  biomass_tsca = ifelse(rep("biomass_tsca" %in% names(state),ne),
                     state$biomass_tsca[1,sample.int(ncol(state$biomass_tsca),ne),1]*.1, ## unit Mg/ha ->kg/m2
                     runif(ne,0,14000)) ## prior    
  biomass_acsa3 = ifelse(rep("biomass_acsa3" %in% names(state),ne),
                        state$biomass_acsa3[1,sample.int(ncol(state$biomass_acsa3),ne),1]*.1, ## unit Mg/ha ->kg/m2
                        runif(ne,0,14000)) ## prior  
  biomass_beal2 = ifelse(rep("biomass_beal2" %in% names(state),ne),
                        state$biomass_beal2[1,sample.int(ncol(state$biomass_beal2),ne),1]*.1, ## unit Mg/ha ->kg/m2
                        runif(ne,0,14000)) ## prior  
  biomass_thoc2 = ifelse(rep("biomass_thoc2" %in% names(state),ne),
                        state$biomass_thoc2[1,sample.int(ncol(state$biomass_thoc2),ne),1]*.1, ## unit Mg/ha ->kg/m2
                        runif(ne,0,14000)) ## prior  
  IC = data.frame(biomass_tsca,biomass_acsa3,biomass_beal2,biomass_thoc2)
  return(IC)
  
}