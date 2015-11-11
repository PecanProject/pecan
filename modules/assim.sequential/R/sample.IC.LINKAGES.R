sample.IC.LINKAGES <- function(ne,state){
  ## g C * m-2 ground area in wood (above-ground + roots)
  plantWood = ifelse(rep("AGB" %in% names(state),ne),
                     state$AGB[1,sample.int(ncol(state$AGB),ne),1]*.1, ## unit Mg/ha ->kg/m2
                     runif(ne,0,14000)) ## prior                
  IC = data.frame(plantWood)
  return(IC)
  
}