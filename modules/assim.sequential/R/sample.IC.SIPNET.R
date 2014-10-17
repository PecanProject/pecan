sample.IC.SIPNET <- function(ne,state){
  ## g C * m-2 ground area in wood (above-ground + roots)
  plantWood = ifelse(rep("AGB" %in% names(state),ne),
                     state$AGB[1,sample.int(ncol(state$AGB),ne),1]*50, ## unit Mg/ha
                     runif(ne,0,14000)) ## prior
  ## initial leaf area, m2 leaves * m-2 ground area (multiply by leafCSpWt to get initial plant leaf C)
  lai = ifelse(rep("LAI" %in% names(state),ne),
               state$LAI[1,sample.int(ncol(state$LAI),ne),1],
               runif(ne,0,7)) ## prior
  ## g C * m-2 ground area
  litter = ifelse(rep("litter" %in% names(state),ne),
                  state$litter[1,sample.int(ncol(state$litter),ne),1],
                  runif(ne,0,1200)) ## prior
  ## g C * m-2 ground area
  soil   = ifelse(rep("soil" %in% names(state),ne),
                  state$soil[1,sample.int(ncol(state$soil),ne),1],
                  runif(ne,0,19000)) ## prior
  ## unitless: fraction of litterWHC
  litterWFrac = ifelse(rep("litterW" %in% names(state),ne),
                       state$litterW[1,sample.int(ncol(state$litterW),ne),1],
                       runif(ne)) ## prior
  ## unitless: fraction of soilWHC
  soilWFrac = ifelse(rep("soilW" %in% names(state),ne),
                     state$soilW[1,sample.int(ncol(state$soilW),ne),1],
                     runif(ne)) ## prior
  ## cm water equiv
  snow = ifelse(rep("snow" %in% names(state),ne),
                state$snow[1,sample.int(ncol(state$snow),ne),1],
                runif(ne,0,1)) ## prior
  microbe = ifelse(rep("microbe" %in% names(state),ne),
                   state$microbe[1,sample.int(ncol(state$microbe),ne),1],
                   runif(ne,0,1)) ## prior                  
  IC = data.frame(plantWood,lai,litter,soil,litterWFrac,soilWFrac,snow,microbe) 
  return(IC)
  
}