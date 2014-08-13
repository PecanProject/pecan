sample.IC.SIPNET <- function(ne,state){
  plantWood = ifelse(rep("AGB" %in% names(state),ne),
                     state$AGB[1,sample.int(ncol(state$AGB),ne),1]*0.1,
                     runif(ne,0,300)) ## prior
  lai = ifelse(rep("LAI" %in% names(state),ne),
               state$LAI[1,sample.int(ncol(state$LAI),ne),1],
               runif(ne,0,7)) ## prior
  litter = ifelse(rep("litter" %in% names(state),ne),
                  state$litter[1,sample.int(ncol(state$litter),ne),1],
                  runif(ne,0,4)) ## prior
  soil   = ifelse(rep("soil" %in% names(state),ne),
                  state$soil[1,sample.int(ncol(state$soil),ne),1],
                  runif(ne,0,10)) ## prior
  litterWFrac = ifelse(rep("litterW" %in% names(state),ne),
                       state$litterW[1,sample.int(ncol(state$litterW),ne),1],
                       runif(ne)) ## prior
  soilWFrac = ifelse(rep("soilW" %in% names(state),ne),
                     state$soilW[1,sample.int(ncol(state$soilW),ne),1],
                     runif(ne)) ## prior
  snow = ifelse(rep("snow" %in% names(state),ne),
                state$snow[1,sample.int(ncol(state$snow),ne),1],
                runif(ne,0,1)) ## prior
  microbe = ifelse(rep("microbe" %in% names(state),ne),
                   state$microbe[1,sample.int(ncol(state$microbe),ne),1],
                   runif(ne,0,1)) ## prior                  
  IC = data.frame(plantWood,lai,litter,soil,litterWFrac,soilWFrac,snow,microbe) 
  return(IC)
  
}