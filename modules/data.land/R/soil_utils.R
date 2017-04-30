#' Estimate soil parameters from texture class or sand/silt/clay
#'
#' @param soil_type USDA Soil Class. See Details
#' @param sand       percent sand
#' @param silt       percent silt
#' @param clay       percent clay
#'
#' @details 
#' * Specify _either_ soil_type or sand/silt/clay. soil_type will be ignored if sand/silt/clay is provided
#' * If only 2 out of sand/silt/clay are provided, it will be assumed they sum to 100%
#' * Valid soil class options: "Sand","Loamy sand","Sandy loam","Silt loam","Loam",
#'                             "Sandy clay loam","Silty clay loam","Clayey loam",
#'                             "Sandy clay","Silty clay","Clay","Peat","Bedrock",
#'                             "Silt","Heavy clay","Clayey sand","Clayey silt"
#' * Based on ED2/R-utils/soilutils.r
#' * Hydraulics based on Cosby et al 1984, using table 4 and equation 1 (which is incorrect it should be saturated moisture potential over moisture potential)
#' 
#'
#' @return table of soil hydraulic and thermal parmeters
#' @export
#'
#' @examples
#' sand <- c(0.3,0.4,0.5)
#' clay <- c(0.3,0.3,0.3)
#' soil_params(sand=sand,clay=clay)
soil_params <- function(soil_type,sand,silt,clay){

  ## load soil parameters
  data("soil_class")
  mysoil <- list()
  
  #---------------------------------------------------------------------------------------#
  #     Find soil class and sand, silt, and clay fractions.                               #
  #---------------------------------------------------------------------------------------#
  if (missing(sand) & missing(clay)){
    ## insufficient texture data, infer from soil_type
    if(missing(soil_type)) PEcAn.utils::logger.error("insufficient arguments")
    mysoil$soil_type <- soil_type
    mysoil$soil_n <- which(toupper(soil.name) == toupper(soil_type))
    mysoil$key   <- soil.key [mysoil$soil_n]
    mysoil$xsand <- xsand.def[soil_type]
    mysoil$xclay <- xclay.def[soil_type]
    mysoil$xsilt <- 1. - mysoil$xsand - mysoil$xclay
  } else {
    if(missing(sand)){
      sand <- 1-silt-clay
    }else if(missing(silt)){
      silt <- 1-sand-clay
    }else if(missing(clay)){
      clay <- 1-sand-silt
    } else {
      #not missing anything else, normalize
      stot <- sand+silt+clay
      if(any(stot > 2)) stot <- stot*100 ## assume values reported in % not proportion
      sand <- sand/stot
      silt <- silt/stot
      clay <- clay/stot
    }
    
    mysoil$soil_n <- sclass(sand,clay)
    mysoil$soil_type  <- soil.name[mysoil$soil_n]
    mysoil$key   <- soil.key [mysoil$soil_n]
    mysoil$xsand <- sand
    mysoil$xclay <- clay
    mysoil$xsilt <- 1. - mysoil$xsand - mysoil$xclay
  }
  #---------------------------------------------------------------------------------------#
  
  
  
  
  #---------------------------------------------------------------------------------------#
  #       Set up primary properties.                                                      #
  #---------------------------------------------------------------------------------------#
  for(z in which(mysoil$soil_n == 13)){
    #----- Bedrock.  Most things are zero, because it is an impermeable soil. -----------#
    mysoil$slbs[z]      <-  0.
    mysoil$slpots[z]    <-  0.
    mysoil$slcons[z]    <-  0.
    mysoil$slmsts[z]    <-  0.
    mysoil$sfldcap[z]   <-  0.
    mysoil$soilcp[z]    <-  0.
    mysoil$soilwp[z]    <-  0.
    mysoil$slcpd[z]     <-  2130000.
    #------------------------------------------------------------------------------------#
  }
  for(z in which(mysoil$soil_n == 12)){
    #------------------------------------------------------------------------------------#
    #      Peat.  High concentration of organic matter.  Mineral soil equations don't    #
    # apply here.                                                                        #
    #------------------------------------------------------------------------------------#
    mysoil$slbs[z]    <-  6.180000
    mysoil$slpots[z]  <- -0.534564359
    mysoil$slcons[z]  <-  2.357930e-6
    mysoil$slmsts[z]  <-  0.469200
    mysoil$sfldcap[z] <-  0.285709966
    mysoil$slcpd[z]   <-  874000.
    #------------------------------------------------------------------------------------#
  }
  for(z in which(!(mysoil$soil_n %in% c(12,13)))){
    #------------------------------------------------------------------------------------#
    #      Mineral soil.  Use the standard Cosby et al 1984 eqns                         #
    #------------------------------------------------------------------------------------#
    ## TO-DO: Cosby Table 4 has equations for soil property STANDARD DEVIATIONS in addition to means
    ## in future, upgrade to return these and do ensemble sampling
    
    # B exponent [unitless]
    mysoil$slbs[z]    <- 3.10 + 15.7*mysoil$xclay[z] - 0.3*mysoil$xsand[z]
    
    # Soil moisture potential at saturation [ m ]
    mysoil$slpots[z]  <- -0.01 * (10.^(2.17 - 0.63*mysoil$xclay[z] - 1.58*mysoil$xsand[z]))
    
    # Hydraulic conductivity at saturation [ m/s ]
    mysoil$slcons[z]  <- udunits2::ud.convert(10.^(-0.60 + 1.26*mysoil$xsand[z] - 0.64*mysoil$xclay[z]),
                                           "inch/hour","meters/second") 
    
    # Soil moisture at saturation [ m^3/m^3 ]
    mysoil$slmsts[z]  <- (50.5 - 14.2*mysoil$xsand[z] - 3.7*mysoil$xclay[z]) / 100.
    
    # Soil field capacity[ m^3/m^3 ]
    mysoil$sfldcap[z] <- mysoil$slmsts[z] * ( fieldcp.K/mysoil$slcons[z])^ (1. / (2.*mysoil$slbs[z]+3.))
  
    #---------------------------------------------------------------------------------!
    #     Heat capacity.  Here we take the volume average amongst silt, clay, and     !
    # sand, and consider the contribution of air sitting in.  In order to keep it     !
    # simple, we assume that the air fraction won't change, although in reality its   !
    # contribution should be a function of soil moisture.  Here we use the amount of  !
    # air in case the soil moisture was halfway between dry air and saturated, so the !
    # error is not too biased.                                                        !
    #---------------------------------------------------------------------------------!
    mysoil$slcpd[z]   <- (1. - mysoil$slmsts[z]) * ( mysoil$xsand[z] * sand.hcap + mysoil$xsilt[z] * silt.hcap +
                        mysoil$xclay[z] * clay.hcap ) + 0.5 * (mysoil$slmsts[z] - mysoil$soilcp[z]) * air.hcap

  } ## end primary properties
  
  #---------------------------------------------------------------------------------------#
  #      Calculate the derived properties in case this is not bedrock.                    #
  #---------------------------------------------------------------------------------------#
  mysoil$slpotcp = mysoil$soilcp = mysoil$slpotwp = soilwp = 0.0
  for(z in which(!(mysoil$soil_n == 13))){
    # Dry soil capacity (at -3.1MPa) [ m^3/m^3 ]
    mysoil$slpotcp[z]   <- - soilcp.MPa * 1000. / grav
    mysoil$soilcp[z]    <- mpot2smoist(mysoil$slpotcp[z],mysoil$slpots,mysoil$slbs[z],mysoil$slmsts[z])
    
    # Wilting point capacity (at -1.5MPa) [ m^3/m^3 ]
    mysoil$slpotwp[z]   <- - soilwp.MPa * 1000. / grav
    mysoil$soilwp[z]    <- mpot2smoist(mysoil$slpotwp[z], mysoil$slpots,mysoil$slbs[z],mysoil$slmsts[z])
    
    # Water potential for field capacity            [ m]
    # mysoil$slpotfc   <- smoist2mpot(mysoil$sfldcap, mysoil)
  }
  
  #---------------------------------------------------------------------------------------#
  #      Soil thermal conductivity.                                                       #
  #                                                                                       #
  # Thermal conductivity is the weighted average of thermal conductivities of             #
  # all materials, although a further weighting factor due to thermal gradient of         #
  # different materials.  We use the de Vries model described at:                         #
  #                                                                                       #
  # Camillo, P., T.J. Schmugge, 1981: A computer program for the simulation of heat       #
  #     and moisture flow in soils, NASA-TM-82121, Greenbelt, MD, United States.          #
  #                                                                                       #
  # Parlange, M.B., et al., 1998: Review of heat and water movement in field soils,       #
  #    Soil Till. Res., 47(1-2), 5-10.                                                    #
  #                                                                                       #
  #---------------------------------------------------------------------------------------#
  mysoil$thcond0 <- ( ksand * mysoil$xsand  * ( 1. - mysoil$slmsts ) * sand.cond 
                     + ksilt * mysoil$xsilt  * ( 1. - mysoil$slmsts ) * silt.cond
                     + kclay * mysoil$xclay  * ( 1. - mysoil$slmsts ) * clay.cond
                     + kair                  *       mysoil$slmsts    *  air.cond  )
  mysoil$thcond1 <- h2o.cond - kair * air.cond
  mysoil$thcond2 <- ( ksand * mysoil$xsand  * ( 1. - mysoil$slmsts )
                     + ksilt * mysoil$xsilt  * ( 1. - mysoil$slmsts )
                     + kclay * mysoil$xclay  * ( 1. - mysoil$slmsts )
                     + kair                  *        mysoil$slmsts   )
  mysoil$thcond3 <- 1. - kair
  #---------------------------------------------------------------------------------------#
  
  ## final values to look up
  for(z in which(!(mysoil$soil_n <= 13))){
    mysoil$soil_albedo[z] <- texture$albdry[mysoil$soil_n[z]]
    mysoil$xrobulk[z]     <- texture$xrobulk[mysoil$soil_n[z]]
    mysoil$slden[z]       <- texture$slden[mysoil$soil_n[z]]
  }
  for(z in which(!(mysoil$soil_n > 13))){
    ## if lack class-specific values, use across-soil average
    mysoil$soil_albedo[z] <- median(texture$albdry)
    mysoil$xrobulk[z]     <- median(texture$xrobulk)
    mysoil$slden[z]       <- median(texture$slden)
  }
  
  return(mysoil)
  }#end function
#==========================================================================================#
#==========================================================================================#





 
#' This function determines the soil class number based on the fraction of sand, clay, and silt
#'
#' @param sandfrac 
#' @param clayfrac 
#'
#' @return
#' @export
#'
#' @examples
#' sclass(0.3,0.3)
sclass <- function(sandfrac,clayfrac){
  
  #----- Define the percentage of sand, clay, and silt. ----------------------------------#
  sand <- 100. * sandfrac
  clay <- 100. * clayfrac
  silt <- 100. - sand - clay
  #---------------------------------------------------------------------------------------#
  
  #---------------------------------------------------------------------------------------#
  #     Here there is not much we can do other than explore where in the triangle space   #
  # we are.                                                                               #
  #---------------------------------------------------------------------------------------#
  
  if (any(silt > 100.) | any(silt < 0.) | any(sand > 100.) | 
      any(sand < 0.) | any(clay > 100.) | any(clay < 0.) ) {
    print("---------------------------------------------------")
    print(" At least one of your percentages is screwy...")
    print(paste("SAND <- ",sprintf("%.2f",sand),"%",sep=""))
    print(paste("CLAY <- ",sprintf("%.2f",clay),"%",sep=""))
    print(paste("SILT <- ",sprintf("%.2f",silt),"%",sep=""))
    print("---------------------------------------------------")
    stop ("This soil doesn''t fit into any category...")
    
  }
  nlayer = max(length(silt),length(clay),length(sand))
  mysoil = NA
  for(z in seq_len(nlayer)){
    if(sand[z] > 85.0 + 0.5 * clay[z]) {
      mysoil[z] <-  1 #----- Sand. ------------------------------------------------------------#
    }else if(sand[z] > 70.0 + clay[z]) {
      mysoil[z] <-  2 #----- Loamy sand. ------------------------------------------------------#
    }else if((clay[z] <= 20.0 & sand[z] > 52.5) | (clay[z] <= 7.5 & silt[z] <= 50.0)) {
      mysoil[z] <-  3 #----- Sandy loam. ------------------------------------------------------#
    }else if((clay[z] <= 27.5 & silt[z] > 50.0 & silt[z] <= 80.0) | (silt[z] >  80.0 & clay[z] > 12.5)) {
      mysoil[z] <-  4 #----- Silt loam. -------------------------------------------------------#
    }else if(clay[z] > 7.5 & clay[z] <= 27.5 & silt[z] > 27.5 & silt[z] <= 50.0 & sand[z] <= 52.5) {
      mysoil[z] <-  5 #----- Loam. ------------------------------------------------------------#
    }else if(clay[z] > 20.0 & clay[z] <= 35.0 & silt[z] <= 27.5 & sand[z] > 45.0) {
      mysoil[z] <-  6 #----- Sandy clay loam. -------------------------------------------------#
    }else if(clay[z] > 27.5 & clay[z] <= 40.0 & sand[z] <= 20.0) {
      mysoil[z] <-  7 #----- Silty clay loam. -------------------------------------------------#
    }else if(clay[z] > 27.5 & clay[z] <= 40.0 & sand[z] > 20.0 & sand[z] <= 45.0) {
      mysoil[z] <-  8 #----- Clayey loam. -----------------------------------------------------#
    }else if(clay[z] > 35.0 & sand[z] > 45.0) {
      mysoil[z] <-  9 #----- Sandy clay. ------------------------------------------------------#
    }else if(clay[z] > 40.0 & silt[z] > 40.0) {
      mysoil[z] <- 10 #----- Silty clay. ------------------------------------------------------#
    }else if(clay[z] <= 70.0 & sand[z] <= 30.0 & silt[z] <= 30.0) {
      mysoil[z] <- 11 #----- Clay. ------------------------------------------------------------#
    }else if( silt[z] > 80.0 & clay[z] <= 12.5) {
      mysoil[z] <- 14 #----- Silt. ------------------------------------------------------------#
    }else if( clay[z] > 70.0) {
      mysoil[z] <- 15 #----- Heavy clay. ------------------------------------------------------#
    }else if( clay[z] > 40.0 & sand[z] > 30.0 & sand[z] <= 45.0) {
      mysoil[z] <- 16 #----- Clayey sand. -----------------------------------------------------#
    }else if( clay[z] > 40.0 & silt[z] > 30.0 & silt[z] <= 40.0) {
      mysoil[z] <- 17 #----- Clayey silt. -----------------------------------------------------#
    }else{
      print("---------------------------------------------------")
      print(paste("SAND <- ",sprintf("%.2f",sand[z]),"%",sep=""))
      print(paste("CLAY <- ",sprintf("%.2f",clay[z]),"%",sep=""))
      print(paste("SILT <- ",sprintf("%.2f",silt[z]),"%",sep=""))
      print("---------------------------------------------------")
      stop ("This soil doesn''t fit into any category...")
    }#end if
  }
  return(mysoil)
}#end function


#' Convert a matric potential to a soil moisture
#'
#' @param mpot   water potential
#' @param mysoil soil property list
#'
#' @return
#' @export
#'
#' @examples
mpot2smoist <<- function(mpot,slpots,slbs,slmsts){
  smfrac = ( mpot / slpots) ^ (-1. / slbs)
  smoist = smfrac * slmsts
  return(smoist)
}#end function