#' Estimate soil parameters from texture class or sand/silt/clay
#'
#' @param soil_class USDA Soil Class. See Details
#' @param sand       percent sand
#' @param silt       percent silt
#' @param clay       percent clay
#'
#' @details 
#' * Specify _either_ soil_class or sand/silt/clay. soil_class will be ignored if sand/silt/clay is provided
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
#' sand <- 0.3
#' clay <- 0.3
soil_params <- function(soil_class,sand,silt,clay){

  ## load soil parameters
  data("soil_class")
  mysoil <- list()
  
  #---------------------------------------------------------------------------------------#
  #     Find soil class and sand, silt, and clay fractions.                               #
  #---------------------------------------------------------------------------------------#
  if (missing(sand) & missing(clay)){
    ## insufficient texture data, infer from soil_class
    if(missing(soil_class)) PEcAn.utils::logger.error("insufficient arguments")
    mysoil$soil_class <- soil_class
    mysoil$soil_n <- which(toupper(soil.name) == toupper(soil_class))
    mysoil$key   <- soil.key [mysoil$soil_n]
    mysoil$xsand <- xsand.def[soil_class]
    mysoil$xclay <- xclay.def[soil_class]
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
      if(stot > 2) stot <- stot*100 ## assume values reported in % not proportion
      sand <- sand/stot
      silt <- silt/stot
      clay <- clay/stot
    }
    
    mysoil$soil_n <- sclass(sand,clay)
    mysoil$soil_class  <- soil.name[mysoil$soil_n]
    mysoil$key   <- soil.key [mysoil$soil_n]
    mysoil$xsand <- sand
    mysoil$xclay <- clay
    mysoil$xsilt <- 1. - mysoil$xsand - mysoil$xclay
  }
  #---------------------------------------------------------------------------------------#
  
  
  
  
  #---------------------------------------------------------------------------------------#
  #       Set up primary properties.                                                      #
  #---------------------------------------------------------------------------------------#
  if (mysoil$soil_n == 13){
    #----- Bedrock.  Most things are zero, because it is an impermeable soil. -----------#
    mysoil$slbs      <-  0.
    mysoil$slpots    <-  0.
    mysoil$slcons    <-  0.
    mysoil$slmsts    <-  0.
    mysoil$sfldcap   <-  0.
    mysoil$soilcp    <-  0.
    mysoil$soilwp    <-  0.
    mysoil$slcpd     <-  2130000.
    #------------------------------------------------------------------------------------#
  }else if (mysoil$soil_n == 12){
    #------------------------------------------------------------------------------------#
    #      Peat.  High concentration of organic matter.  Mineral soil equations don't    #
    # apply here.                                                                        #
    #------------------------------------------------------------------------------------#
    mysoil$slbs    <-  6.180000
    mysoil$slpots  <- -0.534564359
    mysoil$slcons  <-  2.357930e-6
    mysoil$slmsts  <-  0.469200
    mysoil$sfldcap <-  0.285709966
    mysoil$slcpd   <-  874000.
    #------------------------------------------------------------------------------------#
  }else{
    #------------------------------------------------------------------------------------#
    #      Mineral soil.  Use the standard Cosby et al 1984 eqns                         #
    #------------------------------------------------------------------------------------#
    ## TO-DO: Cosby Table 4 has equations for soil property STANDARD DEVIATIONS in addition to means
    ## in future, upgrade to return these and do ensemble sampling
    
    # B exponent [unitless]
    mysoil$slbs    <- 3.10 + 15.7*mysoil$xclay - 0.3*mysoil$xsand
    
    # Soil moisture potential at saturation [ m ]
    mysoil$slpots  <- -0.01 * (10.^(2.17 - 0.63*mysoil$xclay - 1.58*mysoil$xsand))
    
    # Hydraulic conductivity at saturation [ m/s ]
    mysoil$slcons  <- udunits2::ud.convert(10.^(-0.60 + 1.26*mysoil$xsand - 0.64*mysoil$xclay),
                                           "inch/hour","meters/second") 
    
    # Soil moisture at saturation [ m^3/m^3 ]
    mysoil$slmsts  <- (50.5 - 14.2*mysoil$xsand - 3.7*mysoil$xclay) / 100.
    
    # Soil field capacity[ m^3/m^3 ]
    mysoil$sfldcap <- mysoil$slmsts * ( fieldcp.K/mysoil$slcons)^ (1. / (2.*mysoil$slbs+3.))
  
    #---------------------------------------------------------------------------------!
    #     Heat capacity.  Here we take the volume average amongst silt, clay, and     !
    # sand, and consider the contribution of air sitting in.  In order to keep it     !
    # simple, we assume that the air fraction won't change, although in reality its   !
    # contribution should be a function of soil moisture.  Here we use the amount of  !
    # air in case the soil moisture was halfway between dry air and saturated, so the !
    # error is not too biased.                                                        !
    #---------------------------------------------------------------------------------!
    mysoil$slcpd   <- ( (1. - mysoil$slmsts)
                       * ( mysoil$xsand * sand.hcap + mysoil$xsilt * silt.hcap
                           + mysoil$xclay * clay.hcap )
                       + 0.5 * (mysoil$slmsts - mysoil$soilcp) * air.hcap )

  } ## end primary properties
  
  #---------------------------------------------------------------------------------------#
  #      Calculate the derived properties in case this is not bedrock.                    #
  #---------------------------------------------------------------------------------------#
  if (mysoil$soil_n != 13){
    # Dry soil capacity (at -3.1MPa) [ m^3/m^3 ]
    mysoil$slpotcp   <- - soilcp.MPa * 1000. / grav
    mysoil$soilcp    <- mpot2smoist(mysoil$slpotcp, mysoil)
    
    # Wilting point capacity (at -1.5MPa) [ m^3/m^3 ]
    mysoil$slpotwp   <- - soilwp.MPa * 1000. / grav
    mysoil$soilwp    <- mpot2smoist(mysoil$slpotwp, mysoil)
    
    # Water potential for field capacity            [ m]
    # mysoil$slpotfc   <- smoist2mpot(mysoil$sfldcap, mysoil)
  } else {
    mysoil$slpotcp = mysoil$soilcp = mysoil$slpotwp = soilwp = 0.0
  }#end if
  
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
  ksand <- 3. * h2o.cond / ( 2. * h2o.cond + sand.cond )
  ksilt <- 3. * h2o.cond / ( 2. * h2o.cond + silt.cond )
  kclay <- 3. * h2o.cond / ( 2. * h2o.cond + clay.cond )
  kair  <- 3. * h2o.cond / ( 2. * h2o.cond +  air.cond )
  
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
  
  if (silt > 100. | silt < 0. | sand > 100. | sand < 0. | clay > 100. | clay < 0. ) {
    print("---------------------------------------------------")
    print(" At least one of your percentages is screwy...")
    print(paste("SAND <- ",sprintf("%.2f",sand),"%",sep=""))
    print(paste("CLAY <- ",sprintf("%.2f",clay),"%",sep=""))
    print(paste("SILT <- ",sprintf("%.2f",silt),"%",sep=""))
    print("---------------------------------------------------")
    stop ("This soil doesn''t fit into any category...")
    
  }else if(sand > 85.0 + 0.5 * clay) {
    mysoil <-  1 #----- Sand. ------------------------------------------------------------#
  }else if(sand > 70.0 + clay) {
    mysoil <-  2 #----- Loamy sand. ------------------------------------------------------#
  }else if((clay <= 20.0 & sand > 52.5) | (clay <= 7.5 & silt <= 50.0)) {
    mysoil <-  3 #----- Sandy loam. ------------------------------------------------------#
  }else if((clay <= 27.5 & silt > 50.0 & silt <= 80.0) | (silt >  80.0 & clay > 12.5)) {
    mysoil <-  4 #----- Silt loam. -------------------------------------------------------#
  }else if(clay > 7.5 & clay <= 27.5 & silt > 27.5 & silt <= 50.0 & sand <= 52.5) {
    mysoil <-  5 #----- Loam. ------------------------------------------------------------#
  }else if(clay > 20.0 & clay <= 35.0 & silt <= 27.5 & sand > 45.0) {
    mysoil <-  6 #----- Sandy clay loam. -------------------------------------------------#
  }else if(clay > 27.5 & clay <= 40.0 & sand <= 20.0) {
    mysoil <-  7 #----- Silty clay loam. -------------------------------------------------#
  }else if(clay > 27.5 & clay <= 40.0 & sand > 20.0 & sand <= 45.0) {
    mysoil <-  8 #----- Clayey loam. -----------------------------------------------------#
  }else if(clay > 35.0 & sand > 45.0) {
    mysoil <-  9 #----- Sandy clay. ------------------------------------------------------#
  }else if(clay > 40.0 & silt > 40.0) {
    mysoil <- 10 #----- Silty clay. ------------------------------------------------------#
  }else if(clay <= 70.0 & sand <= 30.0 & silt <= 30.0) {
    mysoil <- 11 #----- Clay. ------------------------------------------------------------#
  }else if( silt > 80.0 & clay <= 12.5) {
    mysoil <- 14 #----- Silt. ------------------------------------------------------------#
  }else if( clay > 70.0) {
    mysoil <- 15 #----- Heavy clay. ------------------------------------------------------#
  }else if( clay > 40.0 & sand > 30.0 & sand <= 45.0) {
    mysoil <- 16 #----- Clayey sand. -----------------------------------------------------#
  }else if( clay > 40.0 & silt > 30.0 & silt <= 40.0) {
    mysoil <- 17 #----- Clayey silt. -----------------------------------------------------#
  }else{
    print("---------------------------------------------------")
    print(paste("SAND <- ",sprintf("%.2f",sand),"%",sep=""))
    print(paste("CLAY <- ",sprintf("%.2f",clay),"%",sep=""))
    print(paste("SILT <- ",sprintf("%.2f",silt),"%",sep=""))
    print("---------------------------------------------------")
    stop ("This soil doesn''t fit into any category...")
  }#end if
  
  return(mysoil)
}#end function