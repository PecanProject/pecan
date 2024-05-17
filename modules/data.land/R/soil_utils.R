#' Estimate soil parameters from texture class or sand/silt/clay
#'
#' @param soil_type USDA Soil Class. See Details
#' @param sand       percent sand
#' @param silt       percent silt
#' @param clay       percent clay
#' @param bulk       soil bulk density (optional, kg m-3)
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
#' @return list of soil hydraulic and thermal parameters
#' @export
#' @importFrom rlang %||%
#' @examples 
#' sand <- c(0.3, 0.4, 0.5)
#' clay <- c(0.3, 0.3, 0.3)
#' soil_params(sand=sand,clay=clay)
soil_params <- function(soil_type=NULL, sand=NULL, silt=NULL, clay=NULL, bulk=NULL){
  ## load soil parameters
  mysoil <- list()
  # 'soil_class' is package data, lazy-loaded here when needed
  #   see data-raw/build_soil_texture_variables.R
  air.cond <- PEcAn.data.land::soil_class$air.cond
  air.hcap <- PEcAn.data.land::soil_class$air.hcap
  clay.cond <- PEcAn.data.land::soil_class$clay.cond
  clay.hcap <- PEcAn.data.land::soil_class$clay.hcap
  fieldcp.K <- PEcAn.data.land::soil_class$fieldcp.K
  grav <- PEcAn.data.land::soil_class$grav
  h2o.cond <- PEcAn.data.land::soil_class$h2o.cond
  kair <- PEcAn.data.land::soil_class$kair
  kclay <- PEcAn.data.land::soil_class$kclay
  ksand <- PEcAn.data.land::soil_class$ksand
  ksilt <- PEcAn.data.land::soil_class$ksilt
  sand.cond <- PEcAn.data.land::soil_class$sand.cond
  sand.hcap <- PEcAn.data.land::soil_class$sand.hcap
  silt.cond <- PEcAn.data.land::soil_class$silt.cond
  silt.hcap <- PEcAn.data.land::soil_class$silt.hcap
  soil.name <- PEcAn.data.land::soil_class$soil.name
  soilcp.MPa <- PEcAn.data.land::soil_class$soilcp.MPa
  soilwp.MPa <- PEcAn.data.land::soil_class$soilwp.MPa
  texture <- PEcAn.data.land::soil_class$texture
  xclay.def <- PEcAn.data.land::soil_class$xclay.def
  xsand.def <- PEcAn.data.land::soil_class$xsand.def
  
  #---------------------------------------------------------------------------------------#
  #     Find soil class and sand, silt, and clay fractions.                               #
  #---------------------------------------------------------------------------------------#
  if (sum(is.null(sand), is.null(silt), is.null(clay)) > 1) {
    ## insufficient texture data, infer from soil_type
    if (is.null(soil_type)) {
      PEcAn.logger::logger.severe(
        "Insufficient arguments:",
        "Must specify either soil_type or at least 2 of sand, silt, clay")
    }
    mysoil$soil_type <- soil_type
    mysoil$soil_n <- which(toupper(soil.name) == toupper(soil_type))
    sand <- xsand.def[soil_type]
    clay <- xclay.def[soil_type]
  } else {
    if (any(c(sand, silt, clay) > 2)) {
      # assume values reported in % not proportion
      sand <- if (is.null(sand)) { NULL } else { sand / 100 }
      silt <- if (is.null(silt)) { NULL } else { silt / 100 }
      clay <- if (is.null(clay)) { NULL } else { clay / 100 }
    }
    # compute up to one missing value (>1 missing was handled above)
    # %||% is a null-filling operator: A %||% B == "B if A is null, A in all other cases"
    sand <- sand %||% (1-silt-clay)
    silt <- silt %||% (1-sand-clay)
    clay <- clay %||% (1-sand-silt)
    #normalize
    stot <- sand + silt + clay
    sand <- sand / stot
    silt <- silt / stot
    clay <- clay / stot
    mysoil$soil_n <- sclass(sand,clay)
    mysoil$soil_type  <- soil.name[mysoil$soil_n]
  }
  # mysoil$key   <- soil.key [mysoil$soil_n] # turning off these abreviations since they lack a CF equivalent
  mysoil$fraction_of_sand_in_soil <- sand
  mysoil$fraction_of_clay_in_soil <- clay
  mysoil$fraction_of_silt_in_soil <- 1. - mysoil$fraction_of_sand_in_soil - mysoil$fraction_of_clay_in_soil
  #---------------------------------------------------------------------------------------#
  
  if(!is.null(bulk)) mysoil$soil_bulk_density = bulk
  
  #---------------------------------------------------------------------------------------#
  #       Set up primary properties.                                                      #
  #---------------------------------------------------------------------------------------#
  for(z in which(mysoil$soil_n == 13)){
    #----- Bedrock.  Most things are zero, because it is an impermeable soil. -----------#
    mysoil$soil_hydraulic_b[z]      <-  0.
    mysoil$soil_water_potential_at_saturation[z]    <-  0.
    mysoil$soil_hydraulic_conductivity_at_saturation[z]    <-  0.
    mysoil$volume_fraction_of_water_in_soil_at_saturation[z]    <-  0.
    mysoil$volume_fraction_of_water_in_soil_at_field_capacity[z]   <-  0.
    mysoil$volume_fraction_of_condensed_water_in_dry_soil[z]    <-  0.
    mysoil$volume_fraction_of_condensed_water_in_soil_at_wilting_point[z]    <-  0.
    mysoil$slcpd[z]     <-  2130000.
    #------------------------------------------------------------------------------------#
  }
  for(z in which(mysoil$soil_n == 12)){
    #------------------------------------------------------------------------------------#
    #      Peat.  High concentration of organic matter.  Mineral soil equations don't    #
    # apply here.                                                                        #
    #------------------------------------------------------------------------------------#
    mysoil$soil_hydraulic_b[z]    <-  6.180000
    mysoil$soil_water_potential_at_saturation[z]  <- -0.534564359
    mysoil$soil_hydraulic_conductivity_at_saturation[z]  <-  2.357930e-6
    mysoil$volume_fraction_of_water_in_soil_at_saturation[z]  <-  0.469200
    mysoil$volume_fraction_of_water_in_soil_at_field_capacity[z] <-  0.285709966
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
    mysoil$soil_hydraulic_b[z]    <- 3.10 + 15.7*mysoil$fraction_of_clay_in_soil[z] - 0.3*mysoil$fraction_of_sand_in_soil[z]
    
    # Soil moisture potential at saturation [ m ]
    mysoil$soil_water_potential_at_saturation[z]  <- -0.01 * (10.^(2.17 - 0.63*mysoil$fraction_of_clay_in_soil[z] - 1.58*mysoil$fraction_of_sand_in_soil[z]))
    
    # Hydraulic conductivity at saturation [ m/s ]
    mysoil$soil_hydraulic_conductivity_at_saturation[z]  <- PEcAn.utils::ud_convert(
      10.^(-0.60 
           + 1.26*mysoil$fraction_of_sand_in_soil[z]
           - 0.64*mysoil$fraction_of_clay_in_soil[z]),
      "inch/hour",
      "meters/second") 
    
    # Soil moisture at saturation [ m^3/m^3 ]
    mysoil$volume_fraction_of_water_in_soil_at_saturation[z]  <- (50.5 - 14.2*mysoil$fraction_of_sand_in_soil[z] - 3.7*mysoil$fraction_of_clay_in_soil[z]) / 100.
    
    # Soil field capacity[ m^3/m^3 ]
    mysoil$volume_fraction_of_water_in_soil_at_field_capacity[z] <- mysoil$volume_fraction_of_water_in_soil_at_saturation[z] * ( fieldcp.K/mysoil$soil_hydraulic_conductivity_at_saturation[z])^ (1. / (2.*mysoil$soil_hydraulic_b[z]+3.))
  } ## end primary properties
  
  #---------------------------------------------------------------------------------------#
  #      Calculate the derived properties in case this is not bedrock.                    #
  #---------------------------------------------------------------------------------------#
  mysoil$slpotcp = mysoil$volume_fraction_of_condensed_water_in_dry_soil = mysoil$slpotwp = mysoil$volume_fraction_of_condensed_water_in_soil_at_wilting_point = 0.0
  for(z in which(!(mysoil$soil_n == 13))){
    # Dry soil capacity (at -3.1MPa) [ m^3/m^3 ]
    mysoil$slpotcp[z]   <- - soilcp.MPa * 1000. / grav
    mysoil$volume_fraction_of_condensed_water_in_dry_soil[z]    <- mpot2smoist(mysoil$slpotcp[z],mysoil$soil_water_potential_at_saturation[z],mysoil$soil_hydraulic_b[z],mysoil$volume_fraction_of_water_in_soil_at_saturation[z])
    
    # Wilting point capacity (at -1.5MPa) [ m^3/m^3 ]
    mysoil$slpotwp[z]   <- - soilwp.MPa * 1000. / grav
    mysoil$volume_fraction_of_condensed_water_in_soil_at_wilting_point[z]    <- mpot2smoist(mysoil$slpotwp[z], mysoil$soil_water_potential_at_saturation[z],mysoil$soil_hydraulic_b[z],mysoil$volume_fraction_of_water_in_soil_at_saturation[z])
    
    # Water potential for field capacity            [ m]
    # mysoil$slpotfc   <- smoist2mpot(mysoil$volume_fraction_of_water_in_soil_at_field_capacity, mysoil)
    
    #---------------------------------------------------------------------------------!
    #     Specific heat of dry soil                     [  J/m3/K]                    !
    # Here we take the volume average amongst silt, clay, and                         !
    # sand, and consider the contribution of air sitting in.  In order to keep it     !
    # simple, we assume that the air fraction won't change, although in reality its   !
    # contribution should be a function of soil moisture.  Here we use the amount of  !
    # air in case the soil moisture was halfway between dry air and saturated, so the !
    # error is not too biased.                                                        !
    #---------------------------------------------------------------------------------!
    mysoil$slcpd[z]   <- (1. - mysoil$volume_fraction_of_water_in_soil_at_saturation[z]) * 
      ( mysoil$fraction_of_sand_in_soil[z] * sand.hcap + 
          mysoil$fraction_of_silt_in_soil[z] * silt.hcap +
          mysoil$fraction_of_clay_in_soil[z] * clay.hcap ) +
      0.5 * (mysoil$volume_fraction_of_water_in_soil_at_saturation[z] - 
               mysoil$volume_fraction_of_condensed_water_in_dry_soil[z]) * air.hcap
    
  }
  
  #---------------------------------------------------------------------------------------#
  #      Soil thermal conductivity.     W/m/K                                             #
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
  mysoil$thcond0 <- ( ksand * mysoil$fraction_of_sand_in_soil  * ( 1. - mysoil$volume_fraction_of_water_in_soil_at_saturation ) * sand.cond 
                      + ksilt * mysoil$fraction_of_silt_in_soil  * ( 1. - mysoil$volume_fraction_of_water_in_soil_at_saturation ) * silt.cond
                      + kclay * mysoil$fraction_of_clay_in_soil  * ( 1. - mysoil$volume_fraction_of_water_in_soil_at_saturation ) * clay.cond
                      + kair                  *       mysoil$volume_fraction_of_water_in_soil_at_saturation    *  air.cond  )
  mysoil$thcond1 <- rep(h2o.cond - kair * air.cond,length=length(mysoil$thcond0))
  mysoil$thcond2 <- ( ksand * mysoil$fraction_of_sand_in_soil  * ( 1. - mysoil$volume_fraction_of_water_in_soil_at_saturation )
                      + ksilt * mysoil$fraction_of_silt_in_soil  * ( 1. - mysoil$volume_fraction_of_water_in_soil_at_saturation )
                      + kclay * mysoil$fraction_of_clay_in_soil  * ( 1. - mysoil$volume_fraction_of_water_in_soil_at_saturation )
                      + kair                  *        mysoil$volume_fraction_of_water_in_soil_at_saturation   )
  mysoil$thcond3 <- rep(1. - kair,length=length(mysoil$thcond0))
  ## default soil thermal conductivity = dry
  mysoil$soil_thermal_conductivity <- ( mysoil$thcond0 + mysoil$thcond1 * mysoil$volume_fraction_of_condensed_water_in_dry_soil) /
    ( mysoil$thcond2 + mysoil$thcond3 * mysoil$volume_fraction_of_condensed_water_in_dry_soil)
  mysoil$soil_thermal_conductivity_at_saturation <- ( mysoil$thcond0 + mysoil$thcond1 * mysoil$volume_fraction_of_water_in_soil_at_saturation) /
    ( mysoil$thcond2 + mysoil$thcond3 * mysoil$volume_fraction_of_water_in_soil_at_saturation)
  
  #---------------------------------------------------------------------------------------#
  
  ## final values to look up
  for(z in which(!(mysoil$soil_n <= 13))){
    mysoil$soil_albedo[z] <- texture$albdry[mysoil$soil_n[z]]
    if(is.null(bulk))  mysoil$soil_bulk_density[z] <- texture$xrobulk[mysoil$soil_n[z]]
    mysoil$slden[z]       <- texture$slden[mysoil$soil_n[z]]
  }
  for(z in which(!(mysoil$soil_n > 13))){
    ## if lack class-specific values, use across-soil average
    mysoil$soil_albedo[z] <- stats::median(texture$albdry)
    if(is.null(bulk)) mysoil$soil_bulk_density[z] <- stats::median(texture$xrobulk)
    mysoil$slden[z]       <- stats::median(texture$slden)
  }
  
  ## Conversions to standard variables
  mysoil$soil_thermal_capacity <- mysoil$slcpd / mysoil$soil_bulk_density   ## J/m3/K / [kg m-3] -> J/kg/K
  
  ## drop variables that are only meaningful internally
  #mysoil$slpotcp <- NULL
  #mysoil$slpotwp <- NULL
  #mysoil$slden <- NULL ## not clear how this is is different from bulk density in the look-up-table
  #mysoil$slcpd <- NULL
  
  return(mysoil)
}#end function
#==========================================================================================#
#==========================================================================================#






#' This function determines the soil class number based on the fraction of sand, clay, and silt
#'
#' @param sandfrac 
#' @param clayfrac 
#'
#' @return vector of integers identifying textural class of each input layer.
#'  Possible values are 1 through 17; NB these are NOT the same class
#'  boundaries as the 12 USDA soil texture classes.
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
    PEcAn.logger::logger.warn(" At least one of your percentages is screwy...")
    PEcAn.logger::logger.warn(paste("SAND <- ",sprintf("%.2f",sand),"%",sep=""))
    PEcAn.logger::logger.warn(paste("CLAY <- ",sprintf("%.2f",clay),"%",sep=""))
    PEcAn.logger::logger.warn(paste("SILT <- ",sprintf("%.2f",silt),"%",sep=""))
    PEcAn.logger::logger.severe("This soil doesn''t fit into any category...")
    
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
      PEcAn.logger::logger.warn(paste("SAND <- ",sprintf("%.2f",sand[z]),"%",sep=""))
      PEcAn.logger::logger.warn(paste("CLAY <- ",sprintf("%.2f",clay[z]),"%",sep=""))
      PEcAn.logger::logger.warn(paste("SILT <- ",sprintf("%.2f",silt[z]),"%",sep=""))
      PEcAn.logger::logger.severe ("This soil doesn''t fit into any category...")
    }#end if
  }
  return(mysoil)
}#end function


#' Convert a matric potential to a soil moisture
#'
#' @param mpot   water potential
#' @param mysoil soil property list
#'
#' @return volumetric soil water content
#' @export
#'
#' 
mpot2smoist <- function(mpot,soil_water_potential_at_saturation,soil_hydraulic_b,volume_fraction_of_water_in_soil_at_saturation){
  smfrac = ( mpot / soil_water_potential_at_saturation) ^ (-1. / soil_hydraulic_b)
  smoist = smfrac * volume_fraction_of_water_in_soil_at_saturation
  return(smoist)
}#end function