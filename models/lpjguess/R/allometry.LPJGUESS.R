## Matthew Forrest 2019-06-19 Simple helper function transcribed from the LPJ-GUESS C++ to support the allocation funcrion below

# NEGLIGABLE
# Returns true if |dval| < exp(limit), otherwise false
##' @keywords internal
negligible <- function(dval, limit = -30) {
  if(abs(dval) < exp(limit)) return(TRUE)
  else return(FALSE)
}

# LAMBERT-BEER
##' @keywords internal
lambertbeer <- function(lai) {
  return(exp(-.5 * lai))
}




## 
## In LPJ-GUESS this individual would be killed as a result of any of these happening.  So instead PEcAn should
## modify the ratio of nudged biomass and density and try again.



###########################################
# // ALLOMETRY
# // Should be called to update allometry, FPC and FPC increment whenever biomass values
# // for a vegetation individual (cohort) change.

##' LPJ-GUESS allometry
##' 
##' The LPJ-GUESS allometry function transcribed into R.
##'
##' @param lifeform An integer code for the lifeform of this individual (cohort): 1 = Tree, 2 = Grass
##' @param cmass_leaf The leaf C pool size (kgC/m^2)
##' @param cmass_sap The sapwood C pool size (kgC/m^2)
##' @param cmass_heart The heartwood C pool size (kgC/m^2)
##' @param densindiv The density of individuals in the cohort (indiv/m^2) 
##' @param age The age of the coort
##' @param fpc The folar projective cover
##' @param deltafpc The change in foliar projective cover
##' @param sla The SLA (specific leaf area) (per PFT parameter)
##' @param k_latosa The leaf area to sapwood area ratio (per PFT parameter)
##' @param k_rp,k_allom1,k_allom2,k_allom3, Allometry coefficients (per PFT parameters)
##' @param wooddens Wood density (kgC/m^2) (per PFT parameter)
##' @param crownarea_max Maximum allowed crown area (m^2)  (per PFT parameter)
##' @param HEIGHT_MAX Maximum allowed height of an individual.  This is the maximum height that a tree
##' can have.  This is hard-coded in LPJ-GUESS to 150 m, but for SDA that might be unrealistically big, 
##' so this argument allows adjustment. 
##' 
##' This function was transcribed from LPJ-GUESS (v4.0) C++ to R for the purpose of nudging the LPJ-GUESS state offline.
##' The idea is of course to use the output from the analysis step from an SDA routine to provide the nudged values, although that isn't
##' relevant to the following code.
##'
##' Since the original C++ code took as its only argument an LPJ-GUESS C++ class of type 'Individual' there was no way (to my knowledge)
##' of directly compiling using Rcpp (unlike for allocation.cpp/allocation.R. which was easy to compile from the native C++ using 
##' Rcpp with very few changes).
##'
##' As noted in the original function header taken from the the C++ code (copied above), this function should be run after its biomass values 
##' have been updated.  In this case that means after the allocation() function has been applied to an individual.
##'  
##' This function can return following error codes:
##'  1.  "NegligibleLeafMass" - The individual has negligible leaf biomass.
##'  2.  "MaxHeightExceeded" - The indidual exceeds the maximum allowed height
##'  3.  "LowWoodDensity" - The individual's *actual* wood density drops below 90% of prescribed value.  This (slighty weird
##'  and unphysical) requirement is necessary because sometimes LPJ-GUESS can take carbon from the heartwood to
##'  ensure C-balance.  I think.  Or some other hockery-pockery.
##' 
##'  If all is well the code is simply "OK".
##'
##' @keywords internal
##' @return A named list of updated state variables for the individual/cohort.  The first value in the list is the error code. 
##' @author Matthew Forrest
##' 
allometry <- function(
  # initial allometry/pools
  lifeform, 
  cmass_leaf, 
  cmass_sap, 
  cmass_heart, 
  densindiv, 
  age, 
  fpc,
  deltafpc,
  # parameter values
  sla, 
  k_latosa, 
  k_rp,
  k_allom1,
  k_allom2, 
  k_allom3, 
  wooddens,
  crownarea_max,
  HEIGHT_MAX = 150) {
  
  # DESCRIPTION
  # Calculates tree allometry (height and crown area) and fractional projective
  # given carbon biomass in various compartments for an individual.
  
  # Returns true if the allometry is normal, otherwise false - guess2008
  
  # TREE ALLOMETRY
  # Trees aboveground allometry is modelled by a cylindrical stem comprising an
  # inner cylinder of heartwood surrounded by a zone of sapwood of constant radius,
  # and a crown (i.e. foliage) cylinder of known diameter. Sapwood and heartwood are
  # assumed to have the same, constant, density (wooddens). Tree height is related
  # to sapwood cross-sectional area by the relation:
  #   (1) height = cmass_sap / (sapwood xs area)
  # Sapwood cross-sectional area is also assumed to be a constant proportion of
  # total leaf area (following the "pipe model"; Shinozaki et al. 1964a,b; Waring
  # et al 1982), i.e.
  #   (2) (leaf area) = k_latosa * (sapwood xs area)
  # Leaf area is related to leaf biomass by specific leaf area:
  #   (3) (leaf area) = sla * cmass_leaf
  # From (1), (2), (3),
  #   (4) height = cmass_sap / wooddens / sla / cmass_leaf * k_latosa
  # Tree height is related to stem diameter by the relation (Huang et al 1992)
  # [** = raised to the power of]:
  #   (5) height = k_allom2 * diam ** k_allom3
  # Crown area may be derived from stem diameter by the relation (Zeide 1993):
  #   (6) crownarea = min ( k_allom1 * diam ** k_rp , crownarea_max )
  # Bole height (individual/cohort mode only; currently set to 0):
  #   (7) boleht = 0
  
  # FOLIAR PROJECTIVE COVER (FPC)
  # The same formulation for FPC (Eqn 8 below) is now applied in all vegetation
  # modes (Ben Smith 2002-07-23). FPC is equivalent to fractional patch/grid cell
  # coverage for the purposes of canopy exchange calculations and, in population
  # mode, vegetation dynamics calculations.
  #
  #   FPC on the modelled area (stand, patch, "grid-cell") basis is related to mean
  #   individual leaf area index (LAI) by the Lambert-Beer law (Monsi & Saeki 1953,
  #   Prentice et al 1993) based on the assumption that success of a PFT population
  #   in competition for space will be proportional to competitive ability for light
  #   in the vertical profile of the forest canopy:
  #     (8) fpc = crownarea * densindiv * ( 1.0 - exp ( -0.5 * lai_ind ) )
  #   where
  #     (9) lai_ind = cmass_leaf/densindiv * sla / crownarea
  #
  #   For grasses,
  #    (10) fpc = ( 1.0 - exp ( -0.5 * lai_ind ) )
  #    (11) lai_ind = cmass_leaf * sla
  
  diam = 0.0 # stem diameter (m)
  fpc_new = 0.0 # updated FPC
  
  # guess2008 - max tree height allowed (metre).
  # MF - removed to make this tuneable
  # HEIGHT_MAX = 150.0

  
  # MF - added for providing the error code
  error.string <- "OK"
  vol = 0
  
  if (lifeform == 1) {
    
    # TREES
    
    # Height (Eqn 4)
    
    # guess2008 - new allometry check
    if (!negligible(cmass_leaf)) {
      
      height = cmass_sap / cmass_leaf / sla * k_latosa / wooddens
      
      # Stem diameter (Eqn 5)
      #diam = pow(height / k_allom2, 1.0 / k_allom3)
      diam = (height / k_allom2) ^ (1.0 / k_allom3)
      
      # Stem volume
      vol = height * pi * diam * diam * 0.25
      
      
      # print("-----------------------------------------------------")
      # print(paste0("height = ", height))
      # print(paste0("vol = ", vol))
      # print(paste0("wooddens = ", wooddens))
      # print(paste0("densindiv = ", densindiv))
      # print(paste0("age = ", age))
      # print(paste0("cmass_heart = ", cmass_heart))
      # print(paste0("cmass_sap = ", cmass_sap))
      # print("-----------------------------------------------------")
      
      
      
      
      if (age > 0 & (cmass_heart + cmass_sap) / densindiv / vol < wooddens * 0.9) {
        error.string <- "LowWoodDensity"
      }
    }
    else {
      height = 0.0
      diam = 0.0
      error.string <- "NegligibleLeafMass"
    }
    
    
    # guess2008 - extra height check
    if (height > HEIGHT_MAX) {
      height = 0.0
      diam = 0.0
      error.string <- "MaxHeightExceeded"
    }
    
    
    # Crown area (Eqn 6)
    crownarea = min(k_allom1 * (diam ^ k_rp), crownarea_max)
    
    if (!negligible(crownarea)) {
      
      # Individual LAI (Eqn 9)
      lai_indiv = cmass_leaf / densindiv * sla / crownarea
      
      # FPC (Eqn 8)
      
      fpc_new = crownarea * densindiv *
        (1.0 - lambertbeer(lai_indiv))
      
      # Increment deltafpc
      deltafpc =  deltafpc + fpc_new - fpc
      fpc = fpc_new
    }
    else {
      lai_indiv = 0.0
      fpc = 0.0
    }
    
    # Bole height (Eqn 7)
    boleht = 0.0
    
    # Stand-level LAI
    lai = cmass_leaf * sla
  }
  
  # 
  else if (lifeform == 2) {
    
    # GRASSES
    
    # MF ignore land cover
    #if(indiv.pft.landcover != CROPLAND) {
    
    # guess2008 - bugfix - added if
    if (!negligible(cmass_leaf)) {
      
      # Grass "individual" LAI (Eqn 11)
      lai_indiv = cmass_leaf * sla
      
      # FPC (Eqn 10)
      fpc = 1.0 - lambertbeer(lai_indiv)
      
      # Stand-level LAI
      lai = lai_indiv
      
      # MF extra returns not normally defined for grasses but needed for the return list below
      vol =0
      height = 0
      diam = 0
      crownarea = 0
      deltafpc = 0
      boleht = 0
      
    }
    else {
      error.string <- "NegligibleLeafMass"
    }
  }
  # MF ignore land cover
  #else {
  
  # True crops use cmass_leaf_max, cover-crop grass uses lai of stands with whole-year grass growth
  #allometry_crop(indiv)
  #}
  #}
  
  # guess2008 - new return value (was void)
  # MF: return a list of the updated allometric state of this individual
  # this should be manually copied into the representation of the LPJ-GUESS state
  return(
    list(
      error.string = error.string,
      height = height,
      crownarea = crownarea,
      lai_indiv = lai_indiv,
      lai = lai,
      deltafpc = deltafpc,
      fpc = fpc,
      boleht = boleht)
  )
  
}

