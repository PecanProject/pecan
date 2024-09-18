# R Code to convert NetCDF CF met files into LPJ-GUESS met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' read binary state file of LPJ-GUESS
##'
##' @title readStateBinary.LPJGUESS
##' @export
##' @param out.path location on disk where model run outputs are stored
##' @param npft number of pfts specified in instruction file
##' @return Patchpft_list state variables common to all individuals of a particular PFT 
##' @author Istem Fer
readStateBinary <- function(out.path, npft){
  # test path
  out.path = "/fs/data2/output/PEcAn_1000002393/out/1000458390"
  setwd(out.path)
  
  
  Patchpft_list <- list()
  Vegetation_list <- list()
  Individual_list <- list()
  Soil_list <- list()
  Sompool_list <- list()
  SompoolCent_list <- list()  
  Fluxes_list <- list()
  
  # open connection to the binary state file
  zz <- file("0.state", "rb")
  
  ##################### Class : Climate #####################
  Climate <- list()
  
  # mean air temperature today (deg C)
  Climate$temp <- readBin(zz, double(), 1, size = 8)
  
  # total daily net downward shortwave solar radiation today (J/m2/day)
  Climate$rad <- readBin(zz, double(), 1, size = 8)
  
  # total daily photosynthetically-active radiation today (J/m2/day)
  Climate$par <- readBin(zz, double(), 1, size = 8)
  
  # precipitation today (mm)
  Climate$prec <- readBin(zz, double(), 1, size = 8)
  
  # day length today (h)
  Climate$daylength <- readBin(zz, double(), 1, size = 8)
  
  # atmospheric ambient CO2 concentration today (ppmv)
  Climate$co2 <- readBin(zz, double(), 1, size = 8)
  
  # latitude (degrees; +=north, -=south)
  Climate$lat <- readBin(zz, double(), 1, size = 8)
  
  # Insolation today, see also instype
  Climate$insol <- readBin(zz, double(), 1, size = 8)
  
  # Type of insolation
  Climate$instype <- readBin(zz, integer(), 1, size = 4)
  
  # equilibrium evapotranspiration today (mm/day)
  Climate$eet <- readBin(zz, double(), 1, size = 8)
  
  # mean temperature for the last 31 days (deg C)
  Climate$mtemp <- readBin(zz, double(), 1, size = 8)
  
  # mean of lowest mean monthly temperature for the last 20 years (deg C)
  Climate$mtemp_min20 <- readBin(zz, double(), 1, size = 8)
  
  # mean of highest mean monthly temperature for the last 20 years (deg C)
  Climate$mtemp_max20 <- readBin(zz, double(), 1, size = 8)
  
  # highest mean monthly temperature for the last 12 months (deg C)
  Climate$mtemp_max <- readBin(zz, double(), 1, size = 8)
  
  # accumulated growing degree day sum on 5 degree base 
  Climate$gdd5 <- readBin(zz, double(), 1, size = 8)
  
  # total gdd5 (accumulated) for this year (reset 1 January)
  Climate$agdd5 <- readBin(zz, double(), 1, size = 8)
  
  # number of days with temperatures <5 deg C   bytes[18,1]
  Climate$chilldays <- readBin(zz, integer(), 1, size = 4)
  
  # true if chill day count may be reset by temperature fall below 5 deg C
  Climate$ifsensechill <- readBin(zz, logical(), 1, size = 1)
  
  # Respiration response to today's air temperature incorporating damping of Q10 due to temperature acclimation (Lloyd & Taylor 1994)
  Climate$gtemp <- readBin(zz, double(), 1, size = 8)
  
  # daily temperatures for the last 31 days (deg C)
  Climate$dtemp_31 <- readBin(zz, double(), 31, size = 8)
  
  Climate$unk1 <- readBin(zz, double(), 1, size = 8)
  Climate$unk2 <- readBin(zz, logical(), 1, size = 1)
  
  # minimum monthly temperatures for the last 20 years (deg C)
  Climate$mtemp_min_20 <- readBin(zz, double(), 20, size = 8)
  
  # maximum monthly temperatures for the last 20 years (deg C) - bytes[25,1]
  Climate$mtemp_max_20 <- readBin(zz, double(), 20, size = 8)
  
  # minimum monthly temperature for the last 12 months (deg C)
  Climate$mtemp_min <- readBin(zz, double(), 1, size = 8)
  
  # mean of monthly temperatures for the last 12 months (deg C)
  Climate$atemp_mean <- readBin(zz, double(), 1, size = 8)
  
  
  
  
  # Saved parameters used by function daylengthinsoleet
  Climate$sinelat <- readBin(zz, double(), 1, size = 8)
  Climate$cosinelat <- readBin(zz, double(), 1, size = 8)
  
  Climate$qo <- readBin(zz, double(), 365, size = 8)
  Climate$u <- readBin(zz, double(), 365, size = 8)
  Climate$v <- readBin(zz, double(), 365, size = 8)
  Climate$hh <- readBin(zz, double(), 365, size = 8)
  Climate$sinehh <- readBin(zz, double(), 365, size = 8)
  Climate$daylength_save <- readBin(zz, double(), 365, size = 8)
  
  # indicates whether saved values exist for this day - bytes[36,1]
  Climate$doneday <- readBin(zz, logical(), 365, size = 1) 
  
  
  # /// diurnal temperature range, used in daily/monthly BVOC (deg C)
  # double dtr;
  # 
  # /// Sub-daily temperature (deg C) (\see temp)
  # std::vector<double> temps;
  # 
  # /// Sub-daily insolation (\see insol)
  # std::vector<double> insols;
  # 
  # /// Sub-daily PAR (\see par)
  # std::vector<double> pars;
  # 
  # /// Sub-daily net downward shortwave solar radiation (\see rad)
  # std::vector<double> rads;
  # 
  # /// Sub-daily respiration response (\see gtemp)
  # std::vector<double> gtemps;
  
  
  # annual nitrogen deposition (kgN/m2/year)
  Climate$andep <- readBin(zz, double(), 1, size = 8)
  # daily nitrogen deposition (kgN/m2)
  Climate$dndep <- readBin(zz, double(), 1, size = 8)
  
  
  # /// annual nitrogen fertilization (kgN/m2/year)
  Climate$anfert<- readBin(zz, double(), 1, size = 8)
  # /// daily nitrogen fertilization (kgN/m2/year)
  Climate$dnfert <- readBin(zz, double(), 1, size = 8)
  
  ##################### Class : Gridcell #####################
  Gridcell <- list()
  
  Gridcell$landcoverfrac <- readBin(zz, double(), 6, size = 8) 
  Gridcell$landcoverfrac_old <- readBin(zz, double(), 6, size = 8) 
  Gridcell$LC_updated <- readBin(zz, logical(), 1, size = 1) 
  Gridcell$seed <- readBin(zz, integer(), 1, size = 8) # C type long
  
  ##################### Class : Gridcellpft #####################
  Gridcellpft <- list()
  
  # annual degree day sum above threshold damaging temperature
  Gridcellpft$addtw <- rep(NA,npft) # npft : number of PFTs
  # Michaelis-Menten kinetic parameters 
  Gridcellpft$Km <- rep(NA,npft)
  
  for(p in 1:npft){
    Gridcellpft$addtw[p] <- readBin(zz, double(), 1, size = 8)
    Gridcellpft$Km[p] <- readBin(zz, double(), 1, size = 8)
  }
  
  
  ##################### Class : Stand #####################
  Stand <- list()
  
  Stand$unk1 <- readBin(zz, integer(), 1, size = 4) # ???
  Stand$unk2 <- readBin(zz, integer(), 1, size = 4) # ??? 
  
  ##################### Class : Standpft #####################
  Standpft <- list()
  Standpft$cmass_repr <- rep(NA,npft)
  Standpft$anetps_ff_max <- rep(NA,npft)
  Standpft$fpc_total <- rep(NA,npft)
  Standpft$active <- rep(NA,npft)
  
  for(p in 1:npft){
    # net C allocated to reproduction for this PFT in all patches of this stand this year (kgC/m2)
    Standpft$cmass_repr[p] <- readBin(zz, double(), 1, size = 8)
    
    # maximum value of Patchpft::anetps_ff for this PFT in this stand so far in the simulation (kgC/m2/year)
    Standpft$anetps_ff_max[p] <- readBin(zz, double(), 1, size = 8)
    
    # FPC sum for this PFT as average for stand
    Standpft$fpc_total[p] <- readBin(zz, double(), 1, size = 8)
    
    # Is this PFT allowed to grow in this stand?
    Standpft$active[p] <- readBin(zz, logical(), 1, size = 1)
  } 
  
  # number of patches to loop around
  nofpatch <- readBin(zz, integer(), 1, size = 4)  
  
  for(pat in 1:nofpatch){
    Patchpft_list[[pat]] <- getClass_Patchpft()
    Vegetation_list[[pat]] <- getClass_Vegetation()
    Individual_list[[pat]] <- getClass_Individual(Vegetation_list[[pat]]$indv)
    Soil_list[[pat]] <- getClass_Soil()
    Sompool_list[[pat]] <- getClass_Sompool()
    SompoolCent_list[[pat]] <- getClass_SompoolCent() 
    Fluxes_list[[pat]] <- getClass_Fluxes()
  }
  
  # close connection to the binary state file
  close(zz)
  
  return(Patchpft_list)
}



##################### Class : Patchpft #####################
getClass_Patchpft <- function(){
  
  Patchpft <- list()
  
  # potential annual net assimilation (leaf-level net photosynthesis) at forest floor (kgC/m2/year)
  Patchpft$anetps_ff <- rep(NA,npft)
  
  # water stress parameter (0-1 range; 1=minimum stress)
  Patchpft$wscal <- rep(NA,npft)
  
  # running sum (converted to annual mean) for wscal
  Patchpft$wscal_mean <- rep(NA,npft)
  
  
  # potential annual net assimilation at forest floor averaged over establishment interval (kgC/m2/year)
  Patchpft$anetps_ff_est <- rep(NA,npft)
  
  # first-year value of anetps_ff_est
  Patchpft$anetps_ff_est_initial <- rep(NA,npft)
  
  
  # annual mean wscal averaged over establishment interval
  Patchpft$wscal_mean_est <- rep(NA,npft)
  
  # vegetation phenological state (fraction of potential leaf cover), updated daily		
  Patchpft$phen <- rep(NA,npft)
  
  # annual sum of daily fractional leaf cover 
  Patchpft$aphen <- rep(NA,npft)
  
  # whether PFT can establish in this patch under current conditions
  Patchpft$establish <- rep(NA,npft)
  
  # running total for number of saplings of this PFT to establish (cohort mode)
  Patchpft$nsapling <- rep(NA,npft)
  
  # leaf-derived litter for PFT on modelled area basis (kgC/m2)
  Patchpft$litter_leaf <- rep(NA,npft)
  
  # fine root-derived litter for PFT on modelled area basis (kgC/m2)
  Patchpft$litter_root <- rep(NA,npft)
  
  # sapwood-derived litter for PFT on modelled area basis (kgC/m2)
  Patchpft$litter_sap <- rep(NA,npft)
  
  # year's sapwood-derived litter for PFT on modelled area basis (kgC/m2)
  #Patchpft$litter_sap_year <- rep(NA,npft)
  
  # heartwood-derived litter for PFT on modelled area basis (kgC/m2)
  Patchpft$litter_heart <- rep(NA,npft)
  
  # year's heartwood-derived litter for PFT on modelled area basis (kgC/m2)
  #Patchpft$litter_heart_year <- rep(NA,npft)
  
  # litter derived from allocation to reproduction for PFT on modelled area basis (kgC/m2)
  Patchpft$litter_repr <- rep(NA,npft)
  
  
  # non-FPC-weighted canopy conductance value for PFT under water-stress conditions (mm/s)
  Patchpft$gcbase <- rep(NA,npft)
  
  # daily value of the above variable (mm/s)
  Patchpft$gcbase_day <- rep(NA,npft)
  
  # two are extra, don't know which though
  # evapotranspirational "supply" function for this PFT today (mm/day)
  Patchpft$wsupply <- rep(NA,npft)
  Patchpft$wsupply_leafon <- rep(NA,npft)
  
  # fractional uptake of water from each soil layer today
  Patchpft$fwuptake <- matrix(NA,nrow=npft,ncol=2)
  
  
  # whether water-stress conditions for this PFT
  Patchpft$wstress <- rep(NA,npft)
  # daily version of the above variable
  Patchpft$wstress_day <- rep(NA,npft)
  
  # carbon depository for long-lived products like wood
  Patchpft$harvested_products_slow <- rep(NA,npft)
  
  
  # # leaf-derived nitrogen litter for PFT on modelled area basis (kgN/m2)
  Patchpft$nmass_litter_leaf <- rep(NA,npft)
  
  # # root-derived nitrogen litter for PFT on modelled area basis (kgN/m2)
  Patchpft$nmass_litter_root <- rep(NA,npft)
  
  # # sapwood-derived nitrogen litter for PFT on modelled area basis (kgN/m2)
  Patchpft$nmass_litter_sap <- rep(NA,npft)
  
  # # year's sapwood-derived nitrogen litter for PFT on modelled area basis (kgN/m2)
  # Patchpft$nmass_litter_sap_year <- rep(NA,npft)
  # 
  # # heartwood-derived nitrogen litter for PFT on modelled area basis (kgN/m2)
  Patchpft$nmass_litter_heart <- rep(NA,npft)
  # 
  # # year's heartwood-derived nitrogen litter for PFT on modelled area basis (kgN/m2)
  # Patchpft$nmass_litter_heart_year <- rep(NA,npft)
  
  
  # nitrogen depository for long-lived products like wood
  Patchpft$harvested_products_slow_nmass  <- rep(NA,npft)  
  
  
  for(p in 1:npft){
    Patchpft$anetps_ff[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$wscal[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$wscal_mean[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$anetps_ff_est[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$anetps_ff_est_initial[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$wscal_mean_est[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$phen[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$aphen[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$establish[p] <- readBin(zz, logical(), 1, size = 1)
    Patchpft$nsapling[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$litter_leaf[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$litter_root[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$litter_sap[p] <- readBin(zz, double(), 1, size = 8)
    # Patchpft$litter_sap_year[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$litter_heart[p] <- readBin(zz, double(), 1, size = 8)
    #Patchpft$litter_heart_year[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$litter_repr[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$gcbase[p] <- readBin(zz, double(), 1, size = 8) 
    Patchpft$gcbase_day[p] <- readBin(zz, double(), 1, size = 8) 
    Patchpft$wsupply[p] <- readBin(zz, double(), 1, size = 8) 
    Patchpft$wsupply_leafon[p] <- readBin(zz, double(), 1, size = 8) 
    Patchpft$fwuptake[p,] <- readBin(zz, double(), 2, size = 8)
    Patchpft$wstress[p] <- readBin(zz, logical(), 1, size = 1) 
    Patchpft$wstress_day[p] <- readBin(zz, logical(), 1, size = 1) 
    Patchpft$harvested_products_slow[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$nmass_litter_leaf[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$nmass_litter_root[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$nmass_litter_sap[p] <- readBin(zz, double(), 1, size = 8)
    # Patchpft$nmass_litter_sap_year[p] <- readBin(zz, double(), 1, size = 8) 
    Patchpft$nmass_litter_heart[p] <- readBin(zz, double(), 1, size = 8) 
    # Patchpft$nmass_litter_heart_year[p] <- readBin(zz, double(), 1, size = 8)
    Patchpft$harvested_products_slow_nmass[p] <- readBin(zz, double(), 1, size = 8)
  }
  
  return(Patchpft)
}

##################### Class : Vegetation #####################
getClass_Vegetation <- function(){
  
  Vegetation <- list()
  Vegetation$indv <- readBin(zz, integer(), 1, size = 4)
  
  return(Vegetation)
}


##################### Class : Individual #####################
getClass_Individual <- function(nind){      #  nind <- Vegetation$indv 
  
  Individual <- list()
  
  # id code (0-based, sequential)
  Individual$id <- rep(NA,nind)
  
  # leaf C biomass on modelled area basis (kgC/m2)
  Individual$cmass_leaf <- rep(NA,nind)
  
  # fine root C biomass on modelled area basis (kgC/m2)
  Individual$cmass_root <- rep(NA,nind)
  
  # sapwood C biomass on modelled area basis (kgC/m2)
  Individual$cmass_sap <- rep(NA,nind)
  
  # heartwood C biomass on modelled area basis (kgC/m2)
  Individual$cmass_heart <- rep(NA,nind)
  
  # C "debt" (retrospective storage) (kgC/m2)
  Individual$cmass_debt <- rep(NA,nind)
  
  # foliar projective cover (FPC) under full leaf cover as fraction of modelled area
  Individual$fpc <- rep(NA,nind)
  
  # fraction of PAR absorbed by foliage over projective area today, taking account of leaf phenological state
  Individual$fpar <- rep(NA,nind)
  
  # average density of individuals over patch (indiv/m2)
  Individual$densindiv <- rep(NA,nind)
  
  # vegetation phenological state (fraction of potential leaf cover)
  Individual$phen <- rep(NA,nind)
  
  # annual sum of daily fractional leaf cover
  Individual$aphen <- rep(NA,nind)
  
  # annual number of days with full leaf cover) (raingreen PFTs only; reset on 1 January)
  Individual$aphen_raingreen <- rep(NA,nind)
  
  # accumulated NPP over modelled area (kgC/m2/year)
  Individual$anpp <- rep(NA,nind)
  
  # actual evapotranspiration over projected area (mm/day)
  Individual$aet <- rep(NA,nind)
  
  # annual actual evapotranspiration over projected area (mm/year)
  Individual$aaet <- rep(NA,nind)
  
  # leaf to root mass ratio
  Individual$ltor <- rep(NA,nind)
  
  # plant height (m)
  Individual$height <- rep(NA,nind)
  
  # plant crown area (m2)
  Individual$crownarea <- rep(NA,nind)
  
  # increment in fpc since last simulation year
  Individual$deltafpc <- rep(NA,nind)
  
  # bole height, i.e. height above ground of bottom of crown cylinder (m)
  Individual$boleht <- rep(NA,nind)
  
  # patch-level lai for this individual or cohort (function fpar)
  Individual$lai <- rep(NA,nind)
  
  # patch-level lai for cohort in current vertical layer (function fpar)
  Individual$lai_layer <- rep(NA,nind)
  
  # individual leaf area index (individual and cohort modes only)
  Individual$lai_indiv <- rep(NA,nind)
  
  # growth efficiency (NPP/leaf area) for each of the last five simulation years (kgC/m2/yr)
  Individual$greff_5 <- matrix(NA,nrow=nind, ncol=5)
  
  # individual/cohort age (years)
  Individual$age <- rep(NA,nind)
  
  # monthly LAI (including phenology component)
  Individual$mlai <- matrix(NA,nrow=nind, ncol=12)
  
  # FPAR assuming full leaf cover for all vegetation
  Individual$fpar_leafon <- rep(NA,nind)
  
  # LAI for current layer in canopy (cohort/individual mode; see function fpar)
  Individual$lai_leafon_layer <- rep(NA,nind)
  
  # interception associated with this individual today (patch basis)
  Individual$intercep <- rep(NA,nind)
  
  # accumulated mean fraction of potential leaf cover
  Individual$phen_mean <- rep(NA,nind)
  
  # whether individual subject to water stress
  Individual$wstress <- rep(NA,nind)
  
  # Whether this individual is truly alive.
  Individual$alive <- rep(NA,nind)
  
  # bvoc
  # isoprene production (mg C m-2 d-1)
  Individual$iso <- rep(NA,nind)
  
  # monoterpene production (mg C m-2 d-1)
  Individual$mon <- rep(NA,nind)
  
  # monoterpene storage pool (mg C m-2)
  Individual$monstor <- rep(NA,nind)
  
  # isoprene seasonality factor (-)
  Individual$fvocseas <- rep(NA,nind)
  
  # leaf N biomass on modelled area basis (kgC/m2)
  Individual$nmass_leaf <- rep(NA,nind)
  
  # root N biomass on modelled area basis (kgC/m2)	
  Individual$nmass_root <- rep(NA,nind)
  
  # sap N biomass on modelled area basis (kgC/m2)	
  Individual$nmass_sap <- rep(NA,nind)
  
  # heart N biomass on modelled area basis (kgC/m2)
  Individual$nmass_heart <- rep(NA,nind)
  
  # leaf nitrogen that is photosyntetic active
  Individual$nactive <- rep(NA,nind)
  
  # Nitrogen extinction scalar
  Individual$nextin <- rep(NA,nind)
  
  # long-term storage of labile nitrogen
  Individual$nstore_longterm <- rep(NA,nind)
  
  # storage of labile nitrogen
  Individual$nstore_labile <- rep(NA,nind)
  
  # daily total nitrogen demand
  Individual$ndemand <- rep(NA,nind)
  
  # fraction of individual nitrogen demand available for uptake
  Individual$fnuptake <- rep(NA,nind)
  
  # annual nitrogen uptake
  Individual$anuptake <- rep(NA,nind)
  
  # maximum size of nitrogen storage
  Individual$max_n_storage <- rep(NA,nind)
  
  # scales annual npp to maximum nitrogen storage
  Individual$scale_n_storage <- rep(NA,nind)
  
  # annual nitrogen limitation on vmax
  Individual$avmaxnlim <- rep(NA,nind)
  
  # annual optimal leaf C:N ratio
  Individual$cton_leaf_aopt <- rep(NA,nind)
  
  # annual average leaf C:N ratio
  Individual$cton_leaf_aavr <- rep(NA,nind)
  
  # plant mobile nitrogen status
  Individual$cton_status <- rep(NA,nind)
  
  # total carbon in compartments before growth
  Individual$cmass_veg <- rep(NA,nind)
  
  # total nitrogen in compartments before growth
  Individual$nmass_veg <- rep(NA,nind)
  
  # whether individual subject to nitrogen stress
  Individual$nstress <- rep(NA,nind)
  
  # daily leaf nitrogen demand calculated from Vmax (kgN/m2)
  Individual$leafndemand <- rep(NA,nind)
  
  # daily root nitrogen demand based on leafndemand
  Individual$rootndemand <- rep(NA,nind)
  
  # daily sap wood nitrogen demand based on leafndemand
  Individual$sapndemand <- rep(NA,nind)
  
  # daily labile nitrogen demand based on npp
  Individual$storendemand <- rep(NA,nind)
  
  # leaf fraction of total nitrogen demand
  Individual$leaffndemand <- rep(NA,nind)
  
  # root fraction of total nitrogen demand
  Individual$rootfndemand <- rep(NA,nind)
  
  # sap fraction of total nitrogen demand
  Individual$sapfndemand <- rep(NA,nind)
  
  # store fraction of total nitrogen demand
  Individual$storefndemand <- rep(NA,nind)
  
  # daily leaf nitrogen demand over possible uptake (storage demand)
  Individual$leafndemand_store <- rep(NA,nind)
  
  #  daily root nitrogen demand over possible uptake (storage demand)
  Individual$rootndemand_store <- rep(NA,nind)
  
  # Number of days with non-negligible phenology this month
  Individual$nday_leafon <- rep(NA,nind)
  Individual$unk <- rep(NA,nind)
  
  
  for(i in 1:nind){
    
    Individual$id[i] <- readBin(zz, integer(), 1, size = 4)
    Individual$cmass_leaf[i] <- readBin(zz, double(), 1, size = 8)
    Individual$cmass_root[i] <- readBin(zz, double(), 1, size = 8)
    Individual$cmass_sap[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$cmass_heart[i] <- readBin(zz, double(), 1, size = 8)
    Individual$cmass_debt[i] <- readBin(zz, double(), 1, size = 8)
    Individual$fpc[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$fpar[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$densindiv[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$phen[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$aphen[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$aphen_raingreen[i] <- readBin(zz, integer(), 1, size = 4) 
    Individual$anpp[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$aet[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$aaet[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$ltor[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$height[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$crownarea[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$deltafpc[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$boleht[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$lai[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$lai_layer[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$lai_indiv[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$greff_5[i,] <- readBin(zz, double(), 5, size = 8)  
    
    #not sure about this
    Individual$unk[i] <- readBin(zz, double(), 1, size = 8)  
    
    Individual$alogical[i] <- readBin(zz, logical(), 1, size = 1)  
    Individual$age[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$mlai[i,] <- readBin(zz, double(), 12, size = 8)   
    
    Individual$fpar_leafon[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$lai_leafon_layer[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$intercep[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$phen_mean[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$wstress[i] <- readBin(zz, logical(), 1, size = 1)  
    Individual$alive[i] <- readBin(zz, logical(), 1, size = 1)  
    Individual$iso[i] <- readBin(zz, double(), 1, size = 8)   
    Individual$mon[i] <- readBin(zz, double(), 1, size = 8)    
    Individual$monstor[i] <- readBin(zz, double(), 1, size = 8)    
    Individual$fvocseas[i] <- readBin(zz, double(), 1, size = 8)    
    Individual$nmass_leaf[i] <- readBin(zz, double(), 1, size = 8)
    Individual$nmass_root[i] <- readBin(zz, double(), 1, size = 8)
    Individual$nmass_sap[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$nmass_heart[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$nactive[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$nextin[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$nstore_longterm[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$nstore_labile[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$ndemand[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$fnuptake[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$anuptake[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$max_n_storage[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$scale_n_storage[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$avmaxnlim[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$cton_leaf_aopt[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$cton_leaf_aavr[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$cton_status[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$cmass_veg[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$nmass_veg[i] <- readBin(zz, double(), 1, size = 8)  
    Individual$nstress[i] <- readBin(zz, logical(), 1, size = 1)  
    Individual$leafndemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$rootndemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$sapndemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$storendemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$leaffndemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$rootfndemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$sapfndemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$storefndemand[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$leafndemand_store[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$rootndemand_store[i] <- readBin(zz, double(), 1, size = 8) 
    Individual$nday_leafon[i] <- readBin(zz, integer(), 1, size = 4) 
    
  }
  
  return(Individual)
}

##################### Class : Soil #####################
getClass_Soil <- function(){
  Soil <- list()
  
  # water content of soil layers [0=upper layer] as fraction of available water holding capacity;
  Soil$wcont <- readBin(zz, double(), 2, size = 8) 
  
  # DLE - the average wcont over the growing season, for each soil layer
  Soil$awcont <- readBin(zz, double(), 2, size = 8) 
  
  # water content of sublayer of upper soil layer for which evaporation from the bare soil surface is possible 
  Soil$wcont_evap <- readBin(zz, double(), 1, size = 8) 
  
  # daily water content in upper soil layer for each day of year
  Soil$dwcontupper <- readBin(zz, double(), 365, size = 8) 
  
  # mean water content in upper soil layer for last month
  Soil$mwcontupper <- readBin(zz, double(), 1, size = 8) 
  
  # stored snow as average over modelled area (mm rainfall equivalents)
  Soil$snowpack <- readBin(zz, double(), 1, size = 8) 
  
  # total runoff today (mm/day)
  Soil$runoff <- readBin(zz, double(), 1, size = 8)
  
  # soil temperature today at 0.25 m depth (deg C)
  Soil$temp <- readBin(zz, double(), 1, size = 8)
  
  # daily temperatures for the last month (deg C)
  Soil$dtemp <- readBin(zz, double(), 31, size = 8)
  
  # mean soil temperature for the last month (deg C)
  Soil$mtemp <- readBin(zz, double(), 1, size = 8)
  Soil$gtemp <- readBin(zz, double(), 1, size = 8)
  
  # soil organic matter (SOM) pool with c. 1000 yr turnover (kgC/m2)
  Soil$cpool_slow <- readBin(zz, double(), 1, size = 8)
  
  # soil organic matter (SOM) pool with c. 33 yr turnover (kgC/m2)
  Soil$cpool_fast <- readBin(zz, double(), 1, size = 8)
  
  # mean annual litter decomposition (kgC/m2/yr)
  Soil$decomp_litter_mean <- readBin(zz, double(), 1, size = 8)
  
  # mean value of decay constant for fast SOM fraction
  Soil$k_soilfast_mean <- readBin(zz, double(), 1, size = 8)
  
  # mean value of decay constant for slow SOM fraction
  Soil$k_soilslow_mean <- readBin(zz, double(), 1, size = 8)
  
  # Parameters used by function soiltemp and updated monthly
  Soil$alag <- readBin(zz, double(), 1, size = 8)
  Soil$exp_alag <- readBin(zz, double(), 1, size = 8)
  
  # water content of soil layers [0=upper layer] as fraction of available water holding capacity
  # double mwcont[12][NSOILLAYER]
  Soil$mwcont <- readBin(zz, double(), 24, size = 8)
  
  # daily water content in lower soil layer for each day of year
  Soil$dwcontlower <- readBin(zz, double(), 365, size = 8)
  
  # mean water content in lower soil layer for last month
  Soil$mwcontlower <- readBin(zz, double(), 1, size = 8)
  
  # rainfall and snowmelt today (mm)
  Soil$rain_melt <- readBin(zz, double(), 1, size = 8)
  
  # upper limit for percolation (mm)
  Soil$max_rain_melt <- readBin(zz, double(), 1, size = 8)
  
  # whether to percolate today
  Soil$percolate <- readBin(zz, logical(), 1, size = 1)
  
  return(Soil)
}



##################### Class : Sompool #####################
getClass_Sompool <- function(){
  Sompool <- list()
  npools <- 12
  
  # C mass in pool kgC/m2
  Sompool$cmass <- rep(NA,npools)
  
  # Nitrogen mass in pool kgN/m2
  Sompool$nmass <- rep(NA,npools)
  
  # (potential) decrease in C following decomposition today (kgC/m2)
  Sompool$cdec <- rep(NA,npools) 
  
  # (potential) decrease in nitrogen following decomposition today (kgN/m2)
  Sompool$ndec <- rep(NA,npools)
  
  # daily change in carbon and nitrogen
  Sompool$delta_cmass <- rep(NA,npools)
  Sompool$delta_nmass <- rep(NA,npools)
  
  # lignin fractions
  Sompool$ligcfrac <- rep(NA,npools)
  
  # fraction of pool remaining after decomposition
  Sompool$fracremain <- rep(NA,npools)
  
  # nitrogen to carbon ratio
  Sompool$ntoc <- rep(NA,npools)
  
  # Fire
  # soil litter moisture flammability threshold (fraction of AWC)
  Sompool$litterme <- rep(NA,npools)
  
  # soil litter fire resistance (0-1)
  Sompool$fireresist  <- rep(NA,npools)
  
  # Fast SOM spinup variables
  # monthly mean fraction of carbon pool remaining after decomposition
  Sompool$mfracremain_mean <- matrix(NA,nrow=npools, ncol=12)
  
  for(n in 1:npools){
    Sompool$cmass[n] <- readBin(zz, double(), 1, size = 8) 
    Sompool$nmass[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$cdec[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$ndec[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$delta_cmass[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$delta_nmass[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$ligcfrac[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$fracremain[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$ntoc[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$litterme[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$fireresist[n] <- readBin(zz, double(), 1, size = 8)
    Sompool$mfracremain_mean[n,] <- readBin(zz, double(), 12, size = 8)
  }
  return(Sompool)
}


##################### Class : Sompool, CENTURY #####################
getClass_SompoolCent <- function(){
  SompoolCent <- list()
  
  # daily percolation (mm)
  SompoolCent$dperc <- readBin(zz, double(), 1, size = 8)
  
  # fraction of decayed organic nitrogen leached each day;
  SompoolCent$orgleachfrac <- readBin(zz, double(), 1, size = 8)
  
  # soil mineral nitrogen pool (kgN/m2)
  SompoolCent$nmass_avail <- readBin(zz, double(), 1, size = 8)
  
  # soil nitrogen input (kgN/m2)
  SompoolCent$ninput <- readBin(zz, double(), 1, size = 8)
  
  # annual sum of nitrogen mineralisation
  SompoolCent$anmin <- readBin(zz, double(), 1, size = 8)
  
  # annual sum of nitrogen immobilisation
  SompoolCent$animmob <- readBin(zz, double(), 1, size = 8)
  
  # annual leaching from available nitrogen pool
  SompoolCent$aminleach	<- readBin(zz, double(), 1, size = 8)
  
  # annual leaching of organics from active nitrogen pool
  SompoolCent$aorgleach <- readBin(zz, double(), 1, size = 8)
  
  # total annual nitrogen fixation 
  SompoolCent$anfix <- readBin(zz, double(), 1, size = 8)
  
  # calculated annual mean nitrogen fixation
  SompoolCent$anfix_calc <- readBin(zz, double(), 1, size = 8)
  
  # annual nitrogen fixation
  SompoolCent$anfix_mean <- readBin(zz, double(), 1, size = 8)
  
  # stored nitrogen deposition in snowpack
  SompoolCent$snowpack_nmass <- readBin(zz, double(), 1, size = 8)
  
  # years at which to begin documenting for calculation of Century equilibrium
  SompoolCent$solvesomcent_beginyr <- readBin(zz, integer(), 1, size = 4)
  
  # years at which to end documentation and start calculation of Century equilibrium
  SompoolCent$solvesomcent_endyr <- readBin(zz, integer(), 1, size = 4)
  
  # Cumulative litter pools for one year.
  SompoolCent$solvesom <- readBin(zz, double(), 1, size = 8)
  
  # monthly fraction of available mineral nitrogen taken up
  SompoolCent$fnuptake_mean <- readBin(zz, double(), 12, size = 8)
  
  # monthly fraction of organic carbon/nitrogen leached
  SompoolCent$morgleach_mean <- readBin(zz, double(), 12, size = 8)
  
  # monthly fraction of available mineral nitrogen leached
  SompoolCent$mminleach_mean <- readBin(zz, double(), 12, size = 8)
  
  return(SompoolCent)
}


##################### Class : Fluxes #####################
getClass_Fluxes <- function(){
  Fluxes <- list()
  #f.bytes <- bytes[1570:1667,]
  Fluxes$f1 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f2 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f3 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f4 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f5 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f6 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f7 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f8 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f9 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f10 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f11 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f12 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f13 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f14 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f15 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f16 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f17 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f18 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f19 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f20 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f21 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f22 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f23 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f24 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f25 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f26 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f27 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f28 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f29 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f30 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f31 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f32 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f33 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f34 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f35 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f36 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f37 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f38 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f39 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f40 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f41 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f42 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f43 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f44 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f45 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f46 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f47 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f48 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f49 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f50 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f51 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f52 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f53 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f54 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f55 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f56 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f57 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f58 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f59 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f60 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f61 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f62 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f63 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f64 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f65 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f66 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f67 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f68 <- readBin(zz, double(), 144, size = 8)
  Fluxes$f69 <- readBin(zz, double(), 60, size = 8)
  Fluxes$f70 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f71 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f72 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f73 <- readBin(zz, integer(), 1, size = 4)
  Fluxes$f74 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f75 <- readBin(zz, logical(), 1, size = 1)
  Fluxes$f76 <- readBin(zz, integer(), 1, size = 4)
  Fluxes$f77 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f78 <- readBin(zz, integer(), 1, size = 4)
  Fluxes$f79 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f80 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f81 <- readBin(zz, double(), 5, size = 8)
  Fluxes$f82 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f83 <- readBin(zz, logical(), 1, size = 1)
  Fluxes$f84 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f85 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f86 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f87 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f88 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f89 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f90 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f91 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f92 <- readBin(zz, double(), 1, size = 8)
  Fluxes$f93 <- readBin(zz, double(), 12, size = 8)
  Fluxes$f94 <- readBin(zz, double(), 12, size = 8)
  Fluxes$f95 <- readBin(zz, double(), 12, size = 8)
  Fluxes$f96 <- readBin(zz, double(), 12, size = 8)
  Fluxes$f97 <- readBin(zz, double(), 12, size = 8)
  Fluxes$f98 <- readBin(zz, double(), 1, size = 8)
  
  return(Fluxes)
  
  # # Fluxes stored as totals for the whole patch
  # Fluxes$PerPatchFluxType <- list()
  # 
  # # Carbon flux to atmosphere from burnt vegetation and litter (kgC/m2)
  # Fluxes$PerPatchFluxType$FIREC 
  # 
  # # Carbon flux to atmosphere from soil respiration (kgC/m2)
  # Fluxes$PerPatchFluxType$SOILC
  # 
  # # Flux from atmosphere to vegetation associated with establishment (kgC/m2)
  # Fluxes$PerPatchFluxType$ESTC 
  # 
  # # Flux to atmosphere from consumed harvested products (kgC/m2)
  # Fluxes$PerPatchFluxType$HARVESTC 
  # 
  # # Flux to atmosphere from consumed harvested products (kgN/m2)
  # Fluxes$PerPatchFluxType$HARVESTN 
  # 
  # # NH3 flux to atmosphere from fire
  # Fluxes$PerPatchFluxType$NH3_FIRE 
  # 
  # # NO flux to atmosphere from fire	
  # Fluxes$PerPatchFluxType$NO_FIRE 
  # 
  # # NO2 flux to atmosphere from fire
  # Fluxes$PerPatchFluxType$NO2_FIRE 
  # 
  # # N2O flux to atmosphere from fire	
  # Fluxes$PerPatchFluxType$N2O_FIRE 
  # 
  # # N2 flux to atmosphere from fire	
  # Fluxes$PerPatchFluxType$N2_FIRE 
  # 
  # # N flux from soil
  # Fluxes$PerPatchFluxType$N_SOIL
  # 
  # # Reproduction costs
  # Fluxes$PerPatchFluxType$REPRC 
  # 
  # # Number of types, must be last
  # Fluxes$PerPatchFluxType$NPERPATCHFLUXTYPES # 12
  # 
  # # Fluxes stored per pft
  # Fluxes$PerPFTFluxType <- list()
  # 
  # # NPP (kgC/m2)
  # Fluxes$PerPFTFluxType$NPP
  # 
  # # GPP (kgC/m2)
  # Fluxes$PerPFTFluxType$GPP
  # 
  # # Autotrophic respiration (kgC/m2)
  # Fluxes$PerPFTFluxType$RA
  # 
  # # Isoprene (mgC/m2)
  # Fluxes$PerPFTFluxType$ISO
  # 
  # # Monoterpene (mgC/m2)
  # Fluxes$PerPFTFluxType$MON
  # 
  # # Number of types, must be last
  # Fluxes$PerPFTFluxType$NPERPFTFLUXTYPES # 5
}



