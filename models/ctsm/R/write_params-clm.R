#' Write FATES Parameter File
#'
#' @param defaults 
#' @param trait.values named list or data frame of traits, e.g.
#' \code{data.frame(vmax = 1, b0 = 2)} or \code{list(vmax = 1, b0 = 2)}
#' @param settings 
#' @param run.id 
#'
#' @return
#' @export
#'
#' @examples
write_params_ctsm <- function(defaults = system.file('', package = 'PEcAn.CTSM'),
                              trait.values, settings, run.id){
  
  ## COPY AND OPEN DEFAULT PARAMETER FILES
  # TODO: update this to read param files (FATES ONLY) out of the refcase directory, not the PEcAn package
  # TODO: clear out variables that aren't need to run ED model
  ## See issue https://github.com/PecanProject/pecan/issues/1008
  fates.param.default <- system.file('clm5_params.c171117_0001.nc', package = 'PEcAn.CTSM')
  #fates.param.default <- file.path(refcase,"clm5_params.c171117.nc") # probably need to allow custom param file names here (pecan.xml?)
  fates.param.file <- file.path(local.rundir,paste0("fates_params.",run.id,".nc"))
  file.copy(fates.param.default,fates.param.file)
  fates.param.nc <- ncdf4::nc_open(fates.param.file,write=TRUE)
  
  clm_vars = c()
  
  ## Loop over PFTS
  npft <- length(trait.values)
  PEcAn.logger::logger.debug(npft)
  PEcAn.logger::logger.debug(dim(trait.values))
  PEcAn.logger::logger.debug(names(trait.values))
  #pftnames <- stringr::str_trim(tolower(ncvar_get(param.nc,"pftname"))) 
  pftnames <- stringr::str_trim(tolower(ncdf4::ncvar_get(fates.param.nc,"pftname")))
  PEcAn.logger::logger.debug(paste0("fates PFT names: "),pftnames)
  for (i in seq_len(npft)) {
    pft <- trait.values[[i]]
    print(c("PFT",i))
    PEcAn.logger::logger.info(pft)
    pft.name <- names(trait.values)[i]
    if(is.null(pft.name) | is.na(pft.name)){
      PEcAn.logger::logger.error("pft.name missing")
    } else {
      PEcAn.logger::logger.info(paste("PFT =",pft.name))
      PEcAn.logger::logger.debug(paste0("fates-fates PFT number: ",which(pftnames==pft.name)))
    }
    if(pft.name == 'env') next   ## HACK, need to remove env from default
    
    ## Match PFT name to COLUMN
    ipft <- match(tolower(pft.name),pftnames)
    PEcAn.logger::logger.debug(paste0("ipft: ",ipft))
    
    if(is.na(ipft)){
      PEcAn.logger::logger.severe(paste("Unmatched PFT",pft.name,
                                        "in FATES. PEcAn does not yet support non-default PFTs for this model"))
    }
    
    # hard code hack until we can use more than 2 pfts in FATES 
    ipft <- 2
    PEcAn.logger::logger.debug(paste0("*** PFT number hard-coded to ", ipft," in fates. This will be updated when FATES allows more PFTs"))
    
    ## Special variables used in conversions
    #     leafC <- pft['leafC']/100  ## percent to proportion
    leafC <- NA
    if(is.na(leafC)) leafC <- 0.48
    
    # determine photo pathway
    PEcAn.logger::logger.debug(paste0("Photosynthesis pathway flag value: ", photo_flag))
    
    ## Loop over VARIABLES
    for (v in seq_along(pft)) {
      var <- names(pft)[v]
      
      ## Required variables from google doc for CLM/FATES workshop Feb 2019: https://docs.google.com/document/d/12XWb7DmmOuvRAOEDviBt-x2uCUWIRDASZA2T9u_fbY0/edit#
      ### ----- Leaf physiological parameters
      ## missing from params.nc 
      ## Vcmax
      # if(var == "Vcmax"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_vcmax25top', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      # 
      ## missing from params.nc 
      ##Jmax
      # if(var == "Jmax"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_vcmax25top', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      ## missing from params.nc 
      ## Leaf Respiration
      # if(var == "Rd"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_vcmax25top', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      ## missing from params.nc 
      ## Area based leaf nitrogen concentration
      # if(var == "Na"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_vcmax25top', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      
      # Leaf C:N ratio
      if(var == "leafcn"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='leafcn', start = ipft, count = 1,
                         vals=pft[v])  ## (gC/gN)
      }
      
      # Fraction of leaf nitrogen in Rubisco
      if(var == "flnr"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='flnr', start = ipft, count = 1,
                         vals=pft[v])  ## (gC/gN)
      }
      ## missing from params.nc 
      ## Mass ratio of total Rubisco molecular mass to nitrogen in Rubisco
      # if(var == "mrnr"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  
      # }
      
      # SLA is top of canopy in fates (units = m^2/gC)
      if(var == "sla"){                                  ## default 0.012
        ncdf4::ncvar_put(nc=fates.param.nc, varid='slatop', start = ipft, count = 1,  
                         vals=udunits2::ud.convert(pft[v],"m2 kg-1","m2 g-1")/leafC)
        
      }
      ## missing from params.nc 
      ## gs
      # if(var == "gs"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (gC/gN)
      # }
      
      # g1 BB: Ball-Berry slope of conductance-photosynthesis relationship, unstressed
      if(var == "stomatal_slope.BB"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='mbbopt', start = ipft, count = 1,
                         vals=pft[v]) ##(fates: umol H2O/umol CO2)
      }
      ## missing from params.nc 
      # # g0 BB: Ball-Berry intercept of conductance-photosynthesis relationship, unstressed
      #   if(var == "stomatal_int.BB"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) ##(fates: )
      #   }
      
     
      # g1 M: Medlyn slope of conductance-photosynthesis relationship
      if(var == "stomatal_slope.M"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='medlynslope', start = ipft, count = 1,
                         vals=pft[v]) ##(fates: umol H2O/umol CO2)
      }
      
      # g0 M: Medlyn intercept of conductance-photosynthesis relationship
      if(var == "stomatal_int.M"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='medlynintercept', start = ipft, count = 1,
                         vals=pft[v]) ##(fates: umol H2O)
      }
      
      
      # Ratio of new fine root : new leaf carbon allocation
      if(var == "froot_leaf"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='froot_leaf', start = ipft, count = 1,
                         vals=pft[v]) ##(fates: gC/gC)
      }
     
      # # Ratio of growth respiration carbon : new growth carbon
      # ## missing from params.nc 
      # if(var == "grc2ngc"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v]) ##(fates:)
      # }
      # 
      # Target C:N ratio leaf
      if(var == "leafcn"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='leafcn', start = ipft, count = 1,
                         vals=pft[v]) ##(fates:gC/gN)
      }

      
      # Target C:N ratio fine roots
      if(var == "frootcn"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='frootcn', start = ipft, count = 1,
                         vals=pft[v]) 
      }
      ## missing from params.nc 
      # # Root mass per soil layer profile
      #   if(var == "b"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v])
      #   }
      ## missing from params.nc   
      # # Leaf, seed, (stem), fine roots
      #   if(var == "biomass"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) 
      #   } 
      
      ## missing from params.nc   
      # # Time series of different pools (leaf, root, reproductive, etc)
      #   if(var == "phenology"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) 
      #   } 
      #
      ## missing from params.nc   
      # # meterological forcings
      #   if(var == "meterol"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) 
      #   } 
      
      ## missing from params.nc   
      # # Percent sand, silt, clay, organic matter
      #   if(var == "soilPhys"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) 
      #   } 
      
      
      ### ----- Leaf physiological parameters
      
      
      ### These variable names (from ED2) should updated in BETY to be more generic
     
      if(var == "growth_resp_factor"){                    ## r_growth = grperc * (gpp+r_maint)  fates_grperc:long_name = "Growth respiration factor" ;
        ncdf4::ncvar_put(nc=fates.param.nc, varid='grperc', start = ipft, count = 1,
                         vals=pft[v])  # unitless
      }
      
      if(var == "leaf_reflect_nir"){      # Leaf reflectance: near-IR	[0-1]
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rholnir', start = ipft, count = 1,
                         vals=pft[v])
      }
      if(var == "leaf_reflect_vis"){      # Leaf reflectance: visible	[0-1]
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rholnir', start = ipft, count = 1,
                         vals=pft[v])
      }
     
      if(var == "leaf_trans_nir"){        # Leaf transmittance: near-IR
        ncdf4::ncvar_put(nc=fates.param.nc, varid='taulnir', start = ipft, count = 1,
                         vals=pft[v])
      }
      if(var == "leaf_trans_vis"){        # Leaf transmittance: visible	pft
        ncdf4::ncvar_put(nc=fates.param.nc, varid='taulvis', start = ipft, count = 1,
                         vals=pft[v])
      }
      
      if(var == "orient_factor"){         # Leaf/stem orientation index	[-0/4 <xl< 0.6], fates_xl:valid_range = -1., 1. ;
        ncdf4::ncvar_put(nc=fates.param.nc, varid='xl', start = ipft, count = 1,
                         vals=pft[v])
      }
     
      if(var == "roota_par"){            # fates rooting distribution parameter [1/m]
        ncdf4::ncvar_put(nc=fates.param.nc, varid='roota_par', start = ipft, count = 1,
                         vals=pft[v])
      }
      if(var == "rootb_par"){            # fates rooting distribution parameter [1/m] 
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rootb_par', start = ipft, count = 1,
                         vals=pft[v])
      }
      
      ### plant hydrology parameters
     
      if(var == "psi_stomata_closure"){         # Soil water potential at full stomatal closure	[mm]
        # fates_smpsc:long_name = "Soil water potential at full stomatal closure" ;
        ncdf4::ncvar_put(nc=fates.param.nc, varid='smpsc', start = ipft, count = 1,
                         vals=udunits2::ud.convert(pft[v],"m","mm"))
      }
      if(var == "psi_stomata_open"){            # Soil water potential at full stomatal opening	pft	[mm]
        ncdf4::ncvar_put(nc=fates.param.nc, varid='smpso', start = ipft, count = 1,
                         vals=udunits2::ud.convert(pft[v],"m","mm"))
      }
     
      if(var == "kmax"){         # plant segment max conductance
        ncdf4::ncvar_put(nc=fates.param.nc, varid='kmax', start = ipft, count = 1,
                         vals=pft[v]) # mm h2o (transpired)/mm h2o (water potential gradient)/sec
      }
      
      if(var == "kmax_root"){         # Maximum root hydraulic conductivity per unit xs sapwood [kg m-1 s-1 Mpa-1]
        ncdf4::ncvar_put(nc=fates.param.nc, varid='krmax', start = ipft, count = 1,
                         vals=pft[v])
      }
      
      if(var == "p50_gs"){         # leaf water potential at 50% loss of stomatal conductance (Pgs50)	[MPa]
        ncdf4::ncvar_put(nc=fates.param.nc, varid='psi50', start = ipft, count = 1,
                         vals=pft[v])
      }
      
      
      if(var == "displar"){                              # fates_displar:long_name = "Ratio of displacement height to canopy top height"
        ncdf4::ncvar_put(nc=fates.param.nc, varid='displar', start = ipft, count = 1,
                         vals=pft[v])
      }
      if(var == "z0mr"){                                 # Ratio of momentum roughness length to canopy top height
        # fates_z0mr:long_name = "Ratio of momentum roughness length to canopy top height"
        ncdf4::ncvar_put(nc=fates.param.nc, varid='z0mr', start = ipft, count = 1,
                         vals=pft[v])
      }
      
      ## BINARY FLAGS: These should be set-able by PEcAn but not sampled
      
      if(var == "crop"){         # Binary crop flag: 0. = not crop, 1. = crop
        ncdf4::ncvar_put(nc=fates.param.nc, varid='crop', start = ipft, count = 1,
                         vals=pft[v])
      }
      if(var == "irrigated"){         # Binary Irrigated PFT flag
        ncdf4::ncvar_put(nc=fates.param.nc, varid='irrigated', start = ipft, count = 1,
                         vals=pft[v])
      }
      if(var == "cold_deciduous"){         # Binary flag for seasonal-deciduous leaf habit (0-not,1-it is)
        # fates_season_decid:flag_meanings = "NOT seasonal-deciduous"
        ncdf4::ncvar_put(nc=fates.param.nc, varid='season_decid', start = ipft, count = 1,
                         vals=pft[v])
        ncdf4::ncvar_put(nc=fates.param.nc, varid='evergreen', start = ipft, count = 1,
                         vals=0)
      }
      if(var == "stress_deciduous"){         # Binary flag for stress-deciduous leaf habit (0-not,1-it is)
        ncdf4::ncvar_put(nc=fates.param.nc, varid='stress_decid', start = ipft, count = 1,
                         vals=pft[v])
        ncdf4::ncvar_put(nc=fates.param.nc, varid='evergreen', start = ipft, count = 1,
                         vals=0)
      }
      if(var == "woody"){         # Binary woody lifeform flag (0-is not woody, 1-it is woody)
        ncdf4::ncvar_put(nc=fates.param.nc, varid='woody', start = ipft, count = 1,
                         vals=pft[v])
      }
      if(var == "evergreen"){         # Binary flag for evergreen leaf habit
        ncdf4::ncvar_put(nc=fates.param.nc, varid='evergreen', start = ipft, count = 1,
                         vals=pft[v])
        ncdf4::ncvar_put(nc=fates.param.nc, varid='stress_decid', start = ipft, count = 1,
                         vals=0)
        ncdf4::ncvar_put(nc=fates.param.nc, varid='season_decid', start = ipft, count = 1,
                         vals=0)
      }
      
      ## ALLPFT indexed (size = 1) 
      if(var == "veg_respiration_Q10"){            ## Q10 for maintenance respiration. fates param. q10_mr(allpfts)
        ncdf4::ncvar_put(nc=fates.param.nc, varid='q10_mr', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "CelluloseS"){            ## Cellulose fraction for CWD
        ncvar_put(nc=fates.param.nc, varid='cwd_fcel', start = 1, count = 1,
                  vals=pft[v])
      }
      if(var == "s_lignin"){            ## Lignin fraction for CWD
        ncdf4::ncvar_put(nc=fates.param.nc, varid='cwd_flig', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "c2n_som1"){            ## C:N for SOM pool 1. fates param
        ncdf4::ncvar_put(nc=fates.param.nc, varid='cn_s1_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "c2n_som2"){            ## C:N for SOM pool 2. fates param
        ncdf4::ncvar_put(nc=fates.param.nc, varid='cn_s2_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "c2n_som3"){            ## C:N for SOM pool 3. fates param
        ncdf4::ncvar_put(nc=fates.param.nc, varid='cn_s3_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "cnscalefactor"){            ## Scale factor on CN decomposition for assigning methane flux . fates param
        ncdf4::ncvar_put(nc=fates.param.nc, varid='cnscalefactor', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "decomp_depth_efolding"){            ## e-folding depth for reduction in decomposition. 
        ## Set to large number for depth-independance. fates param
        ncdf4::ncvar_put(nc=fates.param.nc, varid='decomp_depth_efolding', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "CWD_fragmentation_rate"){            ## Fragmentation rate for CWD. units = "1/day", fates param
        ncvar_put(nc=fates.param.nc, varid='k_frag', start = 1, count = 1,
                  vals=pft[v])
      }
      if(var == "rf_cwdl2_bgc"){            ## respiration fraction from CWD to litter 2 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_cwdl2_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "rf_cwdl3_bgc"){            ## respiration fraction from CWD to litter 3 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_cwdl3_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "rf_l1s1_bgc"){            ## Respiration fraction for litter 1 -> SOM 1 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_l1s1_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "rf_l2s1_bgc"){            ## respiration fraction litter 2 to SOM 1 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_l2s1_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "rf_l3s2_bgc"){            ## respiration fraction from litter 3 to SOM 2 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_l3s2_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "rf_s2s1_bgc"){            ## respiration fraction SOM 2 to SOM 1 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_s2s1_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "rf_s2s3_bgc"){            ## Respiration fraction for SOM 2 -> SOM 3 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_s2s3_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "rf_s3s1_bgc"){            ## respiration fraction SOM 3 to SOM 1 - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='rf_s3s1_bgc', start = 1, count = 1,
                         vals=pft[v])
      }
      if(var == "Q10_frozen_soil"){            ## Separate q10 for frozen soil respiration rates - REMOVED FROM FATES PARAMS?
        ncdf4::ncvar_put(nc=fates.param.nc, varid='froz_q10', start = 1, count = 1,
                         vals=pft[v])
      }
      
      
    } ## end loop over VARIABLES
  } ## end loop over PFTs
  ncdf4::nc_close(.param.nc)
  
  
}
}