#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes config files for use with FATES.
##'
##' @name write.config.FATES
##' @title Write FATES configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return none
##' @export
##' @author Mike Dietze
##-------------------------------------------------------------------------------------------------#
write.config.FATES <- function(defaults, trait.values, settings, run.id){

   ## function references
   ncvar_put <- ncdf4::ncvar_put
   ncvar_get <- ncdf4::ncvar_get

   ## site information
   site <- settings$run$site
   site.id <- as.numeric(site$id)
  
   # find out where things are
   local.rundir <- file.path(settings$rundir, run.id) ## this is on local machine for staging
   rundir <- file.path(settings$host$rundir, run.id)  ## this is on remote machine for execution
   casedir <- file.path(rundir,"case") 
   outdir <- file.path(settings$host$outdir, run.id)
   refcase   <- settings$model$binary
   bld    <- file.path(refcase,"bld")
   binary <- file.path(bld,"cesm.exe")
   indir  <- file.path(rundir,"input") ## input directory
   default <- settings$run$inputs$default$path ## reference inputs file structure
   site_name <- paste0(site.id %/% 1000000000, "-", site.id %% 1000000000)
   
   ## DATES
   ## CLM is a bit odd and takes a start date and length, so we need to precompute
   ## this needs to be generalized to fractional years, but accounting for 365 day year
   start_date <- as.Date(settings$run$start.date)
   end_date   <- as.Date(settings$run$end.date)
   stop_n     <- as.numeric(end_date - start_date, units="days") - PEcAn.utils::n_leap_day(start_date,end_date) + 1  
   
   ##-----------------------------------------------------------------------##
   ##                                                                       ##
   ##                             INPUTS                                    ##
   ##                                                                       ##
   ##-----------------------------------------------------------------------##

   ## SITE INFO --> DOMAIN FILE (lat/lon)
   gridres = 0.125  ## ultimately this should be a variable
   lat = site$lat
   lon = (site$lon + 360) %% 360 ## make sure coords in 0-360 range, not negative
   domain.default <- system.file("domain.lnd.1x1pt-brazil_navy.090715.nc",package="PEcAn.FATES")
   domain.file <- file.path(local.rundir,paste0("domain.lnd.",site_name,".nc"))
   file.copy(domain.default,domain.file)
   domain.nc <- ncdf4::nc_open(domain.file,write=TRUE)
   ncvar_put(nc=domain.nc, varid='xc', vals=lon)
   ncvar_put(nc=domain.nc, varid='yc', vals=lat)
   ncvar_put(nc=domain.nc, varid='xv', vals=lon+c(-1,1,1,-1)*gridres)
   ncvar_put(nc=domain.nc, varid='yv', vals=lat+c(-1,-1,1,1)*gridres)
   ncvar_put(nc=domain.nc, varid='area', vals=(2*gridres*pi/180)^2)   
   ncdf4::nc_close(domain.nc)
   
   ## SURF
   surf.default <- system.file("surfdata_ref.nc",package = "PEcAn.FATES")
   surf.file    <- file.path(local.rundir,paste0("surfdata_",site_name,"_simyr2000.nc"))
   file.copy(surf.default,surf.file)
   Sys.chmod(surf.file)
   surf.nc <- ncdf4::nc_open(surf.file,write=TRUE)
   ncdf4::ncvar_put(nc=surf.nc, varid='LONGXY', vals=lon)
   ncdf4::ncvar_put(nc=surf.nc, varid='LATIXY', vals=lat)
   ncdf4::nc_close(surf.nc)   
   
   ## MET HEADERS
   if(!is.null(settings$run$inputs$met)){

     ## DATM HEADER: datm_atm_in
     datm <- readLines(con=system.file("datm_atm_in.template",package = "PEcAn.FATES"),n=-1)
     datm <- gsub('@DOMAIN@', file.path(indir,"share/domains/domain.clm",basename(domain.file)), datm)
     datm <- gsub('@START_YEAR@',lubridate::year(start_date), datm)
     datm <- gsub('@END_YEAR@',lubridate::year(end_date), datm)
     writeLines(datm, con=file.path(local.rundir, "datm_atm_in"))
     
     ## DATM STREAM MET
     met <- readLines(con=system.file("datm.streams.txt.PEcAn_met.template",package = "PEcAn.FATES"),n=-1)
     met <- gsub('@INDIR@',indir, met)
     met <- gsub('@MET_PATH@',settings$run$inputs$met$path, met)
     met.files <- dir(settings$run$inputs$met$path,"*.nc")
     met <- gsub('@MET_FILES@',paste(met.files,collapse = "\n            "), met)
     writeLines(met, con=file.path(local.rundir, "datm.streams.txt.PEcAn_met"))
     
   }
   
#   ... fill in this template, the met template, and then have jobs.sh put them in the right place. 
#   ... Test, then adjust DB to have met required
   
   
   ##-----------------------------------------------------------------------##
   ##                                                                       ##
   ##                             JOB.SH                                    ##
   ##                                                                       ##
   ##-----------------------------------------------------------------------##
   
# create launch script (which will create symlink)
   if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
     jobsh <- readLines(con=settings$model$jobtemplate, n=-1)
   } else {
     jobsh <- readLines(con=system.file("template.job", package = "PEcAn.FATES"), n=-1)
   }
   
 # create host specific setttings
   hostsetup <- ""
   if (!is.null(settings$model$prerun)) {
     hostsetup <- paste(hostsetup, sep="\n", paste(settings$model$prerun, collapse="\n"))
   }
   if (!is.null(settings$host$prerun)) {
     hostsetup <- paste(hostsetup, sep="\n", paste(settings$host$prerun, collapse="\n"))
   }

   hostteardown <- ""
   if (!is.null(settings$model$postrun)) {
     hostteardown <- paste(hostteardown, sep="\n", paste(settings$model$postrun, collapse="\n"))
   }
   if (!is.null(settings$host$postrun)) {
     hostteardown <- paste(hostteardown, sep="\n", paste(settings$host$postrun, collapse="\n"))
   }

# create job.sh
   jobsh <- gsub('@HOST_SETUP@', hostsetup, jobsh)
   jobsh <- gsub('@HOST_TEARDOWN@', hostteardown, jobsh)

   ## PATHS
   jobsh <- gsub('@RUNDIR@', rundir, jobsh)
   jobsh <- gsub('@CASEDIR@', casedir, jobsh)
   jobsh <- gsub('@OUTDIR@', outdir, jobsh)
   jobsh <- gsub('@REFCASE@', refcase, jobsh)
   jobsh <- gsub('@BLD@', bld, jobsh)
   jobsh <- gsub('@BINARY@', binary, jobsh)
   jobsh <- gsub('@INDIR@', indir, jobsh)
   jobsh <- gsub('@DEFAULT@', default, jobsh)
   jobsh <- gsub('@SITE_NAME@', site_name, jobsh) 
  
   ## DATES -> ENV_RUN
   jobsh <- gsub('@START_DATE@', start_date, jobsh)
   jobsh <- gsub('@STOP_N@', stop_n, jobsh)
   jobsh <- gsub('@RUN_ID@', run.id, jobsh)
   
   ## MET --> DATM
#   jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
   ## FOR FIRST STEP, CAN USE DEFAULT
   
   writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
   Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
#   
#   ## Write PARAMETER file
   
   ## COPY AND OPEN DEFAULT PARAMETER FILE
   param.default <- system.file("clm_params_ed_l2f0.5_tropid2.c161020.nc",package="PEcAn.FATES")
   param.file <- file.path(local.rundir,paste0("clm_params_ed.",run.id,".nc"))
   file.copy(param.default,param.file)
   param.nc <- ncdf4::nc_open(param.file,write=TRUE)
    
   ## Loop over PFTS
   npft <- length(trait.values)
   print(npft)
   print(dim(trait.values))
   pftnames <- stringr::str_trim(tolower(ncvar_get(param.nc,"pftname")))
   for (i in seq_len(npft)) {
     pft <- trait.values[[i]]
     pft.name <- names(trait.values)[i]
     if(pft.name == 'env') next   ## HACK, need to remove env from default
     
     ## Match PFT name to COLUMN
     ipft <- match(tolower(pft.name),pftnames)
     if(is.na(ipft)){
       PEcAn.utils::logger.severe(paste("Unmatched PFT",pft.name,
                          "in FATES. PEcAn does not yet support non-default PFTs for this model"))
     }
     
     ## Special variables used in conversions
#     leafC <- pft['leafC']/100  ## percent to proportion
     leafC <- NA
     if(is.na(leafC)) leafC <- 0.48
     
     ## Loop over VARIABLES
     for (v in seq_along(pft)) {
       var <- names(pft)[v]
       
       if(var == "SLA"){
         ncvar_put(nc=param.nc, varid='slatop', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"m2 kg-1","m2 g-1")/leafC)
       }
       if(var == "leaf_turnover_rate"){
         ncvar_put(nc=param.nc, varid='leaf_long', start = ipft, count = 1,
                   vals=1/pft[v]) ## leaf_long = 1/leaf_turnover_rate, 1/years -> years
       }
       if(var == "root_turnover_rate"){
         ncvar_put(nc=param.nc, varid='root_long', start = ipft, count = 1,
                   vals=1/pft[v]) ## root_long = 1/root_turnover_rate, 1/years -> years
       }
       if(var == "c2n_leaf"){
         ncvar_put(nc=param.nc, varid='leafcn', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "fineroot2leaf"){
         ncvar_put(nc=param.nc, varid='froot_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "sapwood_ratio"){         # leaf to sapwood area ratio
         ncvar_put(nc=param.nc, varid='latosa', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"m2 m-2","m2 cm-2"))
       }
       if(var == "leaf_width"){            # Characteristic leaf dimension use for aerodynamic resistance
         ncvar_put(nc=param.nc, varid='dleaf', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"mm","m"))
       }
       if(var == "nonlocal_dispersal"){    # Place-holder parameter for important seed dispersal parameters
         ncvar_put(nc=param.nc, varid='seed_dispersal_x', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "hgt_min"){               # The height of a new recruit
         ncvar_put(nc=param.nc, varid='hgt_min', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_reflect_nir"){      # Leaf reflectance: near-IR	[0-1]
         ncvar_put(nc=param.nc, varid='rholnir', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_reflect_vis"){      # Leaf reflectance: visible	[0-1]
         ncvar_put(nc=param.nc, varid='rholvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_reflect_nir"){      # Stem reflectance: near-IR	[0-1]
         ncvar_put(nc=param.nc, varid='rhosnir', start = ipft, count = 1,
                   vals=pft[v])
       }

       if(var == "wood_reflect_vis"){      # Stem reflectance: visible	[0-1]
         ncvar_put(nc=param.nc, varid='rhosvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_trans_nir"){        # Leaf transmittance: near-IR
         ncvar_put(nc=param.nc, varid='taulnir', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_trans_vis"){        # Leaf transmittance: visible	pft
         ncvar_put(nc=param.nc, varid='taulvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_trans_nir"){        # Stem transmittance: near-IR
         ncvar_put(nc=param.nc, varid='tausnir', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_trans_vis"){        # Stem transmittance: visible
         ncvar_put(nc=param.nc, varid='tausvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "orient_factor"){         # Leaf/stem orientation index	[-0/4 <xl< 0.6]
         ncvar_put(nc=param.nc, varid='xl', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_density"){         # Wood Specific Gravity (ie density of wood relative to density of water)
         ncvar_put(nc=param.nc, varid='wood_density', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "roota_par"){            # CLM rooting distribution parameter [1/m]
         ncvar_put(nc=param.nc, varid='roota_par', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "rootb_par"){            # CLM rooting distribution parameter [1/m] 
         ncvar_put(nc=param.nc, varid='rootb_par', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "gsmax"){         # Maximum stomatal conductance [m s-1]
         ncvar_put(nc=param.nc, varid='gsmax', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "psi_stomata_closure"){         # Soil water potential at full stomatal closure	[mm]
         ncvar_put(nc=param.nc, varid='smpsc', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"m","mm"))
       }
       if(var == "psi_stomata_open"){            # Soil water potential at full stomatal opening	pft	[mm]
         ncvar_put(nc=param.nc, varid='smpso', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"m","mm"))
       }
       if(var == "root_bulk_modulus"){         # coarse root bulk elastic modulus (εroot)	[MPa]
         ncvar_put(nc=param.nc, varid='epsil_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "sapwood_bulk_modulus"){         # sapwood bulk elastic modulus (εstem)	[MPa]
         ncvar_put(nc=param.nc, varid='epsil_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_bulk_modulus"){         # leaf bulk elastic modulus (εleaf) [MPa]
         ncvar_put(nc=param.nc, varid='epsil_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "root_osmotic_potential"){         # coarse root osmotic potential at full turgor (πoroot)	[MPa]
         ncvar_put(nc=param.nc, varid='pinot_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "sapwood_osmotic_potential"){         # sapwood osmotic potential at full turgor (πostem) [MPa]
         ncvar_put(nc=param.nc, varid='pinot_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_osmotic_potential"){         # leaf osmotic potential at full turgor (πoleaf) [MPa]
         ncvar_put(nc=param.nc, varid='pinot_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "kmax_leaf"){         # Maximum leaf hydraulic conductivity per unit leaf area [mmol m-2 s-1 Mpa-1]
         ncvar_put(nc=param.nc, varid='kmax_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "kmax_root"){         # Maximum root hydraulic conductivity per unit xs sapwood [kg m-1 s-1 Mpa-1]
         ncvar_put(nc=param.nc, varid='kmax_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "kmax_stem"){         # Maximum stem hydraulic conductivity per unit xs sapwood area	[kg m-1 s-1 Mpa-1]
         ncvar_put(nc=param.nc, varid='kmax_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_gs"){         # leaf water potential at 50% loss of stomatal conductance (Pgs50)	[MPa]
         ncvar_put(nc=param.nc, varid='p50_gs', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_leaf"){         # leaf water potential at 50% loss of leaf hydraulic conductivity (P50leaf)	pft	[MPa]
         ncvar_put(nc=param.nc, varid='p50_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_root"){         # root water potential at 50% loss of root hydraulic conductivity	[MPa]
         ncvar_put(nc=param.nc, varid='p50_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_stem"){         # stem water potential at 50% loss of stem hydraulic conductivity [MPa]
         ncvar_put(nc=param.nc, varid='p50_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "water_content_TLP_root"){         # coarse root relative water content at turgor loss (RWCtlproot)
         ncvar_put(nc=param.nc, varid='rwctlp_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "water_content_TLP_sapwood"){         # sapwood relative water content at turgor loss
         ncvar_put(nc=param.nc, varid='rwctlp_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "water_content_TLP_leaf"){         # leaf relative water content at turgor loss
         ncvar_put(nc=param.nc, varid='rwctlp_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leafp_min"){         # Minimum leaf water potential [MPa]
         ncvar_put(nc=param.nc, varid='leafp_min', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "clone_alloc"){         # A carbon allocation that is added on to seed_alloc for trees larger than dbh_max.	[0-1]
         ncvar_put(nc=param.nc, varid='clone_alloc', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "storage_target_ratio"){         # The target fraction of storage carbon over leaf carbon	[0-1]
         ncvar_put(nc=param.nc, varid='cushion', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "crown_depth_fraction"){         # Crown depth fraction of a cohort relative to its total height [0-1]
         ncvar_put(nc=param.nc, varid='crown', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "hydraulic_stress_mortality"){         # The mortality rate imposed on plants meeting hydraulic stress failure condition [1/yr]
         ncvar_put(nc=param.nc, varid='stress_mort', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "PPA_comp_exclusion"){         # Competetive exclusion parameter for weighting demotions from the upper canopy classification in PPA
         ncvar_put(nc=param.nc, varid='comp_excln', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "grass_spread"){         # Controls the area footprint of a grass pft, as a function of number density and dbh
         ncvar_put(nc=param.nc, varid='grass_spread', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_stor_priority"){         # Leaf turnover vs labile carbon use prioritisation. This is the fraction of maintenance demand that will be replenished at all costs and before storage is filled.	pft	[0-1]
         ncvar_put(nc=param.nc, varid='leaf_stor_priority', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "understory_treefall_mortality"){         # The fraction of trees in understory that die from impacts of large treefalls. In the model this is not a rate 1/year, so we need to convert it
         ncvar_put(nc=param.nc, varid='understorey_death', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "displar"){         # 
         ncvar_put(nc=param.nc, varid='displar', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "z0mr"){         # Ratio of momentum roughness length to canopy top height
         ncvar_put(nc=param.nc, varid='z0mr', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "inital_stem_density"){         # Stem density of different PFTs during a bare ground initialization.	[/m2]
         ncvar_put(nc=param.nc, varid='initd', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "bark_scaler"){         # Fraction of tree diameter that is bark. Used in fire.	[0-1]
         ncvar_put(nc=param.nc, varid='bark_scaler', start = ipft, count = 1,
                   vals=pft[v])
       }

       ## BINARY FLAGS: These should be set-able by PEcAn but not sampled
       if(var == "photosynthetic_pathway"){         # 
         ncvar_put(nc=param.nc, varid='c3psn', start = ipft, count = 1,
                   vals=as.numeric(pft[v] == 3))
       }
       if(var == "crop"){         # Binary crop flag: 0. = not crop, 1. = crop
         ncvar_put(nc=param.nc, varid='', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "irrigated"){         # Binary Irrigated PFT flag
         ncvar_put(nc=param.nc, varid='irrigated', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "cold_deciduous"){         # Binary flag for seasonal-deciduous leaf habit (0-not,1-it is)
         ncvar_put(nc=param.nc, varid='season_decid', start = ipft, count = 1,
                   vals=pft[v])
         ncvar_put(nc=param.nc, varid='evergreen', start = ipft, count = 1,
                   vals=0)
       }
       if(var == "stress_deciduous"){         # Binary flag for stress-deciduous leaf habit (0-not,1-it is)
         ncvar_put(nc=param.nc, varid='stress_decid', start = ipft, count = 1,
                   vals=pft[v])
         ncvar_put(nc=param.nc, varid='evergreen', start = ipft, count = 1,
                   vals=0)
       }
       if(var == "woody"){         # Binary woody lifeform flag (0-is not woody, 1-it is woody)
         ncvar_put(nc=param.nc, varid='woody', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "evergreen"){         # Binary flag for evergreen leaf habit
         ncvar_put(nc=param.nc, varid='evergreen', start = ipft, count = 1,
                   vals=pft[v])
         ncvar_put(nc=param.nc, varid='stress_decid', start = ipft, count = 1,
                   vals=0)
         ncvar_put(nc=param.nc, varid='season_decid', start = ipft, count = 1,
                   vals=0)
       }
       
       ## NEED TO INQUIRE ABOUT UNITS / MEANING
       # Vcmax (umol CO2 m-2 s-1) -> fnitr -- need to know units
       # growth_resp_factor or growth_respiration_coefficient -> grperc r_growth = grperc * (gpp+r_maint)
       # HTMAX -> max_dbh
       # cuticular_cond	umol H2O m-2 s-1 -> gsmin [m s-1]
       # seed_rain [seed m-2 yr-1] -> seed_rain	Seed rain input from outside the patch.	pft	[kgC/m2/year]
       # crown_kill	Mortality from fire scorching susceptibility parameter.	pft	[?]

       ### NEED TO CHANGE BETY
       # mort3 -> background_mort_rate
       # r_fract -> seed_alloc	Fraction of carbon balance remaining after maintenance costs have been met that is dedicated to seed production.	pft	[0-1]
       # agf_bs -> ag_biomass	The fraction of sapwood and structural biomass that is above ground.	-	[0-1]
       
       
       ## ALLPFT indexed (size = 1)
       if(var == "veg_respiration_Q10"){            ## Q10 for maintenance respiration
         ncvar_put(nc=param.nc, varid='q10_mr', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "CelluloseS"){            ## Cellulose fraction for CWD
         ncvar_put(nc=param.nc, varid='cwd_fcel', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "s_lignin"){            ## Lignin fraction for CWD
         ncvar_put(nc=param.nc, varid='cwd_flig', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "c2n_som1"){            ## C:N for SOM pool 1
         ncvar_put(nc=param.nc, varid='cn_s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "c2n_som2"){            ## C:N for SOM pool 2
         ncvar_put(nc=param.nc, varid='cn_s2_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "c2n_som3"){            ## C:N for SOM pool 3
         ncvar_put(nc=param.nc, varid='cn_s3_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "cnscalefactor"){            ## Scale factor on CN decomposition for assigning methane flux
         ncvar_put(nc=param.nc, varid='cnscalefactor', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "decomp_depth_efolding"){            ## e-folding depth for reduction in decomposition. Set to large number for depth-independance
         ncvar_put(nc=param.nc, varid='decomp_depth_efolding', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_fragmentation_rate"){            ## Fragmentation rate for CWD
         ncvar_put(nc=param.nc, varid='k_frag', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_cwdl2_bgc"){            ## respiration fraction from CWD to litter 2
         ncvar_put(nc=param.nc, varid='rf_cwdl2_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_cwdl3_bgc"){            ## respiration fraction from CWD to litter 3
         ncvar_put(nc=param.nc, varid='rf_cwdl3_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_l1s1_bgc"){            ## Respiration fraction for litter 1 -> SOM 1
         ncvar_put(nc=param.nc, varid='rf_l1s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_l2s1_bgc"){            ## respiration fraction litter 2 to SOM 1
         ncvar_put(nc=param.nc, varid='rf_l2s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_l3s2_bgc"){            ## respiration fraction from litter 3 to SOM 2
         ncvar_put(nc=param.nc, varid='rf_l3s2_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_s2s1_bgc"){            ## respiration fraction SOM 2 to SOM 1
         ncvar_put(nc=param.nc, varid='rf_s2s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_s2s3_bgc"){            ## Respiration fraction for SOM 2 -> SOM 3
         ncvar_put(nc=param.nc, varid='rf_s2s3_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_s3s1_bgc"){            ## respiration fraction SOM 3 to SOM 1
         ncvar_put(nc=param.nc, varid='rf_s3s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "Q10_frozen_soil"){            ## Separate q10 for frozen soil respiration rates
         ncvar_put(nc=param.nc, varid='froz_q10', start = 1, count = 1,
                   vals=pft[v])
       }
       
       ## NONE indexed
       ##   -- FIRE
       if(var == "max_fire_duration"){            ## maximum duration of fire	none	hours
         ncvar_put(nc=param.nc, varid='max_durat',vals=pft[v])
       }
       if(var == "nfires"){            ## The number of fires initiated per m2 per year, from lightning and humans
         ncvar_put(nc=param.nc, varid='nfires',vals=pft[v])
       }
       if(var == "fuel_energy"){            ## energy content of fuel [kj kg-1]
         ncvar_put(nc=param.nc, varid='fuel_energy',vals=pft[v])
       }
       if(var == "fuel_particle_density"){            ## particle density of fuel [kg m-3]
         ncvar_put(nc=param.nc, varid='part_dens',vals=pft[v])
       }
       if(var == "durat_slope"){            ## SPITFIRE: change in fire duration with fire danger index. from Canadian Forest Service	
         ncvar_put(nc=param.nc, varid='durat_slope',vals=pft[v])
       }
       if(var == "miner_damp"){            ## SPITFIRE mineral dampening coefficient
         ncvar_put(nc=param.nc, varid='miner_damp',vals=pft[v])
       }
       if(var == "fuel_minerals"){            ## mineral content of fuel
         ncvar_put(nc=param.nc, varid='miner_total',vals=pft[v])
       }
       if(var == "alpha_scorch_height"){            ## SPITFIRE scorch height parameter
         ncvar_put(nc=param.nc, varid='alpha_SH',vals=pft[v])
       }
       if(var == "fdi_a"){            ## SPITFIRE Constant in calculation of dewpoint for Fire Danger Index (FDI)
         ncvar_put(nc=param.nc, varid='fdi_a',vals=pft[v])
       }
       if(var == "fdi_b"){            ## SPITFIRE Constant in calculation of dewpoint for Fire Danger Index (FDI)
         ncvar_put(nc=param.nc, varid='fdi_b',vals=pft[v])
       }
       # fdi_alpha	SPITFIRE fire danger index (FDI) coefficient	
       if(var == ""){            ## 
         ncvar_put(nc=param.nc, varid='',vals=pft[v])
       }
       ##   -- CANOPY
       if(var == "canopy_max_spread"){            ## Maximum allowable "dynamic ratio of dbh to canopy area" for cohorts in closed canopies.	-	[cm/m2]
         ncvar_put(nc=param.nc, varid='maxspread',vals=pft[v])
       }
       # 	
       if(var == "canopy_min_spread"){            ## Minimum allowable "dynamic ratio of dbh to canopy area" for cohorts in closed canopies.	-	[cm/m2]
         ncvar_put(nc=param.nc, varid='minspread',vals=pft[v])
       }

       ## LITTERCLASS indexed (Size:6)
       # low_moisture_C	Intercept (constant) of fuel moisture to burned fraction term for drier fuel	litterclass	
       # low_moisture_S	Slope of fuel moisture to burned fraction term for drier fuel	litterclass	
       # max_decomp	Maximum decomposition rate of litter in the absence of moisture or temperature stress, per fuel class	litterclass	y-1
       # mid_moisture	Parameter of burned fraction term. Below this 'low' constants apply, above this, 'mid' constants apply, 	litterclass	
       # mid_moisture_C	Intercept (constant) of fuel moisture to burned fraction term for wetter fuel	litterclass	
       # min_moisture	Parameter of burned fraction term. Below this value all litter is burned by a fire. Above, 'low' constants apply	litterclass	
       # FBD	Fuel Bulk Density of fuel class 	litterclass	kg m-3
       # alpha_FMC	Parameter of function relating fuel moisture content to meteorological fire danger index 	litterclass	
       # SAV	Surface Area to Volume Ratio of fuel class 	litterclass	cm-1
       
       ## NCWD dimensioned       Size:4
       if(var == "CWD_frac1"){            ##Fraction of coarse woody debris (CWD) that is moved into each of the four woody fuel classes
         ncvar_put(nc=param.nc, varid='CWD_frac', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_frac2"){
         ncvar_put(nc=param.nc, varid='CWD_frac', start = 2, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_frac3"){
         ncvar_put(nc=param.nc, varid='CWD_frac', start = 3, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_frac4"){
         ncvar_put(nc=param.nc, varid='CWD_frac', start = 4, count = 1,
                   vals=pft[v])
       }
       
       
     } ## end loop over VARIABLES
   } ## end loop over PFTs
   ncdf4::nc_close(param.nc)
   
#   ## Write SETTINGS file
#     
 }
