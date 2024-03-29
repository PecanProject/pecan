#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' @name model2netcdf.FATES
##' @title Code to convert FATES netcdf output into into CF standard
##'
##' @param outdir Location of FATES model output
##' 
##' @examples  
##' \dontrun{
##' example.output <- system.file("case.clm2.h0.2004-01-01-00000.nc",package="PEcAn.FATES")
##' model2netcdf.FATES(outdir="~/")
##' }
##' 
##' @export
##'
##' @author Michael Dietze, Shawn Serbin
model2netcdf.FATES <- function(outdir) {

    # E.g. var_update("AR","AutoResp","kgC m-2 s-1", "Autotrophic Respiration")
    # currently only works for xyt variables, need to expand to work for cohort-level outputs, 
    # age bins, soils, etc
    var_update <- function(out,oldname,newname,newunits=NULL,long_name=NULL){
      if (oldname %in% ncin_names) {
        ## define variable
        oldunits <- ncdf4::ncatt_get(ncin,oldname,"units")$value
        if (oldunits=="gC/m^2/s") oldunits <- "gC m-2 s-1"
        if (oldname=="TLAI" && oldunits=="none") oldunits <- "m2 m-2"
        if(is.null(newunits)) newunits = oldunits
        newvar <- ncdf4::ncvar_def(name = newname, units = newunits, longname=long_name, dim = xyt)
        
        ## convert data
        dat <- ncdf4::ncvar_get(ncin,oldname)
        dat.new <- PEcAn.utils::misc.convert(dat,oldunits,newunits)
        
        ## prep for writing
        if(is.null(out)) {
          out <- list(var <- list(),dat <- list())
          out$var[[1]] <- newvar
          out$dat[[1]] <- dat.new
        } else {
          i <- length(out$var) + 1
          out$var[[i]] <- newvar
          out$dat[[i]] <- dat.new
        }
      } else {
        ## correct way to "skip" and output variables that may be missing in the HLM-FATES output?
        PEcAn.logger::logger.info(paste0("HLM-FATES variable: ", oldname," not present. Skipping conversion"))
      }
      return(out)
    }
    
    ## Get files and years
    files <- dir(outdir, "*clm2.h0.*.nc", full.names = TRUE)  # currently specific to clm2.h0 files
    file.dates <- as.Date(sub(".nc", "", sub(".*clm2.h0.", "", files)))
    years <- lubridate::year(file.dates)
    init_year <- unique(years)[1]

    ## Loop over years
    for (year in unique(years)) {
        ysel <- which(years == year)  ## subselect files for selected year
        if (length(ysel) > 1) {
            PEcAn.logger::logger.warn("PEcAn.FATES::model2netcdf.FATES does not currently support multiple files per year")
        }
        
        fname <- files[ysel[1]]
        oname <- file.path(dirname(fname), paste0(year, ".nc"))
        PEcAn.logger::logger.info(paste("model2netcdf.FATES - Converting:",  fname, "to", oname))
        ncin <- ncdf4::nc_open(fname, write = TRUE)
        ncin_names <- names(ncin$var)                               # get netCDF variable names in HLM-FATES output
        
        
        ## FATES time is in multiple columns, create 'time'
        mcdate <- ncdf4::ncvar_get(ncin, "mcdate")                  # current date (YYYYMMDD)
        if (length(mcdate)==1) {
          ## do we need to bother converting outputs where FATES provides only a single timepoint for a date?
          ## usually happens when the model starts/finishes at the end/start of a new year
          PEcAn.logger::logger.debug("*** Skipping conversion for output with only a single timepoint ***")
          next
        }
        cal_dates <- as.Date(as.character(mcdate),format="%Y%m%d")  # in standard YYYY-MM-DD format
        julian_dates <- lubridate::yday(cal_dates)                  # current year DOY values
        day  <- ncdf4::ncvar_get(ncin, "mdcur")                     # current day (from base day)
        sec  <- ncdf4::ncvar_get(ncin, "mscur")                     # current seconds of current day
        nstep <- ncdf4::ncvar_get(ncin, "nstep")                    # model time step
        time <- day + sec / 86400                                   # fractional time since base date (typically first day of full model simulation)
        iter_per_day <- length(unique(sec))                         # how many outputs per day (e.g. 1, 24, 48)
        timesteps <- utils::head(seq(0, 1, by = 1 / iter_per_day), -1)     # time of day fraction
        current_year_tvals <- (julian_dates-1 + timesteps)          # fractional DOY of current year
        nt <- length(time)                                          # output length
        nc_time <- ncin$dim$time$vals                               # days since "start_date"

        # !! Is this a useful/reasonable check? That is that our calculated time
        # matches FATES internal time var.
        if (length(time)!=length(nc_time)) {
          PEcAn.logger::logger.severe("Time dimension mismatch in output, simulation error?")
        }

        ## Create time bounds to populate time_bounds variable
        bounds <- array(data = NA, dim = c(length(time), 2))
        bounds[, 1] <- time
        bounds[, 2] <- bounds[, 1] + (1 / iter_per_day)
        bounds <- round(bounds, 4)  # create time bounds for each timestep in t, t+1; t+1, t+2... format

        #******************** Declare netCDF dimensions ********************#
        nc_var  <- list()
        sitelat <- ncdf4::ncvar_get(ncin,"lat")
        sitelon <- ncdf4::ncvar_get(ncin,"lon")
        ## time variable based on internal calc, nc$dim$time is the FATES output time
        t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", init_year, "-01-01 00:00:00"),
                       vals = as.vector(time), calendar = "noleap", unlim = TRUE)
        time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                          longname = "history time interval endpoint dimensions", 
                                          vals = 1:2, units = "")
        lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "coordinate_latitude")
        lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "coordinate_longitude")
        xyt <- list(lon, lat, t)
        
        ### build netCDF data
        ## !! TODO: ADD MORE OUTPUTS HERE
        out <- NULL
        out <- var_update(out,"AR","AutoResp","kgC m-2 s-1","Autotrophic Respiration")
        out <- var_update(out,"HR","HeteroResp","kgC m-2 s-1","Heterotrophic Respiration")
        out <- var_update(out,"GPP","GPP","kgC m-2 s-1","Gross Primary Productivity")
        out <- var_update(out,"NPP","NPP","kgC m-2 s-1","Net Primary Productivity")
        out <- var_update(out,"NEP","NEE","kgC m-2 s-1", "Net Ecosystem Exchange")
        out <- var_update(out,"FLDS","LWdown","W m-2","Surface incident longwave radiation") 
        out <- var_update(out,"FSDS","SWdown","W m-2","Surface incident shortwave radiation")
        out <- var_update(out,"TBOT","Tair","K","Near surface air temperature") # not certain these are equivelent yet
        out <- var_update(out,"QBOT","Qair","kg kg-1","Near surface specific humidity") # not certain these are equivelent yet
        out <- var_update(out,"RH","RH","%","Relative Humidity") 
        out <- var_update(out,"WIND","Wind","m s-1","Near surface module of the wind") # not certain these are equivelent yet
        out <- var_update(out,"EFLX_LH_TOT","Qle","W m-2","Latent heat")
        out <- var_update(out,"QVEGT","Transp","mm s-1","Total Transpiration") ## equiv to std of kg m-2 s but don't trust udunits to get right
        out <- var_update(out,"ED_balive","TotLivBiom","kgC m-2","Total living biomass")
        out <- var_update(out,"ED_biomass","AbvGrndWood","kgC m-2","Above ground woody biomass")  # not actually correct, need to update
        out <- var_update(out,"AGB","AGB","kgC m-2","Total aboveground biomass") # not actually correct, need to update
        out <- var_update(out,"ED_bleaf","leaf_carbon_content","kgC m-2","Leaf Carbon Content")
        out <- var_update(out,"TLAI","LAI","m2 m-2","Leaf Area Index")
        out <- var_update(out,"TSOI_10CM","SoilTemp","K","Average Layer Soil Temperature at 10cm")
        
        ## put in time_bounds before writing out new nc file
        length(out$var)
        out$var[[length(out$var) + 1]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                                           longname = "history time interval endpoints", 
                                                           dim=list(time_interval,time = t), 
                                                           prec = "double")
        out$dat[[length(out$dat) + 1]] <- c(rbind(bounds[, 1], bounds[, 2]))
  
        ## close input nc file
        try(ncdf4::nc_close(ncin))
        
        ## write netCDF data
        ncout <- ncdf4::nc_create(oname,out$var)
        ncdf4::ncatt_put(ncout, "time", "bounds", "time_bounds", prec=NA)
        for (i in seq_along(out$var)) {
          ncdf4::ncvar_put(ncout, out$var[[i]], out$dat[[i]])
        }

        ## extract variable and long names to VAR file for PEcAn vis
        utils::write.table(sapply(ncout$var, function(x) { x$longname }), 
                    file = paste0(oname, ".var"), 
                    col.names = FALSE, 
                    row.names = TRUE, 
                    quote = FALSE)
        
        
        try(ncdf4::nc_close(ncout))

    } # end of year for loop
} # model2netcdf.FATES

### !!! NOTES
### extract variables. These need to be read in and converted to PEcAN standard

#   levgrnd:long_name = "coordinate soil levels" ;
#   levlak:long_name = "coordinate lake levels" ;
#   levdcmp:long_name = "coordinate soil levels" ;
#   mcdate:long_name = "current date (YYYYMMDD)" ;
#   mcsec:long_name = "current seconds of current date" ;
#   mdcur:long_name = "current day (from base day)" ;
#   mscur:long_name = "current seconds of current day" ;
#   nstep:long_name = "time step" ;
#   lon:long_name = "coordinate longitude" ;
#   lat:long_name = "coordinate latitude" ;
#   area:long_name = "grid cell areas" ;
#   topo:long_name = "grid cell topography" ;
#   landfrac:long_name = "land fraction" ;
#   landmask:long_name = "land/ocean mask (0.=ocean and 1.=land)" ;
#   pftmask:long_name = "pft real/fake mask (0.=fake and 1.=real)" ;
#   ZSOI:long_name = "soil depth" ;
#   DZSOI:long_name = "soil thickness" ;
#   WATSAT:long_name = "saturated soil water content (porosity)" ;
#   SUCSAT:long_name = "saturated soil matric potential" ;
#   BSW:long_name = "slope of soil water retention curve" ;
#   HKSAT:long_name = "saturated hydraulic conductivity" ;
#   ZLAKE:long_name = "lake layer node depth" ;
#   DZLAKE:long_name = "lake layer thickness" ;
#   ACTUAL_IMMOB:long_name = "actual N immobilization" ;
#   AGNPP:long_name = "aboveground NPP" ;
#   ALT:long_name = "current active layer thickness" ;
#   ALTMAX:long_name = "maximum annual active layer thickness" ;
#   ALTMAX_LASTYEAR:long_name = "maximum prior year active layer thickness" ;
#   AR:long_name = "autotrophic respiration (MR + GR)" ;
#   BAF_CROP:long_name = "fractional area burned for crop" ;
#   BAF_PEATF:long_name = "fractional area burned in peatland" ;
#   BCDEP:long_name = "total BC deposition (dry+wet) from atmosphere" ;
#   BGNPP:long_name = "belowground NPP" ;
#   BUILDHEAT:long_name = "heat flux from urban building interior to walls and roof" ;
#   COL_CTRUNC:long_name = "column-level sink for C truncation" ;
#   COL_FIRE_CLOSS:long_name = "total column-level fire C loss for non-peat fires outside land-type converted region" ;
#   COL_FIRE_NLOSS:long_name = "total column-level fire N loss" ;
#   COL_NTRUNC:long_name = "column-level sink for N truncation" ;
#   CPOOL:long_name = "temporary photosynthate C pool" ;
#   CWDC:long_name = "CWD C" ;
#   CWDC_HR:long_name = "coarse woody debris C heterotrophic respiration" ;
#   CWDC_LOSS:long_name = "coarse woody debris C loss" ;
#   CWDC_TO_LITR2C:long_name = "decomp. of coarse woody debris C to litter 2 C" ;
#   CWDC_TO_LITR3C:long_name = "decomp. of coarse woody debris C to litter 3 C" ;
#   CWDN:long_name = "CWD N" ;
#   CWDN_TO_LITR2N:long_name = "decomp. of coarse woody debris N to litter 2 N" ;
#   CWDN_TO_LITR3N:long_name = "decomp. of coarse woody debris N to litter 3 N" ;
#   DEADCROOTC:long_name = "dead coarse root C" ;
#   DEADCROOTN:long_name = "dead coarse root N" ;
#   DEADSTEMC:long_name = "dead stem C" ;
#   DEADSTEMN:long_name = "dead stem N" ;
#   DENIT:long_name = "total rate of denitrification" ;
#   DISPVEGC:long_name = "displayed veg carbon, excluding storage and cpool" 
#   DISPVEGN:long_name = "displayed vegetation nitrogen" ;
#   DSTDEP:long_name = "total dust deposition (dry+wet) from atmosphere" ;
#   DSTFLXT:long_name = "total surface dust emission" ;
#   DWT_CLOSS:long_name = "total carbon loss from land cover conversion" ;
#   DWT_CONV_CFLUX:long_name = "conversion C flux (immediate loss to atm)" ;
#   DWT_CONV_NFLUX:long_name = "conversion N flux (immediate loss to atm)" ;
#   DWT_NLOSS:long_name = "total nitrogen loss from landcover conversion" ;
#   DWT_PROD100C_GAIN:long_name = "landcover change-driven addition to 100-yr wood product pool" ;
#   DWT_PROD100N_GAIN:long_name = "addition to 100-yr wood product pool" ;
#   DWT_PROD10C_GAIN:long_name = "landcover change-driven addition to 10-yr wood product pool" ;
#   DWT_PROD10N_GAIN:long_name = "addition to 10-yr wood product pool" ;
#   DWT_SEEDC_TO_DEADSTEM:long_name = "seed source to patch-level deadstem" ;
#   DWT_SEEDC_TO_LEAF:long_name = "seed source to patch-level leaf" ;
#   DWT_SEEDN_TO_DEADSTEM:long_name = "seed source to PFT-level deadstem" ;
#   DWT_SEEDN_TO_LEAF:long_name = "seed source to PFT-level leaf" ;
#   EFLX_DYNBAL:long_name = "dynamic land cover change conversion energy flux" ;
#   EFLX_GRND_LAKE:long_name = "net heat flux into lake/snow surface, excluding light transmission" ;
#   EFLX_LH_TOT:long_name = "total latent heat flux [+ to atm]" ;
#   EFLX_LH_TOT_R:long_name = "Rural total evaporation" ;
#   EFLX_LH_TOT_U:long_name = "Urban total evaporation" ;
#   ELAI:long_name = "exposed one-sided leaf area index" ;
#   ER:long_name = "total ecosystem respiration, autotrophic + heterotrophic" ;
#   ERRH2O:long_name = "total water conservation error" ;
#   ERRH2OSNO:long_name = "imbalance in snow depth (liquid water)" ;
#   ERRSEB:long_name = "surface energy conservation error" ;
#   ERRSOI:long_name = "soil/lake energy conservation error" ;
#   ERRSOL:long_name = "solar radiation conservation error" ;
#   ESAI:long_name = "exposed one-sided stem area index" ;
#   FAREA_BURNED:long_name = "timestep fractional area burned" ;
#   FCEV:long_name = "canopy evaporation" ;
#   FCOV:long_name = "fractional impermeable area" ;
#   FCTR:long_name = "canopy transpiration" ;
#   FGEV:long_name = "ground evaporation" ;
#   FGR:long_name = "heat flux into soil/snow including snow melt and lake / snow light transmission" ;
#   FGR12:long_name = "heat flux between soil layers 1 and 2" ;
#   FGR_R:long_name = "Rural heat flux into soil/snow including snow melt and snow light transmission" ;
#   FGR_U:long_name = "Urban heat flux into soil/snow including snow melt" ;
#   FH2OSFC:long_name = "fraction of ground covered by surface water" ;
#   FIRA:long_name = "net infrared (longwave) radiation" ;
#   FIRA_R:long_name = "Rural net infrared (longwave) radiation" ;
#   FIRA_U:long_name = "Urban net infrared (longwave) radiation" ;
#   FIRE:long_name = "emitted infrared (longwave) radiation" ;
#   FIRE_R:long_name = "Rural emitted infrared (longwave) radiation" ;
#   FIRE_U:long_name = "Urban emitted infrared (longwave) radiation" ;
#   FLDS:long_name = "atmospheric longwave radiation" ;
#   FPG:long_name = "fraction of potential gpp" ;
#   FPI:long_name = "fraction of potential immobilization" ;
#   FPSN:long_name = "photosynthesis" ;
#   FPSN_WC:long_name = "Rubisco-limited photosynthesis" ;
#   FPSN_WJ:long_name = "RuBP-limited photosynthesis" ;
#   FPSN_WP:long_name = "Product-limited photosynthesis" ;
#   FROOTC:long_name = "fine root C" ;
#   FROOTC_ALLOC:long_name = "fine root C allocation" ;
#   FROOTC_LOSS:long_name = "fine root C loss" ;
#   FROOTN:long_name = "fine root N" ;
#   FSA:long_name = "absorbed solar radiation" ;
#   FSAT:long_name = "fractional area with water table at surface" ;
#   FSA_R:long_name = "Rural absorbed solar radiation" ;
#   FSA_U:long_name = "Urban absorbed solar radiation" ;
#   FSDS:long_name = "atmospheric incident solar radiation" ;
#   FSDSND:long_name = "direct nir incident solar radiation" ;
#   FSDSNDLN:long_name = "direct nir incident solar radiation at local noon" ;
#   FSDSNI:long_name = "diffuse nir incident solar radiation" ;
#   FSDSVD:long_name = "direct vis incident solar radiation" ;
#   FSDSVDLN:long_name = "direct vis incident solar radiation at local noon" ;
#   FSDSVI:long_name = "diffuse vis incident solar radiation" ;
#   FSDSVILN:long_name = "diffuse vis incident solar radiation at local noon" ;
#   FSH:long_name = "sensible heat" ;
#   FSH_G:long_name = "sensible heat from ground" ;
#   FSH_NODYNLNDUSE:long_name = "sensible heat not including correction for land use change" ;
#   FSH_R:long_name = "Rural sensible heat" ;
#   FSH_U:long_name = "Urban sensible heat" ;
#   FSH_V:long_name = "sensible heat from veg" ;
#   FSM:long_name = "snow melt heat flux" ;
#   FSM_R:long_name = "Rural snow melt heat flux" ;
#   FSM_U:long_name = "Urban snow melt heat flux" ;
#   FSNO:long_name = "fraction of ground covered by snow" ;
#   FSNO_EFF:long_name = "effective fraction of ground covered by snow" ;
#   FSR:long_name = "reflected solar radiation" ;
#   FSRND:long_name = "direct nir reflected solar radiation" ;
#   FSRNDLN:long_name = "direct nir reflected solar radiation at local noon" ;
#   FSRNI:long_name = "diffuse nir reflected solar radiation" ;
#   FSRVD:long_name = "direct vis reflected solar radiation" ;
#   FSRVDLN:long_name = "direct vis reflected solar radiation at local noon" ;
#   FSRVI:long_name = "diffuse vis reflected solar radiation" ;
#   FUELC:long_name = "fuel load" ;
#   GC_HEAT1:long_name = "initial gridcell total heat content" ;
#   GC_ICE1:long_name = "initial gridcell total ice content" ;
#   GC_LIQ1:long_name = "initial gridcell total liq content" ;
#   GPP:long_name = "gross primary production" ;
#   GR:long_name = "total growth respiration" ;
#   GROSS_NMIN:long_name = "gross rate of N mineralization" ;
#   H2OCAN:long_name = "intercepted water" ;
#   H2OSFC:long_name = "surface water depth" ;
#   H2OSNO:long_name = "snow depth (liquid water)" ;
#   H2OSNO_TOP:long_name = "mass of snow in top snow layer" ;
#   HC:long_name = "heat content of soil/snow/lake" ;
#   HCSOI:long_name = "soil heat content" ;
#   HEAT_FROM_AC:long_name = "sensible heat flux put into canyon due to heat removed from air conditioning" ;
#   HR:long_name = "total heterotrophic respiration" ;
#   HTOP:long_name = "canopy top" ;
#   LAISHA:long_name = "shaded projected leaf area index" ;
#   LAISUN:long_name = "sunlit projected leaf area index" ;
#   LAKEICEFRAC:long_name = "lake layer ice mass fraction" ;
#   LAKEICETHICK:long_name = "thickness of lake ice (including physical expansion on freezing)" ;
#   LAND_UPTAKE:long_name = "NEE minus LAND_USE_FLUX, negative for update" ;
#   LAND_USE_FLUX:long_name = "total C emitted from land cover conversion and wood product pools" ;
#   LEAFC:long_name = "leaf C" ;
#   LEAFC_ALLOC:long_name = "leaf C allocation" ;
#   LEAFC_LOSS:long_name = "leaf C loss" ;
#   LEAFN:long_name = "leaf N" ;
#   LEAF_MR:long_name = "leaf maintenance respiration" ;
#   LFC2:long_name = "conversion area fraction of BET and BDT that burned" ;
#   LF_CONV_CFLUX:long_name = "conversion carbon due to BET and BDT area decreasing" ;
#   LITFALL:long_name = "litterfall (leaves and fine roots)" ;
#   LITHR:long_name = "litter heterotrophic respiration" ;
#   LITR1C:long_name = "LITR1 C" ;
#   LITR1C_TO_SOIL1C:long_name = "decomp. of litter 1 C to soil 1 C" ;
#   LITR1N:long_name = "LITR1 N" ;
#   LITR1N_TNDNCY_VERT_TRANS:long_name = "litter 1 N tendency due to vertical transport" ;
#   LITR1N_TO_SOIL1N:long_name = "decomp. of litter 1 N to soil 1 N" ;
#   LITR1_HR:long_name = "Het. Resp. from litter 1" ;
#   LITR2C:long_name = "LITR2 C" ;
#   LITR2C_TO_SOIL2C:long_name = "decomp. of litter 2 C to soil 2 C" ;
#   LITR2N:long_name = "LITR2 N" ;
#   LITR2N_TNDNCY_VERT_TRANS:long_name = "litter 2 N tendency due to vertical transport" ;
#   LITR2N_TO_SOIL2N:long_name = "decomp. of litter 2 N to soil 2 N" ;
#   LITR2_HR:long_name = "Het. Resp. from litter 2" ;
#   LITR3C:long_name = "LITR3 C" ;
#   LITR3C_TO_SOIL3C:long_name = "decomp. of litter 3 C to soil 3 C" ;
#   LITR3N:long_name = "LITR3 N" ;
#   LITR3N_TNDNCY_VERT_TRANS:long_name = "litter 3 N tendency due to vertical transport" ;
#   LITR3N_TO_SOIL3N:long_name = "decomp. of litter 3 N to soil 3 N" ;
#   LITR3_HR:long_name = "Het. Resp. from litter 3" ;
#   LITTERC:long_name = "litter C" ;
#   LITTERC_HR:long_name = "litter C heterotrophic respiration" ;
#   LITTERC_LOSS:long_name = "litter C loss" ;
#   LIVECROOTC:long_name = "live coarse root C" ;
#   LIVECROOTN:long_name = "live coarse root N" ;
#   LIVESTEMC:long_name = "live stem C" ;
#   LIVESTEMN:long_name = "live stem N" ;
#   MEG_acetaldehyde:long_name = "MEGAN flux" ;
#   MEG_acetic_acid:long_name = "MEGAN flux" ;
#   MEG_acetone:long_name = "MEGAN flux" ;
#   MEG_carene_3:long_name = "MEGAN flux" ;
#   MEG_ethanol:long_name = "MEGAN flux" ;
#   MEG_formaldehyde:long_name = "MEGAN flux" ;
#   MEG_isoprene:long_name = "MEGAN flux" ;
#   MEG_methanol:long_name = "MEGAN flux" ;
#   MEG_pinene_a:long_name = "MEGAN flux" ;
#   MEG_thujene_a:long_name = "MEGAN flux" ;
#   MR:long_name = "maintenance respiration" ;
#   M_LITR1C_TO_LEACHING:long_name = "litter 1 C leaching loss" ;
#   M_LITR2C_TO_LEACHING:long_name = "litter 2 C leaching loss" ;
#   M_LITR3C_TO_LEACHING:long_name = "litter 3 C leaching loss" ;
#   M_SOIL1C_TO_LEACHING:long_name = "soil 1 C leaching loss" ;
#   M_SOIL2C_TO_LEACHING:long_name = "soil 2 C leaching loss" ;
#   M_SOIL3C_TO_LEACHING:long_name = "soil 3 C leaching loss" ;
#   M_SOIL4C_TO_LEACHING:long_name = "soil 4 C leaching loss" ;
#   NBP:long_name = "net biome production, includes fire, landuse, and harvest flux, positive for sink" ;
#   NDEPLOY:long_name = "total N deployed in new growth" ;
#   NDEP_TO_SMINN:long_name = "atmospheric N deposition to soil mineral N" ;
#   NEE:long_name = "net ecosystem exchange of carbon, includes fire, landuse, harvest, and hrv_xsmrpool flux, positive for source" ;
#   NEP:long_name = "net ecosystem production, excludes fire, landuse, and harvest flux, positive for sink" ;
#   NET_NMIN:long_name = "net rate of N mineralization" ;
#   NFIRE:long_name = "fire counts valid only in Reg.C" ;
#   NFIX_TO_SMINN:long_name = "symbiotic/asymbiotic N fixation to soil mineral N" ;
#   NPP:long_name = "net primary production" ;
#   OCDEP:long_name = "total OC deposition (dry+wet) from atmosphere" ;
#   O_SCALAR:long_name = "fraction by which decomposition is reduced due to anoxia" ;
#   PARVEGLN:long_name = "absorbed par by vegetation at local noon" ;
#   PBOT:long_name = "atmospheric pressure" ;
#   PCO2:long_name = "atmospheric partial pressure of CO2" ;
#   PCT_LANDUNIT:long_name = "% of each landunit on grid cell" ;
#   PCT_NAT_PFT:long_name = "% of each PFT on the natural vegetation (i.e., soil) landunit" ;
#   PFT_CTRUNC:long_name = "patch-level sink for C truncation" ;
#   PFT_FIRE_CLOSS:long_name = "total patch-level fire C loss for non-peat fires outside land-type converted region" ;
#   PFT_FIRE_NLOSS:long_name = "total pft-level fire N loss" ;
#   PFT_NTRUNC:long_name = "pft-level sink for N truncation" ;
#   PLANT_NDEMAND:long_name = "N flux required to support initial GPP" ;
#   POTENTIAL_IMMOB:long_name = "potential N immobilization" ;
#   PROD100C:long_name = "100-yr wood product C" ;
#   PROD100C_LOSS:long_name = "loss from 100-yr wood product pool" ;
#   PROD100N:long_name = "100-yr wood product N" ;
#   PROD100N_LOSS:long_name = "loss from 100-yr wood product pool" ;
#   PROD10C:long_name = "10-yr wood product C" ;
#   PROD10C_LOSS:long_name = "loss from 10-yr wood product pool" ;
#   PROD10N:long_name = "10-yr wood product N" ;
#   PROD10N_LOSS:long_name = "loss from 10-yr wood product pool" ;
#   PRODUCT_CLOSS:long_name = "total carbon loss from wood product pools" ;
#   PRODUCT_NLOSS:long_name = "total N loss from wood product pools" ;
#   PSNSHA:long_name = "shaded leaf photosynthesis" ;
#   PSNSHADE_TO_CPOOL:long_name = "C fixation from shaded canopy" ;
#   PSNSUN:long_name = "sunlit leaf photosynthesis" ;
#   PSNSUN_TO_CPOOL:long_name = "C fixation from sunlit canopy" ;
#   Q2M:long_name = "2m specific humidity" ;
#   QBOT:long_name = "atmospheric specific humidity" ;
#   QDRAI:long_name = "sub-surface drainage" ;
#   QDRAI_PERCH:long_name = "perched wt drainage" ;
#   QDRAI_XS:long_name = "saturation excess drainage" ;
#   QDRIP:long_name = "throughfall" ;
#   QFLOOD:long_name = "runoff from river flooding" ;
#   QFLX_ICE_DYNBAL:long_name = "ice dynamic land cover change conversion runoff flux" ;
#   QFLX_LIQ_DYNBAL:long_name = "liq dynamic land cover change conversion runoff flux" ;
#   QH2OSFC:long_name = "surface water runoff" ;
#   QINFL:long_name = "infiltration" ;
#   QINTR:long_name = "interception" ;
#   QIRRIG:long_name = "water added through irrigation" ;
#   QOVER:long_name = "surface runoff" ;
#   QRGWL:long_name = "surface runoff at glaciers (liquid only), wetlands, lakes" ;
#   QRUNOFF:long_name = "total liquid runoff (does not include QSNWCPICE)" ;
#   QRUNOFF_NODYNLNDUSE:long_name = "total liquid runoff (does not include QSNWCPICE) not including correction for land use change" ;
#   QRUNOFF_R:long_name = "Rural total runoff" ;
#   QRUNOFF_U:long_name = "Urban total runoff" ;
#   QSNOMELT:long_name = "snow melt" ;
#   QSNWCPICE:long_name = "excess snowfall due to snow capping" ;
#   QSNWCPICE_NODYNLNDUSE:long_name = "excess snowfall due to snow capping not including correction for land use change" ;
#   QSOIL:long_name = "Ground evaporation (soil/snow evaporation + soil/snow sublimation - dew)" ;
#   QVEGE:long_name = "canopy evaporation" ;
#   QVEGT:long_name = "canopy transpiration" ;
#   RAIN:long_name = "atmospheric rain" ;
#   RETRANSN:long_name = "plant pool of retranslocated N" ;
#   RETRANSN_TO_NPOOL:long_name = "deployment of retranslocated N" ;
#   RH2M:long_name = "2m relative humidity" ;
#   RH2M_R:long_name = "Rural 2m specific humidity" ;
#   RH2M_U:long_name = "Urban 2m relative humidity" ;
#   RR:long_name = "root respiration (fine root MR + total root GR)" ;
#   RSCANOPY:long_name = "canopy resistance" ;
#   SABG:long_name = "solar rad absorbed by ground" ;
#   SABG_PEN:long_name = "Rural solar rad penetrating top soil or snow layer" ;
#   SABV:long_name = "solar rad absorbed by veg" ;
#   SEEDC:long_name = "pool for seeding new Patches" ;
#   SEEDN:long_name = "pool for seeding new PFTs" ;
#   SMINN:long_name = "soil mineral N" ;
#   SMINN_LEACHED:long_name = "soil mineral N pool loss to leaching" ;
#   SMINN_TO_DENIT_L1S1:long_name = "denitrification for decomp. of litter 1to SOIL1" ;
#   SMINN_TO_DENIT_L2S2:long_name = "denitrification for decomp. of litter 2to SOIL2" ;
#   SMINN_TO_DENIT_L3S3:long_name = "denitrification for decomp. of litter 3to SOIL3" ;
#   SMINN_TO_DENIT_S1S2:long_name = "denitrification for decomp. of soil 1to SOIL2" ;
#   SMINN_TO_DENIT_S2S3:long_name = "denitrification for decomp. of soil 2to SOIL3" ;
#   SMINN_TO_DENIT_S3S4:long_name = "denitrification for decomp. of soil 3to SOIL4" ;
#   SMINN_TO_DENIT_S4:long_name = "denitrification for decomp. of soil 4to atmosphe" ;
#   SMINN_TO_NPOOL:long_name = "deployment of soil mineral N uptake" ;
#   SMINN_TO_PLANT:long_name = "plant uptake of soil mineral N" ;
#   SMINN_TO_SOIL1N_L1:long_name = "mineral N flux for decomp. of LITR1to SOIL1" ;
#   SMINN_TO_SOIL2N_L2:long_name = "mineral N flux for decomp. of LITR2to SOIL2" ;
#   SMINN_TO_SOIL2N_S1:long_name = "mineral N flux for decomp. of SOIL1to SOIL2" ;
#   SMINN_TO_SOIL3N_L3:long_name = "mineral N flux for decomp. of LITR3to SOIL3" ;
#   SMINN_TO_SOIL3N_S2:long_name = "mineral N flux for decomp. of SOIL2to SOIL3" ;
#   SMINN_TO_SOIL4N_S3:long_name = "mineral N flux for decomp. of SOIL3to SOIL4" ;
#   SNOBCMCL:long_name = "mass of BC in snow column" ;
#   SNOBCMSL:long_name = "mass of BC in top snow layer" ;
#   SNODSTMCL:long_name = "mass of dust in snow column" ;
#   SNODSTMSL:long_name = "mass of dust in top snow layer" ;
#   SNOINTABS:long_name = "Percent of incoming solar absorbed by lower snow layers" ;
#   SNOOCMCL:long_name = "mass of OC in snow column" ;
#   SNOOCMSL:long_name = "mass of OC in top snow layer" ;
#   SNOW:long_name = "atmospheric snow" ;
#   SNOWDP:long_name = "gridcell mean snow height" ;
#   SNOWICE:long_name = "snow ice" ;
#   SNOWLIQ:long_name = "snow liquid water" ;
#   SNOW_DEPTH:long_name = "snow height of snow covered area" ;
#   SNOW_SINKS:long_name = "snow sinks (liquid water)" ;
#   SNOW_SOURCES:long_name = "snow sources (liquid water)" ;
#   SOIL1C:long_name = "SOIL1 C" ;
#   SOIL1C_TO_SOIL2C:long_name = "decomp. of soil 1 C to soil 2 C" ;
#   SOIL1N:long_name = "SOIL1 N" ;
#   SOIL1N_TNDNCY_VERT_TRANS:long_name = "soil 1 N tendency due to vertical transport" ;
#   SOIL1N_TO_SOIL2N:long_name = "decomp. of soil 1 N to soil 2 N" ;
#   SOIL1_HR:long_name = "Het. Resp. from soil 1" ;
#   SOIL2C:long_name = "SOIL2 C" ;
#   SOIL2C_TO_SOIL3C:long_name = "decomp. of soil 2 C to soil 3 C" ;
#   SOIL2N:long_name = "SOIL2 N" ;
#   SOIL2N_TNDNCY_VERT_TRANS:long_name = "soil 2 N tendency due to vertical transport" ;
#   SOIL2N_TO_SOIL3N:long_name = "decomp. of soil 2 N to soil 3 N" ;
#   SOIL2_HR:long_name = "Het. Resp. from soil 2" ;
#   SOIL3C:long_name = "SOIL3 C" ;
#   SOIL3C_TO_SOIL4C:long_name = "decomp. of soil 3 C to soil 4 C" ;
#   SOIL3N:long_name = "SOIL3 N" ;
#   SOIL3N_TNDNCY_VERT_TRANS:long_name = "soil 3 N tendency due to vertical transport" ;
#   SOIL3N_TO_SOIL4N:long_name = "decomp. of soil 3 N to soil 4 N" ;
#   SOIL3_HR:long_name = "Het. Resp. from soil 3" ;
#   SOIL4C:long_name = "SOIL4 C" ;
#   SOIL4N:long_name = "SOIL4 N" ;
#   SOIL4N_TNDNCY_VERT_TRANS:long_name = "soil 4 N tendency due to vertical transport" ;
#   SOIL4N_TO_SMINN:long_name = "mineral N flux for decomp. of SOIL4" ;
#   SOIL4_HR:long_name = "Het. Resp. from soil 4" ;
#   SOILC:long_name = "soil C" ;
#   SOILC_HR:long_name = "soil C heterotrophic respiration" ;
#   SOILC_LOSS:long_name = "soil C loss" ;
#   SOILPSI:long_name = "soil water potential in each soil layer" ;
#   SOMC_FIRE:long_name = "C loss due to peat burning" ;
#   SOMHR:long_name = "soil organic matter heterotrophic respiration" ;
#   SOM_C_LEACHED:long_name = "total flux of C from SOM pools due to leaching" ;
#   SR:long_name = "total soil respiration (HR + root resp)" ;
#   STORVEGC:long_name = "stored vegetation carbon, excluding cpool" ;
#   STORVEGN:long_name = "stored vegetation nitrogen" ;
#   SUPPLEMENT_TO_SMINN:long_name = "supplemental N supply" ;
#   SoilAlpha:long_name = "factor limiting ground evap" ;
#   SoilAlpha_U:long_name = "urban factor limiting ground evap" ;
#   TAUX:long_name = "zonal surface stress" ;
#   TAUY:long_name = "meridional surface stress" ;
#   TBOT:long_name = "atmospheric air temperature" ;
#   TBUILD:long_name = "internal urban building temperature" ;
#   TG:long_name = "ground temperature" ;
#   TG_R:long_name = "Rural ground temperature" ;
#   TG_U:long_name = "Urban ground temperature" ;
#   TH2OSFC:long_name = "surface water temperature" ;
#   THBOT:long_name = "atmospheric air potential temperature" ;
#   TKE1:long_name = "top lake level eddy thermal conductivity" ;
#   TLAI:long_name = "total projected leaf area index" ;
#   TLAKE:long_name = "lake temperature" ;
#   TOTCOLC:long_name = "total column carbon, incl veg and cpool" ;
#   TOTCOLN:long_name = "total column-level N" ;
#   TOTECOSYSC:long_name = "total ecosystem carbon, incl veg but excl cpool" ;
#   TOTECOSYSN:long_name = "total ecosystem N" ;
#   TOTLITC:long_name = "total litter carbon" ;
#   TOTLITN:long_name = "total litter N" ;
#   TOTPFTC:long_name = "total patch-level carbon, including cpool" ;
#   TOTPFTN:long_name = "total PFT-level nitrogen" ;
#   TOTPRODC:long_name = "total wood product C" ;
#   TOTPRODN:long_name = "total wood product N" ;
#   TOTSOMC:long_name = "total soil organic matter carbon" ;
#   TOTSOMN:long_name = "total soil organic matter N" ;
#   TOTVEGC:long_name = "total vegetation carbon, excluding cpool" ;
#   TOTVEGN:long_name = "total vegetation nitrogen" ;
#   TREFMNAV:long_name = "daily minimum of average 2-m temperature" ;
#   TREFMNAV_R:long_name = "Rural daily minimum of average 2-m temperature" ;
#   TREFMNAV_U:long_name = "Urban daily minimum of average 2-m temperature" ;
#   TREFMXAV:long_name = "daily maximum of average 2-m temperature" ;
#   TREFMXAV_R:long_name = "Rural daily maximum of average 2-m temperature" ;
#   TREFMXAV_U:long_name = "Urban daily maximum of average 2-m temperature" ;
#   TSA:long_name = "2m air temperature" ;
#   TSAI:long_name = "total projected stem area index" ;
#   TSA_R:long_name = "Rural 2m air temperature" ;
#   TSA_U:long_name = "Urban 2m air temperature" ;
#   TSOI_10CM:long_name = "soil temperature in top 10cm of soil" ;
#   TV:long_name = "vegetation temperature" ;
#   TWS:long_name = "total water storage" ;
#   T_SCALAR:long_name = "temperature inhibition of decomposition" ;
#   U10:long_name = "10-m wind" ;
#   URBAN_AC:long_name = "urban air conditioning flux" ;
#   URBAN_HEAT:long_name = "urban heating flux" ;
#   VOCFLXT:long_name = "total VOC flux into atmosphere" ;
#   VOLR:long_name = "river channel water storage" ;
#   WASTEHEAT:long_name = "sensible heat flux from heating/cooling sources of urban waste heat" ;
#   WF:long_name = "soil water as frac. of whc for top 0.05 m" ;
#   WIND:long_name = "atmospheric wind velocity magnitude" ;
#   WOODC:long_name = "wood C" ;
#   WOODC_ALLOC:long_name = "wood C eallocation" ;
#   WOODC_LOSS:long_name = "wood C loss" ;
#   WOOD_HARVESTC:long_name = "wood harvest carbon (to product pools)" ;
#   WOOD_HARVESTN:long_name = "wood harvest N (to product pools)" ;
#   W_SCALAR:long_name = "Moisture (dryness) inhibition of decomposition" 
## ==================================================================================================#
## EOF
