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
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' 
##' @examples  
##' \dontrun{
##' example.output <- system.file("case.clm2.h0.2004-01-01-00000.nc",package="PEcAn.FATES")
##' model2netcdf.FATES(outdir="~/",sitelat=42.56341,sitelon=289.1853,start_date="2004-01-01 00:00:00",
##' end_date="2005-12-31 00:00:00")
##' }
##' 
##' @export
##'
##' @author Michael Dietze, Shawn Serbin
#model2netcdf.FATES <- function(outdir) {
model2netcdf.FATES <- function(outdir, sitelat, sitelon, start_date, end_date) {
    
    ## Load functions
    mstmipvar <- PEcAn.utils::mstmipvar
    ncdim_def <- ncdf4::ncdim_def
    ncvar_def <- ncdf4::ncvar_def
    ncatt_get <- ncdf4::ncatt_get
    ncvar_add <- ncdf4::ncvar_add
    misc.convert <- PEcAn.utils::misc.convert # unit conversions
    logger.info <- PEcAn.utils::logger.info
    logger.severe <- PEcAn.utils::logger.severe
    logger.warn <- PEcAn.utils::logger.warn
      
    ## needs to be a generic function placed in utils, same goes for ED2 version of this function
    ## also not all vars will be in every FATES run
    getnetCDF <- function(nc, var) {
      if (var %in% names(nc$var)) {
        return(ncdf4::ncvar_get(nc, var))
      } else {
        logger.warn("Could not find", var, "in FATES output.")
        return(-999)
      }
    }

    ## Get files and years
    files <- dir(outdir, "*clm2.h0.*.nc", full.names = TRUE)
    file.dates <- as.Date(sub(".nc", "", sub(".*clm2.h0.", "", files)))
    file.years <- lubridate::year(file.dates)
    num.years <- length(unique(file.years))
    simulation.days <- as.Date(start_date):as.Date(end_date)
    simulation.years <- as.numeric(unique(strftime(as.Date(simulation.days, origin = "1970-01-01"), "%Y")))
    
    # nescessary?  A check that output years and selected run years match
    if (!all(file.years==simulation.years)) {
      logger.severe("FATES output file years and simulation years don't match")
    }
    
    ## Loop over years
    for (year in unique(file.years)) {
        ysel <- which(file.years == year)  ## subselect files for selected year
#        ysel <- sort(file.dates[ysel])  ## double check dates are in order
        if (length(ysel) > 1) {
            logger.warn("PEcAn.FATES::model2netcdf.FATES does not currently support multiple files per year")
        }
        
        # !!useful or do we not want this behavior? !!  Or some sort of clobber flag?
        if (file.exists(file.path(outdir, paste(year, "nc", sep = ".")))) {
          logger.info(paste("model2netcdf.FATES output for",  year, "already converted, starting on the next year"))
          next  ## skip, model output already present.
        }
        
        fname <- files[ysel[1]]
        oname <- file.path(dirname(fname), paste0(year, ".nc"))
        logger.info(paste("model2netcdf.FATES - Converting:",  fname, "to", oname))
        nc <- ncdf4::nc_open(fname, write = TRUE)
        
        ## FATES time is in multiple columns, create 'time'
        day  <- ncdf4::ncvar_get(nc, "mdcur")   #current day (from base day)
        sec  <- ncdf4::ncvar_get(nc, "mscur")   #current seconds of current day
        time <- day + sec / 86400
        nc.time <- nc$dim$time$vals             # days since "start_date"
        # round(time,3)==round(nc.time,3)       # quick check, do the time vars match
        # !! Is this a useful/reasonable check? That is that our calculated time
        # matches FATES internal time var.
        if (length(time)!=length(nc.time)) {
          logger.severe("Time dimension mismatch in output, simulation error?")
        }
        steps.per.day <- length(time)/length(unique(day))
        nstep <- ncdf4::ncvar_get(nc,"nstep") 
        
        ## Parse the rest of FATES output
        output <- list()  # create empty output
        miss.val <- -999  # !!what missing?  FATES default (1e+36) or PEcAn (-999)
        ## if pulling out as a dim def
        #pfts <- ncdim_def(name = "pft", units = "",
        #                            vals = nc$dim$pft$vals, 
        #                            unlim = FALSE)
        ##
        ## Dims as output vars
        #output[[1]] <- nc$dim$levgrnd$vals   # meters
        #ncatt_get(nc,"levgrnd")
        #output[[1]] <- nc$dim$pft$vals       # PFT numbers
        ## !! should these instead be used to define var dims? 
        ## as in the CLM(ED) output
        z <- 1
        output[[z]] <- getnetCDF(nc, "pft_levscpf")         # no units
        #ncatt_get(nc,"pft_levscpf")
        z <- z+1
        output[[z]] <- getnetCDF(nc, "scls_levscpf")        # no units
        #ncatt_get(nc,"scls_levscpf")
        z <- z+1
        output[[z]] <- getnetCDF(nc, "mcdate")              # current date (YYYYMMDD)
        #ncatt_get(nc,"mcdate")
        z <- z+1
        output[[z]] <- getnetCDF(nc, "mcsec")               # current seconds of current date, s
        #ncatt_get(nc,"mcsec")        
        z <- z+1
        output[[z]] <- day                                  # current day (from base day)                         
        #ncatt_get(nc, "mdcur") 
        z <- z+1
        output[[z]] <- sec                                  # current seconds of current day
        #ncatt_get(nc, "mscur") 
        z <- z+1
        output[[z]] <- nstep                                # time step
        
        #z <- z+1
        #output[[z]] <- list(getnetCDF(nc, "time_bounds"))   # history time interval endpoints
        #ncatt_get(nc, "time_bounds")  
        
        #### !!! DO WE NEED THESE?
        #z <- z+1
        #output[[9]] <- getnetCDF(nc, "date_written")        # date file was written mm/dd/yy
        # ncatt_get(nc, "date_written")
        #z <- z+1
        #output[[10]] <- getnetCDF(nc, "time_written")       # time output was written, hh:mm:ss
        #### !!!
        
        ## !! trouble with lat/lon, need to fix
        #z <- z+1
        #output[[z]] <- getnetCDF(nc, "lon")                # longitude, degrees east
        #z <- z+1
        #output[[z]] <- getnetCDF(nc, "lat")                # latitude, degrees north
        ## !!
        
        # 
        z <- z+1
        output[[z]] <- getnetCDF(nc, "area")               # grid cell areas, [lngrid]
        # #ncatt_get(nc, "area")
        z <- z+1
        output[[z]] <- getnetCDF(nc, "landfrac")           # land fraction, landfrac[lndgrid]
        # #ncatt_get(nc, "landfrac")
        # z <- z+1
        # output[[z]] <- getnetCDF(nc, "landmask")           # land/ocean mask (0.=ocean and 1.=land), landmask[lndgrid]
        # #output[[16]] <- NA 
        # #output[[17]] <- NA
        # #output[[18]] <- NA
        # #output[[19]] <- NA
        # # ... many more here still to fill in but now on to something more interesting
        z <- z+1
        output[[z]] <- getnetCDF(nc, "AR")                 # autotrophic respiration, [lndgrid,time]
        # #ncatt_get(nc, "AR")
        output[[z]] <- ifelse(output[[z]] == 1e+36, miss.val, 
                             misc.convert(output[[z]],"umol C m-2 s-1","kg C m-2 s-1"))
        z <- z+1
        output[[z]] <- getnetCDF(nc, "GPP")
        # #ncatt_get(nc, "GPP") # units of GPP
        output[[z]] <- ifelse(output[[z]] == 1e+36, miss.val, 
                              misc.convert(output[[z]],"umol C m-2 s-1","kg C m-2 s-1"))
        
        # ... still more here to go!
        
        #******************** Declare netCDF variables ********************#
        rm(z)
        var  <- list()
        ## time variable based on internal calc, nc$dim$time is the FATES output time
        t <- ncdim_def(name = "time", units = paste0("days since ", year, "-01-01 00:00:00"),
                       vals = time, calendar = "standard", 
                       unlim = TRUE)  # a direct analog of internal FATES output dim "time"
        lat <- ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "coordinate_latitude")
        ##!! default is degrees east except we always input as degrees west, need to make sure this is sorted for all
        ## pecan functions!!
        lon <- ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "coordinate_longitude")
        
        z <- 1
        var[[z]] <- ncvar_def(name = "pft_levscpf", units="", nc$dim[["levscpf"]],
                                     missval=miss.val,longname=nc$var[["pft_levscpf"]]$longname)
        z <- z+1
        var[[z]] <- ncvar_def(name = "scls_levscpf", units="", nc$dim[["levscpf"]],
                                     missval=miss.val,longname=nc$var[["scls_levscpf"]]$longname)
        ## !! convert output[[3]] to MSTMIP cal_dat_beg?
        ## !! use time or nc.time here?
        z <- z+1
        var[[z]] <- ncvar_def(name = "mcdate", units="", nc$dim[["time"]],missval=miss.val,
                                     longname=nc$var[["mcdate"]]$longname)
        ## !! use FATES time or PEcAn calculated time, which uses FATES vars?  Shouldn't
        ## make any difference
        z <- z+1
        var[[z]] <- ncvar_def(name = "mcsec", units="", nc$dim[["time"]],missval=miss.val,
                              longname=nc$var[["mcsec"]]$longname)
        z <- z+1
        var[[z]] <- ncvar_def(name = "mdcur", units="", nc$dim[["time"]],missval=miss.val,
                              longname=nc$var[["mdcur"]]$longname)
        z <- z+1
        var[[z]] <- ncvar_def(name = "mscur", units="", nc$dim[["time"]],missval=miss.val,
                              longname=nc$var[["mscur"]]$longname)
        z <- z+1
        var[[z]] <- ncvar_def(name = "nstep", units="", nc$dim[["time"]],missval=miss.val,
                              longname=nc$var[["nstep"]]$longname)
        #z <- z+1
        #var[[z]] <- ncvar_def(name = "time_bounds", units="", missval=miss.val,
        #                      dim=list(nc$dim[["hist_interval"]],nc$dim[["time"]]))
        
        ### !!! Remove?
        #z <- z+1
        #var[[9]] <- ncvar_def(name = "date_written", units="", missval=miss.val,
        #                      dim=list(nc$dim[["string_length"]],nc$dim[["time"]]))
        #z <- z+1
        #var[[10]] <- ncvar_def(name = "time_written", units="", missval=miss.val,
        #                      dim=list(nc$dim[["string_length"]],nc$dim[["time"]]))
        
        #z <- z+1
        #var[[z]] <- ncvar_def(name = "lon", units=nc$var[["lon"]]$units, nc$dim[["lndgrid"]], 
        #                       missval=miss.val,longname=nc$var[["lon"]]$longname)
        #z <- z+1
        #var[[z]] <- ncvar_def(name = "lat", units=nc$var[["lat"]]$units, nc$dim[["lndgrid"]], 
        #                       missval=miss.val,longname=nc$var[["lat"]]$longname)
        ### !!!
         
        # ## !! turn below into a MSTMiP var (area) in km^2
        z <- z+1
        var[[z]] <- ncvar_def(name = "area", units=nc$var[["area"]]$units, nc$dim[["lndgrid"]], 
                               missval=miss.val,longname=nc$var[["area"]]$longname)
        z <- z+1
        var[[z]] <- ncvar_def(name = "landfrac", units=nc$var[["landfrac"]]$units, nc$dim[["lndgrid"]], 
                               missval=miss.val,longname=nc$var[["landfrac"]]$longname)
        # z <- z+1
        # var[[z]] <- ncvar_def(name = "landmask", units=nc$var[["landmask"]]$units, nc$dim[["lndgrid"]], 
        #                        missval=miss.val,longname=nc$var[["landmask"]]$longname)
        # ## !! Fluxes. first default FATES but after unit conversion
        # #var[[16]] <- ncvar_def(name = "AR", units="kg C m-2 s-1", missval=miss.val
        # #                      dim=list(nc$dim[["lndgrid"]],nc$dim[["time"]]))
        # ## Using PEcAn and mstmipvar. Units have already been converted
        z <- z+1
        var[[z]] <- mstmipvar("AutoResp", lat, lon, t, NA)
        z <- z+1
        var[[z]] <- mstmipvar("GPP", lat, lon, t, NA)
        # 
        ### !! TO BE REMOVED
        #var  <- ncdf4::ncvar_def(name = "time", units = "days", dim = nc$dim[["time"]])
        # These lines throw an error saying time already exists, but not showing up in VAR file
        #     nc  <- ncvar_add(nc=nc, v=var)
        #     ncvar_put(nc,"time",time)
        #     ncvar_get(nc,"time")
        
        ## extract variable and long names to VAR file for PEcAn vis
        #write.table(sapply(nc$var, function(x) { x$longname }), 
        #            file = paste0(oname, ".var"), 
        #            col.names = FALSE, 
        #            row.names = TRUE, 
        #            quote = FALSE)
        #
        #ncdf4::nc_close(nc)
        #
        ## !!
        
        ### Output netCDF data
        nc <- ncdf4::nc_create(file.path(outdir, paste(year, "nc", sep = ".")), var)
        varfile <- file(file.path(outdir, paste(year, "nc", "var", sep = ".")), "w")
        for (i in seq_along(var)) {
          print(i)  # just on for debugging
          ncdf4::ncvar_put(nc, var[[i]], output[[i]])
          cat(paste(var[[i]]$name, var[[i]]$longname), file = varfile, sep = "\n")
        }  ## netCDF loop
        close(varfile)
        ncdf4::nc_close(nc)
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