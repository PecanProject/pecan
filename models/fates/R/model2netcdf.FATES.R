
##' @name model2netcdf.FATES
##' @title Code to convert FATES netcdf output into into CF standard
##'
##' @param outdir Location of FATES model output (e.g. a path to a single ensemble output)
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation, not string
##' @param end_date End time of the simulation, not string
##' @param vars_names Names of Selected variables in PEcAn format, (e.g. c("",""))
##' @param pfts a named vector of PFT numbers where the names are PFT names
##' 
##' @examples  
##' \dontrun{
##' example.output <- system.file("case.clm2.h0.2004-01-01-00000.nc",package="PEcAn.FATES")
##' model2netcdf.FATES(outdir="~/",sitelat, sitelon, start_date, end_date, vars_names, pfts)
##' }
##'
##' @author Michael Dietze, Shawn Serbin
##  modified Yucong Hu 22/07/24
##' 
##' @export

model2netcdf.FATES <- function(outdir, sitelat, sitelon, start_date, end_date, vars_names, pfts){
  ## Tips: matched_var could be expanded for more selected variables
  matched_var <- tibble::tribble(
    ~fatesname, ~pecanname, ~pecanunits, ~longname,
    "FATES_GPP_PF","GPP","kgC m-2 s-1","Gross Primary Productivity", 
    "FATES_NPP_PF","NPP","kg m-2 yr-1", "Total PFT-level NPP in kg carbon per m2 land area per second",
    "NEE","NEE","kgC m-2 s-1", "Net Ecosystem Exchange of carbon, includes fire and hrv_xsmrpool",
    "TLAI","LAI","m2 m-2","Total projected leaf area index",
    "ER","TotalResp","kgC m-2 s-1","Total Respiration",
    "AR","AutoResp","kgC m-2 s-1","Autotrophic respiration (MR + GR)",
    "HR","HeteroResp","kgC m-2 s-1","Total heterotrophic respiration",
    "SR","SoilResp","kgC m-2 s-1","Total soil respiration (HR + root resp)",
    "Qle","Evap","kgC m-2 s-1","Total evaporation",
    "QVEGT","Transp","kg m-2 s-1","Canopy transpiration")
  
  ## Update unit, dimension and 
  var_update <- function(out,oldname,newname,nc_month,nc_month_names,newunits=NULL,long_name=NULL){
    if (oldname %in% nc_month_names) {

      ## define units of variables
      oldunits <- ncdf4::ncatt_get(nc_month,oldname,"units")$value
      if (oldunits=="gC/m^2/s") oldunits <- "gC m-2 s-1"
      if (oldname=="TLAI") oldunits <- "m2 m-2" # delete old unit ='none'
      if (is.null(newunits)) newunits = oldunits
      
      ## check pft dimensions
      d_name <- c()
      for (i in (nc_month$var[[oldname]]$dim)){
          d_name <- append(d_name, i$name)
      }
      if (any(grepl('pft',d_name))){
        dimension <- xypt # include fates_levpft
      }else{ 
        dimension <- xyt # only xyt
      } 

      ## transpose dimensions into (,t)
      if (d_name[length(d_name)]=='time'){
            dat_0 <- ncdf4::ncvar_get(nc_month,oldname) # time at the tail of dims
            dat.new <- PEcAn.utils::misc.convert(dat_0,oldunits,newunits) # convert data units
      }
      newvar <- ncdf4::ncvar_def(name = newname, units = newunits, longname=long_name, dim = dimension)
     
      ## adding target variables into out 
      if(is.null(out)) {
        out <- list(var <- list(),dat <- list(), dimm<-list())
        out$var[[1]] <- newvar
        out$dat[[1]] <- dat.new
        out$dimm[[1]]<- length(dimension)
      } else {
        i <- length(out$var) + 1
        out$var[[i]] <- newvar
        out$dat[[i]] <- dat.new
        out$dimm[[i]]<- length(dimension)
      }
    return(out)
    }
  }
    
  ## Get files and years
  files      <- dir(outdir, "*clm2.h0.*.nc", full.names = TRUE)  # currently specific to clm2.h0 files
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  start_month <- lubridate::month(start_date)
  end_month <- lubridate::month(end_date)

  ## Loop over years
  for (year in start_year:end_year){
    oname <- file.path(dirname(files[1]), paste0(year, ".nc"))
    out <- NULL
    
    ## monthly write files
    for (mo in 1:12){
      if (((year == start_year) & mo < start_month) | ((year == end_year) & mo > end_month)){
        next ## skip unselected months
      }
      else{
        if (mo < 10){
          month_file <- paste0(gsub("h0.*.nc","",files[1]),"h0.",year,"-0",mo,".nc")
        }else{
          month_file <- paste0(gsub("h0.*.nc","",files[1]),"h0.",year,"-",mo,".nc")
        }
        nc_month <- ncdf4::nc_open(month_file) # read monthly output file of FATES model
        nc_month_names <- names(nc_month$var)

        ## create time bounds to populate time_bounds variable iteratively
        var_bound <- ncdf4::ncvar_get(nc_month, "time_bounds") # start,end day of month
        
        ## define dimensions 
        t <- ncdf4::ncdim_def(name = "time", units = "days since 1700-01-01 00:00:00",
                    vals = as.double(1.0:1.0), calendar = "noleap", unlim = TRUE)
        time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                    longname = "history time interval endpoint dimensions",vals = 1:2, units = "")
        lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.double(1.0:1.0), longname = "coordinate_latitude")
        lon  <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.double(1.0:1.0), longname = "coordinate_longitude")
        pft  <- ncdf4::ncdim_def('pft', '', vals=1:12, longname = "FATES pft number")
        xyt  <- list(lon, lat, t)
        xypt <- list(lon, lat, pft, t)
        
        ## write monthly files with start(1,1,i)
        for (var_s in vars_names){
          matched_ind <- which(matched_var$pecanname == var_s)
          out <- var_update(out, matched_var$fatesname[matched_ind],matched_var$pecanname[matched_ind],
                            nc_month,nc_month_names,matched_var$pecanunits[matched_ind],matched_var$longname[matched_ind])          
        }
        out$var[[length(out$var) + 1]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                          longname = "history time interval endpoints", dim=list(time_interval,t), prec = "double")
        out$dat[[length(out$dat) + 1]] <- c(rbind(var_bound[1], var_bound[2])) #start, end days of the year
        out$dimm[[length(out$dimm) + 1]] <- 2

        ## define vars
        if (((year != start_year) & (mo == 1)) | ((year == start_year) & (mo == start_month))){
          ncout <- ncdf4::nc_create(oname, out$var) # create yearly nc file
          time_var <- ncdf4::ncvar_def(name = "time", units = "days since 1700-01-01 00:00:00",longname = "time", dim = list(t), prec = "double")
          lat_var  <- ncdf4::ncvar_def(name = "lat", units = "degrees_north", longname = "coordinate_latitude", dim = list(lat), prec = "double")
          lon_var  <- ncdf4::ncvar_def(name = "lon", units = "degrees_east", longname = "coordinate_longitude", dim = list(lon), prec = "double")
          
          ncdf4::ncvar_put(ncout, lat_var, sitelat, start = c(1))
          ncdf4::ncvar_put(ncout, lon_var, sitelon, start = c(1))
        }

        ## put time and vars 
        ncdf4::ncvar_put(ncout, time_var, mean(var_bound), start=c(mo), count=c(1))

        for (i in seq_along(out$var)) {
          if(out$dimm[[i]]==4){ # xypt
            ncdf4::ncvar_put(ncout, out$var[[i]], out$dat[[i]], start=c(1,1,1,mo), count=c(1,1,12,1))
          }else if (out$dimm[[i]]==3) { # xyt
            ncdf4::ncvar_put(ncout, out$var[[i]], out$dat[[i]], start=c(1,1,mo))
          }else{ # time_bounds
            ncdf4::ncvar_put(ncout, out$var[[i]], out$dat[[i]], start=c(1,mo))
          }
        } 
      }
    } ## monthly convert variable into PEcAn format 
  }
  ## extract variable and long names to VAR file for PEcAn vis
  utils::write.table(sapply(ncout$var, function(x) { x$longname }), 
              file = paste0(oname, ".var"), 
              col.names = FALSE, 
              row.names = TRUE, 
              quote = FALSE)
  try(ncdf4::nc_close(ncout)) ## end of year for loop
} ## model2netcdf.FATES