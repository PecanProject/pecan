#' ERA5_extract
#'
#' @param lat latitude
#' @param long longitude
#' @param years years to be extracted
#' @param vars variables to be extarcted. If NULL all the variables will be returned.
#' @details For the list of variables check out the documentation at \link{https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation#ERA5datadocumentation-Spatialgrid}
#'
#' @return a list of xts objects with all the variables for the requested years
#' @import xts
#' @export
#' @examples
#' \dontrun{
#' point.data <- ERA5_extract(lat=40, long=-120, years=c(1990:1995), vars=NULL)
#' 
#  point.data %>% 
#'  purrr::map(~xts::apply.daily(.x, mean))
#'
#' }
ERA5_extract <-
  function(lat ,
           long ,
           years ,
           dbparms,
           vars = NULL) {
    # Distributing the job between whatever core is available. 
     

    ensemblesN <- seq(1, 10)
    

    tryCatch({
       #for each ensemble
      one.year.out <- years %>%
        purrr::map(function(year) {
          
        
          tryCatch({
            bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                                        host     = dbparms$host, 
                                        user     = dbparms$user, 
                                        password = dbparms$password)
            
            con <- bety$con
          },
          error = function(e) {
            PEcAn.logger::logger.severe(paste0("",e))
          }
          )
         

          #--- lets find the big raw tile.
          raw.tiles <- PEcAn.DB::dbfile.input.check(
            siteid = "1000026755",
            startdate = paste0(year,"-01-01")  %>% as.Date(),
            enddate = paste0(year,"-12-31")  %>% as.Date(),
            parentid = NA,
            mimetype = "application/x-netcdf",
            formatname = "CF Meteorology",
            con,
            hostname = PEcAn.remote::fqdn(),
            exact.dates = TRUE,
            pattern = "ERA5",
            return.all=TRUE
          ) 

          # for each year
          point.data <-  ensemblesN %>%
            purrr::map(function(ens) {
              
            
              ncfile <- raw.tiles$file_path
              
              PEcAn.logger::logger.info(paste0("Trying to open :", ncfile, " "))
              
              if (!file.exists(ncfile))
                PEcAn.logger::logger.severe("The nc file was not found.")
              
              #msg
              PEcAn.logger::logger.info(paste0(year, " is being processed ", "for ensemble #", ens, " "))
              #open the file
              nc_data <- ncdf4::nc_open(ncfile)
              # time stamp
              
              t <- ncdf4::ncvar_get(nc_data, "time")
              tunits <- ncdf4::ncatt_get(nc_data, 'time')
              tustr <- strsplit(tunits$units, " ")
              timestamp <-
                as.POSIXct(t * 3600, tz = "UTC", origin = tustr[[1]][3])
              try(ncdf4::nc_close(nc_data))
              
              
              # set the vars
              if (is.null(vars))
                vars <- names(nc_data$var)
              # for the variables extract the data
              
              all.data.point <- vars %>%
                purrr::map_dfc(function(vname) {
                  PEcAn.logger::logger.info(paste0(" \t ",vname, "is being extracted ! "))
                  
                  brick.tmp <-
                    raster::brick(ncfile, varname = vname, level = ens)
                  nn <-
                    raster::extract(brick.tmp,
                                    sp::SpatialPoints(cbind(long, lat)),
                                    method = 'simple')
                  
                  if (!is.numeric(nn)) {
                    PEcAn.logger::logger.severe(paste0(
                      "Expected raster object to be numeric, but it has type `",
                      paste0(typeof(nn), collapse = " "),
                      "`"
                    ))
                  }
                  
                  
                  # replacing the missing/filled values with NA
                  nn[nn == nc_data$var[[vname]]$missval] <- NA
                  # send out the extracted var as a new col
                  t(nn)
                  
                }) %>%
                `colnames<-`(vars)
              #close the connection
              
              # send out as xts object
              xts::xts(all.data.point, order.by = timestamp)
            }) %>%
            setNames(paste0("ERA_ensemble_", ensemblesN))
          
          #Merge mean and the speard
          return(point.data)
          
        }) %>%
        setNames(years)
      
   
      # The order of one.year.out is year and then Ens - Mainly because of the spead  / I wanted to touch each file just once.
      # This now changes the order to ens - year
      OUT <- ensemblesN %>%
        purrr::map(function(Ensn) {
          one.year.out %>% 
            purrr::map( ~ .x [[Ensn]]) %>%
            do.call("rbind.xts", .)
        })
      
      return(OUT)
      
     }, error = function(e) {
      PEcAn.logger::logger.severe(paste0(conditionMessage(e)))
     })
    
  }
