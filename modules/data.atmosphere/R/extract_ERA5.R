#' ERA5_extract
#'
#' @param lat latitude
#' @param long longitude
#' @param years years to be extracted
#' @param vars variables to be extarcted. If NULL all the variables will be returned.
#' @param data.folder Path to the directory where ERA5 nc files are located.
#' @details For the list of variables check out the documentation at \link{https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation#ERA5datadocumentation-Spatialgrid}
#'
#' @return a list of xts objects with all the variables for the requested years
#' @export
#' @examples
#' \dontrun{
#' point.data <- ERA5_extract(lat=40, long=-120, years=c(1990:1995), vars=NULL, data.folder=".")
#' 
#  point.data %>% 
#'  purrr::map(~xts::apply.daily(.x, mean))
#'
#' }
ERA5_extract <-
  function(lat ,
           long ,
           years ,
           vars = NULL, 
           data.folder) {
    
    ensemblesN <- seq(1, 10)
    
    tryCatch({
      # for each ensemble
      one.year.out <- ensemblesN %>%
        purrr::map(function(ens) {
          # for each year
          point.data <- years %>%
            purrr::map(function(year) {
              ncfile <-
                paste0(data.folder,
                       "/ERA5_",
                       year,
                       ".nc")
           
              PEcAn.logger::logger.info(paste0("Trying to open :", ncfile," "))
              
              if(!file.exists(ncfile)) PEcAn.logger::logger.severe("The nc file was not found.")
              
              #msg
              PEcAn.logger::logger.info(paste0(year, " is being processed ","for ensemble #", ens," "))
              #open the file
              nc_data <- ncdf4::nc_open(ncfile)
              # time stamp

              t <- ncdf4::ncvar_get(nc_data, "time")
              tunits <- ncdf4::ncatt_get(nc_data, 'time')
              tustr <- strsplit(tunits$units, " ")
              timestamp <- as.POSIXct(t * 3600, tz = "UTC", origin = tustr[[1]][3])
              try(ncdf4::nc_close(nc_data))

              
              
              # set the vars
              if (is.null(vars))
                vars <- names(nc_data$var)
              # for the variables extract the data
             
              all.data.point <- vars %>%
                purrr::map_dfc(function(vname) {
                 
                  
                  brick.tmp <-
                    raster::brick(ncfile, varname = vname, level = ens)
                  nn <-
                    raster::extract(brick.tmp,
                                    sp::SpatialPoints(cbind(long, lat)),
                                    method = 'simple') 
                  
                  if (!is.numeric(nn)) {
                    PEcAn.logger::logger.severe(paste0(
                      "Expected raster object to be numeric, but it has type `",
                      paste0(typeof(nn), collapse = " "), "`"
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
            })
          
          #binding the years

          point.data <- do.call("rbind", point.data)

          #Merge mean and the speard
          return(point.data)
          
        }) %>%
        setNames(paste0("ERA_ensemble_",ensemblesN))
      
      
    }, error = function(e) {
      PEcAn.logger::logger.severe(paste0(conditionMessage(e)))
    })
    
  }
