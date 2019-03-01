#' ERA5_extract
#'
#' @param lat latitude
#' @param long longitude
#' @param years years to be extracted
#' @param vars variables to be extarcted. If NULL all the variables will be returned.
#' @details For the list of variables check out the documentation at \link{https://confluence.ecmwf.int/display/CKB/ERA5+data+documentation#ERA5datadocumentation-Spatialgrid}
#'
#' @return a list of xts objects with all the variables for the requested years
#' @export
#' @examples
#' \dontrun{
#' point.data <- ERA5_extract(lat=40, long=-120, years=c(1985,1995), vars="d2m")
#  point.data %>% 
#'  map(~xts::apply.daily(.x, mean))
#'
#' }
ERA5_extract <-
  function(lat = 40,
           long = -120,
           years = c(1985, 1995),
           vars = NULL) {
    tryCatch({
      point.data <- years %>%
        map(function(year) {
          one.year.out <-
            c("Mean", "Spread") %>%    # making the name of the files
            map(function(folder) {
              ncfile <-
                paste0("/projectnb/dietzelab/hamzed/ERA5/Data/",
                       folder, "/ERA5_", year, ".nc")
              #msg
              print(paste0(folder, "s in ", year, " is being processed !"))
              #open the file
              nc_data <- nc_open(ncfile)
              # time stamp
              t <- ncvar_get(nc_data, "time")
              tunits <- ncatt_get(nc_data, 'time')
              tustr <- strsplit(tunits$units, " ")
              timestamp = as.POSIXct(t * 3600, tz = 'GMT', origin = tustr[[1]][3])
              # set the vars
              if (is.null(vars))  vars <- names(nc_data$var)
              # for the variables extract the data
              all.data.point <- vars %>%
                map_dfc(function(vname) {
                  brick.tmp <- brick(ncfile, varname = vname)
                  nn <- raster::extract(brick.tmp, SpatialPoints(cbind(long, lat)), method = 'simple')%>%
                    as.numeric()
                  # replacing the missing/filled values with NA
                  nn[nn==nc_data$var[[vname]]$missval] <- NA 
                  #unit conversion for temperature
                  if (nc_data$var[[vname]]$units =="K" & folder=="Mean")  nn <-udunits2::ud.convert(nn, "degree_kelvin","celsius")
                  nn
                }) %>%
                `colnames<-`(paste0(vars, "_", folder))
              #close the connection
              try(nc_close(nc_data))
              # send out as xts object
              xts::xts(all.data.point, order.by = timestamp)
            })
          #Merge mean and the speard
          xts::merge.xts(one.year.out[[1]], one.year.out[[2]])
        }) %>%
        setNames(years)
    }, error = function(e) {
      print(paste0(conditionMessage(e), " just happened!"))
      
    })
    
  }