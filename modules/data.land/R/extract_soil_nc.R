#' Extract soil data from gssurgo
#'
#' @param outdir Output directory for writing down the netcdf file
#' @param lat Latitude 
#' @param lon Longitude
#' @param size Ensemble size
#' @param radius radius in meters is used to take soil type samples around the site
#' @param depths  Standard set of soil depths in m to create the ensemble of soil profiles with.
#'
#' @return It returns the address for the generated soil netcdf file
#' @export
#'
#' @examples
#' \dontrun{
#'    outdir  <- "~/paleon/envTest"
#'    lat     <- 40
#'    lon     <- -80
#'    PEcAn.data.land::extract_soil_gssurgo(outdir, lat, lon)
#' }
#' @author Hamze Dokoohaki
#' 
extract_soil_gssurgo<-function(outdir, lat, lon, size=1, radius=500, depths=c(0.15,0.30,0.60)){
  # I keep all the ensembles here 
  all.soil.ens <-list()
  
  # I ask the gSSURGO to find all the mukeys (loosely can be thought of soil type) within 500m of my site location. 
  # Basically I think of this as me going around and taking soil samples within 500m of my site.
  #https://sdmdataaccess.nrcs.usda.gov/SpatialFilterHelp.htm
  mu.Path <- paste0(
    "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMWGS84Geographic.wfs?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=MapunitPoly&FILTER=<Filter><DWithin><PropertyName>Geometry</PropertyName><gml:Point>%20<gml:coordinates>",
    lon ,
    ",",
    lat,
    "</gml:coordinates></gml:Point><Distance%20units=%27m%27>",radius,"</Distance></DWithin></Filter>"
  )
  # We are trying to find the mapunit key here. Either using rgdal if the driver is defined or parsing it's gml
  
  if ("GML" %in% rgdal::ogrDrivers()$name) {
    
    suppressMessages({

    #disambiguateFIDs if TRUE, and FID values are not unique, they will be set to unique values 1:N for N features; problem observed in GML files
    #idk why but gssurgo api seems to fail for no reason, that'w why I try 3 times.
      for(i in 1:3){
       # try to read the polygon to get the mukey
       sarea <-try(rgdal::readOGR(mu.Path, disambiguateFIDs=T), silent = TRUE)
       if( class(sarea) != "try-error" ) break;
       PEcAn.logger::logger.warn(paste0(i, " attemp was unsuccessful"))
       Sys.sleep(1)
     }

      # flipping the coordinates 
      # gdal reads the gSSUGO layers with fliped coordinateds
      for (i in seq_along(sarea@polygons)){
        for (j in seq_along(sarea@polygons[[i]]@Polygons)){
          # flip the coordinates
          sarea@polygons[[i]]@Polygons[[j]]@coords <- sarea@polygons[[i]]@Polygons[[j]]@coords[, c(2,1)]
        }
      }
      
      areasf <-sf::st_as_sf(sarea)
      # getting the site point ready
      site = sf::st_as_sf(data.frame(long=lon, lat=lat), coords=c("long","lat"))
      
      #buffer the radius around site / and clip the study area based on buffer
      site_buffer = sf::st_buffer(site, (radius/111000)) # converting radius m to dgree - each degree is about 111 Km
      site_area = sf::st_intersection(site_buffer, areasf)
      # calculating areas again for the clipped regions
     mukey_area <- data.frame(Area=raster::area(x= as(site_area, 'Spatial')),
                              mukey=site_area$mukey)

    })
    
    mukeys <- mukey_area$mukey %>% as.character()
  }else{
    #reading the mapunit based on latitude and longitude of the site
    #the output is a gml file which need to be downloaded and read as a spatial file but I don't do that.
    #I just read the file as a text and parse it out and try to find the mukey==mapunitkey
    xmll <-
      RCurl::getURL(mu.Path,
                    ssl.verifyhost = FALSE,
                    ssl.verifypeer = FALSE
      )
    
    startp <- regexpr('<ms:mukey>', xmll)
    stopp <- regexpr('</ms:mukey>', xmll)
    
    #if you found the mapunit key
    if (startp == -1 |
        stopp == -1)
      PEcAn.logger::logger.error("There was no mapunit keys found for this site.")
    mukeys <-substr(xmll, startp %>% as.numeric + 10, stopp %>% as.numeric - 1)  
  }
  
  # calling the query function sending the mapunit keys
  soilprop <- gSSURGO.Query(mukeys,c("chorizon.sandtotal_r",
                                    "chorizon.silttotal_r",
                                    "chorizon.claytotal_r",
                                    "chorizon.hzdept_r"))

  soilprop.new <- soilprop %>%
    dplyr::arrange(hzdept_r) %>%
    dplyr::select(-comppct_r) %>%
    `colnames<-`(
      c(
        "fraction_of_sand_in_soil",
        "fraction_of_silt_in_soil",
        "fraction_of_clay_in_soil",
        "soil_depth",
        "mukey"
      )
    )
  #unit conversion
  soilprop.new [, c("fraction_of_sand_in_soil", "fraction_of_silt_in_soil" , "fraction_of_clay_in_soil" ,
                    "soil_depth")] <- soilprop.new [, c("fraction_of_sand_in_soil", "fraction_of_silt_in_soil" ,
                                                        "fraction_of_clay_in_soil" , "soil_depth")]/100
  soilprop.new <- soilprop.new[ complete.cases(soilprop.new) , ]
  #converting it to list
  soil.data.gssurgo <- names(soilprop.new)[1:4] %>%
    purrr::map(function(var) {
      soilprop.new[, var]
    }) %>%
    setNames(names(soilprop.new)[1:4])
  #This ensures that I have at least one soil ensemble in case the modeling part failed
  all.soil.ens <-c(all.soil.ens, list(soil.data.gssurgo))
  
  
  # What I do here is that I put soil data into depth classes and then model each class speparatly
  #- see if we need to generate soil ensemble and add that to the list of all
  tryCatch({
    # find the soil depth levels based on the depth argument 
    # if soil profile is deeper than what is specified in the argument then I go as deep as the soil profile.
    if (max(soilprop.new$soil_depth) > max(depths)) depths <- sort (c(depths, max(max(soilprop.new$soil_depth))))
    
    depth.levs<-findInterval(soilprop.new$soil_depth, depths)
    depth.levs[depth.levs==0] <-1
    depth.levs[depth.levs>length(depths)] <-length(depths)
    
     soilprop.new.grouped<-soilprop.new %>% 
      mutate(DepthL=depths[depth.levs])
    
    # let's fit dirichlet for each depth level separately
    simulated.soil.props<-soilprop.new.grouped %>%
      split(list(soilprop.new.grouped$DepthL, soilprop.new.grouped$mukey)) %>%
      purrr::map_df(function(DepthL.Data){
        tryCatch({
          # I model the soil properties for this depth
          dir.model <-DepthL.Data[,c(1:3)]%>%
            as.matrix() %>%
            sirt::dirichlet.mle(.)
          # Monte Carlo sampling based on my dirichlet model
          alpha <- dir.model$alpha
          alpha <- matrix(alpha, nrow= size, ncol=length(alpha), byrow=TRUE )
          simulated.soil <- sirt::dirichlet.simul(alpha)
          # # using the simulated sand/silt/clay to generate soil ensemble
          simulated.soil<-simulated.soil %>%
            as.data.frame %>%
            mutate(DepthL=rep(DepthL.Data[1,6], size),
                   mukey=rep(DepthL.Data[1,5], size)) %>%
            `colnames<-`(c("fraction_of_sand_in_soil",
                           "fraction_of_silt_in_soil",
                           "fraction_of_clay_in_soil",
                           "soil_depth",
                           "mukey"))
          simulated.soil
        },
        error = function(e) {
          PEcAn.logger::logger.warn(conditionMessage(e))
          return(NULL)
        })
        
      }) 
    
    # estimating the proportion of areas for those mukeys which are modeled
    mukey_area <- mukey_area %>%
      filter(mukeys %in% simulated.soil.props$mukey) %>%
      mutate(Area=Area/sum(Area))
    
    #--- Mixing the depths
    soil.profiles<-simulated.soil.props %>% 
      split(.$mukey)%>% 
      purrr::map(function(soiltype.sim){
        sizein <- (mukey_area$Area[ mukey_area$mukey == soiltype.sim$mukey %>% unique()])*size
        
        1:ceiling(sizein) %>%
          purrr::map(function(x){
            soiltype.sim %>% 
              split(.$soil_depth)%>%
              purrr::map_dfr(~.x[x,])
          })
      }) %>%
      purrr::flatten()

    #- add them to the list of all the ensembles ready to be converted to .nc file
    all.soil.ens<-soil.profiles %>%
      purrr::map(function(SEns){
        names(SEns) %>%
          purrr::map(function(var){
            SEns[,var]
          })%>% 
          setNames(names(SEns))
      })%>%
      c(all.soil.ens,.)
    
  },
  error = function(e) {
    PEcAn.logger::logger.warn(conditionMessage(e))
  })
  
  
  #-- generating the .nc files for all the collected ensembles
  out.ense <- (1:length(all.soil.ens)) %>%
    purrr::map(function(i) {
      
      tryCatch({
        #browser()
        # calc new filename
        prefix <- paste0("gSSURGO_soil_", i)
        new.file <- file.path(outdir, paste0(prefix, ".nc"))
        #sending it to the func where some new params will be added and then it will be written down as nc file.
        suppressWarnings({
          soil2netcdf(all.soil.ens[[i]][1:4], new.file)
        })
        
        new.file
      },
      error = function(e) {
        PEcAn.logger::logger.warn(conditionMessage(e))
        return(NULL)
      })
      
    })
  # removing the nulls or the ones that throw exception in the above trycatch
  out.ense<- out.ense %>%
    purrr::discard(is.null)
  
  out.ense<-out.ense%>% 
    setNames(rep("path", length(out.ense)))
  
  return(out.ense)
}






#' Extract soil data
#'
#' @param in.file 
#' @param outdir 
#' @param lat 
#' @param lon 
#'
#' @return path to netCDF file containing extracted data
#' @export
#'
#' @examples
#' \dontrun{
#' in.file <- "~/paleon/env_paleon/soil/paleon_soil.nc"
#' outdir  <- "~/paleon/envTest"
#' lat     <- 40
#' lon     <- -80
#' PEcAn.data.land::extract_soil_nc(in.file,outdir,lat,lon)
#' }
extract_soil_nc <- function(in.file,outdir,lat,lon){
  
  ## open soils
  nc <- ncdf4::nc_open(in.file)
  
  ## extract lat/lon
  dims <- names(nc$dim)
  lat.dim <- dims[grep("^lat",dims)]
  lon.dim <- dims[grep("^lon",dims)]
  soil.lat <- ncdf4::ncvar_get(nc, lat.dim)
  soil.lon <- ncdf4::ncvar_get(nc, lon.dim)
  
  ## check in range
  dlat <- abs(median(diff(soil.lat)))
  dlon <- abs(median(diff(soil.lon)))
  if(lat < (min(soil.lat)-dlat) | lat > (max(soil.lat)+dlat)){
    PEcAn.logger::logger.error("site lat out of bounds",lat,range(soil.lat))
  }
  if(lon < (min(soil.lon)-dlon) | lon > (max(soil.lon)+dlon)){
    PEcAn.logger::logger.error("site lon out of bounds",lon,range(soil.lon))
  }
  if(dims[1] == lat.dim){
    soil.row <- which.min(abs(lat-soil.lat))
    soil.col <- which.min(abs(lon-soil.lon))
  } else if(dims[1] == lon.dim){
    soil.col <- which.min(abs(lat-soil.lat))
    soil.row <- which.min(abs(lon-soil.lon))
  } else {
    PEcAn.logger::logger.error("could not determine lat/lon dimension order:: ",dims)
  }
  
  ## extract raw soil data
  soil.data <- list()
  soil.vars <- names(nc$var)
  for(i in seq_along(soil.vars)){
    if(length(dims) == 2){
      soil.data[[soil.vars[i]]] <- ncdf4::ncvar_get(nc,soil.vars[i])[soil.row,soil.col]
    } else {
      ## assuming there's a 3rd dim of soil depth profile
      soil.data[[soil.vars[i]]] <- ncdf4::ncvar_get(nc,soil.vars[i])[soil.row,soil.col,]
    }
  }
  ncdf4::nc_close(nc)
  
  ## PalEON / MSTMIP / UNASM hack
  # t_ variables are topsoil layer (0– 30 cm) and
  # s_ variables are subsoil layer (30–100 cm)
  depth <- ncdf4::ncdim_def(name = "depth", units = "meters", vals = c(0.3,1.0), create_dimvar = TRUE)  
  dvars <- soil.vars[grep("t_",soil.vars,fixed=TRUE)]
  for(i in seq_along(dvars)){
    svar <- sub("t_","s_",dvars[i])
    soil.data[[dvars[i]]] <- c(soil.data[[dvars[i]]],soil.data[[svar]]) ## combine different depths
    soil.data[[svar]] <- NULL  ## drop old variable
    names(soil.data)[which(names(soil.data) == dvars[i])] <- sub("t_","",dvars[i]) ## rename original
  }
  
  
  ## name/unit conversions 
  soil.data$sand   <- soil.data$sand/100
  soil.data$silt   <- soil.data$silt/100
  soil.data$clay   <- soil.data$clay/100
  soil.data$oc     <- soil.data$oc/100
  soil.data$gravel <- soil.data$gravel/100
  soil.data$ref_bulk <- udunits2::ud.convert(soil.data$ref_bulk,"g cm-3","kg m-3")
  names(soil.data)[which(names(soil.data) == "clay")] <- "fraction_of_clay_in_soil"
  names(soil.data)[which(names(soil.data) == "sand")] <- "fraction_of_sand_in_soil"
  names(soil.data)[which(names(soil.data) == "silt")] <- "fraction_of_silt_in_soil"
  names(soil.data)[which(names(soil.data) == "gravel")] <- "fraction_of_gravel_in_soil"
  names(soil.data)[which(names(soil.data) == "ref_bulk")] <- "soil_bulk_density"
  names(soil.data)[which(names(soil.data) == "ph")]   <- "soil_ph"
  names(soil.data)[which(names(soil.data) == "cec")]  <- "soil_cec" ## units = meq/100g
  names(soil.data)[which(names(soil.data) == "oc")]   <- "soilC"  ## this is currently the BETY name, would like to change and make units SI
  
  ## calc new filename
  prefix <- tools::file_path_sans_ext(basename(in.file))
  new.file <- file.path(outdir,paste0(prefix,".nc"))
  
  ## Calculate soil parameters and export to netcdf
  soil2netcdf(soil.data,new.file)
  
  return(new.file)
  
}


#' Get standard units for a soil variable
#'
#' @param varname 
#'
#' @return character
#' @export
#'
#' @examples
#' soil.units("soil_albedo")
soil.units <- function(varname = NA){
  variables <- as.data.frame(matrix(c("soil_depth","m",
                                      "soil_cec","meq/100g",
                                      "fraction_of_clay_in_soil","1",
                                      "fraction_of_sand_in_soil","1",
                                      "fraction_of_silt_in_soil","1",
                                      "fraction_of_gravel_in_soil","1",
                                      "volume_fraction_of_water_in_soil_at_saturation","m3 m-3",
                                      "volume_fraction_of_water_in_soil_at_field_capacity","m3 m-3",
                                      "volume_fraction_of_condensed_water_in_dry_soil","m3 m-3",
                                      "volume_fraction_of_condensed_water_in_soil_at_wilting_point","m3 m-3",
                                      "soilC","percent",
                                      "soil_ph","1",
                                      "soil_bulk_density","kg m-3",
                                      "soil_type","string",
                                      "soil_hydraulic_b","1",
                                      "soil_water_potential_at_saturation","m",
                                      "soil_hydraulic_conductivity_at_saturation","m s-1",
                                      "thcond0","W m-1 K-1",
                                      "thcond1","W m-1 K-1",
                                      "thcond2","1",
                                      "thcond3","1",
                                      "soil_thermal_conductivity","W m-1 K-1", 
                                      "soil_thermal_conductivity_at_saturation","W m-1 K-1", 
                                      "soil_thermal_capacity","J kg-1 K-1",
                                      "soil_albedo","1"
  ),
  ncol=2,byrow = TRUE))
  colnames(variables) <- c('var','unit')
  
  unit = which(variables$var == varname)
  
  if(length(unit) == 0){
    if(is.na(varname)){
      return(variables)
    } else {
      return(NA)
    }
  }else{
    unit = as.character(variables$unit[unit])
    return(unit)
  }
  
}
