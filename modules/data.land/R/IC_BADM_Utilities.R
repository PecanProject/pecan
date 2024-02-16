
#' Read.IC.info.BADM
#'
#' @param lat numeric latitude
#' @param long numeric longitude
#'
#' @description This function returns a dataframe of plant biomass, root and soil carbon for a set of lat and long coordinates.
#' This function first finds the level1 and level2 ecoregions  for the given coordinates, and then tries to filter BADM database for those eco-regions. 
#' If no data found in the BADM database for the given lat/longs eco-regions, then all the data in the database will be used to return the initial condition.
#' All the variables are also converted to kg/m^2. 
#' @return a dataframe with 7 columns of Site, Variable, Date, Organ, AGB, soil_organic_carbon_content, litter_carbon_content.
#'   Variable in the return object refers to what this value was called inside BADM database.
#'
#' @export
#' @examples
#' \dontrun{
#'   badm_test <- Read.IC.info.BADM(45.805925,-90.07961)
#'}
Read.IC.info.BADM <-function(lat, long){
  cov.factor <-1
  #Reading in the DB
  #
  U.S.SB <- PEcAn.data.land::BADM

  
  Regions <- EPA_ecoregion_finder(lat, long)
  Code_Level <- Regions$L2
  
  # Let's find the biomass/.soil and litter
  #L2
  biomass.df <- U.S.SB %>%
    dplyr::filter(
      .data$NA_L2CODE == Code_Level,
      grepl("ROOT_|AG_BIOMASS|SOIL_STOCK|SOIL_CHEM", .data$VARIABLE)
    ) %>%
    dplyr::select("SITE_ID", "GROUP_ID", "VARIABLE_GROUP", "VARIABLE", "DATAVALUE")
  

  # if no data was found on L2, then let's check for L1
  if (nrow(biomass.df) < 3)  {
    Code_Level <- Regions$L1
    
    biomass.df <- U.S.SB %>%
      dplyr::filter(
        .data$NA_L1CODE == Code_Level,
        grepl("ROOT_|AG_BIOMASS|SOIL_STOCK|SOIL_CHEM", .data$VARIABLE)
      ) %>%
      dplyr::select("SITE_ID", "GROUP_ID", "VARIABLE_GROUP", "VARIABLE", "DATAVALUE")
  }
  

  # if no data was found on L1 too, then let's use the whole db
  if (nrow(biomass.df) < 3)  {
    Code_Level <- "ALL"
    biomass.df <- U.S.SB %>%
      dplyr::filter(grepl("ROOT_|AG_BIOMASS|SOIL_STOCK|SOIL_CHEM", .data$VARIABLE)) %>%
      dplyr::select("SITE_ID", "GROUP_ID", "VARIABLE_GROUP", "VARIABLE", "DATAVALUE")
  }

  
  # for each entry
  entries <- biomass.df %>%
    split(.$GROUP_ID) %>%
    purrr::map_dfr(
      function(Gdf) {
  
        # we keep them here
        PlantWoodIni <- NA
        SoilIni <- NA
        litterIni <- NA
        Rootini <- NA
        litterIni <- NA
        Date.in <- NA
        Organ.in <- NA
        # find what type of entry it is - biomass/soil or litter
        if (nrow(Gdf) > 0) {
          type <-
            sapply(c(
              "*LIT",
              "*SOIL",
              "*_BIOMASS",
              "*_ROOT_BIOMASS",
              "*_LIT_BIOMASS"
            ),
            grepl,
            Gdf[1, 3])
          type <- names(type)[type]
        } else{
          return(NULL)
        }
        
        if (length(type) > 1)
          type <- type[-which(type == "*_BIOMASS")]
        #----------------- Unit conversion
        unit.in <- Gdf %>%
          dplyr::filter(grepl("UNIT", .data$VARIABLE)) %>%
          dplyr::pull(.data$DATAVALUE) %>% 
          as.character()
        
        
        #Converting DM to C content
        #Variations and determinants of carbon content in plants:a global synthesis - https://www.biogeosciences.net/15/693/2018/bg-15-693-2018.pdf
        if (length(unit.in) > 0)
        if (unit.in =="kgDM m-2") cov.factor <- cov.factor *0.48
        
        unit.ready <- ifelse(unit.in == "gC m-2",
                             "g/m^2",
                             ifelse(unit.in == "kgDM m-2", "kg/m^2",
                                    "kg/m^2"))
        
        if (length(unit.in) == 0)
          unit.ready <- "kg/m^2"
        
        Date.in <- Gdf %>%
          dplyr::filter(grepl("DATE", .data$VARIABLE)) %>%
          dplyr::pull(.data$DATAVALUE) %>%
          as.Date()
        
        if (length(Date.in) == 0)
          Date.in <- NA
        #----------------collect
      
        # if it's biomass
        if (type == "*_BIOMASS") {
          Organ.in <- Gdf %>%
            dplyr::filter(grepl("ORGAN", .data$VARIABLE)) %>%
            dplyr::pull(.data$DATAVALUE) 
          
          
          PlantWoodIni <-
            PEcAn.utils::ud_convert(Gdf$DATAVALUE[1]%>%
                                   as.numeric()*cov.factor,  unit.ready, "kg/m^2")#"AG_BIOMASS_CROP","AG_BIOMASS_SHRUB","AG_BIOMASS_TREE","AG_BIOMASS_OTHER"
          
        } else if (type == "*SOIL") {
          val <- Gdf %>%
            dplyr::filter(grepl("SOIL_STOCK_C_ORG", .data$VARIABLE)) %>%
            dplyr::pull(.data$DATAVALUE) %>%
            as.numeric()
          
          if (length(val) > 0)
            SoilIni <- PEcAn.utils::ud_convert(val*cov.factor,  "g/m^2", "kg/m^2")
          
        } else if (type == "*_LIT_BIOMASS") {
          litterIni <-
            PEcAn.utils::ud_convert(Gdf$DATAVALUE[1] %>%
                                   as.numeric()*cov.factor,  unit.ready, "kg/m^2")
          
        } else if (type == "*_ROOT_BIOMASS") {
          Rootini <-
            PEcAn.utils::ud_convert(Gdf$DATAVALUE[1]%>%
                                   as.numeric()*cov.factor,  unit.ready, "kg/m^2")
          
        }
        return(
          data.frame(
            Site = Gdf$SITE_ID %>% unique(),
            Var = Gdf$VARIABLE[1],
            Date = Date.in,
            Organ = Organ.in,
            AGB = PlantWoodIni,
            soil_organic_carbon_content = SoilIni,
            litter_carbon_content = litterIni
          )
        )
    })
  

 #cleaning
ind <- apply(entries[,5:7], 1, function(x) all(is.na(x)))
entries <- entries[-which(ind),]



  return(entries)
}


#' netcdf.writer.BADAM
#'
#' @param lat numeric latitude
#' @param long numeric longitude
#' @param siteid site id as a string
#' @param outdir output dir which you want to store the IC netcdf file
#'
#' @return a dataframe with file, host, mimetype, formatname, startdate, enddate and dbfile.name columns
#' @export
#'
netcdf.writer.BADM <- function(lat, long, siteid, outdir, ens){
 
  
  #Reading in the BADM data
  entries <- Read.IC.info.BADM (lat, long)
  
   #--
  input <- list()
  dims <- list(lat = lat ,
               lon = long,
               time = 1)
  
  PWI <- entries$AGB[!is.na(entries$AGB)]
  LIn <- entries$litter_carbon_content[!is.na(entries$litter_carbon_content)]
  SIn <- entries$soil_organic_carbon_content[!is.na(entries$soil_organic_carbon_content)]
  
  
  variables <- list(SoilMoistFrac = 1,
                    SWE = 0)
  
  if (length(PWI) > 0)
    variables <-
    c(variables, wood_carbon_content = sample(PWI, 1, replace = T))
  if (length(LIn) > 0)
    variables <-
    c(variables, litter_carbon_content = sample(LIn, 1, replace = T))
  if (length(SIn) > 0)
    variables <-
    c(variables, soil_organic_carbon_content = sample(SIn, 1, replace = T))
  
  input$dims <- dims
  input$vals <- variables
  

  return(pool_ic_list2netcdf(
    input = input,
    outdir = outdir,
    siteid = siteid,
    ens
  )$file)
}



#' BADM_IC_process
#'
#' @param settings pecan xml settings
#' @param dir output dir which you want to store the IC netcdf file
#' @param overwrite Flag for overwriting the IC file.
#'
#' @return a list of paths to generated and stored IC files.
#' @export
#'
BADM_IC_process <- function(settings, dir, overwrite=TRUE){
  
  
  new.site <-
    data.frame(
      id = settings$run$site$id %>% as.numeric(),
      lat = settings$run$site$lat ,
      lon = settings$run$site$lon %>% as.numeric()
    )
  

  out.ense <- seq_len(settings$ensemble$size) %>%
    purrr::map(~ netcdf.writer.BADM(new.site$lat,
                             new.site$lon,
                             new.site$id,
                             outdir=dir,
                             ens=.x))
  
  out.ense <- out.ense %>%
    stats::setNames(rep("path", length(out.ense)))
  
  return(out.ense)
}

#' EPA_ecoregion_finder
#'
#' @param Lat numeric latitude
#' @param Lon numeric longitude
#' @description This function is designed to find the level1 and level2 code ecoregions for a given lat and long. 
#' You can learn more about ecoregions here: \url{https://www.epa.gov/eco-research/ecoregions}.

#'
#' @return a dataframe with codes corresponding to level1 and level2 codes as two columns
#' @export
#'
EPA_ecoregion_finder <- function(Lat, Lon){
  #lat long to spatial point
  U.S.SB.sp <-
    data.frame(Lati = Lat %>% as.numeric(),
               Long = Lon %>% as.numeric())
  
  sp::coordinates(U.S.SB.sp) <- ~ Long + Lati
  

  # L1 layer
  L1 <-
    sf::read_sf(system.file("extdata","eco-region.json", package = "PEcAn.data.land")) %>%
    sf::st_set_crs(
      "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
    ) %>%
    sf::st_transform("+proj=longlat +datum=WGS84")
  # L2 layer
  L2 <-
    sf::read_sf(system.file("extdata","eco-regionl2.json", package = "PEcAn.data.land")) %>%
    sf::st_set_crs(
      "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
    ) %>%
    sf::st_transform("+proj=longlat +datum=WGS84")
  
  sp::proj4string(U.S.SB.sp) <- sp::proj4string(sf::as_Spatial(L1))
  # finding the code for each site
  over.out.L1 <- sp::over(U.S.SB.sp, sf::as_Spatial(L1))
  over.out.L2 <- sp::over(U.S.SB.sp, sf::as_Spatial(L2))
  
  return(data.frame(L1 = over.out.L1$NA_L1CODE, L2 = over.out.L2$NA_L2CODE))
}


