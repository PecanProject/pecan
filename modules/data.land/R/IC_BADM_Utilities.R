
#Reading the data and writing the netcdf
#' Title
#'
#' @param lat numeric latitude
#' @param long numeric longitude
#'
#' @description This function returns a dataframe of plant biomass, root and soil carbon for a lat and long coordinates.
#' This function first finds the level1 and level2 ecoregions  for the given coordinates, and then tries to filter BADM database for those eco-region. 
#' If no data found in the BADM database for the given lat/longs eco-regions, then all the data in the database will be used to return the initial condition.
#' All the variables are also converted to kg/m^2. 
#' @return a datframe with 8 columns of Site, Variable, Date, Organ, PlantWIni (Initial plant biomass, type of biomass can be found in the Var and Organ), SoilIni (which shows the initial soil C), LitterIni, RootIni.

#' @export
#'
#' @examples
Read.IC.info.BADM <-function(lat, long){
  #Reading in the DB
  #
  U.S.SB <- readRDS(system.file("data","BADM", "U.S.SB_ECORegions_FULL.rds", package = "data.land"))
  #--
  input<-list()
  dims<- list(lat=42.5419 ,lon=-72.185, time= 1)
  
  Regions <- L1_L2_finder(lat, long)
  Code_Level <-Regions$L2

   # Let's find the biomass/.soil and litter
  #L2
  biomass.df <-U.S.SB %>%
    filter(NA_L2CODE == Code_Level,
           VARIABLE %>% grepl("ROOT_|AG_BIOMASS|SOIL_STOCK|SOIL_CHEM", .)) %>%
    dplyr::select(SITE_ID, GROUP_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE )

  cat(nrow(biomass.df),"-",Code_Level %>% as.character(),"\n")
  # if no data was found on L2, then let's check for L1
  if (nrow(biomass.df)<3)  {
    Code_Level <-Regions$L1
  
    biomass.df <-U.S.SB %>%
      filter(NA_L1CODE == Code_Level,
             VARIABLE %>% grepl("ROOT_|AG_BIOMASS|SOIL_STOCK|SOIL_CHEM", .)) %>%
      dplyr::select(SITE_ID, GROUP_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE )
  }
    
  cat(nrow(biomass.df),"-",Code_Level %>% as.character(),"\n")
    # if no data was found on L1 too, then let's use the whole db
  if (nrow(biomass.df)<3)  {
    Code_Level <-"ALL"
    biomass.df <-U.S.SB %>%
      filter( VARIABLE %>% grepl("ROOT_|AG_BIOMASS|SOIL_STOCK|SOIL_CHEM", .)) %>%
      dplyr::select(SITE_ID, GROUP_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE )
  }
    cat(nrow(biomass.df),"-",Code_Level %>% as.character(),"\n")

  # for each entry
 entries<- biomass.df %>%
    split(.$GROUP_ID) %>% 
    map_dfr(function(Gdf){
      # we keep them here
      PlantWoodIni<-NA 
      SoilIni<-NA
      litterIni<-NA
      Rootini<-NA
      litterIni<-NA
      Date.in<-NA
      Oregan.in<-NA
      # find what type of entry it is - biomass/soil or litter
      if (nrow(Gdf)>0){
       type<-sapply(c("*LIT","*SOIL","*_BIOMASS","*_ROOT_BIOMASS","*_LIT_BIOMASS"),
                    grepl, Gdf[1,3])
       type<-names(type)[type]
      }else{
       return(NULL)
      }
      
     if (length(type)>1) type <- type[-which(type=="*_BIOMASS")]
      #----------------- Unit conversion
      unit.in<-Gdf%>%
        filter(VARIABLE %>% grepl("UNIT", .)) %>%
        pull(DATAVALUE)
      
      unit.ready <- ifelse(unit.in=="gC m-2", "g/m^2",
                           ifelse(unit.in=="kgDM m-2","kg/m^2",
                                  "kg/m^2"))
      
      if (length(unit.in)==0) unit.ready <-"kg/m^2"
      
      Date.in<-Gdf%>%
        filter(VARIABLE %>% grepl("DATE", .)) %>%
        pull(DATAVALUE) %>% 
        as.Date()
    if (length(Date.in)==0) Date.in<-NA
    #----------------collect
      # if it's biomass
      if(type=="*_BIOMASS"){
        
        Oregan.in<-Gdf%>%
          filter(VARIABLE %>% grepl("ORGAN", .)) %>%
          pull(DATAVALUE) 

 
          PlantWoodIni<-udunits2::ud.convert(Gdf$DATAVALUE[1],  unit.ready, "kg/m^2")#       "AG_BIOMASS_CROP","AG_BIOMASS_SHRUB","AG_BIOMASS_TREE","AG_BIOMASS_OTHER"
          
      }else if (type=="*SOIL"){

        val <- Gdf%>%
          filter(VARIABLE %>% grepl("SOIL_STOCK_C_ORG", .)) %>%#"SOIL_STOCK_C_ORG"
          pull(DATAVALUE) %>% 
          as.numeric()
        
        if (length(val)>0)SoilIni<-udunits2::ud.convert(val,  "g/m^2", "kg/m^2")
        
      }else if (type=="*_LIT_BIOMASS"){
        
        litterIni<-udunits2::ud.convert(Gdf$DATAVALUE[1],  unit.ready, "kg/m^2")
        
      }else if (type=="*_ROOT_BIOMASS"){
        
        Rootini<-udunits2::ud.convert(Gdf$DATAVALUE[1],  unit.ready, "kg/m^2")
        
      }
    return(data.frame(Site=Gdf$SITE_ID %>% unique(),
                      Var= Gdf$VARIABLE[1],
                      Date=Date.in,
                      Organ=Oregan.in,
                      PlantWIni=PlantWoodIni,
                      SoilIni=SoilIni,
                      LitterIni=litterIni,
                      RootIni=Rootini))
    })
 
ind <- apply(entries[,5:8], 1, function(x) all(is.na(x)))
entries <- entries[-which(ind),]

# PWI <-entries$PWI[!is.na(entries$PWI)]
# LIn <-entries$LitIn[!is.na(entries$LitIn)]
# SIn <- entries$SIn[!is.na(entries$SIn)]


  # variables<- list(wood_carbon_content = sample(PWI,1,replace = T), 
  #                  litter_carbon_content = sample(LIn, 1,replace = T),
  #                  soil_organic_carbon_content= sample(SIn, 1,replace = T),
  #                  SoilMoistFrac = 1,
  #                  SWE = 0)
  # input$dims <- dims
  # input$vals <- variables
  # 
  # pool_ic_list2netcdf(input = input, outdir = outdir, siteid = siteid)$file

  return(entries)
}





#' IC_Maker
#'
#' @param settings PEcAn xml settings
#' @param ens.n number of ensemble memebers needs to be genrated
#' @param outdir output directory 
#' @description This function generates a series of ensemble members for initial condition using the BADM database.
#' @return xml setting
#' @export
#'
#' @examples
IC_Maker <-function(settings, ens.n=5, outdir) {

  ini.pool<-1:ens.n %>%
    purrr::map(~Read.IC.info.BADM(settings$run$site$lat,
                             settings$run$site$lon#,
                             # paste0(settings$run$site$id,.x) %>% as.numeric(),
                             # outdir
                             )
               )%>%
    setNames(rep('path',ens.n)) %>%
    list %>%
    setNames('poolinitcond')
  
  settings$run$inputs<-c(settings$run$inputs, ini.pool)
  return(settings)
}


#' L1_L2_finder
#'
#' @param Lat numeric latitude
#' @param Lon numeric longitude
#' @description This function is designed to find the level1 and level2 code ecoregions for a given lat and long. 
#' Yoi can learn more about ecoregions here: \link{https://www.epa.gov/eco-research/ecoregions}.

#'
#' @return a dataframe with codes corresponding to level1 and level2 codes as two columns
#' @export
#'
#' @examples
L1_L2_finder <- function(Lat, Lon){
  #lat long to spatial point  
  U.S.SB.sp <-data.frame(Lati=Lat %>% as.numeric(), Long=Lon %>% as.numeric())
  coordinates(U.S.SB.sp) <- ~ Long + Lati

  
  # L1 layer 
  L1 <-  sf::read_sf(system.file("data","Layers", "Layers/eco-region.json", package = "data.land")) %>%
  st_set_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>%
  st_transform("+proj=longlat +datum=WGS84")
  # L2 layer 
  L2 <-  sf::read_sf(system.file("data","Layers", "Layers/eco-regionl2.json", package = "data.land")) %>%
    st_set_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>%
    st_transform("+proj=longlat +datum=WGS84")
  
  proj4string(U.S.SB.sp) <- proj4string(as_Spatial(L1))
# finding the code for each site
  over.out.L1 <- sp::over(U.S.SB.sp, as_Spatial(L1))
  over.out.L2 <- sp::over(U.S.SB.sp, as_Spatial(L2))
  
  return(data.frame(L1=over.out.L1$NA_L1CODE, L2=over.out.L2$NA_L2CODE))
}


