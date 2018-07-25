#' Conversion table for PEcAn standard meteorology
#'
#' @export
pecan_standard_met_table <- tibble::tribble(
  ~`cf_standard_name`                                     , ~units        , ~bety               , ~isimip        , ~cruncep , ~narr   , ~ameriflux         ,
  "air_temperature"                                       , "K"           , "airT"              , "tasAdjust"    , "tair"   , "air"   , "TA (C)"           ,
  "air_temperature_max"                                   , "K"           , NA                  , "tasmaxAdjust" , NA       , "tmax"  , NA                 ,
  "air_temperature_min"                                   , "K"           , NA                  , "tasminAdjust" , NA       , "tmin"  , NA                 ,
  "air_pressure"                                          , "Pa"          , "air_pressure"      , NA             , NA       , NA      , "PRESS (KPa)"      ,
  "mole_fraction_of_carbon_dioxide_in_air"                , "mol/mol"     , NA                  , NA             , NA       , NA      , "CO2"              ,
  "moisture_content_of_soil_layer"                        , "kg m-2"      , NA                  , NA             , NA       , NA      , NA                 ,
  "soil_temperature"                                      , "K"           , "soilT"             , NA             , NA       , NA      , "TS1 *(NOT DONE)*" ,
  "relative_humidity"                                     , "%"           , "relative_humidity" , "rhurs"        , NA       , "rhum"  , "RH"               ,
  "specific_humidity"                                     , "1"           , "specific_humidity" , NA             , "qair"   , "shum"  , "CALC(RH)"         ,
  "water_vapor_saturation_deficit"                        , "Pa"          , "VPD"               , NA             , NA       , NA      , "VPD *(NOT DONE)*" ,
  "surface_downwelling_longwave_flux_in_air"              , "W m-2"       , "same"              , "rldsAdjust"   , "lwdown" , "dlwrf" , "Rgl"              ,
  "surface_downwelling_shortwave_flux_in_air"             , "W m-2"       , "solar_radiation"   , "rsdsAdjust"   , "swdown" , "dswrf" , "Rg"               ,
  "surface_downwelling_photosynthetic_photon_flux_in_air" , "mol m-2 s-1" , "PAR"               , NA             , NA       , NA      , "PAR *(NOT DONE)*" ,
  "precipitation_flux"                                    , "kg m-2 s-1"  , "cccc"              , "prAdjust"     , "rain"   , "acpc"  , "PREC (mm/s)"      ,
  "wind_to_direction"                                     , "degrees"     , "wind_direction"    , NA             , NA       , NA      , "WD"               ,
  "wind_speed"                                            , "m/s"         , "Wspd"              , NA             , NA       , NA      , "WS"               ,
  "eastward_wind"                                         , "m/s"         , "eastward_wind"     , NA             , NA       , NA      , "CALC(WS+WD)"      ,
  "northward_wind"                                        , "m/s"         , "northward_wind"    , NA             , NA       , NA      , "CALC(WS+WD)"
)
