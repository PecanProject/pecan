#' Conversion table for PEcAn standard meteorology
#'
#' @export
pecan_standard_met_table <- tibble::tribble(
  ~`cf_standard_name`                                     , ~units        , ~is_required,  ~bety               , ~isimip        , ~cruncep , ~narr   , ~ameriflux         , ~era5   , ~era5_cds,
  "air_temperature"                                       , "K"           ,         TRUE,  "airT"              , "tasAdjust"    , "tair"   , "air"   , "TA (C)"           , "t2m"   , NA,
  "air_temperature_max"                                   , "K"           ,        FALSE,  NA                  , "tasmaxAdjust" , NA       , "tmax"  , NA                 , NA      , NA,
  "air_temperature_min"                                   , "K"           ,        FALSE,  NA                  , "tasminAdjust" , NA       , "tmin"  , NA                 , NA      , NA,
  "air_pressure"                                          , "Pa"          ,         TRUE,  "air_pressure"      , NA             , NA       , NA      , "PRESS (KPa)"      , "sp"    , NA,
  "dew_point_temperature"                                 , "K"           ,        FALSE,  NA                  , NA             , NA       , NA      , NA                 , "d2m"   , NA,
  "mole_fraction_of_carbon_dioxide_in_air"                , "1"           ,        FALSE,  NA                  , NA             , NA       , NA      , "CO2"              , NA      , NA,
  "moisture_content_of_soil_layer"                        , "kg m-2"      ,        FALSE,  NA                  , NA             , NA       , NA      , NA                 , NA      , NA,
  "soil_temperature"                                      , "K"           ,        FALSE,  "soilT"             , NA             , NA       , NA      , "TS1 *(NOT DONE)*" , NA      , NA,
  "relative_humidity"                                     , "%"           ,        FALSE,  "relative_humidity" , "rhurs"        , NA       , "rhum"  , "RH"               , NA      , NA,
  "specific_humidity"                                     , "1"           ,         TRUE,  "specific_humidity" , NA             , "qair"   , "shum"  , "CALC(RH)"         , NA      , NA,
  "water_vapor_saturation_deficit"                        , "Pa"          ,        FALSE,  "VPD"               , NA             , NA       , NA      , "VPD *(NOT DONE)*" , NA      , NA,
  "surface_downwelling_longwave_flux_in_air"              , "W m-2"       ,         TRUE,  "same"              , "rldsAdjust"   , "lwdown" , "dlwrf" , "Rgl"              , "strd"  , NA,
  "surface_downwelling_shortwave_flux_in_air"             , "W m-2"       ,         TRUE,  "solar_radiation"   , "rsdsAdjust"   , "swdown" , "dswrf" , "Rg"               , "ssrd"  , "surface_solar_radiation_downwards",
  "surface_downwelling_photosynthetic_photon_flux_in_air" , "mol m-2 s-1" ,        FALSE,  "PAR"               , NA             , NA       , NA      , "PAR *(NOT DONE)*" , NA      , NA,
  "precipitation_flux"                                    , "kg m-2 s-1"  ,         TRUE,  "cccc"              , "prAdjust"     , "rain"   , "acpc"  , "PREC (mm/s)"      , "tp"    , NA,
  "wind_to_direction"                                     , "degrees"     ,        FALSE,  "wind_direction"    , NA             , NA       , NA      , "WD"               , NA      , NA,
  "wind_speed"                                            , "m s-1"       ,        FALSE,  "Wspd"              , NA             , NA       , NA      , "WS"               , NA      , NA,
  "eastward_wind"                                         , "m s-1"       ,         TRUE,  "eastward_wind"     , NA             , NA       , NA      , "CALC(WS+WD)"      , "u10"   , NA,
  "northward_wind"                                        , "m s-1"       ,         TRUE,  "northward_wind"    , NA             , NA       , NA      , "CALC(WS+WD)"      , "v10"   , NA,
  "volume_fraction_of_condensed_water_in_soil"            , "1"           ,        FALSE,  "soilM"             , NA             , NA       , NA      , "SWC_1"            , "swvl1" , "volumetric_soil_water_layer_1"
)