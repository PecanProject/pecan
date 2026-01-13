# Mapping from RothC to PEcAn standard names
# Not complete yet -- missing pecan_name means I haven't looked yet,
# not that we know no equivalent exists
rothc_varname_map <- dplyr::tribble(
  ~rothc_name, ~rothc_description, ~rothc_unit, ~pecan_name,
  "C_Inp_t_C_ha",  "C input",                 "t C ha-1",  NA,
  "OA_Inp_t_C_ha", "Farmyard manure",         "t C ha-1",  NA,
  "TEMP_C",     "Air temperature",            "degC",      "Tair",
  "RM_TMP",     "Rate modifier for temp",              "1",    NA,
  "RAIN_mm",    "Rainfall",                            "mm", NA,
  "PEVAP_mm",   "Open pan evaporation",                "mm", NA, # probably matches to "Evap" -- but careful with units _and_ with conceptual difference between pan evap and total evap
  "SWC_mm",     "Accumulated soil water deficit",      "mm", NA, # TODO may be called "SMD_mm" now?
  "RM_Moist",   "Rate modifier for soil moist",        "1",    NA,
  "PC",         "Soil plant cover (0=bare 1=covered)", "1",    NA,
  "RM_PC",      "rate modifier for crop cover",        "1",    NA,
  "DPM_t_C_ha", "Decomposable plant material", "t C ha-1", NA,
  "RPM_t_C_ha", "Resistant plant material",    "t C ha-1", NA,
  "Bio_t_C_ha", "Microbial biomass",           "t C ha-1", NA,
  "Hum_t_C_ha", "Humified organic matter",     "t C ha-1", "slow_soil_pool_carbon_content",
  "IOM_t_C_ha", "Inert organic matter",        "t C ha-1", "structural_soil_pool_carbon_content",
  "SOC_t_C_ha", "Total soil organic carbon",   "t C ha-1", "TotSoilCarb",
  "CO2_t_C_ha", "Accumulated CO2",             "t C ha-1", NA # needs time dimension to match to "TotalResp" which is kg C m-2 sec-1
)
