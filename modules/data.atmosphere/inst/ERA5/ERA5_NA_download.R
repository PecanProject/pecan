outfolder <- "/projectnb/dietzelab/dongchen/anchorSites/ERA5/"
start_date <- "2012-01-01"
end_date <- "2021-12-31"
extent <- c(-179, -20, 7, 85)
# cds credentials, from https://cds.climate.copernicus.eu/profile
user <- "your_cds_uid"
key <- "your_cds_api_key"
# ecmwfr keeps the key in the keyring. a headless machine has no os keyring and
# the default backend prompts for a password, so keep the key in the environment
Sys.setenv(R_KEYRING_BACKEND = "env")
variables <- c("2m_temperature",
               "surface_pressure",
               "2m_dewpoint_temperature",
               "total_precipitation",
               "10m_u_component_of_wind",
               "10m_v_component_of_wind",
               "surface_solar_radiation_downwards",
               "surface_thermal_radiation_downwards")
results <- PEcAn.data.atmosphere::download.ERA5_cds(outfolder = outfolder,
                                                    start_date = start_date,
                                                    end_date = end_date,
                                                    extent = extent,
                                                    variables = variables,
                                                    user = user,
                                                    key = key)
