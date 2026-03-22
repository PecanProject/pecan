
# Load example data
all_sites <- read.csv(here::here("models", "peprmt", "demo_run", "raw-data",
                                 "All_sites_master.csv"))

# Format
example_data <- all_sites[c(
  "site_char", "Year", "DOY_disc", "DOY", "Year", 
  "TA_C", "WTD_cm", "PAR_umol_m2_day", "LAI", "EVI", "FPAR", "LUE",
  "Wetland_age_years", "Salinity_daily_ave_ppt", "NO3_mg_L", "SOM_MEM_gC_m3", 
  "CO2_gC_m2_day", "GPP_gC_m2_day", "Reco_gC_m2_day", "CH4_gC_m2_day"
)] |>
  dplyr::rename(site = site_char)

#Five sites, each of which need to be documented in site_info.csv
unique(example_data$site)

#Using values from Oikawa et al. 2024
US_EDN = list(id = "US_EDN",
              lat = 37.615,
              lon = -122.114,
              site.pft = "Not implemented")

US_SRR = list(id = "US_SRR",
              lat = 38.200,
              lon = -122.026,
              site.pft = "Not implemented")

US_STJ = list(id = "US_STJ",
              lat = 39.088,
              lon = -75.437,
              site.pft = "Not implemented")

US_LA1 = list(id = "US_LA1",
              lat = 29.5013,
              lon = -90.4449,
              site.pft = "Not implemented")

US_PLM = list(id = "US_PLM",
              lat = 42.7345,
              lon = -70.8382,
              site.pft = "Not implemented")

site_info <- list(US_EDN,
                  US_SRR,
                  US_STJ,
                  US_LA1,
                  US_PLM) |>
  dplyr::bind_rows()

write.csv(site_info, 
          here::here("models", "peprmt", "demo_run", "data",
                     "site_info.csv"),
          row.names = F)

write.csv(example_data, 
          here::here("models", "peprmt", "demo_run", "data",
                     "all_data.csv"),
          row.names = F)

dates <- as.Date(paste0(example_data$Year, "-01-01")) + lubridate::days(example_data$DOY_disc)
