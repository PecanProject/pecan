## Data provided are in various formats and need to be harmonized for model

site_files_in <- here::here("models","peprmt","demo_run","raw-data", 
                            "site-data-raw")
site_files_out <- here::here("models","peprmt","demo_run","raw-data", 
                            "site-data-formatted")
desired_colnames <- colnames(PEPRMT::example_data)

# Format EDN
US_EDN <- readr::read_csv(here::here(site_files_in, "US_EDN.csv")) 

US_EDN_out <- US_EDN |>
  dplyr::mutate(site = "US_EDN",
                LAI = NA,
                FPAR = 0,
                LUE = 0.006, # From example data
                Wetland_age_years = 7, # Should technically increment. This is the min from example data
                Date = as.Date(paste0(YEAR,"-01-01")) + days(DOY) - 1,
                SOM_MEM_gC_m3 = 550) |> # Need time series data for this
  dplyr::rename(DOY_disc = DOY,
                TA_C = Air_temp_daily_ave_degree_C,
                WTD_cm = Water_table_daily_ave_cm,
                PAR_umol_m2_day = PAR_daily_ave_umol_m2_day,
                Year = YEAR) %>%
  dplyr::mutate(DOY = as.numeric(difftime(Date, min(Date), units = "days")) + 1) %>%
  dplyr::select(all_of(desired_colnames)) %>%
  filter(if_all(-LAI, ~ !is.na(.)))

write.csv(US_EDN_out, here::here(site_files_out, "US_EDN_formatted.csv"), row.names = F)

# Format SRR
US_SRR = list(id = "US_SRR",
              name = "Rush Ranch",
              lat = 38.200,
              lon = -122.026,
              site.pft = "default",
              met.start = "2014-03-12",
              met.end = "2018-09-20")

US_SRR <- readr::read_csv(here::here(site_files_in, "US_SRR.csv")) 

US_SRR_out <- US_SRR |>
  dplyr::mutate(site = "US_SRR",
                LAI = NA,
                FPAR = 0,
                LUE = 0.009, # From example data
                SOM_MEM_gC_m3 = 2000) |> # Need time series data for this
  dplyr::rename(DOY = Day_of_year,
                DOY_disc = Discont_day_of_year,
                TA_C = Air_temp_daily_ave_degree_C,
                WTD_cm = Water_table_daily_ave_cm,
                PAR_umol_m2_day = PAR_daily_ave_umol_m2_day) %>%
  dplyr::select(all_of(desired_colnames)) %>%
  filter(if_all(-LAI, ~ !is.na(.)))

#PEPRMT::example_data %>%
#  filter(site == "US_SRR") %>%
#  pull(LUE) %>%
#  unique()

write.csv(US_SRR_out, here::here(site_files_out, "US_SRR_formatted.csv"), row.names = F)

# Format DMG
US_DMG <- readr::read_csv(here::here(site_files_in, "US_DMG.csv")) 

US_DMG_out <- US_DMG |>
  dplyr::mutate(
    site = "US_DMG",
    LAI = NA,
    FPAR = 0,
    LUE = 0.006, # PLACEHOLDER
    Wetland_age_years = 7, # PLACEHOLDER
    EVI = 0.09, # PLACEHOLDER
    Date = as.Date(paste0(YEAR, "-01-01")) + lubridate::days(DOY) - 1,
    SOM_MEM_gC_m3 = 550 # PLACEHOLDER
  ) |>
  dplyr::rename(
    DOY_disc = DOY,
    TA_C = TA_ave_C,
    WTD_cm = WT_ave_m,
    Salinity_daily_ave_ppt = Salinity_ave_ppt,
    PAR_umol_m2_day = PAR_ave_umol_m2_day,
    Year = YEAR
  ) %>%
  dplyr::arrange(Date) %>%
  tidyr::complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  dplyr::mutate(
    Year = lubridate::year(Date),
    DOY  = lubridate::yday(Date)
  ) %>%
  dplyr::select(all_of(desired_colnames[!grepl("gC_m2_d", desired_colnames)])) %>%
  dplyr::mutate(across(
    -c(Year, DOY, LAI),
    ~ if (is.numeric(.)) zoo::na.approx(., na.rm = FALSE) else .
    )) %>%
  filter(if_all(where(is.numeric), ~ !is.na(.)))

write.csv(US_DMG_out, here::here(site_files_out, "US_DMG_formatted.csv"), row.names = F)
