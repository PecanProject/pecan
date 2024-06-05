context("testing csv import using met2CF.csv")
format <- list(
       header = 1,
       time_zone = "GMT",
       time.row = 1,
       skip = 0,
       unit.row = TRUE,
       na.strings = NA,
       vars = list(
              input_name = c(
                     "Corrected Date/Time", "Solar Radiation (W/m2)", "Temp (C)",
                     "PAR_(umol_m-2_s-1)", "PAR_(mol_m-2_h-1)", "RH (%)", "Wind Speed (m/s)",
                     "VPD", "Rain (mm)"
              ),
              input_units = c(
                     NA, "W m-2", "celsius",
                     "umol m-2 s-1", "mol m-2 h-1", "%", "m s-1", "Pa", "mm h-1"
              ),
              bety_name = c(
                     "datetime", "solar_radiation", "airT",
                     "PAR", NA, "relative_humidity", "Wspd", NA, "precipitation_rate"
              ),
              storage_type = c(
                     "%m/%d/%y %H:%M", NA, NA, NA, NA, NA, NA, NA, NA
              )
       ),
       lat <- 42 + 47 / 60 + 30 / 6000,
       lon <- 76 + 7 / 60 + 20 / 6000
)
# met2CF.csv(in.path = "data", in.file = "met2CF.csv.csv", outfolder = tempdir(),
#            format = format,
#            lat = 42 + 47/60 + 30/6000,
#            lon = 76 + 7/60 + 20/6000)


outfolder <- tempdir()

# Initial test suite to test the met2CF.csv function
test_that("met2CF.csv function works correctly", {
       output <- PEcAn.data.atmosphere::met2CF.csv(
              in.path = "data",
              in.prefix = "test.met2CF.csv.csv",
              outfolder = outfolder,
              start_date = lubridate::ymd_hm("2013-03-01 18:00"),
              end_date = lubridate::ymd_hm("2013-03-27 17:00"),
              format = format,
              lat = format$lat,
              lon = format$lon,
              overwrite = TRUE
       )
       nc_files <- list.files(outfolder, pattern = "\\.nc$", full.names = TRUE)

       expect_true(file.exists(nc_files))
       expect_true(file.size(nc_files) > 0)
       expect_equal(nc_files, file.path(outfolder, "test.met2CF.2013.nc"))
})
