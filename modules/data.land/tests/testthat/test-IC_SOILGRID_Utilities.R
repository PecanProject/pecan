test_that("soilgrids_ic_process rejects MultiSettings", {
  s1 <- PEcAn.settings::Settings(run = list(site = list(id = 1, 
                                                        lat = 0, 
                                                        lon = 0, 
                                                        name = "site1")))
  s2 <- PEcAn.settings::Settings(run = list(site = list(id = 2,
                                                        lat = 10, 
                                                        lon = 10, 
                                                        name = "site2")))
  ms <- PEcAn.settings::MultiSettings(list(s1, s2))
  expect_error(soilgrids_ic_process(ms, dir = tempdir()))
})

test_that("soilgrids_ic_process rejects invalid depth values", {
  settings <- PEcAn.settings::Settings(run = list(site = list(id = 1, 
                                                              lat = 0, 
                                                              lon = 0, 
                                                              name = "test")))
  expect_error(soilgrids_ic_process(settings, dir = tempdir(), depth = 1.0))
})

test_that("fails if site not found in processed SoilGrids data", {
  settings <- PEcAn.settings::Settings(run = list(site = list(id = 999, 
                                                              lat = 0, 
                                                              lon = 0, 
                                                              name = "test")))
  fake_processed <- list(data = data.frame(Site_ID = 1, 
                                          Total_soilC_0-30cm = 10, 
                                          Std_soilC_0-30cm = 1), 
                        cv_distributions = list("0-30cm" = list(type = "none")), 
                        depth_layers = "0-30cm")

  withr::with_environment(list(preprocess_soilgrids_data = function(...) fake_processed),
                          expect_error(soilgrids_ic_process(settings, dir = tempdir(), depth = 0.3)))
})

test_that("returns named list keyed by site id", {
  settings <- PEcAn.settings::Settings(run = list(site = list(id = 42, 
                                                              lat = 0, 
                                                              lon = 0, 
                                                              name = "test")),
                                       ensemble = list(size = 1))

  fake_soil <- data.frame(Site_ID = 42,
                          Total_soilC_0-30cm = 10,
                          Std_soilC_0-30cm = 1)

  withr::with_environment(
    list(
      preprocess_soilgrids_data = function(...) list(
        data = fake_soil,
        cv_distributions = list("0-30cm" = list(type = "none")),
        depth_layers = "0-30cm"
      ),
      generate_soilgrids_ensemble = function(...) c(10),
      PEcAn.data.land::pool_ic_list2netcdf = function(...) list(file = "fake.nc"),
      PEcAn.data.land::soilgrids_soilC_extract = function(...) fake_soil
    ),
    {
      result <- soilgrids_ic_process(settings, dir = tempdir(), depth = 0.3)
      expect_named(result, "42")
      expect_true(is.list(result[[1]]))
    }
  )
})
