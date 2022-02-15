context("*** Test soilgrids.soc.extract function at one location ***")

test_that("The soilgrids.soc.extract function properly obtains data for one lat/long", {
  site_info <- data.frame(id=676, sitename="Willow Creek (US-WCr)", lat=45.805925, lon=-90.07961,
                          time_zone="America/Chicago")
  verbose <- FALSE
  result_soc <- soilgrids.soc.extract(site_info=site_info, verbose=verbose)
  expect_true(!is.null(result_soc))
  expect_gte(result_soc$Mean_soc$`0-5cm`, 48)
})