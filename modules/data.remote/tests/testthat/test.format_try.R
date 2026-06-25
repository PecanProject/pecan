context("formatting TRY data")

test_that("format_try_for_ma maps correctly and handles missing columns", {
  try_data <- data.frame(
    TraitID = c(1, NA, 3, 2),
    TraitName = c("Specific leaf area (SLA) or specific leaf area (SLA)", 
                  "covariate", 
                  "Leaf nitrogen (N) content per leaf dry mass", 
                  "Unknown Trait"),
    StdValue = c(10.5, 20.0, 1.2, 5.0),
    ErrorRisk = c(0.5, 1.0, 0.1, -0.2), # SLA stat 0.5, leafN stat 0.1, covariate 1.0, Unknown -0.2
    Replicates = c(3, 1, 1, 4), # leafN n=1 and stat=0.1 -> n=2
    DatasetID = c(101, 102, NA, 104),
    ObservationID = c(1001, 1002, 1003, 1004),
    Latitude = c(40.1, 40.1, 40.2, 40.3),
    Longitude = c(-88.2, -88.2, -88.3, -88.4),
    Date = c("2020-01-01", "2020-01-01", "2020-05-02", "2020-01-03"),
    Time = c("12:00", "12:00", "13:00", "14:00"),
    AccSpeciesID = c(9001, 9002, 9003, 9004),
    Dataset = c("Field Observational", "Other", "Some Greenhouse Study", "Other")
  )
  
  expect_warning(res <- format_try_for_ma(try_data), "The following TRY traits or covariates were not mapped")
  
  # Only traits with valid TraitID and present in the mapped traits should remain
  expect_equal(nrow(res), 2)
  expect_equal(res$vname, c("SLA", "leafN"))
  expect_equal(res$mean, c(10.5, 1.2))
  expect_equal(res$stat, c(0.5, 0.1))
  expect_equal(res$n, c(3, 2)) # leafN 'Replicates' was 1, but stat is not NA, jagify logic bumps to 2
  expect_equal(res$citation_id, c(101, -9999))
  expect_equal(res$greenhouse, c(0, 1))
  expect_true("species_name" %in% names(res))
})

test_that("format_try_for_ma handles jagify-style edge cases cleanly", {
  try_data <- data.frame(
    TraitID = c(1),
    TraitName = c("Specific leaf area (SLA) or specific leaf area (SLA)"),
    StdValue = c(10.5),
    ErrorRisk = c(-0.5), # <= 0 should become NA w/ warning
    Replicates = c(NA)
  )
  
  expect_warning(res <- format_try_for_ma(try_data), "implausible values of SE <= 0")
  expect_equal(as.numeric(res$stat), as.numeric(NA))
  expect_equal(res$n, 1) # NA Replicates but stat is NA -> n defaults to 1
  expect_equal(res$site_id, 1) # Falls back to 1:nrow -> 1
})
