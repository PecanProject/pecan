context("check that BioCro output is summarized correctly")

# Hand-calculate reference values
ref_output <- mock_run()
ref_met <- read.csv("data/US-Bo1.2004.csv", nrows=7*24)
ref_leaf1 <- max(ref_output$Leaf[ref_output$DayofYear == 1])
ref_soil5 <- sum(ref_output$SoilEvaporation[ref_output$DayofYear == 5])
ref_mat <- mean(ref_met$Temp)

# run setup
metpath <- "data/US-Bo1"
settings <- PEcAn.settings::read.settings("data/pecan.biocro.xml")
settings$database$bety <- do.call(
  PEcAn.DB::get_postgres_envvars,
  settings$database$bety)
config <- PEcAn.settings::prepare.settings(settings)
config$pft$type$genus <- "Salix"
config$run$start.date <- as.POSIXct("2004-01-01")
config$run$end.date <- as.POSIXct("2004-01-07")
config$simulationPeriod$dateofplanting <- as.POSIXct("2004-01-01")
config$simulationPeriod$dateofharvest <- as.POSIXct("2004-01-07")

# test_that("daily summarizes hourly (#1738)", {

# 	# stub out BioCro::willowGro and packageVersion:
# 	# calls to willowGro(...) will be replaced with calls to mock_run(...),
# 	# calls to utils::packageVersion("BioCro") will return 0.95,
# 	# but *only* when originating inside call_biocro_0.9 AND inside a run.biocro call.
# 	# mock_run and mock_version are defined in helper.R
# 	mockery::stub(
# 		where = run.biocro,
# 		what = "call_biocro_0.9",
# 		how = function(...){
# 			mockery::stub(call_biocro_0.9, "BioCro::willowGro", mock_run);
# 			call_biocro_0.9(...)})
# 	mockery::stub(run.biocro, "utils::packageVersion", mock_version)

# 	mock_result <- run.biocro(lat = 44, lon = -88, metpath, soil.nc = NULL, config = config, coppice.interval = 1)
# 	expect_equal(nrow(mock_result$hourly), 24*7)
# 	expect_equal(nrow(mock_result$daily), 7)
# 	expect_equal(nrow(mock_result$annually), 1)
# 	expect_gt(length(unique(mock_result$daily$tmax)), 1)
# 	expect_equal(mock_result$daily$Leaf[mock_result$daily$doy == 1], ref_leaf1)
# 	expect_equal(mock_result$daily$SoilEvaporation[mock_result$daily$doy == 5], ref_soil5)
# 	expect_equal(mock_result$annually$mat, ref_mat)
# })
