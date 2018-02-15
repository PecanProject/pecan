
context("checking call_biocro wrappers")

loglevel <- PEcAn.logger::logger.getLevel()
PEcAn.logger::logger.setLevel("OFF")
testthat::teardown(PEcAn.logger::logger.setLevel(loglevel))

WetDat <- read.csv("data/US-Bo1.2004.csv", nrows=7*24)
config <- list(pft = list(
  name="fake_pft",
  phenoParms=list("3", "10"),
  canopyControl=list(a=1, b=2, c=3),
  parameters=list(aa=1, bb=2, cc=3),
  initial_values=list(Root=10, Leaf=3, Stem=20)))
fake_b0.9_result = structure(
  list(
    DayofYear = c(1, 1, 1),
    Hour = c(1, 2, 3),
    Leaf = c(10, 11, 12),
    Root = c(20, 30, 40),
    LAI = c(2.0, 2.01, 2.03),
    rdMat = matrix(c(0,0,0))),
  class = "BioGro")
fake_b1_result <- data.frame(
      DOY = c(1, 1, 1),
      Hour = c(1, 2, 3),
      Leaf = c(10, 11, 12),
      Root = c(20, 30, 40),
      Stem = c(200, 300, 400),
      lai = c(2.0, 2.01, 2.03),
      TTc = c(1, 1.5, 1.8),
      soil_evaporation = c(0, 0, 0),
      canopy_transpiration = c(0, 0, 0),
      LeafLitter = c(1, 1.01, 1.02),
      StemLitter = c(10, 10.01, 10.02),
      RootLitter = c(15, 15.01, 15.02),
      RhizomeLitter = c(20, 20.01, 20.02))


test_that("call_biocro_0.9 passes expected arguments to every supported genus", {
  # stub out BioCro::caneGro, BioCro::willowGro, BioCro::BioGro,
  # Making them all return a VERY simplified list
  canemock <- mockery::mock(fake_b0.9_result, cycle = TRUE)
  willowmock <- mockery::mock(fake_b0.9_result, cycle = TRUE)
  biomock <- mockery::mock(fake_b0.9_result, cycle = TRUE)
  mockery::stub(call_biocro_0.9, "BioCro::caneGro", canemock)
  mockery::stub(call_biocro_0.9, "BioCro::willowGro", willowmock)
  mockery::stub(call_biocro_0.9, "BioCro::BioGro", biomock)

  for (i in c("Saccharum", "Salix", "Miscanthus", "Sorghum")) {
    res <- call_biocro_0.9(
      WetDat = WetDat, genus = i, year_in_run = 1, config = config,
      lat = 40, lon = -88, tmp.result = list(),
      HarvestedYield = 0)
    expect_length(res, 2)
    expect_equal(names(res), c("tmp.result", "HarvestedYield"))
    expect_type(res$tmp.result, "list")
    expect_equal(res$tmp.result$doy, fake_b0.9_result$DayofYear)
    expect_equal(res$tmp.result$hour, fake_b0.9_result$Hour)
    expect_equal(res$tmp.result$Root, fake_b0.9_result$Root)
    expect_type(res$HarvestedYield, "double")
  }

  # numeric param lists passed unchanged
  mockery::expect_called(willowmock, 1)
  expect_equal(
    mockery::mock_args(willowmock)[[1]]$canopyControl,
    config$pft$canopyControl)
  # character param lists are converted to numeric
  expect_type(
    mockery::mock_args(willowmock)[[1]]$willowphenoControl[[1]],
    "double")
  expect_equal(
    mockery::mock_args(willowmock)[[1]]$willowphenoControl,
    lapply(config$pft$phenoParms, as.numeric))
# genus-specific params not passed to other genera
  mockery::expect_called(canemock, 1)
  expect_null(
    mockery::mock_args(canemock)[[1]]$willowphenoControl)


  # BioGro is called once every invocation to test if BioCro checks input DOY,
  # plus again for output in Miscanthus and Sorghum, equals six calls in total
  mockery::expect_called(biomock, 6)
  for (j in c(1,2,3,5)) {
    mockery::expect_call(
      biomock, j,
      BioCro::BioGro(WetDat = matrix(c(0,10,0,0,0,0,0,0), nrow = 1),
                     day1 = 10, dayn = 10, timestep = 24))
  }
  expect_equal(mockery::mock_args(biomock)[[4]]$day1, min(WetDat$doy))
  expect_equal(mockery::mock_args(biomock)[[4]]$dayn, max(WetDat$doy))

  expect_error(
    call_biocro_0.9(WetDat = WetDat, genus = "not_a_genus", year_in_run = 1,
                    config = config, lat = 40, lon = -88,
                    tmp.result = list(), HarvestedYield = 0),
    "not supported by PEcAn.BIOCRO when using BioCro 0.9x")
})


test_that("call_biocro_0.9 adjusts day1 and dayn when weather is not a whole year",{

  # call_biocro_0.9 tests whether to adjust days by passing a one-line file
  # with DOY > 1 and adjusting iff BioCro::BioGro throws an error.
  # ==> To test, our BioGro stub needs to provide the error too.
  biomock <- mockery::mock(fake_b0.9_result, cycle = TRUE)
  mockery::stub(
    call_biocro_0.9,
    "BioCro::BioGro",
    function(WetDat, ...){
      if(nrow(WetDat)==1 && WetDat$doy > 1){
        stop("This error should be caught silently")
      }else{
        biomock(WetDat, ...)
      }
    })

  # whole file: day numbers unchanged
  res_whole <- call_biocro_0.9(
    WetDat = WetDat, genus = "Miscanthus", year_in_run = 1, config = config,
    lat = 40, lon = -88, tmp.result = list(), HarvestedYield = 0)
  expect_equal(mockery::mock_args(biomock)[[1]]$day1, min(WetDat$doy))
  expect_equal(mockery::mock_args(biomock)[[1]]$dayn, max(WetDat$doy))

  # subset starting DOY 1: day numbers unchanged
  res_start <- call_biocro_0.9(
    WetDat = WetDat[WetDat$doy <= 3,], genus = "Miscanthus", year_in_run = 1,
    config = config, lat = 40, lon = -88,
    tmp.result = list(), HarvestedYield = 0)
  expect_equal(mockery::mock_args(biomock)[[2]]$day1, 1)
  expect_equal(mockery::mock_args(biomock)[[2]]$dayn, 3)

  # subset starting DOY > 1: day numbers adjusted --
  # BioCro secretly wants day of *file*, not day of year
  res_jan <- call_biocro_0.9(
      WetDat = WetDat[WetDat$doy >= 3 & WetDat$doy <= 6,],
      genus = "Miscanthus", year_in_run = 1, config = config, lat = 40,
      lon = -88, tmp.result = list(), HarvestedYield = 0)
  expect_equal(round(mockery::mock_args(biomock)[[3]]$day1), 1)
  expect_equal(round(mockery::mock_args(biomock)[[3]]$dayn), 4)
})


test_that("call_biocro_1 passes expected arguments", {
  # stub out BioCro::Gro
  b1mock <- mockery::mock(fake_b1_result, cycle = TRUE)
  mockery::stub(call_biocro_1, "BioCro::Gro", b1mock)

  for (i in c("Salix", "Miscanthus", "NovelGenus")) {
    res <- call_biocro_1(
      WetDat = WetDat, genus = i, year_in_run = 1, config = config,
      lat = 40, lon = -88, tmp.result = list(),
      HarvestedYield = 0)
    expect_length(res, 2)
    expect_equal(names(res), c("tmp.result", "HarvestedYield"))
    expect_s3_class(res$tmp.result, "data.frame")
    expect_equal(res$tmp.result$DOY, fake_b1_result$DOY)
    expect_equal(res$tmp.result$ThermalT, fake_b1_result$TTc)
    expect_equal(
      res$tmp.result$BelowLitter,
      fake_b1_result$RootLitter + fake_b1_result$RhizomeLitter)
    expect_type(res$HarvestedYield, "double")

    # BioCro 1 param lists passed unchanged
    expect_equal(
      mockery::mock_args(b1mock)[[length(b1mock)]]$parameters,
      config$pft$parameters)
    expect_equal(
      mockery::mock_args(b1mock)[[length(b1mock)]]$varying_parameters,
      WetDat)
    # BioCro 0.9 param lists, day1, dayn not used
    expect_null(mockery::mock_args(b1mock)[[length(b1mock)]]$phenoControl)
    expect_null(mockery::mock_args(b1mock)[[length(b1mock)]]$day1)
    expect_null(mockery::mock_args(b1mock)[[length(b1mock)]]$dayn)
  }

  mockery::expect_called(b1mock, 3)
  
})

test_that("call_biocro_1 updates initial values after year 1", {
  b1mock <- mockery::mock(fake_b1_result, cycle = TRUE)
  mockery::stub(call_biocro_1, "BioCro::Gro", b1mock)

    res1 <- call_biocro_1(
      WetDat = WetDat, genus = "Salix", year_in_run = 1, config = config,
      lat = 40, lon = -88, tmp.result = list(),
      HarvestedYield = 0)
    expect_equal(
      mockery::mock_args(b1mock)[[1]]$initial_values,
      config$pft$initial_values)

    res2 <- call_biocro_1(
      WetDat = WetDat, genus = "Salix", year_in_run = 2, config = config,
      lat = 40, lon = -88, tmp.result = res1$tmp.result,
      HarvestedYield = res1$HarvestedYield)
    for (var in names(config$pft$initial_values)) {
      expect_equal(
        mockery::mock_args(b1mock)[[2]]$initial_values[[!!var]],
        res1$tmp.result[[!!var]][nrow(res1$tmp.result)])
    }
})
