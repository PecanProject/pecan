
context("checking call_biocro wrappers")

loglevel <- PEcAn.logger::logger.getLevel()
PEcAn.logger::logger.setLevel("OFF")
teardown(PEcAn.logger::logger.setLevel(loglevel))

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


# BioGro is called for Miscanthus and Sorghum output
  mockery::expect_called(biomock, 2)
  expect_equal(mockery::mock_args(biomock)[[1]]$day1, min(WetDat$doy))
  expect_equal(mockery::mock_args(biomock)[[1]]$dayn, max(WetDat$doy))

  expect_error(
    call_biocro_0.9(WetDat = WetDat, genus = "not_a_genus", year_in_run = 1,
                    config = config, lat = 40, lon = -88,
                    tmp.result = list(), HarvestedYield = 0),
    "not supported by PEcAn.BIOCRO when using BioCro 0.9x")
})


test_that("call_biocro_0.9 adjusts day1 and dayn when weather is not a whole year",{

  # BioCro 0.9x treats day1/dayn as "day of file", so rescaling is always applied
  # when data doesn't start on DOY 1.
  biomock <- mockery::mock(fake_b0.9_result, cycle = TRUE)
  mockery::stub(call_biocro_0.9, "BioCro::BioGro", biomock)

  # whole file starting DOY 1: day numbers unchanged
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

  # subset starting DOY > 1: day numbers adjusted to "day of file"
  res_jan <- call_biocro_0.9(
      WetDat = WetDat[WetDat$doy >= 3 & WetDat$doy <= 6,],
      genus = "Miscanthus", year_in_run = 1, config = config, lat = 40,
      lon = -88, tmp.result = list(), HarvestedYield = 0)
  expect_equal(mockery::mock_args(biomock)[[3]]$day1, 1)
  expect_equal(mockery::mock_args(biomock)[[3]]$dayn, 4)
})

test_that("adjustments to day1 and dayn work right with live biocro calls", {
  skip_if_not_installed("BioCro")
  skip_if_not(packageVersion("BioCro") == "0.95")

  met_2004 <- read.csv("data/US-Bo1.2004.csv") |>
    dplyr::filter(doy <= 365) # final timepoint is labeled 366, biocro complains

  live_config <- list(pft = list(
    name="fake_pft",
    phenoParms=list(
      tp1=562, tp2=1312, tp3=2063, tp4=2676, tp5=3211, tp6=7000,
      kStem1=0.37, kLeaf1=0.33, kRoot1=0.3, kRhizome1=-0.0008,
      kStem2=0.85, kLeaf2=0.14, kRoot2=0.01, kRhizome2=-0.0005,
      kStem3=0.63, kLeaf3=0.01, kRoot3=0.01, kRhizome3=0.35,
      kStem4=0.63, kLeaf4=0.01, kRoot4=0.01, kRhizome4=0.35,
      kStem5=0.63, kLeaf5=0.01, kRoot5=0.01, kRhizome5=0.35,
      kStem6=0.63, kLeaf6=0.01, kRoot6=0.01, kRhizome6=0.35,
      kGrain6=0),
    canopyControl=list(a=1, b=2, c=3),
    soilControl=list(
      FieldC=-1, WiltP=-1, phi1=0.01, phi2=10,
      soilDepth=1, iWatCont=0.32, soilType=6, soilLayers=1,
      wsFun=0, scsf=1, transpRes=5000000, leafPotTh=-800,
      hydrDist=0, rfl=0.2, rsec=0.2, rsdf=0.44),
    seneControl=list(
      senLeaf=3000, senStem=3500, senRoot=4000, senRhizome=4000),
    iPlantControl=list(
      iRhizome=3, iStem=1, iLeaf=0, iRoot=1,
      ifrRhizome=0.01, ifrStem=0.01),
    photoParms=list(
      vmax=39, alpha=0.04, kparm=0.7, theta=0.83, beta=0.93,
      Rd=0.8, Catm=400, b0=0.01, b1=3, ws=1,
      UPPERTEMP=37.5, LOWERTEMP=3),
    parameters=list(aa=1, bb=2, cc=3),
    initial_values=list(Root=10, Leaf=3, Stem=20)))

  call_with <- function(met) {
    PEcAn.BIOCRO:::call_biocro_0.9(
      WetDat = met,
      config = live_config,
      genus = "Miscanthus",
      year_in_run = 1,
      HarvestedYield = 1
    )$tmp.result
  }

  # Whole year
  expect_equal(length(call_with(met_2004)$LAI), 365 * 24)

  # Q2 data, real DOY
  met_q2_2004 <- met_2004[met_2004$doy %in% 91:180, ]
  res_q2 <- call_with(met_q2_2004)
  expect_equal(length(res_q2$LAI), 90 * 24)

  # Q2 data, labeled as if starting DOY 1
  # day1/dayn rescaling should allow this to run and use identical weather values.
  # Growth results won't be numerically identical because BioCro uses the raw DOY 
  # from the weather matrix for solar angle calculations, so shifting DOY changes
  # the solar geometry.
  res_q2_d1 <- call_with(met_q2_2004 |> dplyr::mutate(doy = doy - 90))
  expect_equal(length(res_q2_d1$LAI), 90 * 24)
  expect_equal(res_q2$ThermalT, res_q2_d1$ThermalT)

  # two whole years
  expect_error(
    call_with(
      dplyr::bind_rows(
        met_2004,
        met_2004 |> dplyr::mutate(year = 2005, Temp = 1)
      )
    ),
    "must contain only one year"
  )

  # Multiple years starting with partial year
  expect_error(
    call_with(
      dplyr::bind_rows(
        met_2004 |> dplyr::filter(doy %in% 91:300),
        met_2004 |>
          dplyr::mutate(year = 2005)
      )
    ),
    "must contain only one year"
  )

  # Multiple years, all partial
  expect_error(
    call_with(
      dplyr::bind_rows(
        met_2004 |> dplyr::filter(doy %in% 91:200),
        met_2004 |>
          dplyr::filter(doy %in% 91:300) |>
          dplyr::mutate(year = 2005)
      )
    ),
    "must contain only one year"
  )
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
