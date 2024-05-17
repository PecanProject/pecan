context("run_BASGRA")

mktmpdir <- function(env = parent.frame()) {
  # they now recommend this style instead of using setup() and teardown()
  outfolder <- tempfile()
  dir.create(outfolder, showWarnings = FALSE)
  withr::defer(unlink(outfolder, recursive = TRUE), env)
  outfolder
}

write_harv_fert <- function(path_harv, path_fert) {
  # write harvest and fertilize files
  df_harvest <- data.frame(
    year = 2019,
    doy = 163,
    CLAIV = 0.7
  )
  harvest_file <- path_harv # 
  write.csv(df_harvest, harvest_file, row.names=FALSE)
  df_fertilize <- data.frame(
    year = 2019,
    doy = 128,
    amount = 10.0
  )
  fertilize_file <- path_fert #
  write.csv(df_fertilize, fertilize_file, row.names=FALSE)
}

write_new_fert <- function(path_fert, which_type) {
  if (which_type == 'mineral') {
    df_fertilize <- data.frame(
      year = 2019,
      doy = 128,
      Nmin = 10.0,
      Norg = 0.0,
      C_soluble = 0.0,
      C_compost = 0.0
    )
  } else if (which_type == 'soluble') {
    df_fertilize <- data.frame(
      year = 2019,
      doy = 128,
      Nmin = 0.0,
      Norg = 10.0,
      C_soluble = 200.0,
      C_compost = 0.0
    )
  } else if (which_type == 'compost') {
    df_fertilize <- data.frame(
      year = 2019,
      doy = 128,
      Nmin = 0.0,
      Norg = 10.0,
      C_soluble = 0.0,
      C_compost = 200.0
    )
  } else if (which_type == 'invalid') {    
    df_fertilize <- data.frame(
      year = 2019,
      doy = 128,
      Nmin = 6.5,
      badstuff = 6.0
    )
  }
  fertilize_file <- path_fert #
  write.csv(df_fertilize, fertilize_file, row.names=FALSE)
}

test_that('two harvests yield more than one', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  fert_file <- file.path(outfolder, 'fertilize.csv')
  write_new_fert(fert_file, 'mineral')

  harv_file <- file.path(outfolder, 'harvest.csv')
  write.csv(
    data.frame(
      year = 2019,
      doy = 125
    ),
    harv_file, row.names=FALSE
  )
  run_BASGRA(
    met_path, run_params, harv_file, fert_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_one_harv <- read.csv(file.path(outfolder, 'output_basgra.csv'))

  harv_file <- file.path(outfolder, 'harvest.csv')
  write.csv(
    data.frame(
      year = c(2019, 2019),
      doy = c(125, 165)
    ),
    harv_file, row.names=FALSE
  )
  run_BASGRA(
    met_path, run_params, harv_file, fert_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_two_harv <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_gt(sum(output_two_harv$YIELD), sum(output_one_harv$YIELD))
})

test_that('harvest followed by cut yields same as only harvest but different mean LAI', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  fert_file <- file.path(outfolder, 'fertilize.csv')
  write_new_fert(fert_file, 'mineral')

  harv_file <- file.path(outfolder, 'harvest.csv')
  write.csv(
    data.frame(
      year = 2019,
      doy = 125,
      CLAIV = 0.7
    ),
    harv_file, row.names=FALSE
  )
  run_BASGRA(
    met_path, run_params, harv_file, fert_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_only_harv <- read.csv(file.path(outfolder, 'output_basgra.csv'))

  harv_file <- file.path(outfolder, 'harvest.csv')
  write.csv(
    data.frame(
      year = c(2019, 2019),
      doy = c(125, 165),
      CLAIV = c(0.7, 0.7),
      cut_only = c(0, 1)
    ),
    harv_file, row.names=FALSE
  )
  run_BASGRA(
    met_path, run_params, harv_file, fert_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE    
  )
  output_harv_cut <- read.csv(file.path(outfolder, 'output_basgra.csv'))

  expect_equal(sum(output_only_harv$YIELD), sum(output_harv_cut$YIELD))
  expect_equal(sum(output_only_harv$FHARVC), sum(output_harv_cut$FHARVC))
  expect_false(mean(output_only_harv$LAI) == mean(output_harv_cut$LAI))
})

test_that('changing CLAIV changes LAI and yield', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  fert_file <- file.path(outfolder, 'fertilize.csv')
  write_new_fert(fert_file, 'mineral')
  harv_file <- file.path(outfolder, 'harvest.csv')
  write.csv(
    data.frame(
      year = 2019,
      doy = 125,
      CLAIV = 1.0
    ),
    harv_file, row.names=FALSE
  )
  run_BASGRA(
    met_path, run_params, harv_file, fert_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_high <- read.csv(file.path(outfolder, 'output_basgra.csv'))

  write.csv(
    data.frame(
      year = 2019,
      doy = 125,
      CLAIV = 0.001
    ),
    harv_file, row.names=FALSE
  )
  run_BASGRA(
    met_path, run_params, harv_file, fert_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_low <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  
  expect_true(any(output_high$LAI > output_low$LAI))
  # for a single harvest, we expect a higher yield with lower CLAIV
  expect_gt(sum(output_low$FHARVC), sum(output_high$FHARVC))
})

test_that('invalid harvest file raises an error', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  fert_file <- file.path(outfolder, 'fertilize.csv')
  write_new_fert(fert_file, 'mineral')
  harv_file <- file.path(outfolder, 'harvest.csv')
  write.csv(
    data.frame(
      year = 2019,
      doy = 125,
      garbage = 1.0
    ),
    harv_file, row.names=FALSE
  )
  expect_error(
    run_BASGRA(
      met_path, run_params, harv_file, fert_file,
      start_date = '2019-01-01',
      end_date = '2019-12-31 23:00',
      outdir = outfolder,
      sitelat = 60.29,
      sitelon = 22.39 # match the test meteo data file
    )
  )
})

test_that('model produces some output', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)

  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(any(output$LAI > 0))
  expect_true(all(!is.na(output)))
})

test_that('Fertilizer C inputs are zeroed without Yasso', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  run_params <- setNames(df_params[,2], df_params[,1])
  
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fert_file_mineral <- file.path(outfolder, 'fert.mineral.csv')
  write_harv_fert(harvest_file, fert_file_mineral)
  fert_file_soluble <- file.path(outfolder, 'fert.soluble.csv')
  write_new_fert(fert_file_soluble, 'soluble')
  run_BASGRA(
    met_path, run_params, harvest_file, fert_file_soluble,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(all(output$FSOILAMDC == 0.0))
})

test_that('Fertilizer C inputs work consistently with Yasso', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 0
  
  run_params <- setNames(df_params[,2], df_params[,1])
  
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fert_file_mineral <- file.path(outfolder, 'fert.mineral.csv')
  write_harv_fert(harvest_file, fert_file_mineral)
  
  run_BASGRA(
    met_path, run_params, harvest_file, fert_file_mineral,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE    
  )
  output_mineral <- read.csv(file.path(outfolder, 'output_basgra.csv'))

  fert_file_soluble <- file.path(outfolder, 'fert.soluble.csv')
  write_new_fert(fert_file_soluble, 'soluble')
  run_BASGRA(
    met_path, run_params, harvest_file, fert_file_soluble,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_soluble <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  
  expect_true(all(output_soluble$CSOM_W >= output_mineral$CSOM_W))
  expect_true(any(output_soluble$CSOM_W > output_mineral$CSOM_W))
  expect_equal(sum(output_soluble$FSOILAMDC), 200.0)
  
  fert_file_compost <- file.path(outfolder, 'fert.compost.csv')
  write_new_fert(fert_file_compost, 'compost')
  run_BASGRA(
    met_path, run_params, harvest_file, fert_file_compost,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_compost <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(all(output_compost$CSOM_A >= output_mineral$CSOM_A))
  expect_true(any(output_compost$CSOM_A > output_mineral$CSOM_A))
  expect_true(all(output_compost$CSOM_N >= output_mineral$CSOM_N))
  expect_true(any(output_compost$CSOM_N > output_mineral$CSOM_N))

  expect_true(all(output_compost$CSOM_A >= output_soluble$CSOM_A))
  expect_true(any(output_compost$CSOM_A > output_soluble$CSOM_A))

  expect_equal(sum(output_compost$FSOILAMDC), 200.0)
  
  fert_file_bad <- file.path(outfolder, 'fert.bad.csv')
  write_new_fert(fert_file_bad, 'invalid')
  expect_error(
    run_BASGRA(
      met_path, run_params, harvest_file, fert_file_bad,
      start_date = '2019-01-01',
      end_date = '2019-12-31 23:00',
      outdir = outfolder,
      sitelat = 60.29,
      sitelon = 22.39 # match the test meteo data file
    )
  )
})


test_that('new fertilization file format matches the old', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  run_params_basic <- setNames(df_params[,2], df_params[,1])

  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 0
  run_params_yasso <- setNames(df_params[,2], df_params[,1])
  
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fert_file_old <- file.path(outfolder, 'fert.old.csv')
  write_harv_fert(harvest_file, fert_file_old)
  fert_file_mineral <- file.path(outfolder, 'fert.mineral.csv')
  write_new_fert(fert_file_mineral, 'mineral')

  run_BASGRA(
    met_path, run_params_basic, harvest_file, fert_file_old,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_old_fert <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  run_BASGRA(
    met_path, run_params_basic, harvest_file, fert_file_mineral,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_mineral <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_equal(output_old_fert, output_mineral)  
})

test_that('model shows no nitrogen limitation when run with use_nitrogen = 0', {
  met_path <- 'test.met'
  #df_params <- read.csv('BASGRA_params_no_nitrogen.csv')
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 0
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)

  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_equal(output$fNgrowth, rep(1.0, 365)) # if fNgrowth == 1 growth is not N-limited
})

## test_that('model crashes when run with use_yasso = 1 and use_nitrogen = 1', {
##   met_path <- 'test.met'
##   #df_params <- read.csv('BASGRA_params_yasso_use_nitrogen.csv')
##   df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
##   df_params[df_params[,1] == 'use_yasso', 2] <- 1
##   run_params <- setNames(df_params[,2], df_params[,1])
##   outfolder <- mktmpdir()
##   harvest_file <- file.path(outfolder, 'harvest.csv')
##   fertilize_file <- file.path(outfolder, 'fertilize.csv')
##   write_harv_fert(harvest_file, fertilize_file)

##   # Now run_BASGRA should crash with ERROR STOP from fortran. We can't test this by
##   # calling run_BASGRA directly (because R would exit) so instead we save it and load into
##   # a child process. I'm open for more elegant solutions...
##   run_wrapper <- function() {
##     dyn.load("../../src/PEcAn.BASGRA.so")
##     run_BASGRA(
##       met_path, run_params, harvest_file, fertilize_file,
##       start_date = '2019-01-01',
##       end_date = '2019-12-31 23:00',
##       outdir = outfolder,
##       sitelat = 60.29,
##       sitelon = 22.39 # match the test meteo data file
##     )
##   }
##   wrap_file <- file.path(outfolder, 'rwrp')
##   save(run_wrapper, run_BASGRA, file=wrap_file)
##   # trying to capture the output, bit of a kludge but maybe works on linux and mac
##   suppressWarnings(messages <- system(sprintf('echo "load(\\"%s\\"); run_wrapper()"|R --vanilla 2>&1', wrap_file), intern=TRUE))
##   expect_true(length(grep('ERROR STOP', messages)) > 0)
##   expect_false(file.exists(file.path(outfolder, 'output_basgra.csv'))) 
## })

test_that('model produces reasonable yasso-specific output when use_yasso = 1 and use_nitrogen = 0', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 0
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(all(output[,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] > 0))
  expect_true(all(output[,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] < 1e6))
  expect_true(all(!is.na(output)))
})

test_that('model produces reasonable yasso-specific output when use_yasso = 1 and use_nitrogen = 1', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 1
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(all(output[,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] > 0))
  expect_true(all(output[,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] < 1e6))
  expect_true(all(output[,'Nmineralisation'] > 0))
  expect_true(all(output[,'NMIN'] >= 0))
  expect_true(all(!is.na(output)))
})

test_that('NSH is positive', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 1
  df_params[df_params[,1] == 'NMIN0', 2] <- 0.0
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()

  df_fertilize <- data.frame(
    year = 2019,
    doy = 128,
    amount = 1e-6
  )
  no_fertilize_file <- file.path(outfolder, 'no-fertilize.csv')
  write.csv(df_fertilize, no_fertilize_file, row.names=FALSE)

  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, no_fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(all(output[, 'NSH'] > 0))
})


test_that('Netcdf output is consistent with the raw output for certain variables', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 0
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output.raw <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  nc <- ncdf4::nc_open(file.path(outfolder, '2019.nc'))

  fastc_nc <- ncdf4::ncvar_get(nc, 'fast_soil_pool_carbon_content')
  fastc_raw <- rowSums(output.raw[,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N')])
  expect_equal(as.vector(fastc_nc), as.vector(fastc_raw*1e-3)) # g vs kg

  slowc_nc <- ncdf4::ncvar_get(nc, 'slow_soil_pool_carbon_content')
  slowc_raw <- output.raw[,'CSOM_H']
  expect_equal(as.vector(slowc_nc), as.vector(slowc_raw*1e-3))

  totc_nc <-  ncdf4::ncvar_get(nc, 'TotSoilCarb')
  totc_raw <- rowSums(output.raw[, c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N', 'CSOM_H')])
  expect_equal(as.vector(totc_nc), as.vector(totc_raw*1e-3))
})

test_that('The yasso_rate_pc parameter has a reasonable effect', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 0
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))

  run_params_mod <- run_params
  run_params_mod['yasso_rate_pc'] <- -1.0 # the component has negative values so this speeds up the decomp
  run_BASGRA(
    met_path, run_params_mod, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output_mod <- read.csv(file.path(outfolder, 'output_basgra.csv'))

  # One could have thought that output_mod should have a higher Rsoil. But it doesn't
  # always, because also the initial state changes. Now we'll just test the
  # initialization.
  expect_gt(output_mod[1, 'CSOM_H'], output[1, 'CSOM_H'])
  awen = c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N')
  expect_lt(sum(output_mod[1, awen]), sum(output[1, awen]))
})

test_that('The yasso ICs are handled consistently', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 1
  
  run_params <- setNames(df_params[,2], df_params[,1])
  yasso_state <- c(
    849, 95, 51, 1092, 14298, 1536
  )
  run_params[c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] <- yasso_state
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  output_state = as.numeric(output[1,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')])
  expect_equal(
    output_state,
    yasso_state,
    tolerance=2 # needed because the end of time step state is in output
  )
})

test_that('The yasso ICs are ignored if negative', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 1
  
  run_params <- setNames(df_params[,2], df_params[,1])
  yasso_state <- rep(-1, 6)
  run_params[c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] <- yasso_state
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  output_state = as.numeric(output[1,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')])
  expect_gt(sum(output_state), 1000)
  # check that met values are reasonable from the defaults
  output_met <- as.numeric(output[1,c('TEMPR30', 'PRECIP30')])
  expect_equal(output_met[1], 10, tolerance=20)
  expect_equal(output_met[2], 1, tolerance=5)
})

test_that('The smoothed tempr and precip are read from params', {
  met_path <- 'test.met'
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'))
  df_params[df_params[,1] == 'use_yasso', 2] <- 1
  df_params[df_params[,1] == 'use_nitrogen', 2] <- 0
  
  run_params <- setNames(df_params[,2], df_params[,1])
  met_values = c(-1e5, 1e5)
  run_params[c('TEMPR30', 'PRECIP30')] <- met_values
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)
  run_BASGRA(
    met_path, run_params, harvest_file, fertilize_file,
    start_date = '2019-01-01',
    end_date = '2019-12-31 23:00',
    outdir = outfolder,
    sitelat = 60.29,
    sitelon = 22.39, # match the test meteo data file
    write_raw_output = TRUE
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  output_values <- as.numeric(output[1,c('TEMPR30', 'PRECIP30')])
  expect_lt(output_values[1], -1e2)
  expect_gt(output_values[2], 1e2)
})
