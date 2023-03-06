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
    amount = 6.5
  )
  fertilize_file <- path_fert #
  write.csv(df_fertilize, fertilize_file, row.names=FALSE)

}

test_that('model produces some output', {
  met_path <- 'test.met'
  df_params <- read.csv('BASGRA_params.csv')
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
    sitelon = 22.39 # match the test meteo data file
  )
  
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(any(output$LAI > 0))
  expect_true(all(!is.na(output)))
})

test_that('model shows no nitrogen limitation when run with use_nitrogen = 0', {
  met_path <- 'test.met'
  df_params <- read.csv('BASGRA_params_no_nitrogen.csv')
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
    sitelon = 22.39 # match the test meteo data file
  )
  
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_equal(output$fNgrowth, rep(1.0, 365)) # if fNgrowth == 1 growth is not N-limited
})

test_that('model crashes when run with use_yasso = 1 and use_nitrogen = 1', {
  met_path <- 'test.met'
  df_params <- read.csv('BASGRA_params_yasso_use_nitrogen.csv')
  run_params <- setNames(df_params[,2], df_params[,1])
  outfolder <- mktmpdir()
  harvest_file <- file.path(outfolder, 'harvest.csv')
  fertilize_file <- file.path(outfolder, 'fertilize.csv')
  write_harv_fert(harvest_file, fertilize_file)

  # Now run_BASGRA should crash with ERROR STOP from fortran. We can't test this by
  # calling run_BASGRA directly (because R would exit) so instead we save it and load into
  # a child process. I'm open for more elegant solutions...
  run_wrapper <- function() {
    dyn.load("../../src/PEcAn.BASGRA.so")
    run_BASGRA(
      met_path, run_params, harvest_file, fertilize_file,
      start_date = '2019-01-01',
      end_date = '2019-12-31 23:00',
      outdir = outfolder,
      sitelat = 60.29,
      sitelon = 22.39 # match the test meteo data file
    )
  }
  wrap_file <- file.path(outfolder, 'rwrp')
  save(run_wrapper, run_BASGRA, file=wrap_file)
  # trying to capture the output, bit of a kludge but maybe works on linux and mac
  suppressWarnings(messages <- system(sprintf('echo "load(\\"%s\\"); run_wrapper()"|R --vanilla 2>&1', wrap_file), intern=TRUE))
  expect_true(length(grep('ERROR STOP', messages)) > 0)
  expect_false(file.exists(file.path(outfolder, 'output_basgra.csv'))) 
})

test_that('model produces reasonable yasso-specific output when use_yasso = 1 and use_nitrogen = 0', {
  met_path <- 'test.met'
  df_params <- read.csv('BASGRA_params_yasso.csv')
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
    sitelon = 22.39 # match the test meteo data file
  )
  output <- read.csv(file.path(outfolder, 'output_basgra.csv'))
  expect_true(all(output[,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] > 0))
  expect_true(all(output[,c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N','CSOM_H','NSOM')] < 1e6))
  expect_true(all(!is.na(output)))
})

