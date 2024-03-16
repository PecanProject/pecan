context("write.config")

outfolder <- tempfile()
setup(dir.create(outfolder, showWarnings = FALSE))
teardown(unlink(outfolder, recursive = TRUE))

basesettings <- list(
  rundir = outfolder,
  host = list(
    rundir=outfolder,
    outdir=outfolder
  ),
  run = list(
    site = list(
      lat = 58.0, # must match the item in BASGRA_params.csv
      lon = 25.0
    ),
    inputs = list(
      met = list(
        path = 'dummy'
      ),
      harvest = list(
        path = 'dummy'
      ),
      fertilize = list(
        path = 'dummy'
      )
    ),
    start.date = 'start_date',
    end.date = 'end_date'
  ),
  model = list()
  
)


create_job_template <- function(content) {
  # write.config has a handy feature to override the default job.template.
  # We'll use this for testing individual items in it.
  filename <- file.path(outfolder, 'job.template')
  write(content, filename)
  filename
}

test_that('write.config retrieves default parameters from the file', {
  jobtemplate <- create_job_template('@RUN_PARAMS@')
  settings <- basesettings
  settings$model$jobtemplate <- jobtemplate
  trait.values = list(list()) # no traits given
  params.from.file <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'), col.names=c('name', 'value'))
  default <- NULL
  run.id <- 9999
  dir.create(file.path(outfolder, run.id))
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  job.file <- file.path(outfolder, run.id, 'job.sh')
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  expect_equal(params.from.file$name, names(param.vector))
  # a few of the parameters are redundant and get reset byt write.config based on the other parameters.
  # these parameters need to be set consistently in the default parameter file or otherwise this test fails.
  expect_equal(params.from.file$value, setNames(param.vector, NULL), tolerance=1e-4)
})

test_that('default param path from settings overrides the global default', {
  jobtemplate <- create_job_template('@RUN_PARAMS@')
  settings <- basesettings
  settings$model$jobtemplate <- jobtemplate
  trait.values = list(list()) # no traits given
  param_path <- file.path(outfolder, 'modified.defaults.csv')
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'), col.names=c('name', 'value'))
  df_params[df_params$name == 'NMIN0', 'value'] <- -9991
  write.csv(df_params, param_path, row.names=FALSE)
  settings$run$inputs$defaults$path <- param_path
  run.id = 9998
  dir.create(file.path(outfolder, run.id))
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  job.file <- file.path(outfolder, run.id, 'job.sh')
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  expect_equal(setNames(param.vector['NMIN0'], NULL), -9991)
})

test_that('write.config modifies some trait values', {
  jobtemplate <- create_job_template('@RUN_PARAMS@')
  settings <- basesettings
  settings$model$jobtemplate <- jobtemplate
  trait.values = list(
    list(
      c2n_fineroot = 50.0,
      leaf_width = 6.1
    )
  )
  default <- NULL
  run.id <- 9999
  dir.create(file.path(outfolder, run.id), showWarnings = FALSE)
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  job.file <- file.path(outfolder, run.id, 'job.sh')
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  expect_equal(param.vector['NCR'], c(NCR = 0.02))
  expect_equal(param.vector['LFWIDV'], c(LFWIDV = 6.1e-3)) # in meters
})

test_that('the force column in defaulfs.csv keep the default parameters even if pecan provides trait values', {
  jobtemplate <- create_job_template('@RUN_PARAMS@')
  settings <- basesettings
  settings$model$jobtemplate <- jobtemplate
  trait.values = list(
    list(
      c2n_fineroot = 50.0,
      leaf_width = 6.1
    )
  )
  param_path <- file.path(outfolder, 'modified.defaults.csv')
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'), col.names=c('name', 'value'))
  df_params$force = rep(FALSE, nrow(df_params))
  df_params[df_params$name == 'LFWIDV', 'force'] <- TRUE
  leaf_width_value <- df_params[df_params$name=='LFWIDV', 'value']
  write.csv(df_params, param_path, row.names=FALSE)
  settings$run$inputs$defaults$path <- param_path
  run.id = 9998
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  job.file <- file.path(outfolder, run.id, 'job.sh')
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  expect_equal(param.vector['LFWIDV'], c(LFWIDV=leaf_width_value)) # deafult value
  expect_equal(param.vector['NCR'], c(NCR=0.02)) # trait value
})

test_that('the force column values are interpreted flexibly', {
  jobtemplate <- create_job_template('@RUN_PARAMS@')
  settings <- basesettings
  settings$model$jobtemplate <- jobtemplate
  param_path <- file.path(outfolder, 'modified.defaults.csv')
  df_params <- read.csv(system.file('BASGRA_params.csv', package='PEcAn.BASGRA'), col.names=c('name', 'value'))
  settings$run$inputs$defaults$path <- param_path
  run.id = 9998
  flagvalue <-  -999999
  trait.values <- list(list(leaf_width = 6.1))
  job.file <- file.path(outfolder, run.id, 'job.sh')
  df_params[,2] <-flagvalue
  
  df_params$force = rep('True', nrow(df_params))
  write.csv(df_params, param_path, row.names=FALSE)
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- setNames(eval(parse(text=content)), NULL)
  expect_equal(length(param.vector), nrow(df_params))
  expect_equal(param.vector, rep(flagvalue, length(param.vector)))

  df_params$force = rep(1, nrow(df_params))
  write.csv(df_params, param_path, row.names=FALSE)
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- setNames(eval(parse(text=content)), NULL)
  expect_equal(length(param.vector), nrow(df_params))
  expect_true(all(param.vector == flagvalue))

  df_params$force = rep(0, nrow(df_params))
  write.csv(df_params, param_path, row.names=FALSE)
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  expect_equal(length(param.vector), nrow(df_params))
  expect_equal(param.vector['LFWIDV'], c(LFWIDV=6.1*1e-3)) # in mm
})

test_that('YASSO pool ICs pass thru (list)', {
  jobtemplate <- create_job_template('@RUN_PARAMS@')
  settings <- basesettings
  settings$model$jobtemplate <- jobtemplate
  default <- NULL
  run.id <- 9999
  dir.create(file.path(outfolder, run.id), showWarnings = FALSE)
  load(system.file('last_vals_basgra.Rdata', package='PEcAn.BASGRA'))
  ic_list <-  list(
    CSOM_A = 1,
    CSOM_W = 2,
    CSOM_E = 3,
    CSOM_N = 4,
    CSOM_H = 5,
    NSOM = 6,
    TEMPR30 = 7,
    PRECIP30 = 8,
    test_vals = last_vals
  )
  write.config.BASGRA(defaults, trait.values=list(), settings=settings, run.id=run.id, IC=ic_list)
  job.file <- file.path(outfolder, run.id, 'job.sh')
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  state <- param.vector[c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N', 'CSOM_H', 'NSOM', 'TEMPR30', 'PRECIP30')]
  expect_equal(setNames(state, NULL), seq(8))
})

test_that('YASSO pool ICs pass thru (file)', {
  jobtemplate <- create_job_template('@RUN_PARAMS@')
  settings <- basesettings
  settings$model$jobtemplate <- jobtemplate
  settings$run$inputs$poolinitcond = list(
    path='ic_with_yasso_pools_and_met.nc'
  )
  default <- NULL
  run.id <- 9999
  dir.create(file.path(outfolder, run.id), showWarnings = FALSE)
  write.config.BASGRA(defaults, trait.values=list(), settings=settings, run.id=run.id)
  job.file <- file.path(outfolder, run.id, 'job.sh')
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  state <- param.vector[c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N', 'CSOM_H', 'NSOM', 'TEMPR30', 'PRECIP30')]
  correct_state <- c(
    1011.55245115532, 118.194058863007, 62.5131705827862, 1153.2435021838, 14274.4980088834, 1549.22075041662,
    12.0709309808298, 1.28496155077734
  )
  expect_equal(setNames(state, NULL), correct_state)
})



