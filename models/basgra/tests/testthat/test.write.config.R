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
  #print(param.vector)
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
  print(df_params)
  leaf_width_value <- df_params[df_params$name=='LFWIDV', 'value']
  print(leaf_width_value)
  write.csv(df_params, param_path, row.names=FALSE)
  settings$run$inputs$defaults$path <- param_path
  run.id = 9998
  write.config.BASGRA(defaults, trait.values, settings, run.id)
  job.file <- file.path(outfolder, run.id, 'job.sh')
  content <- paste(readLines(job.file), collapse='\n')
  param.vector <- eval(parse(text=content))
  print(names(param.vector))
  expect_equal(param.vector['LFWIDV'], c(LFWIDV=leaf_width_value)) # deafult value
  expect_equal(param.vector['NCR'], c(NCR=0.02)) # trait value
})
