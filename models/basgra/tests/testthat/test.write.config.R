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

