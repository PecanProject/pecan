context("input validation for write.ensemble.configs")

# Mock a model write.configs function to avoid model-specific errors
write.configs.SIPNET <- function(...) TRUE

# Helper: make input with correct structure
make_input_sets <- function(paths) {
  lapply(paths, function(p) list(path = p))
}

# Helper: make ensemble.samples with the correct structure
make_samples <- function(samples) {
  lapply(paths, function(p) list(path = p))
}

# 1. One input, no samples → should pass
test_that("1 input, no samples: passes", {
  settings <- list(run = list(inputs = list(input = list(path = "IC1"))))
  ensemble.samples <- NULL
  defaults <- list()
  
  expect_silent(write.ensemble.configs(
    defaults = defaults,
    ensemble.samples = ensemble.samples,
    settings = settings,
    model = "SIPNET",
    write.to.db = FALSE
  ))
})



test_that("no input error", {
  settings <- list(run = list(inputs = list(input = NULL)))
  ensemble.samples <- NULL
  defaults <- list()
  
  # Capture logger message
  expect_silent(write.ensemble.configs(
    defaults = defaults,
    ensemble.samples = ensemble.samples,
    settings = settings,
    model = "SIPNET",
    write.to.db = FALSE
  ))
})




  



test_that("multiple inputs and multiple samples", {
  # Mock the SIPNET config writer
  mockery::stub(write.ensemble.configs, "write.config.SIPNET", function(...) TRUE)
  
  # Create temp directories
  temp_rundir <- tempfile()
  temp_modeloutdir <- tempfile()
  dir.create(temp_rundir)
  dir.create(temp_modeloutdir)
  on.exit({
    unlink(temp_rundir, recursive = TRUE)
    unlink(temp_modeloutdir, recursive = TRUE)
  }, add = TRUE)
  
  # Complete settings
  settings <- list(
    run = list(
      inputs = list(input = list(path = "IC1")),
      site = list(id = 1, name = "Test Site"),
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      outdir = temp_modeloutdir
    ),
    ensemble = list(size = 5),
    database = NULL,
    rundir = temp_rundir,
    modeloutdir = temp_modeloutdir,
    host = list(
      rundir = temp_rundir,
      outdir = temp_modeloutdir
    ),
    model = list(id = "SIPNET", type = "SIPNET"),
    pfts = list(
      list(name = "temperate", 
           constants = list(1),
           posteriorid = 1)
    )
  )
  
  # Sample parameters
  ensemble.samples <- list(
    temperate = data.frame(
      SLA = c(15.2, 16.8, 14.7, 18.1, 17.5),
      Vm0 = c(45.0, 50.3, 47.8, 49.1, 51.0)
    )
  )
  
  # Default PFT settings
  defaults <- list(
    list(
      name = "temperate",
      constants = list(1),
      posteriorid = 1
    )
  )
  
  # Run test - should create directories and configs
  result <- expect_silent(
    write.ensemble.configs(
      defaults = defaults,
      ensemble.samples = ensemble.samples,
      settings = settings,
      model = "SIPNET",
      write.to.db = FALSE
    )
  )
  
  # Verify outputs
  expect_type(result, "list")
  expect_named(result, c("runs", "ensemble.id", "samples"))
  expect_equal(nrow(result$runs), settings$ensemble$size)
})



  


