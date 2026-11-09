
# Mock a model write.configs function to avoid model-specific errors
make_write_env <- function() {
  e <- new.env()
  e$write.configs.SIPNET <- function(...) TRUE

  e
}

# 1. One input, no samples → should pass
test_that("1 input, no samples: passes", {
  settings <- list(run = list(inputs = list(input = list(path = "IC1"))))
  ensemble.samples <- NULL
  defaults <- list()
  
  withr::with_environment(
    make_write_env(),
    expect_silent(write.ensemble.configs(
      defaults = defaults,
      ensemble.samples = ensemble.samples,
      settings = settings,
      model = "SIPNET",
      write.to.db = FALSE
    ))
  )
})



test_that("no input error", {
  settings <- list(run = list(inputs = list(input = NULL)))
  ensemble.samples <- NULL
  defaults <- list()
  
  # Capture logger message
 withr::with_environment(
    make_write_env(),
     expect_silent(write.ensemble.configs(
      defaults = defaults,
      ensemble.samples = ensemble.samples,
      settings = settings,
      model = "SIPNET",
      write.to.db = FALSE
    ))
  )
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
  
  input_design <- data.frame(
    met = c(1,7,3,5,6),
    poolinitcond = c(43,48,4,21,39),
    param = 1:5 # Or your actual param column
  )
  
  # Run test - should create directories and configs
  result <- expect_silent(
     withr::with_environment(
      make_write_env(),
      write.ensemble.configs(
        defaults = defaults,
        ensemble.size = 5,
        ensemble.samples = ensemble.samples,
        settings = settings,
        model = "SIPNET",
        write.to.db = FALSE,
        input_design = input_design
      )
    )
  )
  
  # Verify outputs
  expect_type(result, "list")
  expect_named(result, c("runs", "ensemble.id", "samples", "manifest"))
  expect_equal(nrow(result$runs), settings$ensemble$size)
})


test_that("get.ensemble.samples", {
  pft_samp <- list(
    a = data.frame(aa = c(1, 5, 2, 8, 2)),
    b = data.frame(aa = c(20, 44, 53, 70, 65), bb = c(1, 5, 3, 4, 2))
  )
  env_samp <- list()

  res <- get.ensemble.samples(2, pft_samp, env_samp)
  res_idx <- attr(res, "sampled.indices")
  expect_type(res, "list")
  expect_named(res, c("a", "b", "env"))
  expect_true(all(sapply(res, inherits, "data.frame")))
  expect_equal(lengths(res), c(a = 1, b = 2, env = 0))
  expect_equal(sapply(res, nrow), c(a = 2, b = 2, env = 2))

  expect_type(res_idx, "list")
  expect_named(res_idx, c("a", "b", "env"))
  expect_true(all(sapply(res_idx, inherits, "matrix")))
  expect_equal(sapply(res_idx, ncol), c(a = 1, b = 2, env = 0))
  expect_equal(sapply(res_idx, nrow), c(a = 2, b = 2, env = 2))
  # pft_samp has 5 rows -> all indices <= 5
  expect_true(all(sapply(res_idx, \(mat)all(mat %in% 1:5))))
})
