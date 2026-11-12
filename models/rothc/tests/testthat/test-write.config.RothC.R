test_that("write.config.RothC writes opt_RMmoist and opt_SMDbare as separate flags", {
  pth <- withr::local_tempdir()
  dir.create(file.path(pth, "run", "r1"), recursive = TRUE)
  dir.create(file.path(pth, "out", "r1"), recursive = TRUE)
  met <- file.path(pth, "met.dat")
  data.frame(
    year = 2020, month = 1:12, Tmp_C = 10, Rain_mm = 50, Evap_mm = 0
  ) |>
    utils::write.table(met, row.names = FALSE, quote = FALSE)

  fake_soil <- function(...) {
    dat <- data.frame(
      depth = c(0.1, 0.2, 0.3, 0.4), # m
      fraction_of_clay_in_soil = 0.3,
      fraction_of_silt_in_soil = 0.3,
      soil_bulk_density = 1350,
      soil_organic_carbon_stock = 2
    )
    attr(dat, "units") <- c(
      depth = "meters",
      fraction_of_clay_in_soil = "1",
      fraction_of_silt_in_soil = "1",
      soil_bulk_density = "kg m-3",
      soil_organic_carbon_stock = "kg m-2"
    )
    dat
  }

  run_config <- function(model_opts) {
    s <- list(
      rundir = file.path(pth, "run"),
      host = list(rundir = file.path(pth, "run"), outdir = file.path(pth, "out")),
      model = c(list(binary = "rothc"), model_opts),
      run = list(
        start.date = "2020-01-01",
        end.date = "2020-12-31",
        site = list(
          lat = 1, lon = 1, met.start = "2020-01-01", met.end = "2020-12-31"
        ),
        inputs = list(
          met = list(path = met),
          soil_physics = list(path = "ignored")
        )
      )
    )
    with_mocked_bindings(
      write.config.RothC(NULL, NULL, s, "r1"),
      netcdf2df = fake_soil
    )
    lines <- readLines(file.path(pth, "run", "r1", "RothC_input.dat"))
    lines <- lines[!grepl("^#", lines)]
    list(
      flags = scan(text = lines[[1]], quiet = TRUE),
      soil = scan(text = lines[[2]], quiet = TRUE)
    )
  }

  res <- run_config(list(opt_RMmoist = 2, opt_SMDbare = 1))
  expect_equal(res$flags, c(2, 1))
  res <- run_config(list(opt_RMmoist = 1, opt_SMDbare = 2))
  expect_equal(res$flags, c(1, 2))
})
