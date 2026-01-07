test_that("model2netcdf", {
  outdir <- withr::local_tempdir()
  file.copy("data/month_results.out", outdir)
  ref_output <- read.csv("data/month_results.out")


  # happy path
  expect_silent(
    model2netcdf.RothC(
      outdir = outdir,
      sitelat = 0,
      sitelon = 0,
      start_date = "1939-01-01",
      end_date = "1941-12-31"
    )
  )
  expected_files <- file.path(outdir, c("1939.nc", "1940.nc", "1941.nc"))
  expect_true(all(file.exists(expected_files)))

  pred_soc <- PEcAn.utils::read.output(
    ncfiles = expected_files,
    variables = "TotSoilCarb",
    dataframe = TRUE,
    verbose = FALSE,
    print_summary = FALSE
  )
  expect_equal(nrow(pred_soc), 3 * 12)
  expect_true(min(pred_soc$posix) == as.Date("1939-01-15"))
  expect_true(max(pred_soc$posix) == as.Date("1941-12-15"))
  expect_equal(
    pred_soc$TotSoilCarb,
    ref_output |>
      dplyr::filter(.data$Year > 1) |>
      dplyr::pull("SOC_t_C_ha") |>
      PEcAn.utils::ud_convert("t C ha-1", "kg C m-2")
  )


  # date ranges enforced
  expect_error(
    model2netcdf.RothC(
      outdir = outdir,
      sitelat = 0,
      sitelon = 0,
      start_date = "1939-01-01",
      end_date = "1945-12-31"
    ),
    "end_date <= max(res$month_ended) is not TRUE",
    fixed = TRUE
  )
})
