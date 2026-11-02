test_that("`do_conversions` passes overwrite arguments on to every site of a MultiSettings", {
  withr::with_tempdir({
    seen_overwrite <- c()
    testthat::local_mocked_bindings(
      met.process = function(..., overwrite = FALSE) {
        seen_overwrite <<- c(seen_overwrite, overwrite)
        list(path = "converted.clim")
      },
      .package = "PEcAn.data.atmosphere"
    )
    make_site <- function(id) {
      dir.create(file.path(getwd(), paste0("site", id)))
      PEcAn.settings::Settings(list(
        host = list(name = "test", folder = "test"),
        outdir = file.path(getwd(), paste0("site", id)),
        run = list(site = list(id = id), inputs = list(met = list(id = id)))
      ))
    }
    settings <- PEcAn.settings::MultiSettings(make_site(1), make_site(2))

    res <- do_conversions(settings, overwrite.met = TRUE)

    expect_equal(seen_overwrite, c(TRUE, TRUE))
    expect_true(PEcAn.settings::is.MultiSettings(res))
  })
})

test_that("`do_conversions` does not overwrite by default for a MultiSettings", {
  withr::with_tempdir({
    seen_overwrite <- c()
    testthat::local_mocked_bindings(
      met.process = function(..., overwrite = FALSE) {
        seen_overwrite <<- c(seen_overwrite, overwrite)
        list(path = "converted.clim")
      },
      .package = "PEcAn.data.atmosphere"
    )
    dir.create(file.path(getwd(), "site1"))
    settings <- PEcAn.settings::MultiSettings(
      PEcAn.settings::Settings(list(
        host = list(name = "test", folder = "test"),
        outdir = file.path(getwd(), "site1"),
        run = list(site = list(id = 1), inputs = list(met = list(id = 1)))
      ))
    )
    do_conversions(settings)
    expect_equal(seen_overwrite, FALSE)
  })
})
