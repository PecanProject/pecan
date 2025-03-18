test_that("setEnsemblePaths sets paths across sites", {
  template_settings <- Settings(
    list(
      run = list(
        start.date = "2015-01-01",
        end.date = "2015-12-31",
        inputs = list("a", "b")
      )
    )
  )
  siteids <- c("s1", "s2")
  settings <- createMultiSiteSettings(template_settings, siteids)
  with_paths <- setEnsemblePaths(settings, n_reps = 3)

  # only inputs should have changed
  expect_identical(
    lapply(settings$run, function(x) x[names(x) != "inputs"]),
    lapply(with_paths$run, function(x) x[names(x) != "inputs"])
  )

  # n and id handled right
  for (i in seq_along(with_paths)) {
    expect_named(
      with_paths$run[[i]]$inputs$met$path,
      c("path1", "path2", "path3")
    )
    expect_match(
      unlist(with_paths$run[[i]]$inputs$met$path),
      paste0("./", siteids[[i]], "/3.nc"),
      fixed = TRUE,
      all = FALSE
    )
  }

  # only known input types accepted
  expect_error(
    setEnsemblePaths(settings, 3, input_type = "fake"),
    ".arg. should be one of .met., .poolinitcond., .soilinitcond."
  )

  # extra vars passed through to glue
  with_extras <- setEnsemblePaths(
    settings,
    n_reps = 3,
    foo = "bar", ext = "txt",
    path_template = "../{id}_{foo}{n}.{ext}"
  )
  expect_match(with_extras$run$site.s1$inputs$met$path$path2, "s1_bar2.txt")
})
