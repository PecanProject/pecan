test_that("setEnsemblePaths sets paths across sites", {
  template_settings <- Settings(
    list(
      run = list(
        start.date = "2015-01-01",
        end.date = "2015-12-31",
        inputs = list(
          met = list(id = "a"),
          soil_physics = list(name = "b", path = list("overwritten"))
        )
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

  # input block is added if not present in original,
  # without changing length of existing inputs
  add_in <- setEnsemblePaths(with_paths, 2, input_type = "novel")
  for (i in seq_along(add_in)) {
    expect_named(
      add_in$run[[i]]$inputs,
      c("met", "soil_physics", "novel")
    )
    expect_length(add_in$run[[i]]$inputs$novel$path, 2)
    expect_length(add_in$run[[i]]$inputs$met$path, 3)
  }

  # extra vars passed through to glue
  with_extras <- setEnsemblePaths(
    settings,
    n_reps = 3,
    foo = "bar", ext = "txt",
    path_template = "../{id}_{foo}{n}.{ext}"
  )
  expect_match(with_extras$run$site.s1$inputs$met$path$path2, "s1_bar2.txt")

  # Ensembles of 1 return bare path not list
  one_path <- setEnsemblePaths(settings, n_reps = 1)
  two_path <- setEnsemblePaths(settings, n_reps = 2)
  expect_identical(
    one_path$run$site.s1$inputs$met$path,
    two_path$run$site.s1$inputs$met$path$path1
  )
})
