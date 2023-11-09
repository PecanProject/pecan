test_that("`createSitegroupMultiSettings` able to create a MultiSettings object containing (identical) run blocks for different sites in a site group", {
  templateSettings <- Settings(
    list(
      run = list(
        start.date = "2015-01-01",
        end.date = "2015-12-31",
        inputs = list("a", "b")
      )
    )
  )
  siteIds <- list(
    site_id = list("1000025731", "1000025732")
  )
  mockery::stub(
    createSitegroupMultiSettings, 
    'PEcAn.DB::db.query',
    siteIds
  )
  
  # without specifying nSite
  multi_site_settings <- createSitegroupMultiSettings(
    templateSettings = templateSettings, 
    sitegroupId = 10000, 
    params = NULL
  )
  for(i in seq_along(multi_site_settings)) {
    expect_equal(multi_site_settings[[i]]$run$site$id, siteIds$site_id[[i]])
  }
  expect_equal(length(multi_site_settings), length(siteIds$site_id))
  

  # with nSite specified
  multi_site_settings <- createSitegroupMultiSettings(
    templateSettings = templateSettings, 
    sitegroupId = 10000, 
    nSite = 1,
    params = NULL
  )
  expect_equal(length(multi_site_settings), 1)
})

test_that("`createMultiSiteSettings` able to create a MultiSettings object containing (identical) run blocks for different sites", {
  templateSettings <- Settings(
    list(
      run = list(
        start.date = "2015-01-01",
        end.date = "2015-12-31",
        inputs = list("a", "b")
      )
    )
  )
  siteIds <- c("1000025731", "1000025732")
  multi_site_settings <- createMultiSiteSettings(templateSettings, siteIds)
  for (i in seq_along(multi_site_settings)) {
    expect_equal(multi_site_settings[[i]]$run$site$id, siteIds[i])
    expect_equal(multi_site_settings[[i]]$run$site$met.start, templateSettings$run$start.date)
    expect_equal(multi_site_settings[[i]]$run$site$met.end, templateSettings$run$end.date)
    expect_equal(multi_site_settings[[i]]$run$start.date, templateSettings$run$start.date)
    expect_equal(multi_site_settings[[i]]$run$end.date, templateSettings$run$end.date)
    expect_equal(multi_site_settings[[i]]$run$inputs, templateSettings$run$inputs)
  }
})

test_that("`getRunSettings` able to build correct run settings for a given site id", {
  templateSettings <- list(
    run = list(
      start.date = "2015-01-01",
      end.date = "2015-12-31",
      inputs = list("a", "b")
    )
  )
  siteId <- "1000025731"
  run_settings <- getRunSettings(templateSettings, siteId)

  expect_equal(run_settings$site$id, siteId)
  expect_equal(run_settings$site$met.start, templateSettings$run$start.date)
  expect_equal(run_settings$site$met.end, templateSettings$run$end.date)
  expect_equal(run_settings$start.date, templateSettings$run$start.date)
  expect_equal(run_settings$end.date, templateSettings$run$end.date)
  expect_equal(run_settings$inputs, templateSettings$run$inputs)
})

test_that("`setOutDir` function sets main output directory and nulls out the others", {
  settings <- list(
    outdir = NULL,
    rundir = "old_rundir",
    modeloutdir = "old_modeloutdir",
    host = list(
      rundir = "old_host_rundir",
      outdir = "old_host_outdir",
      modeloutdir = "old_host_modeloutdir"
    ),
    pfts = list(
      list(outdir = "old_outdir1"),
      list(outdir = "old_outdir2")
    )
  )

  outDir <- "new_outdir"
  updated_settings <- setOutDir(settings, outDir)

  expect_equal(updated_settings$outdir, outDir)
  expect_equal(updated_settings$rundir, NULL)
  expect_equal(updated_settings$modeloutdir, NULL)
  expect_equal(updated_settings$host$rundir, NULL)
  expect_equal(updated_settings$host$outdir, NULL)
  expect_equal(updated_settings$host$modeloutdir, NULL)
  for (j in seq_along(updated_settings$pfts)) {
    expect_equal(updated_settings$pfts[[j]]$outdir, NULL)
  }
})

test_that("`setDates` function sets start and end dates correctly", {
  settings <- list(
    run = list(
      start.date = NULL,
      end.date = NULL
    ),
    ensemble = list(
      start.year = NULL,
      end.year = NULL
    ),
    sensitivity.analysis = list(
      start.year = NULL,
      end.year = NULL
    )
  )
  
  startDate <- "2023-01-01"
  endDate <- "2023-12-31"
  updated_settings <- setDates(settings, startDate, endDate)
  
  expect_equal(updated_settings$run$start.date, startDate)
  expect_equal(updated_settings$run$end.date, endDate)
  expect_equal(updated_settings$ensemble$start.year, lubridate::year(startDate))
  expect_equal(updated_settings$ensemble$end.year, lubridate::year(endDate))
  expect_equal(updated_settings$sensitivity.analysis$start.year, lubridate::year(startDate))
  expect_equal(updated_settings$sensitivity.analysis$end.year, lubridate::year(endDate))
})
