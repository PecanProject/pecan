test_that("`site.pft.linkage` gives error for empty or incomplete lookup-table(LUT)", {
  settings <- list()
  expect_error(
    site.pft.linkage(settings, NULL),
    paste0(
      "Your look up table should have two columns of site and pft",
      " with site ids under site column and pft names under pft column."
    )
  )
  
  LUT <- data.frame(h1 = c("1000025731", "1000025731"))

  expect_error(
    site.pft.linkage(settings, LUT),
    paste0(
      "Your look up table should have two columns of site and pft",
      " with site ids under site column and pft names under pft column."
    )
  )
})

test_that("`site.pft.linkage` does not add site pft name when site id is not specified", {
  settings <- list(
    run = list(
      site = list(
        name = "test"
      )
    )
  )
  LUT <- data.frame(
    site = c("1000025731", "1000025732"),
    pft = c("temperate.broadleaf.deciduous1", "temperate.needleleaf.evergreen")
  )
  new_settings <- site.pft.linkage(settings, LUT)
  expect_equal(
    new_settings$run$site$site.pft,
    NULL
  )
})

test_that("`site.pft.linkage` able to add site pft name if id is specified and is a part of the lookup-table(LUT)", {
  settings <- list(
    run = list(
      site = list(
        id = "1000025731"
      )
    )
  )
  LUT <- data.frame(
    site = c("1000025731", "1000025732"),
    pft = c("temperate.broadleaf.deciduous1", "temperate.needleleaf.evergreen")
  )
  
  new_settings <- site.pft.linkage(settings, LUT)
  expect_equal(
    new_settings$run$site$site.pft$pft.name,
    "temperate.broadleaf.deciduous1"
  )
})