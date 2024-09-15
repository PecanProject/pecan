test_that("`addSecrets` returns settings without updating them when `~/.pecan.xml` does not exist", {
  settings <- list()
  mockery::stub(addSecrets, 'file.exists', FALSE)
  expect_equal(addSecrets(settings), settings)
})

test_that("`addSecrets` returns settings without updating them when force is FALSE and secrets have already been added", {
  settings <- list(
    settings.info = list(
      secrets.added = TRUE
    )
  )
  mockery::stub(addSecrets, 'file.exists', TRUE)
  expect_equal(addSecrets(settings, force = FALSE), settings)
})

test_that("`addSecrets` adds secret settings when force is TRUE and secrets have already been added", {
  settings <- list(
    settings.info = list(
      secrets.added = TRUE
    )
  )

  mocked_xmlToList_result <- list(
      database = list(
        section = list(
          name = "pecan",
          password = "pecan"
        )
      )
    )
  mockery::stub(addSecrets, 'file.exists', TRUE)
  mockery::stub(addSecrets, 'xmlToList', mocked_xmlToList_result)
  updated_settings <- addSecrets(settings, force = TRUE)
  expect_equal(updated_settings$database$section$name, "pecan")
  expect_equal(updated_settings$database$section$password, "pecan")  
})

test_that("`addSecrets` adds secret settings when force is FALSE and secrets have not been added", {
  settings <- list(
    settings.info = list(
      secrets.added = FALSE
    )
  )

  mocked_xmlToList_result <- list(
    database = list(
      section = list(
        name = "pecan",
        password = "pecan"
      )
    )
  )
  mockery::stub(addSecrets, 'file.exists', TRUE)
  mockery::stub(addSecrets, 'xmlToList', mocked_xmlToList_result)
  updated_settings <- addSecrets(settings, force = FALSE)
  expect_equal(updated_settings$database$section$name, "pecan")
  expect_equal(updated_settings$database$section$password, "pecan")
})