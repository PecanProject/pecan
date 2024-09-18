context("tests for read.settings and related functions")

PEcAn.logger::logger.setQuitOnSevere(FALSE)
PEcAn.logger::logger.setLevel("OFF")
testdir <- tempfile()
dir.create(testdir, showWarnings = FALSE)
teardown(unlink(testdir, recursive = TRUE))

test_that("`strip_comments()` function removes comments from nested lists", {
  nestedList <- list(
    "run",
    "outdir",
    list(
      "database",
      list(
        "user",
        comment = "A comment"
      )
    ),
    comment = "A comment"
  )
  stripped_list <- strip_comments(nestedList)
  expect_false("comment" %in% names(stripped_list))
  expect_false("comment" %in% names(stripped_list[[3]]))
})

test_that("read.settings() strips comments", {
  s_comments <- read.settings("data/testsettings-comment.xml")
  s <- read.settings("data/testsettings.xml")
  expect_equal(s_comments, s)
})

test_that("read.settings() warns if named input file doesn't exist (but pecan.xml does)", {
  old_setting <- PEcAn.logger::logger.setLevel("DEBUG")
  on.exit(PEcAn.logger::logger.setLevel(old_setting))

  # this returns FALSE in the first call to the mock function,
  # FALSE in the second call, and TRUE in the third call
  m <- mockery::mock(FALSE, FALSE, TRUE)
  mockery::stub(read.settings, 'file.exists', m)
  mockery::stub(
    read.settings, 
    'XML::xmlParse', 
    "<pecan>
      <model>
        <site>test</site>
      </model>
    </pecan>")

  #hacky way to check for errors b/c PEcAn.logger errors are non-standard and
  #not captured by testthat::expect_message() or expect_error()
  x <- capture.output(
    read.settings("blahblahblah.xml"),
    type = "message"
  )
  expect_equal(length(mockery::mock_calls(m)), 3)
  expect_true(any(grepl("WARN", x)))
  expect_true(any(grepl("blahblahblah.xml not found", x)))
})

test_that("read settings returns error if no settings file found (#1124)", {
  withr::with_tempdir({ #in a dir with no pecan.xml
    expect_error(read.settings("nofile.xml"), "Could not find a pecan.xml file")
  })
})

# ANS: Many of these functions seem like they shouldn't require a
# database connection, but `check.settings` throws an error when it
# can't connect to the database.
s <- .get.test.settings()
skip_if_not(
  PEcAn.DB::db.exists(s[[c("database", "bety")]]),
  "Skipping read.settings tests because cannot connect to database"
)

test_that("read.settings returned correctly", {
  s <- .get.test.settings()
  skip("Tests failing due to multisite?")
  expect_true(file.exists(s$outdir))
  expect_true(file.info(s$outdir)$isdir)
})


test_that("check.settings throws error if pft has different type than model", {
  s <- .get.test.settings(testdir)
  s[["model"]]$model_type <- "SIPNET"
  expect_error(check.settings(update.settings(s)))
})

test_that("check.settings gives sensible defaults", {
  ## This provides the minimum inputs
  s1 <- list(
          pfts = list(
            pft = list(name = "salix", outdir = file.path(testdir, "pft"))),
          database = NULL, model = list(type = "BIOCRO"),
          run = list(
            start.date = lubridate::now(),
            end.date = lubridate::days(1) + lubridate::now()
          ),
          # would create in cwd if not specified
          outdir = file.path(testdir, "PEcAn_@WORKFLOW@")
        )
  s2 <- check.settings(update.settings(s1))
  expect_true(is.null(s2$database)
              || (length(s2$database) == 1 && names(s2$database) == "dbfiles"))

  s <- .get.test.settings(testdir)
  s1$database <- s$database
  s1$database$bety$write <- FALSE # RyK added because throws an error otherwise!
  s2 <- check.settings(update.settings(s1))
  expect_equal(s2$database$bety$driver, "PostgreSQL")

  ## dir. paths, with default localhost
  expect_equal(s2$host$name, "localhost")

  ## outdirs
  outdir <- file.path(testdir, paste0("PEcAn_", s2$workflow$id))
  expect_equal(s2$outdir, outdir)
  expect_equal(s2$host$outdir, file.path(outdir, "out"))
  expect_equal(s2$modeloutdir, s2$host$outdir)

  ## rundir
  expect_equal(s2$rundir, file.path(outdir, "run"))
  expect_equal(s2$rundir, s2$host$rundir)

  ## meta.analysis
  s1$meta.analysis$update <- TRUE # Required to trigger fixes to meta analysis settings
  s2 <- check.settings(s1)
  expect_true(s2$meta.analysis$iter > 1000)
  expect_false(s2$meta.analysis$random.effects$on)
})

test_that("pfts are defined and are in database", {
  s <- .get.test.settings(testdir)
  s$outdir <- testdir

  s$pfts <- list(pft = list())
  expect_error(check.settings(update.settings(s)))

  s$pfts <- list(pft = list(name = ""))
  expect_error(check.settings(update.settings(s)))

  s$pfts <- list(pft = list(name = "blabla"))
  expect_error(check.settings(update.settings(s)))
})

test_that("check.settings uses run dates if dates not given in ensemble or sensitivity analysis", {
  s <- .get.test.settings(testdir)

  for (node in c("ensemble", "sensitivity.analysis")) {
    s1 <- list(pfts = s$pfts, database = list(bety = s$database$bety),
               run = s$run, model = s$model, outdir = s$outdir)
    s1[[node]] <- list(variable = "FOO")
    s2 <- check.settings(update.settings(s1))
    expect_equivalent(s2[[node]]$start.year, lubridate::year(s2$run$start.date))
    expect_equivalent(s2[[node]]$end.year, lubridate::year(s2$run$end.date))

    s1 <- list(pfts = s$pfts, database = list(bety = s$database$bety),
               run = NA, model = s$model)
    s1[[node]] <- list(variable = "FOO", start.year = 1000, end.year = 1000)

    expect_error(check.settings(update.settings(s1)))
  }
})

test_that("sensitivity.analysis and ensemble use other's settings if null", {
  s <- .get.test.settings(testdir)
  s$run$start.date <- s$run$end.date <- NULL # Otherwise these would be used
  s$database$bety$write <- FALSE # otherwise will error for trying to add with no run dates

  nodes <- c("sensitivity.analysis", "ensemble")
  for (node1 in nodes) {
    node2 <- nodes[nodes != node1]
    s1 <- list(pfts = s$pfts, database = list(bety = s$database$bety),
               run = s$run, model = s$model, outdir = s$outdir)
    s1[[node1]] <- list(variable = "FOO", start.year = 2003, end.year = 2004)
    s1[[node2]] <- list()
    s2 <- check.settings(update.settings(s1))
    for (setting in c("variable", "start.year", "end.year")) {
      expect_equal(s2[[node1]][[setting]], s2[[node2]][[setting]])
    }
    expect_equal(s2$ensemble$size, 1)
  }
})

test_that("workflow id is numeric if settings$database$bety$write = FALSE", {
  s <- .get.test.settings(testdir)
  s1 <- check.settings(update.settings(s))
  expect_is(s1$workflow$id, c("character", "numeric"))

  s$workflow <- NULL
  s1 <- check.settings(update.settings(s))
  expect_is(s1$workflow$id, c("integer", "numeric"))
})

test_that("check.settings will fail if db does not exist", {
  s <- .get.test.settings(testdir)
  expect_true(PEcAn.DB::db.exists(s$database$bety))
  s$database$bety$dbname <- "blabla"
  expect_false(PEcAn.DB::db.exists(s$database$bety))

  expect_error(check.settings(update.settings(s)))
})



test_that("check.settings handles userid and username properly", {
  s1 <- .get.test.settings(testdir)
  s1$database$bety[["userid"]] <- "bety"
  s1$database$bety[["user"]] <- NULL
  s2 <- check.settings(update.settings(s1))
  expect_true("user" %in% names(s2$database$bety))
  expect_true(!"userid" %in% names(s2$database$bety))

  s1 <- .get.test.settings(testdir)
  s1$database$bety[["username"]] <- "bety"
  s1$database$bety[["user"]] <- NULL
  s2 <- check.settings(update.settings(s1))
  expect_true("user" %in% names(s2$database$bety))
  expect_true(!"username" %in% names(s2$database$bety))

  s1 <- .get.test.settings(testdir)
  s1$database$bety[["userid"]] <- "bety"
  s2 <- check.settings(update.settings(s1))
  expect_true("user" %in% names(s2$database$bety))
  expect_true(!"userid" %in% names(s2$database$bety))

  s1 <- .get.test.settings(testdir)
  s1$database$bety[["username"]] <- "bety"
  s2 <- check.settings(update.settings(s1))
  expect_true("user" %in% names(s2$database$bety))
  expect_true(!"username" %in% names(s2$database$bety))
})

test_that("check settings sets model$type based on model$name and model$model_type", {
  s <- .get.test.settings(testdir)
  s$model <- list(name = "BIOCRO")
  s1 <- check.settings(update.settings(s))
  expect_identical(s$model$name, s1$model$type)

  s <- .get.test.settings(testdir)
  s$model <- list(model_type = "BIOCRO")
  s1 <- check.settings(update.settings(s))
  expect_identical(s$model$model_type, s1$model$type)

  s <- .get.test.settings(testdir)
  s$model <- list(id = 7)
  s1 <- check.settings(update.settings(s))
  expect_identical(s1$model$type, "BIOCRO")

  s <- .get.test.settings(testdir)
  s$model <- list(binary = "/bin/true")
  expect_error(check.settings(update.settings(s)))
})

test_that("check settings runs with only model$name and no database", {
  s <- .get.test.settings(testdir)
  s$model <- list(name = "BIOCRO")
  s$database <- NULL
  s1 <- check.settings(update.settings(s))
  expect_identical(s$model$name, s1$model$type)
})

test_that("invalid pathname is placed in home directory", {
  s <- .get.test.settings(testdir)
  s$database$dbfiles <- "foo/bar"
  s1 <- check.settings(update.settings(s))
  expect_equal(
    s1$database$dbfiles,
    file.path(Sys.getenv("HOME"), s$database$dbfiles))
})

test_that("update.settings only runs once unless forced", {
  s <- .get.test.settings(testdir)
  expect_null(s$model$type)

  s <- update.settings(s)
  expect_equal(s$model$type, "BIOCRO")
  expect_true(s$settings.info$settings.updated)

  # Won't run a second time...
  s$model$name <- s$model$type
  s$model$type <- NULL
  s <- update.settings(s)
  expect_null(s$model$type)

  # ...unless forced
  s <- update.settings(s, force = TRUE)
  expect_equal(s$model$type, "BIOCRO")
})


test_that("check.settings only runs once unless forced", {
  s <- .get.test.settings(testdir)
  s$database$bety$driver <- NULL
  s <- check.settings(update.settings(s))

  expect_equal(s$database$bety$driver, "PostgreSQL")
  expect_true(s$settings.info$checked)


  # Won't run a second time...
  s$database$bety$driver <- NULL
  s <- check.settings(s)
  expect_null(s$database$bety$driver)

  # ...unless forced
  s <- check.settings(s, force = TRUE)
  expect_equal(s$database$bety$driver, "PostgreSQL")
})


test_that("fix.deprecated.settings only runs once unless forced", {
  s <- .get.test.settings(testdir)
  expected <- s$database$dbfiles
  s$run$dbfiles <- s$database$dbfiles
  s$database$dbfiles <- NULL

  s <- fix.deprecated.settings(s)
  expect_identical(s$database$dbfiles, expected)
  expect_null(s$run$dbfiles)

  # Won't run a second time...
  s$run$dbfiles <- s$database$dbfiles
  s$database$dbfiles <- NULL
  s <- fix.deprecated.settings(s)
  expect_identical(s$run$dbfiles, expected)
  expect_null(s$database$dbfiles)

  # ...unless forced
  s <- fix.deprecated.settings(s, force = TRUE)
  expect_identical(s$database$dbfiles, expected)
  expect_null(s$run$dbfiles)
})


test_that("check.settings works for a MultiSettings", {
  s1 <- .get.test.settings(testdir)
  s1 <- check.settings(update.settings(s1)) # Make sure all other settings OK
  s1$database$bety$driver <- NULL
  s2 <- s1

  # Change a setting to ensure that there will be a difference between the two Settings
  s2$database$dbfiles <- file.path(s2$database$dbfiles, "dummy")

  ms <- MultiSettings(s1, s2)
  ms <- check.settings(ms, force = TRUE)

  expect_equal(length(ms$database), 2)
  expect_false(identical(ms[[1]]$database, ms[[2]]$database))
  expect_equal(ms[[1]]$database$bety$driver, "PostgreSQL")
  expect_equal(ms[[2]]$database$bety$driver, "PostgreSQL")
})
