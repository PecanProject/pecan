context("test MultiSettings class")


# SETUP
l <- list(aa = 1, bb = 2, cc = list(dd = 3, ee = 4))
settings <- settings2 <- Settings(l)
settings2$aa <- 9
multiSettingsTemplate <- MultiSettings(settings, settings2)

test_that("MultiSettings constructor works as expected", {
  sl <- SafeList(l)
  expect_error(MultiSettings(l, l))
  expect_error(MultiSettings(sl, l))
  expect_error(MultiSettings(settings, l))

  multiSettings <- MultiSettings(settings, settings, settings)
  multiSettings2 <- MultiSettings(list(settings, settings, settings))
  multiSettings3 <- MultiSettings(multiSettings)
  expect_identical(multiSettings2, multiSettings)
  expect_identical(multiSettings3, multiSettings)

  for (i in seq_along(multiSettings)) {
    expect_identical(multiSettings[[i]], settings)
  }

  expect_true(inherits(multiSettings, "list"))
  expect_true(inherits(multiSettings, "MultiSettings"))
  expect_true(is.MultiSettings(multiSettings))
  expect_false(is.MultiSettings(l))
  expect_equal(length(class(multiSettings)), 2)
})

# -------------- EXTRACT
test_that("MultiSettings extracts work as expected", {
  s1 <- Settings(a = 1, b = 2, c = 3)
  s2 <- Settings(a = 1, b = 22, d = 4)
  s3 <- s2
  multiSettings <- MultiSettings(s1, s2, s3)

  # --- Normal extraction
  expect_identical(multiSettings[[1]], s1)
  expect_identical(multiSettings[1], MultiSettings(s1))
  expect_identical(multiSettings[1:3], multiSettings)

  # --- Extract by name
  # Collapses normally
  expect_equal(multiSettings$a, 1)
  expect_equal(multiSettings[["a"]], 1)
  expect_equivalent(
    multiSettings[["a", collapse = FALSE]],
    replicate(3, 1, FALSE))

  # Can't collapse because not equal
  expect_equivalent(multiSettings$b, list(s1$b, s2$b, s3$b))
  expect_equivalent(multiSettings[["b"]], list(s1$b, s2$b, s3$b))
  expect_equivalent(
    multiSettings[["b", collapse = FALSE]],
    list(s1$b, s2$b, s3$b))

  # Can't collapse because not shared by all
  expect_equivalent(multiSettings$c, list(s1$c, s2$c, s3$c))
  expect_equivalent(multiSettings[["c"]], list(s1$c, s2$c, s3$c))
  expect_equivalent(multiSettings[["c", collapse = FALSE]],
    list(s1$c, s2$c, s3$c))

  # Explicitly prohibited to prevent confusion
  expect_error(multiSettings["a"])
  expect_error(multiSettings[c("a", "b", "c")])
})



# ------------ ADD BY NUMERIC INDEX
test_that("Error thrown for trying to add anything other than Settings", {
  multiSettings <- multiSettingsTemplate
  expect_error(multiSettings[[1]] <- 3)
  expect_error(multiSettings[[1]] <- l)
  expect_error(multiSettings[2:3] <- c("a", "b"))
  expect_error(multiSettings[2:3] <- list(l, l))
})

test_that("Error thrown for trying to add anything via [", {
  multiSettings <- multiSettingsTemplate
  expect_error(multiSettings[2:5] <- multiSettings)
  expect_error(multiSettings[2:5] <- 2:5)
  expect_error(multiSettings[2:5] <- NULL)
})

test_that("Settings can be added by numerical indexing to [[, removed by adding NULL", {
  multiSettings <- multiSettingsTemplate
  length0 <- length(multiSettingsTemplate)
  expect_silent(multiSettings[[length(multiSettings) + 1]] <- settings)
  expect_equal(length(multiSettings), length0 + 1)
  for (i in seq_len(length0)) {
    expect_identical(multiSettings[[i]], multiSettingsTemplate[[i]])
  }
  expect_identical(multiSettings[[length(multiSettings)]], settings)

  expect_silent(multiSettings[[1]] <- NULL)
  expect_equal(length(multiSettings), length0)
  if (length0 > 1) {
    for (i in seq_len(length0 - 1)) {
      expect_identical(multiSettings[[i]], multiSettingsTemplate[[i + 1]])
    }
  }
  expect_identical(multiSettings[[length0]], settings)
})


# -------------------- ADD BY CHARACTER INDEX
test_that("Assignments by name apply to each Settings individually", {
  multiSettings <- expected <- multiSettingsTemplate
  expect_silent(multiSettings$x <- 1)

  for (i in seq_along(multiSettings)) {
    expected[[i]]$x <- 1
    expect_identical(multiSettings[[i]], expected[[i]])
  }
})

test_that("Assignments by name works as expcted for list value", {
  multiSettings <- expected <- multiSettingsTemplate
  value <- list(x = 1, y = 3:5, z = "z")
  expect_silent(multiSettings$x <- value)

  for (i in seq_along(multiSettings)) {
    expected[[i]]$x <- value
    expect_identical(multiSettings[[i]], expected[[i]])
    expect_identical(multiSettings[[i]]$x, value)
  }
})


test_that("Assigning NULL by name removes setting from each Setting", {
  multiSettings <- expected <- multiSettingsTemplate
  length0 <- length(multiSettings[[1]])
  expect_silent(multiSettings$aa <- NULL)
  expect_equal(length(multiSettings[[1]]), length0 - 1)

  for (i in seq_along(multiSettings)) {
    expected[[i]]$aa <- NULL
    expect_identical(multiSettings[[i]], expected[[i]])
  }
})

test_that("Assigning non-globally applies values sequentially to Settings", {
  multiSettings <- expected <- multiSettingsTemplate
  expect_silent(
    multiSettings[["x", global = FALSE]] <- seq_along(multiSettings))
  for (i in seq_along(multiSettings)) {
    expected[[i]]$x <- i
    expect_identical(multiSettings[[i]], expected[[i]])
  }

  expect_true(length(multiSettings) > 1)
  for (i in 2:length(multiSettings)) {
    expected[[i]]$y <- i
  }

  y <- expected[["y", FALSE]]
  expect_silent(multiSettings[["y", global = FALSE]] <- y)
  expect_identical(multiSettings, expected)
  expect_true(is.null(multiSettings[[1]]$y))
})

test_that("Assigning non-globally applies values sequentially to Settings", {
  multiSettings <- expected <- multiSettingsTemplate
  x <- seq_along(multiSettings)
  expect_silent(multiSettings[["x", global = FALSE]] <- x)
  for (i in seq_along(multiSettings)) {
    expected[[i]]$x <- i
    expect_identical(multiSettings[[i]], expected[[i]])
    expect_true(is.numeric(multiSettings[[i]]$x))
  }
})


test_that("Assigning a list of values non-globally works as expected", {
  multiSettings <- expected <- multiSettingsTemplate
  x <- list()
  for (i in seq_along(multiSettings)) {
    x[[i]] <- as.list(i * 1:3)
  }

  expect_silent(multiSettings[["x", global = FALSE]] <- x)
  for (i in seq_along(multiSettings)) {
    expected[[i]]$x <- x[[i]]
    expect_identical(multiSettings[[i]], expected[[i]])
    expect_true(is.list(multiSettings[[i]]$x))
    expect_equal(length(multiSettings[[i]]$x), 3)
  }
})


test_that("Assigning non-globally works as expected for a values list containing NULL", {
  multiSettings <- expected <- multiSettingsTemplate
  y <- seq_along(multiSettings)
  y[1] <- list(NULL)

  expect_silent(multiSettings[["y", global = FALSE]] <- y)
  expect_true(is.null(multiSettings[[1]]$y))
  for (i in 2:length(multiSettings)) {
    expected[[i]]$y <- y[[i]]
    expect_identical(multiSettings[[i]], expected[[i]])
  }
})

test_that("Assigning non-globally works as expected for a values list containing NULL when previous value was non-NULL", {
  multiSettings <- expected <- multiSettingsTemplate
  y <- seq_along(multiSettings)
  y[1] <- list(NULL)

  multiSettings$y <- 1
  expect_silent(multiSettings[["y", global = FALSE]] <- y)
  expect_true(is.null(multiSettings[[1]]$y))
  for (i in 2:length(multiSettings)) {
    expected[[i]]$y <- y[[i]]
    expect_identical(multiSettings[[i]], expected[[i]])
  }
})


test_that("Assigning non-globally by name throws error for length mismatch", {
  multiSettings <- multiSettingsTemplate
  expect_error(
    multiSettings[["x", global = FALSE]] <- rep(1, length(multiSettings) - 1))
  expect_error(
    multiSettings[["x", global = FALSE]] <- rep(1, length(multiSettings) + 1))
})

test_that("Assigning non-globally to a single-element MultiSettings expands it to match length of value", {
  multiSettings <- MultiSettings(settings)
  x <- 1:3
  expect_silent(multiSettings[["x", global = FALSE]] <- x)
  expect_equal(length(multiSettings), 3)
  for (i in seq_along(multiSettings)) {
    newSettings <- settings
    newSettings$x <- i
    expect_identical(multiSettings[[i]], newSettings)
  }
})


test_that("Assigning non-globally to a single-element MultiSettings expands it to match length of value, when value is a list", {
  multiSettings <- MultiSettings(settings)
  x <- list()
  for (i in 1:3) {
    x[[i]] <- as.list(i * 1:3)
  }

  expect_silent(multiSettings[["x", global = FALSE]] <- x)
  expect_equal(length(multiSettings), 3)
  for (i in seq_along(multiSettings)) {
    newSettings <- settings
    newSettings$x <- x[[i]]
    expect_identical(multiSettings[[i]], newSettings)
    expect_true(is.list(multiSettings[[i]]$x))
    expect_equal(length(multiSettings[[i]]$x), 3)
  }
})


# ------------ To/From XML
# helper fn
are.equal.possiblyNumericToCharacter <- function(o1, o2) {
  if (length(o1) != length(o2)) {
    return(FALSE)
  } else if (is.list(o1)) {
    for (i in seq_along(o1)) {
      if (!are.equal.possiblyNumericToCharacter(o1[[i]], o2[[i]])) {
        return(FALSE)
      }
    }
    return(TRUE)
  } else {
    if (is.numeric(o1) || is.numeric(o2)) {
      o2 <- as.numeric(o2)
      o1 <- as.numeric(o1)
    }
    return(isTRUE(all.equal(o1, o2)))
  }
}

test_that("multiSettings write to and read from xml as expcted (i.e., with collapsing/expanding global settings)", {
  msOrig <- multiSettingsTemplate

  msXML <- PEcAn.settings::listToXml(msOrig, "pecan.multi")
  listNew <- XML::xmlToList(msXML)
  msNew <- expandMultiSettings(listNew)

  expect_true(are.equal.possiblyNumericToCharacter(msNew, msOrig))
})


test_that("expandMultiSettings does nothing to a non-MultiSettings list", {
  expect_identical(settings, expandMultiSettings(settings))
  expect_identical(l, expandMultiSettings(l))
})
