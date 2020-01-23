context("status")

make_testdir <- function() {
	td <- tempfile()
	dir.create(td)
	teardown(unlink(td, recursive = TRUE, force = TRUE))

	td
}

test_that("status functions accept explicit filename", {
	d <- make_testdir()
	f <- file.path(d, "MY_STATUS")

	expect_silent(status.start("TRAITS", f))
	expect_silent(status.end("DONE", f))
	expect_silent(status.skip("MET", f))
	expect_silent(status.start("ENSEMBLE", f))
	expect_silent(status.end("ERROR", f))

	res <- readLines(f)
	expect_length(res, 3)
	expect_match(res[[1]], "^TRAITS.*DONE\\s*$")
	expect_match(res[[2]], "^MET.*SKIPPED\\s*$")
	expect_match(res[[3]], "^ENSEMBLE.*ERROR\\s*$")
	expect_equal(status.check("TRAITS", f), 1L)
	expect_equal(status.check("MET", f), 0L)
	expect_equal(status.check("ENSEMBLE", f), -1L)
})

test_that("status handles file = dir/", {
	d <- make_testdir()
	status.start("NONE", d)
	status.end("DONE", d)
	expect_equal(status.check("NONE", file.path(d, "STATUS")), 1L)
})

test_that("status functions read from local settings", {
	settings <- list(outdir = make_testdir())
	expect_silent(status.skip("auto"))
	expect_match(
		readLines(file.path(settings$outdir, "STATUS"))[[1]],
		"^auto.*SKIPPED\\s*$")
})

test_that("status finds settings defined outside immediate calling scope", {
	settings <- list(outdir = make_testdir())
	f <- function(name) {
		status.start(name)
		status.end()
	}
	g <- function(name) {
		f(name)
	}
	expect_silent(g("WRAPPED"))
	expect_equal(
		status.check("WRAPPED", file.path(settings$outdir, "STATUS")),
		1L)
})

test_that("status writes to stdout on bad filename", {
	expect_output(status.start("NOFILE"), "NOFILE")
	settings <- list(outdir = file.path(make_testdir(), "fake", "path"))
	expect_output(status.end(), "\\d{4}-\\d{2}-\\d{2}.*DONE")
})

test_that("status.check returns 0 on bad filename", {
	expect_equal(status.check("NOFILE"), 0L)
	expect_equal(status.check("NOFILE", file.path(make_testdir(), "fake")), 0L)
})
