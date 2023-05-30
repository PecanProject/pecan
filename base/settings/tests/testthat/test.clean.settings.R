PEcAn.logger::logger.setQuitOnSevere(FALSE)
on.exit(PEcAn.logger::logger.setQuitOnSevere(TRUE))

test_that("`test.clean.settings` works correctly for invalid and correct inputs", {

  # Error if input file is NULL or does not exist
  expect_error(
    clean.settings(inputfile = NULL), 
    "Could not find input file."
  )
  expect_error(
    clean.settings(inputfile = "nonexistent.xml"), 
    "Could not find input file."
  )

  # Works if correct input file provided
  t <- withr::with_tempfile("tf", {
    clean.settings(inputfile = "testinputcleanup.xml", outputfile = tf)
    readLines(tf)
  })

  t <- XML::xmlToList(XML::xmlParse(t))
	
  # Check for updated settings after cleanup
  expect_equal(t$outdir, "pecan")
  expect_equal(t$rundir, NULL)
  expect_equal(t$modeloutdir, NULL)
  expect_equal(t$host$rundir, NULL)
  expect_equal(t$host$outdir, NULL)
  expect_equal(t$database$dbfiles, NULL)
  expect_equal(t$workflow, NULL)
  expect_equal(t$pfts[[1]]$pft$outdir, NULL)
  expect_equal(t$pfts[[1]]$pft$posteriorid, NULL)
  expect_equal(t$host, list(name = "localhost"))
})
