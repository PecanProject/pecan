test_that("write.settings able to write a settings file based on input list", {
  withr::with_tempfile("tf", fileext = ".xml", {
    writeLines(
      "<pecan>
        <outdir>testdir</outdir>
      </pecan>",
      con = tf
    )
    t <- XML::xmlToList(XML::xmlParse(tf))
    mockery::stub(write.settings, "file.path", tf)
    expect_equal(write.settings(t, tf), tf)
    expect_equal(XML::xmlToList(XML::xmlParse(tf)), t)
  })
})

test_that("write.settings accepts a null outputdir", {
  withr::with_tempfile("tf", fileext = ".xml", {
    s <- list(outdir = "/fake/path", model = "NONE")
    pth <- write.settings(s, tf, NULL)
    ss <- XML::xmlToList(XML::xmlParse(tf))
    expect_equal(pth, tf)
    expect_equal(s, ss)
  })
})

test_that("write.settings returns character if outputfile is null", {
  s <- list(one = 1, two = list(a = "a", b = "b"))
  expect_equal(
    write.settings(s, NULL, NULL),
    paste0(
      "<?xml version=\"1.0\"?>\n",
      "<pecan>\n",
      " <one>1</one>\n",
      " <two>\n",
      "  <a>a</a>\n",
      "  <b>b</b>\n",
      " </two>\n",
      "</pecan>"
    )
  )
})
