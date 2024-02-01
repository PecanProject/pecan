test_that("`write.settings` able to write a settings file based on input list",{
  withr::with_tempfile("tf", fileext=".xml",{
    writeLines(
      "<pecan>
        <outdir>testdir</outdir>
      </pecan>", 
      con = tf)
    t <- XML::xmlToList(XML::xmlParse(tf))
    mockery::stub(write.settings, 'file.path', tf)
    expect_equal(write.settings(t, tf), tf)
    expect_equal(XML::xmlToList(XML::xmlParse(tf)), t)
  })
})