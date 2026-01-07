test_that("listToXml", {
  s <- read.settings("data/testsettings.xml")
  s_xml <- listToXml(s)

  # root tag defaults to "pecan" whether passed named or unnamed
  expect_identical(XML::xmlName(s_xml), "pecan")
  expect_identical(s_xml, listToXml(s, tag = "pecan"))
  expect_identical(s_xml, listToXml(s, "pecan"))
})
