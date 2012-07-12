require(XML)

context("tests for read.settings and related functions")
test_that("read settings returns error if no settings file found (issue #1124)",{
  expect_message(read.settings(), "
  
test_that("merge 2 xml files", {
#  ## merge the files
#  print(xmlMerge(xmlParse("a.xml"), xmlParse("b.xml")))
#  settings <- xmlToList(xmlMerge(xmlParse("a.xml"), xmlParse("b.xml")))
#  
#  ##<root>
#  ##  <a var1="2" var2="2">b</a>
#  ##  <b var1="1">a</b>
#  ##  <c var1="1" var2="2">
#  ##    <d>b</d>
#  ##    <e var1="2" var2="1">b</e>
#  ##    <f>a</f>
#  ##    <g>b</g>
#  ##  </c>
#  ##</root>
#  
#  ## check results
#  expect_equal(names(settings), c("a", "b", "c"))
#  
#  expect_equal(names(settings$a), c("text", ".attrs"))
#  expect_equal(settings$a$text, "b")
#  expect_equal(names(settings$a$.attrs), c("var1", "var2"))
#  expect_equal(settings$a$.attrs[['var1']], "2")
#  expect_equal(settings$a$.attrs[['var2']], "2")
#  
#  expect_equal(names(settings$b), c("text", ".attrs"))
#  expect_equal(settings$b$text, "a")
#  expect_equal(names(settings$b$.attrs), c("var1"))
#  expect_equal(settings$b$.attrs[['var1']], "1")
#  
#  expect_equal(names(settings$c), c("d", "e", "f", "g", ".attrs"))
#  expect_equal(names(settings$c$.attrs), c("var1", "var2"))
#  expect_equal(settings$b$.attrs[['var1']], "1")
#  
#  expect_equal(names(settings$c$d), NULL)
#  expect_equal(settings$c$d, "b")
#  
#  expect_equal(names(settings$c$e), c("text", ".attrs"))
#  expect_equal(settings$c$e$text, "b")
#  expect_equal(names(settings$c$e$.attrs), c("var1", "var2"))
#  expect_equal(settings$c$e$.attrs[['var1']], "2")      
#  expect_equal(settings$c$e$.attrs[['var2']], "1")      
#  
#  expect_equal(names(settings$c$f), NULL)
#  expect_equal(settings$c$f, "a")
#  
#  expect_equal(names(settings$c$g), NULL)
#  expect_equal(settings$c$g, "b")
})
