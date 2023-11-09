test_that("`listToArgString()` able to format list of named function args in a comma separated list", {
  expect_equal(
    listToArgString(c(host = 'pecan', settings = 'test', id = 2020)),
    "host='pecan', settings='test', id='2020'"
  )
})

test_that("`.parseArg()` works for all different types of entries in the list of function args passed to listToArgString", {
  # character
  expect_equal(.parseArg('pecan'), "'pecan'")
  # NULL
  expect_equal(.parseArg(NULL), "NULL")
  # list
  expect_equal(.parseArg(list(a = 1, b = 2)), "list(a='1', b='2')")
  # data.frame
  expect_equal(.parseArg(data.frame(a = 1, b = 2)), "data.frame(a =c(' 1 '),b =c(' 2 '))")
  # nested list
  expect_equal(.parseArg(list(a = 1, b = list(c = 3, d = 4))), "list(a='1', b=list(c='3', d='4'))")
})