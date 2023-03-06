test_that("`remote.copy.to()` raises errors for false inputs",{
  expect_error(remote.copy.to(NULL, "path/to/local/file", "path/to/remote/file"), "`host` object passed to the function is NULL : Try passing a valid host object")
  expect_error(remote.copy.to(host = list(name = "pecan",user="test_user", tunnel="test_tunnel"), "path/to/local/file", "path/to/remote/file"), "Could not find tunnel")
  expect_error(remote.copy.to(host = list(name = "",user="test_user", tunnel="test_tunnel"), "path/to/local/file", "path/to/remote/file"), "`name` parameter in the `host` object is NULL or empty : Try passing a valid host object")
})
