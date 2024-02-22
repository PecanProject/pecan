test_that("`check_model_run()` gives correct output for the passed `out` value",{
  # failure
  expect_error(
    check_model_run(c("ERROR IN MODEL RUN")), 
    "Model run aborted with the following error:\nERROR IN MODEL RUN"
  )

  # success
  expect_equal(check_model_run(c("SUCCESS")), TRUE)
})