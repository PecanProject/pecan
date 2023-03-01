test_that("qsub_get_jobid returns the correct job ID", {
  
  out_with_jobid <- "Job ID: 1234"
  out_no_jobid <- "Job ID: "
  qsub.jobid <- "Job ID: (\\d+)"
  
  expect_equal(qsub_get_jobid(out_with_jobid, qsub.jobid, FALSE), "1234")
  expect_equal(qsub_get_jobid(out_no_jobid, qsub.jobid, FALSE), NA)

})