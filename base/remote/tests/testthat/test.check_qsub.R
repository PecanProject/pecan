library(PEcAn.remote)
library(testthat)

host <- list(name = "localhost")

# The `echo` command here is a substitute for what `qstat` would return.
qstat <- 'echo "Job 456 is still running" | grep @JOBID@ || echo "DONE"'

finished_job <- 123
unfinished_job <- 456

test_that("qstat job status checking works, even with piped commands", {
  expect_true(qsub_run_finished(run = finished_job, host = host, qstat = qstat))
  expect_false(qsub_run_finished(run = unfinished_job, host = host, qstat = qstat))
})
