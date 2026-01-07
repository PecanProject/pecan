
# avoid exiting on error
oq <- PEcAn.logger::logger.setQuitOnSevere(FALSE)
withr::defer(PEcAn.logger::logger.setQuitOnSevere(oq), teardown_env())

# write to stdout so expect_output() works on logger messages
oc <- PEcAn.logger::logger.setUseConsole(console = TRUE, stderr = FALSE)
withr::defer(
  PEcAn.logger::logger.setUseConsole(console = oc$console, stderr = oc$stderr),
  teardown_env()
)
