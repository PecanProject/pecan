
# A testing shim:
# Identical to utils::download.file during normal operation,
# but lets us replace it under test with stubs that don't use the network.
# See ?testthat::with_mocked_bindings for details
download_file_shim <- function(...) utils::download.file(...)
