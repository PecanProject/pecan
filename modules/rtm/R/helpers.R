if (requireNamespace('PEcAn.logger')) {
  stop <- PEcAn.logger::logger.severe
  warning <- PEcAn.logger::logger.warn
  message <- PEcAn.logger::logger.info
}

testForPackage <- function(pkg) {
  if (!requireNamespace(pkg)) {
    stop("Package", pkg, "required but not installed")
  }
}
