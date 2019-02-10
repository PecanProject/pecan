stop <- function(...) {
  if (requireNamespace("PEcAn.logger", quietly = TRUE)) {
    PEcAn.logger::logger.severe(...)
  } else {
    stop(...)
  }
}

warning <- function(...) {
  if (requireNamespace("PEcAn.logger", quietly = TRUE)) {
    PEcAn.logger::logger.warn(...)
  } else {
    warning(...)
  }
}

message <- function(...) {
  if (requireNamespace("PEcAn.logger", quietly = TRUE)) {
    PEcAn.logger::logger.info(...)
  } else {
    message(...)
  }
}

testForPackage <- function(pkg) {
  if (!requireNamespace(pkg)) {
    stop("Package", pkg, "required but not installed")
  }
}

