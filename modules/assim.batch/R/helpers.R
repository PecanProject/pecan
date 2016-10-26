stop <- function(...) {
  if (requireNamespace("PEcAn.utils")) {
    logger.severe(...)
  } else {
    stop(...)
  }
}

warning <- function(...) {
  if (requireNamespace("PEcAn.utils")) {
    logger.warn(...)
  } else {
    warning(...)
  }
}

message <- function(...) {
  if (requireNamespace("PEcAn.utils")) {
    logger.info(...)
  } else {
    message(...)
  }
}

testForPackage <- function(pkg) {
  if (!requireNamespace(pkg)) {
    stop("Package", pkg, "required but not installed")
  }
}

