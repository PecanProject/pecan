if (requireNamespace('PEcAn.utils')) {
    stop <- PEcAn.logger::logger.severe
    warning <- PEcAn.logger::logger.warn
    message <- PEcAn.logger::logger.info
}

testForPackage <- function(pkg) {
  if (!requireNamespace(pkg)) {
    stop("Package", pkg, "required but not installed")
  }
}

distplot <- function(distname, param,
                     lower = NULL, upper = NULL, 
                     length.out = 1000, ..., 
                     plot = TRUE) {
  if (!is.character(distname)) {
    distchar <- deparse(substitute(distname))
  } else {
    distchar <- distname
  }
  qdist <- gsub("^d(.*$)", "q\\1", distchar)
  param <- as.list(unname(param))
  if (is.null(lower)) {
    lower <- do.call(qdist, c(list(p = 0.01), param))
  }
  if (is.null(upper)) {
    upper <- do.call(qdist, c(list(p = 0.99), param))
  }
  x <- seq(lower, upper, length.out = length.out)
  y <- do.call(distname, c(list(x = x), param))
  if (plot) {
    plot(y ~ x, type ='l', ...)
  } else {
    return(list(x = x, y = y))
  }
}
