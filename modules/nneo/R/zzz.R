pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

jc <- function(x) Filter(Negate(is.null), x)

neon_base <- function() "http://data.neonscience.org/api/v0"

neon_parse <- function(x, parse = TRUE, flatten = TRUE) {
  jsonlite::fromJSON(x, parse, flatten = flatten)
}
