nGET <- function(url, query = list(), ...) {
  cli <- crul::HttpClient$new(
    url = url,
    opts = list(...),
    headers = list(
      `User-Agent` = nneo_ua(),
      'X-USER-AGENT' = nneo_ua()
    )
  )
  res <- cli$get(query = query)
  errs(res)
  res$parse("UTF-8")
}

errs <- function(x) {
  if (x$status_code > 201) {
    xx <- jsonlite::fromJSON(x$parse("UTF-8"))
    if ("error" %in% names(xx)) {
      # match by status code
      fun <- match_err(x$status_code)$new()
      fun$mssg <- xx$error$detail
      fun$do_verbose(x)
    } else {
      # if no error message in response, just general stop
      fauxpas::http(x)
    }
  }
}

match_err <- function(code) {
  tmp <- paste0("fauxpas::",
                grep("HTTP*", getNamespaceExports("fauxpas"), value = TRUE))
  fxns <- lapply(tmp, function(x) eval(parse(text = x)))
  codes <- vapply(fxns, function(z) z$public_fields$status_code, 1)
  fxns[[which(code == codes)]]
}

nneo_ua <- function() {
  versions <- c(
    paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("rOpenSci(nneo/%s)", utils::packageVersion("nneo"))
  )
  paste0(versions, collapse = " ")
}
