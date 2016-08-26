##' @export
papply <- function(settings, fn, ...) {
  if(is.MultiSettings(settings)) {
    result <- lapply(settings, fn, ...)
    if(all(sapply(result, is.Settings)))
      result <- MultiSettings(result)
    return(result)
  } else if(is.Settings(settings)) {
    return(fn(settings, ...))
  } else if(is.list((settings))) {
    # Assume it's settings list that hasn't been coerced to Settings class...
    return (fn(as.Settings(settings), ...))
  } else {
    logger.severe("The function", fn, "requires input of type MultiSettings or Settings")
  }
}