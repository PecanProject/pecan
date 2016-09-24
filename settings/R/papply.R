##' @export
papply <- function(settings, fn, stop.on.error=FALSE, ...) {
  if(is.MultiSettings(settings)) {
    result <- list()
    for(i in seq_along(settings)) {
      logger.debug(paste0("papply executing ", deparse(substitute(fn)), 
        " on element ", i, " of ", length(settings), "."))
      
      result.i <- try(fn(settings[[i]], ...), silent = TRUE)
                      
      if(!is(result.i, "try-error")) {
        ind <- length(result) + 1
        if(!is.null(result.i)) {
          result[[ind]] <- result.i
        } else {
          result[ind] <- list(NULL) # Have to use special syntax to actually get a null value in
        }
        if(!is.null(settingNames(settings))) {
          names(result)[ind] <- settingNames(settings)[i]
        }
      } else {
        if(stop.on.error) {
          stop(paste0("papply threw an error for element ", i, " of ", length(settings),
            ", and is aborting since stop.on.error=TRUE. Message was: '", 
            as.character(result.i), "'"))
        } else {
          warning(paste0("papply threw an error for element ", i, " of ", length(settings),
            ", but is continuing since stop.on.error=FALSE", 
            " (there will be no results for this element, however). Message was: '", 
            as.character(result.i), "'"))
        }
      }
    }
    if(all(sapply(result, is.Settings)))
      result <- MultiSettings(result)

    if(length(errors) > 0) {
      PEcAn.utils::logger.warn(paste0("papply encountered the following errors, ",
        "but continued since stop.on.error=FALSE. ",
        paste(errors, collapse='; ')))
    }
    return(invisible(result))

  } else if(is.Settings(settings)) {
    return(fn(settings, ...))
  } else if(is.list((settings))) {
    # Assume it's settings list that hasn't been coerced to Settings class...
    return (fn(as.Settings(settings), ...))
  } else {
    logger.severe("The function", fn, "requires input of type MultiSettings or Settings")
  }
}