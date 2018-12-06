#' Get default parameters
#' 
#' Extract default parameter values from `model.list`
#' @param modname Model name. Must match `modname` in `model.list`
#' @return Named vector of default parameter values
#' @export
defparam <- function(modname) {
  data(model.list)
  model.list$modname <- trimws(model.list$modname)
  p.raw   <- model.list[model.list$modname == modname, "par.default"]
  p.split <- strsplit(trimws(as.character(p.raw)), " ")[[1]]
  p.names <- lapply(p.split, function(x) gsub("=.*", "", x))
  p.vals  <- lapply(p.split, function(x) as.numeric(gsub(".*=", "", x)))
  p.out   <- unlist(p.vals)
  names(p.out) <- unlist(p.names)
  p.out
} # defparam
