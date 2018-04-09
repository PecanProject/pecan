#' Get default parameters
#' 
#' Extract default parameter values from `model.list`
#' @param modname Model name. Must match `modname` in `model.list`
#' @return Named vector of default parameter values
#' @export
defparam <- function(modname) {
  data(model.list)
  p.raw   <- model.list[model.list$modname == modname, "par.default"]
  p.split <- strsplit(p.raw, " ")
  p.names <- sapply(p.split, function(x) gsub("=.*", "", x))
  p.vals  <- sapply(p.split, function(x) as.numeric(gsub(".*=", "", x)))
  p.out   <- drop(p.vals)
  names(p.out) <- drop(p.names)
  return(p.out)
} # defparam
