#' @name defparam
#' @title Get default parameters
#' @details Extract default parameter values from `model.list`
#' @param modname Model name. Must match `modname` in `model.list`
#' @return Named vector of default parameter values
defparam <- function(modname){
    data(model.list)
    setkey(model.list, modname)
    p.raw <- model.list[modname, par.default]
    p.split <- strsplit(p.raw, " ")
    p.names <- sapply(p.split, function(x) gsub("=.*", "", x))
    p.vals <- sapply(p.split, function(x) as.numeric(gsub(".*=", "", x)))
    p.out <- p.vals[,1]
    names(p.out) <- p.names[,1]
    return(p.out)
}
    
