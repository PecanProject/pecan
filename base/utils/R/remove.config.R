remove.config <- function(dir, settings, model) {
    
    fcn.name <- paste0("remove.config.", model)
    if (exists(fcn.name)) {
        do.call(fcn.name, args = list(dir, settings))
    } else {
        warning(paste(fcn.name, "does not exist"))
        warning("This function is not required, but its implementation is recommended")
    }
} # remove.config
