#' @title List only files in a directory
#' 
#' @author Alexey Shiklomanov
#' @inheritParams base::list.files
#' @export
list.files.nodir <- function(path, ...) {
    allfiles <- list.files(path, ...)
    dirs <- list.dirs(path, full.names = FALSE)
    outfiles <- setdiff(allfiles, dirs)
    return(outfiles)
}


#' Function translating pecan vars to ED vars
#' var.names <- c("DBH", "AGB", "AbvGrndWood")
#' @export 
translate_vars_ed <- function(varnames) {
  
  var.list <- add.list <- list()
  ed_vars <- ed_derivations <- ed_units <- rep(NA, length(varnames))
 
  for(n in seq_along(varnames)){
    edvarout          <- ed.var(varnames[n])
    var.list[[n]]     <- edvarout$readvar
    ed_derivations[n] <- edvarout$expr
    add.list[[n]]     <- edvarout$drelated
    ed_units[n]       <- edvarout$units
  }
  
  varnames    <- unique(unlist(var.list))
  addvarnames <- unique(unlist(add.list))
  return(list(vars = varnames, addvars = addvarnames, expr = ed_derivations, units = ed_units))
}

