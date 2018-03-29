#' @title List only files in a directory
#' 
#' @author Alexey Shiklomanov
#' @inheritParams base::list.files
list.files.nodir <- function(path, ...) {
    allfiles <- list.files(path, ...)
    dirs <- list.dirs(path, ...)
    outfiles <- setdiff(allfiles, dirs)
    return(outfiles)
}


#' Function translating pecan vars to ED vars
#' var.names <- c("DBH", "AGB", "AbvGrndWood")
#' @export 
translate_vars_ed <- function(var.names) {
  
  var.list <- list()
  ed_derivations <- rep(NA, length(var.names))
  
  data("standard_vars", package = "PEcAn.utils")
  for(n in seq_along(var.names)){
    longname          <- standard_vars$Long.name[standard_vars$Variable.Name == var.names[n]]
    edvarout          <- ed.var(longname)
    var.list[[n]]     <- edvarout$readvar
    ed_derivations[n] <- edvarout$expr
  }
  
  var.names <- unique(unlist(var.list))
  return(list(vars = var.names, expr = ed_derivations))
}
