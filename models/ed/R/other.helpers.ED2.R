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
translate_vars_ed <- function(varnames) {
  
  var.list <- list()
  ed_derivations <- rep(NA, length(varnames))
 
  for(n in seq_along(varnames)){
    edvarout          <- ed.var(varnames[n])
    var.list[[n]]     <- edvarout$readvar
    ed_derivations[n] <- edvarout$expr
  }
  
  varnames <- unique(unlist(var.list))
  return(list(vars = varnames, expr = ed_derivations))
}

