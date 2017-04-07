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
