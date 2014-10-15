##' @name met2CF.NARR
##' @title met2CF.NARR
##' @export
##' 
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' @param convert NARR files to CF files
##' @author Elizabeth Cowdery, Rob Kooper
##' 
met2CF.NARR <- function(in.path, in.prefix, outfolder) {
  cmd <- system.file("scripts/CF.NARR.sh", package="PEcAn.data.atmosphere")
  args <- paste(c(in.path, in.prefix, outfolder), collapse=" ")
  system2(cmd, args)
}

