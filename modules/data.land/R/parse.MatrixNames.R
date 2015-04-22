##' @name parse.MatrixNames
##' @author Michael Dietze
##' @param w mcmc object containing matrix outputs
##' @param pre prefix (variable name) for the matrix variable to be extracted
##' @param numeric boolean, whether to coerce class to numeric
##' @return matrix
##' @export
parse.MatrixNames <- function(w,pre="x",numeric=FALSE){ 
  w = sub(pre,"",w)
  w = sub("[","",w,fixed=TRUE)
  w = sub("]","",w,fixed=TRUE)
  w = matrix(unlist(strsplit(w,",")),nrow=length(w),byrow=TRUE)
  if(numeric){
    class(w) <- "numeric"
  }
  colnames(w)<-c("row","col")
  return(as.data.frame(w))
}