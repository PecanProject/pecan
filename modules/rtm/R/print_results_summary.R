#' Neatly print inversion results summary
#' 
#' @author Alexey Shiklomanov
#' @param output Output from \code{invert.auto}
#' @export
print_results_summary <- function(output) {
  results <- output$results
  npar <- sum(grepl(".q975$", names(results)))
  rawvec <- unlist(results)
  rawmat <- matrix(rawvec, ncol = npar, byrow = TRUE)
  colnames(rawmat) <- gsub(".mu$", '', names(results)[1:npar])
  rownames(rawmat) <- c("Mean", "SD", "2.5", "50", "97.5")
  return(rawmat)
}
