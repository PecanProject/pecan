#' Combine spectra by wavelength
#'
#' @param ... Spectra to combine 
#' @export
cbind.spectra <- function(...) {
  dots <- list(...)
  lens <- vapply(dots, ncol, numeric(1))
  waves <- lapply(dots, wavelengths)
  new_waves <- Reduce(union, waves)
  names_list <- lapply(dots, colnames)
  m <- spectra(matrix(NA_real_, length(new_waves), sum(lens)), new_waves)
  colnames(m) <- as.character(seq(1, sum(lens)))
  j <- 1
  for (i in seq_along(dots)) {
    jseq <- seq(j, j + lens[i] - 1)
    m[[waves[[i]], jseq]] <- dots[[i]]
    if (!is.null(names_list[[i]])) {
      colnames(m)[jseq] <- names_list[[i]]
    }
    j <- j + lens[i]
  }
  m
}
