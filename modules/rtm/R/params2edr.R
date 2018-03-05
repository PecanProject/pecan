#' Convert named parameter vector to EDR-compatible inputs
#'
#' Creates a nested list whose components are suitable for passing to EDR.
#'
#' If `prospect = TRUE`, parameters prefixed with `prospect_` are passed to 
#' [prospect] with the specified `version`, and other parameters are passed to 
#' `trait.values`.
#'
#' The regular expression defining the separation is greedy, i.e. 
#' `temperate.Early_Hardwood.SLA` will separate into `temperate.Early_Hardwood` 
#' and `SLA` (assuming the default `sep = "."`). Therefore, it is crucial that 
#' trait names not contain any `sep` characters (neither ED nor PROSPECT 
#' parameters should anyway). If this is a problem, use an alternate separator 
#' (e.g. `|`).
#'
#' Note that using `sep = "."` allows this function to directly invert the 
#' default behavior of `unlist`. That is, calling 
#' `unlist(params2edr(params, prospect = FALSE)$trait.values)` will return the input vector of 
#' trait values. This makes `unlist` a convenient way to go from a 
#' `trait.values` list to a properly formatted `params` vector.
#'
#' Because unused ED parameters in the `config.xml` are ignored, the PROSPECT 
#' parameters are saved in the `trait.values` object as well, which may be 
#' useful for debugging.
#'
#' @param params Named parameter vector
#' @param sep Separator between PFT name and trait name. Must be a single 
#' character (default = ".").
#' @param prospect Logical. If `TRUE` (default), scan for PROSPECT traits and 
#' pass them to PROSPECT.
#' @param version PROSPECT version
#' @return List containing `spectra_list` and `trait.values`, both objects needed by [EDR].
#' @author Alexey Shiklomanov
#' @export
params2edr <- function(params, sep = ".", prospect = TRUE, version = 5) {
  stopifnot(
    !is.null(names(params)),
    all(grepl(sep, names(params)))
  )
  split_regex <- paste0("(.*)[", sep, "](.*)")
  pfts <- gsub(split_regex, "\\1", names(params))
  traits <- gsub(split_regex, "\\2", names(params))
  distinct_pfts <- unique(pfts)
  result <- list(trait.values = vector("list", length(distinct_pfts)))
  names(result$trait.values) <- distinct_pfts
  if (prospect) {
    result$spectra_list <- result$trait.values

  }
  for (pft in distinct_pfts) {
    pft_ind <- pfts == pft
    pft_traits <- params[pft_ind]
    names(pft_traits) <- traits[pft_ind]
    if (prospect) {
      prospect_traits <- grepl("prospect_", pft_traits)
      result$spectra_list[[pft]] <- prospect(pft_traits[prospect_traits], version)
    }
    result$trait.values[[pft]] <- pft_traits
  }
  result
}
