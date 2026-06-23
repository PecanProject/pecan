#' PEcAn.PEPRMT-package
#'
#' PEcAn support for the tidal biochemistry model PEPRMT
#'
#' @importFrom dplyr all_of select %>% .data
#' @keywords internal
"_PACKAGE"


# Define null-coalescing operator unless already available in base (R >= 4.4).
# (We could also import it from rlang, but why add a whole import for one
# line of code?)
if (!exists("%||%", envir=.BaseNamespaceEnv)) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
