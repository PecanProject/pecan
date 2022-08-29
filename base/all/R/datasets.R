#' Versions of all PEcAn packages in each release of PEcAn
#'
#' @docType data
#' @keywords datasets
#' @source Extracted from Git history of the
#'  [PEcAn development repository](https://github.com/PecanProject/pecan)
#' @seealso `pecan_version`, which queries this table and also reports the
#'  versions currently installed
#' @format Data frame with at least 33 columns (plus one more every release!)
#' \describe{
#'  \item{package}{Package name, as character}
#'  \item{v1.0, ..., v1.7.2, ....}{Release numbers,
#'    each with the Git tag as the column name and numeric package versions
#'    as the row contents.
#'    NA means `package` did not exist in this version of PEcAn.}
#' }
"pecan_version_history"

#' Dates, tags, and versions of all PEcAn releases
#'
#' @source Extracted from Git tags in the
#'  [PEcAn development repository](https://github.com/PecanProject/pecan)
#' @docType data
#' @keywords datasets
#' @format Data frame with 3 columns
#' \describe{
#'  \item{tag}{name used to mark the release}
#'  \item{date}{date released}
#' 	\item{version}{numeric version assigned to this release}
#' }
"pecan_releases"
