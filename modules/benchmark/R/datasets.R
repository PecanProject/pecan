#' Clone Run Input Conversion Lookup
#'
#' A lookup table for determining which file format is necessary for converting
#' inputs from a run into inputs for a cloned run.
#' For example for met we need the CF standard 
#' For IC it depends on pool vs cohort - may need special cases
#'
#' @name clone_run_inputs
#' @docType data
#' @keywords datasets
#' @format data frame, all columns character
#' \describe{
#'  \item{old}{The format of the old data}
#'  \item{new}{The format of the new data}
#'  \item{convert_type}{The closest format that can be used to convert between old and new}
#'  \item{convert_format_id}{Format id of convert_type}

#'}
#'
"clone_run_inputs"
