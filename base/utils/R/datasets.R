
#' Standardized variable names and units for PEcAn
#'
#' A lookup table giving standard names, units and descriptions for variables in PEcAn input/output files.
#' Originally based on the \href{https://nacp.ornl.gov/MsTMIP_variables.shtml}{MsTMIP} standards,
#' with additions to accomodate a wider range of model inputs and outputs.
#' The standard_vars table replaces both `mstmip_vars` and `mstmip_local`,
#' both of which are now deprecated.
#'
#' @name standard_vars
#' @docType data
#' @keywords datasets
#' @format data frame, all columns character
#' \describe{
#'  \item{Variable.Name}{Short name suitable for programming with}
#'  \item{standard_name}{Name used in the NetCDF \href{http://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html}{CF metadata conventions} }
#'  \item{Units}{Standard units for this variable. Do not call variables by these names if they are in different units.
#'    See `ud_convert` for conversions to and from non-standard units}
#'  \item{Long.Name}{Human-readable variable name, suitable for e.g. axis labels}
#'  \item{Category}{What kind of variable is it? (Carbon pool, N flux, dimension, input driver, etc)}
#'  \item{var_type}{Storage type (character, integer, etc)}
#'  \item{dim1,dim2,dim3,dim4}{Dimensions across which is this variable allowed to vary.
#'    Dimension names are themselves standard vars and must be present in the table with category "Dimension"}
#'  \item{Description}{Further details. For composite measures, list the variables it is calculated from}
#'}
#'
"standard_vars"

#' Trait dictionary for PEcAn trait and parameter names
#'
#' Lookup table mapping canonical PEcAn trait identifiers to display labels,
#' unit or scale strings, and model aliases where available.
#'
#' @format A data frame with four columns:
#' \describe{
#'   \item{id}{Canonical PEcAn trait or parameter identifier. This is the
#'   lookup key used by [trait.lookup()].}
#'   \item{figid}{Human-readable label used for figures, plot titles, and
#'   tables.}
#'   \item{units}{Unit or scale label for the trait or parameter.}
#'   \item{model.id}{Model-specific parameter identifier.}
#' }
#'
#' @name trait.dictionary
NULL
