#' nneo - R client for the NEON API
#'
#' NEON API docs: <http://data.neonscience.org/data-api>
#'
#' @section nneo API:
#' \itemize{
#'  \item [nneo_products()]
#'  \item [nneo_product()]
#'  \item [nneo_sites()]
#'  \item [nneo_site()]
#'  \item [nneo_locations()]
#'  \item [nneo_location()]
#'  \item [nneo_data()]
#'  \item [nneo_file()]
#'  \item [nneo_wrangle()]
#' }
#'
#' @section Curl options:
#' Curl options follow those in the \pkg{curl} package. Find the curl
#' options by looking at [curl::curl_options()]. Pass them
#' on into any \pkg{nneo} function call.
#'
#' @section User-Agent:
#' A user agent string is sent in every request just to let the NEON
#' servers know that the request is coming from R and from this package.
#' No personal info is shared, just the package name and version for
#' \pkg{nneo} and the underlying HTTP packages (\pkg{crul}, \pkg{curl})
#'
#' @name nneo-package
#' @aliases nneo
#' @docType package
#' @keywords package
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @author Josh Roberti \email{jaroberti87@@gmail.com}
NULL
