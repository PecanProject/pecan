#' Timezone Hour
#'
#' Returns the number of hours offset to UTC for a timezone.
#' @author Rob Kooper
#' @param timezone to be converted
#' @return hours offset of the timezone
#' @examples
#' \dontrun{
#' timezone_hour('America/New_York')
#' }
#' @export
timezone_hour <- function(timezone) {
  if (is.numeric(timezone)) {
    return(timezone)
  } else {
    return(tryCatch(stringi::stri_timezone_info(timezone)$RawOffset,
                    error=function(e) NaN))
  }
}

