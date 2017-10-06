#' Check if host is local
#'
#' Given the hostname is this the localhost. This returns true if either
#' the value is localhost, or the value is the same as the fqdn.
#'
#' @title Check if local host
#' @param host the hostname to be checked
#' @return true if the host is the local host name
#' @author Rob Kooper
#' @export
#' @examples
#' is.localhost(fqdn())
is.localhost <- function(host) {
  # PEcAn.utils::fqdn() would result in a circular dependency.
  fqdn <- system2("hostname", "-f", stdout = TRUE)
  
  if (is.character(host)) {
    return((host == "localhost") || (host == fqdn))
  } else if (is.list(host)) {
    return((host$name == "localhost") || (host$name == fqdn))
  } else {
    return(FALSE)
  }
} # is.localhost
