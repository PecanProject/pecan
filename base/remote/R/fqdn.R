##' Returns the fully qualified hostname. This is potentially different from `Sys.info()['nodename']`
##' which can return just the hostname part and not the domain as well. For example the machine
##' pecan.ncsa.illinois.edu will return just that as fqdn but only pecan for hostname.
##'
##' @title Returns the fully qualified hostname.
##' @name fqdn
##' @author Rob Kooper
##' @return fully qualified hostname
##' @export
##' @examples
##' fqdn()
fqdn <- function() {
  if (Sys.getenv("FQDN") == "") {
    fqdn <- as.character(Sys.info()["nodename"])
  } else {
    fqdn <- Sys.getenv("FQDN")
  }
  return(fqdn)
} # fqdn
