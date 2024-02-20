#' Copy file/dir from remote server to local server
#'
#' Copies the file/dir from the remote server to the local server. If the dst
#' is a folder it will copy the file into that folder.
#'
#' @title Copy file from remote to local
#' @param host list with server, user and optionally tunnel to use.
#' @param src remote file/dir to copy
#' @param dst local file/dir to copy to
#' @param options to be passed to rsync command, if nothing is specified everything will be rsynced
#' @param delete in case of local dir should all non-existent files be removed
#' @param stderr should stderr be returned
#' @return output of command executed
#'
#' @author Rob Kooper
#' @export
#' @examples
#' \dontrun{
#'   host <- list(name='geo.bu.edu', user='kooper', tunnel='/tmp/geo.tunnel')
#'   remote.copy.from(host, '/tmp/kooper', '/tmp/geo.tmp', delete=TRUE)
#' }
remote.copy.from <- function(host, src, dst, options = NULL, delete = FALSE, stderr = FALSE) {
  args <- c("-az", "-q", options)
  if (as.logical(delete)) {
    args <- c(args, "--delete")
  }
  if (is.localhost(host)) {
    args <- c(args, src, dst)
  } else {
    tunnel <- host$tunnel
    if(!is.null(host$data_tunnel)) tunnel <- host$data_tunnel
    hostname <- host$name
    if(!is.null(host$data_hostname)) hostname <- host$data_hostname
    if (!is.null(tunnel)) {
      if (!file.exists(tunnel)) {
        PEcAn.logger::logger.severe("Could not find tunnel", tunnel)
      }
      args <- c(args, "-e", paste0("ssh -o ControlPath=\"", tunnel, "\"",
                                   collapse = ""))
      args <- c(args, paste0(hostname, ":", src), dst)
    } else if (!is.null(host$user)) {
      args <- c(args, paste0(host$user, "@", hostname, ":", src), dst)
    } else {
      args <- c(args, paste0(hostname, ":", src), dst)
    }
  }
  PEcAn.logger::logger.debug("rsync", shQuote(args))
  out <-
    system2("rsync", shQuote(args), stdout = "", stderr = as.logical(stderr))
  if (out != 0) {
    PEcAn.logger::logger.severe(paste0("rsync status: ", out))
  } else {
    PEcAn.logger::logger.info("rsync status: success!")
  }
} # remote.copy.from
