#' Copy file/dir to remote server from local server
#'
#' Copies the file/dir to the remote server from the local server. If the dst
#' is a folder it will copy the file into that folder.
#'
#' @inheritParams remote.execute.cmd
#' @param src local file/dir to copy
#' @param dst remote file/dir to copy to
#' @param options additional arguments to be passed to rsync command
#' @param delete in case of local dir should all non-existent files be removed
#' @return output of command executed
#'
#' @author Rob Kooper
#' @export
#' @examples
#' \dontrun{
#'   host <- list(name='geo.bu.edu', user='kooper', tunnel='/tmp/geo.tunnel')
#'   remote.copy.to(host, '/tmp/kooper', '/tmp/kooper', delete=TRUE)
#' }
remote.copy.to <- function(host, src, dst, options = NULL, delete = FALSE, stderr = FALSE) {
  args <- c("-a", "-q", options)
  if (as.logical(delete)) {
    args <- c(args, "--delete")
  }
  if (is.null(host)) {
    PEcAn.logger::logger.severe("`host` object passed to the function is NULL : Try passing a valid host object")
  }
  if (is.localhost(host)) {
    args <- c(args, src, dst)
  } else {
    if (is.null(host$name) || host$name == "") {
      PEcAn.logger::logger.severe("`name` parameter in the `host` object is NULL or empty : Try passing a valid host object")
    }
    tunnel <- host$tunnel
    if (!is.null(host$data_tunnel)) {
      tunnel <- host$data_tunnel
    }
    hostname <- host$name
    if (!is.null(host$data_hostname)) {
      hostname <- host$data_hostname
    }
    if (!is.null(tunnel)) {
      if (!file.exists(tunnel)) {
        PEcAn.logger::logger.severe("Could not find tunnel", tunnel)
      }
      args <- c(args, "-e", paste0("ssh -o ControlPath=\"", tunnel, "\"",
                                   collapse = ""))
      args <- c(args, src, paste0(hostname, ":", dst))
    } else if (!is.null(host$user)) {
      args <- c(args, src, paste0(host$user, "@", hostname, ":", dst))
    } else {
      args <- c(args, src, paste0(hostname, ":", dst))
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
} # remote.copy.to
