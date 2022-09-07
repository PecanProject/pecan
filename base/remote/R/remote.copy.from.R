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
    system2("rsync", shQuote(args), stdout = TRUE, stderr = as.logical(stderr))
  # Informative errors from rsync man page
  msg <-
    switch(
      as.character(attr(out, "status")),
      '0' = "Success",
      '1' = "Syntax or usage error",
      '2' = "Protocol incompatibility",
      '3' = "Errors selecting input/output files, dirs",
      '4' = "Requested action not supported",
      '5' = "Error starting client-server protocol",
      '6' = "Daemon unable to append to log-file",
      '10' = "Error in socket I/O",
      '11' = "Error in file I/O",
      '12' = "Error in rsync protocol data stream",
      '13' = "Errors with program diagnostics",
      '14' = "Error in IPC code",
      '20' = "Received SIGUSR1 or SIGINT",
      '21' = "Some error returned by waitpid()",
      '22' = "Error allocating core memory buffers",
      '23' = "Partial transfer due to error",
      '24' = "Partial transfer due to vanished source files",
      '25' = "The -⁠-⁠max-delete limit stopped deletions",
      '30' = "Timeout in data send/receive",
      '35' = "Timeout waiting for daemon connection"
    )
  PEcAn.logger::logger.debug(paste0("rsync status: ", msg))
} # remote.copy.from
