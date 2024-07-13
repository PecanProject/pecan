#' Execute command remotely
#'
#' Executes the given command on the remote host using ssh. If the user is set
#' the system will login as the given user. If the host given is the local
#' machine it will execute the command locally without ssh.
#'
#' @title Execute command remotely
#' @param cmd the system command to be invoked, as a character string.
#' @param host host structure to execute command on
#' @param args a character vector of arguments to command.
#' @param stderr should stderr be returned as well.
#' @return the captured output of the command (both stdout and stderr)
#' @author Rob Kooper
#' @export
#' @examples
#' \dontrun{
#'   host <- list(name='geo.bu.edu', user='kooper', tunnel='/tmp/geo.tunnel')
#'   print(remote.execute.cmd(host, 'ls', c('-l', '/'), stderr=TRUE))
#' }
remote.execute.cmd <- function(host, cmd, args = character(), stderr = FALSE) {
  if(is.null(host)) {
    PEcAn.logger::logger.severe("`host` cannot be `NULL` for remote execution")
  }
  
  if (is.character(host)) {
    host <- list(name = host)
  }

  if (is.localhost(host)) {
    PEcAn.logger::logger.debug(paste(c(cmd, args), collapse = ' '))
    system2(cmd, args, stdout = TRUE, stderr = as.logical(stderr))
  } else {
    if(is.null(host$name) || host$name == "") {
      PEcAn.logger::logger.severe("`name`parameter in `host` object cannot be `NULL` or empty for remote execution")
    }
    remote <- host$name
    if (!is.null(host$tunnel)) {
      if (!file.exists(host$tunnel)) {
        PEcAn.logger::logger.severe("Could not find tunnel", host$tunnel)
      }
      remote <- c("-o", paste0("ControlPath=\"", host$tunnel, "\""), remote)
    } else if (!is.null(host$user)) {
      remote <- c("-l", host$user, remote)
    }
    PEcAn.logger::logger.debug(paste(c("ssh", "-T", remote, cmd, args), collapse = " "))
    system2("ssh", c("-T", remote, cmd, args), stdout = TRUE, stderr = as.logical(stderr))
  }
} # remote.execute.cmd