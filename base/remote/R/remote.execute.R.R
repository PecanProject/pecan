#' Execute command remotely
#'
#' Executes the given command on the remote host using ssh. If the user is set
#' the system will login as the given user. If the host given is the local
#' machine it will execute the command locally without ssh.
#'
#' @title Execute command remotely
#' @param script the script to be invoked, as a list of commands.
#' @param args a character vector of arguments to command.
#' @param host settings host list
#' @param user the username to use for remote login
#' @param verbose should the output be printed to the console
#' @return the captured output of the command (both stdout and stderr)
#' @author Rob Kooper
#' @export
#' @examples
#' \dontrun{
#'   remote.execute.R('list.files()', host='localhost', verbose=FALSE)
#' }
remote.execute.R <- function(script, host = "localhost", user = NA, verbose = FALSE,
                             R = "R", scratchdir = tempdir()) {
  if (is.character(host)) {
    host <- list(name = host)
  }
  uuid <- paste0("pecan-", paste(sample(c(letters[1:6], 0:9), 30, replace = TRUE),
                                 collapse = ""))
  tmpfile <- file.path(scratchdir, uuid)
  input <- c(paste0("remotefunc <- function() {", script, "}"),
             "remoteout <- remotefunc()",
             "print(remoteout)",
             paste0("fp <- file('", tmpfile, "', 'w')"),
             paste0("ign <- serialize(remoteout, fp)"),
             "close(fp)")
  verbose <- ifelse(as.logical(verbose), "", FALSE)
  if (is.localhost(host)) {
    if (R == "R") {
      Rbinary <- file.path(Sys.getenv("R_HOME"), "bin", "R")
      if (file.exists(Rbinary)) {
        R <- Rbinary
      }
    }
    result <- try(system2(R, "--no-save","--no-restore", stdout = verbose, stderr = verbose,
                          input = input))
    print(result)
    if (!file.exists(tmpfile)) {
      fp <- file(tmpfile, "w")
      serialize(result, fp)
      close(fp)
    }
    ## get result
    fp <- file(tmpfile, "r")
    result <- unserialize(fp)
    close(fp)
    file.remove(tmpfile)
    return(invisible(result))

  } else {
    remote <- c(host$name)
    if (!is.null(host$tunnel)) {
      if (!file.exists(host$tunnel)) {
        PEcAn.logger::logger.severe("Could not find tunnel", host$tunnel)
      }
      remote <- c("-o", paste0("ControlPath=\"", host$tunnel, "\""), remote)
    } else if (!is.null(host$user)) {
      remote <- c("-l", host$user, remote)
    }
    PEcAn.logger::logger.debug(paste(c("ssh", "-T", remote, R), collapse = " "))
    result <- system2("ssh", c("-T", remote, R, "--no-save","--no-restore"), stdout = verbose,
                      stderr = verbose, input = input)
    remote.copy.from(host, tmpfile, uuid)
    remote.execute.cmd(host, "rm", c("-f", tmpfile))
    # load result
    fp <- file(uuid, "r")
    result <- unserialize(fp)
    close(fp)
    file.remove(uuid)
    return(invisible(result))
  }


} # remote.execute.R

