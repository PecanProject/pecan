#-------------------------------------------------------------------------------
# Copyright (c) 2014 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Remote utilities. Allows for the following functionality
# - execute system call on remote machine
# - execute R call on remote machine, returns result
# - copy files to/from remote machines
#-------------------------------------------------------------------------------

#' Execute command remotely
#'
#' Executes the given command on the remote host using ssh. If the user is set
#' the system will login as the given user. If the host given is the local
#' machine it will execute the command locally without ssh.
#' 
#' @title Execute command remotely
#' @param command the system command to be invoked, as a character string.
#' @param host host structure to execute command on
#' @param args a character vector of arguments to command.
#' @param stderr should stderr be returned as well.
#' @return the captured output of the command (both stdout and stderr)
#' @author Rob Kooper
#' @export
#' @examples 
#' \dontrun{
#'   host <- list(name="geo.bu.edu", user="kooper", tunnel="/tmp/geo.tunnel")
#'   pritn(remote.execute.cmd(host, "ls", c("-l", "/"), stderr=TRUE))
#' }
remote.execute.cmd <- function(host, cmd, args=character(), stderr=FALSE) {
  if ((host$name == "localhost") || (host$name == fqdn())) {
    logger.debug(paste(cmd, args))
    system2(cmd, args, stdout=TRUE, stderr=as.logical(stderr))
  } else {
    remote <- c(host$name)
    if (!is.null(host$tunnel)) {
      if (!file.exists(host$tunnel)) logger.severe("Could not find tunnel", host$tunnel)
      remote <- c("-o", paste0('ControlPath="', host$tunnel, '"'), remote)
    } else if (!is.null(host$user)) {
      remote <- c("-l", host$user, remote)
    }
    logger.debug(paste(c("ssh", "-T", remote, cmd, args), collapse=" "))
    system2('ssh', c('-T', remote, cmd, args), stdout=TRUE, stderr=as.logical(stderr))
  }
}


#' Copy file/dir from remote server to local server
#'
#' Copies the file/dir from the remote server to the local server. If the dst
#' is a folder it will copy the file into that folder.
#' 
#' @title Copy file from remote to local
#' @param host list with server, user and optionally tunnel to use.
#' @param src remote file/dir to copy
#' @param dst local file/dir to copy to
#' @param delete in case of local dir should all non-existent files be removed
#' @param stderr should stderr be returned
#' @return output of command executed
#'
#' @author Rob Kooper
#' @export
#' @examples 
#' \dontrun{
#'   host <- list(name="geo.bu.edu", user="kooper", tunnel="/tmp/geo.tunnel")
#'   remote.copy.from(host, "/tmp/kooper", "/tmp/geo.tmp", delete=TRUE)
#' }
remote.copy.from <- function(host, src, dst, delete=FALSE, stderr=FALSE) {
  args <- c("-a", "-q")
  if (as.logical(delete)) args <- c(args, "--delete")
  if (is.localhost(host)) {
    args <- c(args, src, dst)
  } else {
    if (!is.null(host$tunnel)) {
      if (!file.exists(host$tunnel)) logger.severe("Could not find tunnel", host$tunnel)
      args <- c(args, "-e", paste0('ssh -o ControlPath="', host$tunnel, '"', collapse=""))
      args <- c(args, paste0(host$name, ":", src), dst)
    } else if (!is.null(host$user)) {
      args <- c(args, paste0(host$user, "@", host$name, ":", src), dst)
    } else {
      args <- c(args, paste0(host$name, ":", src), dst)
    }
  }
  logger.debug("rsync", shQuote(args))
  system2('rsync', shQuote(args), stdout=TRUE, stderr=as.logical(stderr))
}

#' Copy file/dir to remote server from local server
#'
#' Copies the file/dir to the remote server from the local server. If the dst
#' is a folder it will copy the file into that folder.
#' 
#' @title Copy file from remote to local
#' @param host list with server, user and optionally tunnel to use.
#' @param src local file/dir to copy
#' @param dst remote file/dir to copy to
#' @param delete in case of local dir should all non-existent files be removed
#' @param stderr should stderr be returned
#' @return output of command executed
#'
#' @author Rob Kooper
#' @export
#' @examples 
#' \dontrun{
#'   host <- list(name="geo.bu.edu", user="kooper", tunnel="/tmp/geo.tunnel")
#'   remote.copy.to(host, "/tmp/kooper", "/tmp/kooper", delete=TRUE)
#' }
remote.copy.to <- function(host, src, dst, delete=FALSE, stderr=FALSE) {
  args <- c("-a", "-q")
  if (as.logical(delete)) args <- c(args, "--delete")
  if (is.localhost(host)) {
    args <- c(args, src, dst)
  } else {
    if (!is.null(host$tunnel)) {
      if (!file.exists(host$tunnel)) logger.severe("Could not find tunnel", host$tunnel)
      args <- c(args, "-e", paste0('ssh -o ControlPath="', host$tunnel, '"', collapse=""))
      args <- c(args, src, paste0(host$name, ":", dst))
    } else if (!is.null(host$user)) {
      args <- c(args, src, paste0(host$user, "@", host$name, ":", dst))
    } else {
      args <- c(args, src, paste0(host$name, ":", dst))
    }
  }
  logger.debug("rsync", shQuote(args))
  system2('rsync', shQuote(args), stdout=TRUE, stderr=as.logical(stderr))
}

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
  if (is.character(host)) {
    (host == "localhost") || (host == fqdn())
  } else if (is.list(host)) {
    (host$name == "localhost") || (host$name == fqdn())
  } else {
      FALSE
  }
}

#host <- list(name="geo.bu.edu", user="kooper", tunnel="/tmp/geo.tunnel")
#out <- remote.copy.to(host, "/tmp/kooper/", "/tmp/kooper/", delete=TRUE, stderr=TRUE)
#print(out)


#' Execute command remotely
#'
#' Executes the given command on the remote host using ssh. If the user is set
#' the system will login as the given user. If the host given is the local
#' machine it will execute the command locally without ssh.
#' 
#' @title Execute command remotely
#' @param script the script to be invoked, as a list of commands.
#' @param args a character vector of arguments to command.
#' @param host host to execute command on
#' @param user the username to use for remote login
#' @param verbose should the output be printed to the console
#' @return the captured output of the command (both stdout and stderr)
#' @author Rob Kooper
#' @export
#' @examples 
#' \dontrun{
#'   remote.execute.R("list.files()", host="localhost", verbose=FALSE)
#' }
remote.execute.R <- function(script, host="localhost", user=NA, verbose=FALSE, R="R") {
  uuid <- paste0("pecan-", paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse=""))
  tmpfile <- file.path("/tmp", uuid)
  input <- c(paste0("remotefunc <- function() {"),
             script,
             "}",
             "remoteout <- remotefunc()",
             paste0("fp <- file('", tmpfile, "', 'w')"),
             paste0("ign <- serialize(remoteout, fp)"),
             "close(fp)")
  verbose <- ifelse(as.logical(verbose), "", FALSE)
  if ((host == "localhost") || (host == fqdn())) {
    if (R == "R") {
      Rbinary <- file.path(Sys.getenv("R_HOME"), "bin", "R")
      if (file.exists(Rbinary)) {
        R <- Rbinary
      }
    }
    result = try(system2(R, "--vanilla", stdout=verbose, stderr=verbose, input=input))
    print(result)
    if(!file.exists(tmpfile)){fp <- file(tmpfile, 'w');serialize(result,fp);close(fp)}
  } else {
    remote <- ifelse(is.na(user), host, paste(user, host, sep='@'))[[1]]
    result = system2('ssh', c('-T', remote, R, "--vanilla"), stdout=verbose, stderr=verbose, input=input)
    remote.copy.file(host, tmpfile, user, "localhost", tmpfile)
    remote.execute.cmd("rm", c("-f", tmpfile), host, user)
  }
  
  # load result
  fp <- file(tmpfile, 'r')
  result <- unserialize(fp)
  close(fp)
  unlink(tmpfile)
  invisible(result)
}
