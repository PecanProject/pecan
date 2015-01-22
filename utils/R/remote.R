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
#' @param args a character vector of arguments to command.
#' @param host host to execute command on
#' @param user the username to use for remote login
#' @param stderr should stderr be returned as well.
#' @return the captured output of the command (both stdout and stderr)
#' @author Rob Kooper
#' @export
#' @examples 
#' remote.execute.cmd("ls", c("-l", "/"), host="localhost", stderr=TRUE)
remote.execute.cmd <- function(cmd, args=character(), host="localhost", user=NA, stderr=FALSE) {
  if ((host == "localhost") || (host == fqdn())) {
    system2(cmd, args, stdout=TRUE, stderr=as.logical(stderr))
  } else {
    remote <- ifelse(is.na(user), host, paste(user, host, sep='@'))
    system2('ssh', c('-T', remote, cmd, args), stdout=TRUE, stderr=as.logical(stderr))
  }
}

#out <- remote.execute.cmd("ls", c("-l", "/"), host="localhost", stderr=TRUE)
#print(out)

#' Execute command remotely
#'
#' Executes the given command on the remote host using ssh. If the user is set
#' the system will login as the given user. If the host given is the local
#' machine it will execute the command locally without ssh.
#' 
#' @title Execute command remotely
#' @param command the system command to be invoked, as a character string.
#' @param args a character vector of arguments to command.
#' @param host host to execute command on
#' @param user the username to use for remote login
#' @param stderr 
#' @return the captured output of the command (both stdout and stderr)
#' @author Rob Kooper
#' @export
#' @examples 
#' remote.execute.cmd("ls", c("-l", "/"))
remote.copy.file <- function(srchost, srcfiles, srcuser=NA, dsthost="localhost", dstfile=getwd(), dstuser=NA) {
  src <- ifelse(is.na(srcuser), srchost, paste(srcuser, srchost, sep='@'))
  dst <- ifelse(is.na(dstuser), dsthost, paste(dstuser, dsthost, sep='@'))      
  
  if ((srchost == "localhost") || (srchost == fqdn())) {
    if ((dsthost == "localhost") || (dsthost == fqdn())) {
      # local copy
      for (file in srcfiles) {
        print(paste("cp", file, dstfile))
        file.copy(file, dstfile, recursive=TRUE)
      }
    } else {
      # copy files to remote machine
      print(paste("rsync -aq", paste(srcfile), paste(dst, dstfile, sep=":")))
      system2("rsync", c("-aq", paste(srcfile), paste(dst, dstfile, sep=":")))
    }
  } else {
    if ((dsthost == "localhost") || (dsthost == fqdn())) {
      # copy files from remote machine
      for (file in srcfiles) {
        print(paste("rsync -aq", paste(src, file, sep=":"), dstfile))
        system2("rsync", c("-aq", paste(src, file, sep=":"), dstfile))
      }
    } else {
      # all is remote
      for (file in srcfiles) {
        print(paste("rsync -aq", paste(src, file, sep=":"), paste(dst, dstfile, sep=":")))
        system2("rsync", c("-aq", paste(src, file, sep=":"), paste(dst, dstfile, sep=":")))
      }
    }
  }
}

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
#' remote.execute.R("list.files()", host="localhost", verbose=FALSE)
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
    system2(R, "--vanilla", stdout=verbose, stderr=verbose, input=input)
  } else {
    remote <- ifelse(is.na(user), host, paste(user, host, sep='@'))
    system2('ssh', c('-T', remote, R, "--vanilla"), stdout=verbose, stderr=verbose, input=input)
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
