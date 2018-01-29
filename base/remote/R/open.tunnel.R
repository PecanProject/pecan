#' Open an SSH tunnel, prompting for passwords as needed
#'
#' @param remote_host name of remote server to connect to (e.g. geo.bu.edu)
#' @param tunnel_dir  directory to store tunnel file in, typically from settings$host
#' @param user        username on remote_host
#' @param password    password on remote_host
#' @param wait.time   how long to give system to connect before deleting password (seconds)
#' @param tunnel_script Path to sshtunnel.sh script file for opening tunnel
#'
#' @return numeric giving ssh PID if configured, otherwise logical with TRUE = success
#' @export
open_tunnel <- function(remote_host, user = NULL, password = NULL, tunnel_dir = "~/.pecan/tunnel/",
                        wait.time = 15, tunnel_script = '~/pecan/web/sshtunnel.sh'){

  ## make sure local tunnel directory exists
  dir.create(tunnel_dir)

  ## get username if not provided
  if(is.null(user)){
    user <- readline("Username:: ")
  }

  ## get password if not provided
  if(is.null(password)){
    password <- getPass::getPass()
  }

  sshTunnel   <- file.path(tunnel_dir, "tunnel")
  sshPID      <- file.path(tunnel_dir, "pid")
  sshPassFile <- file.path(tunnel_dir, "password")

  if(file.exists(sshTunnel)){
    PEcAn.logger::logger.warn("Tunnel already exists. If tunnel is not working try calling kill.tunnel then reopen")
    return(TRUE)
  }

  ## write password to temporary file
  PEcAn.logger::logger.warn(sshPassFile)
  write(password, file = sshPassFile)

#  start <- system(paste0("ssh -nN -o ControlMaster=yes -o ControlPath=",sshTunnel," -l ",user," ",remote_host),wait = FALSE,input = password)
#  Sys.sleep(5)
#  end <- system2("send",password)

  stat <- system(paste(tunnel_script, remote_host, user, tunnel_dir), wait=FALSE)

  ##wait for tunnel to connect
  Sys.sleep(wait.time)

  if (file.exists(sshPassFile)) {
    file.remove(sshPassFile)
    PEcAn.logger::logger.error("Tunnel open failed")
    return(FALSE)
  }

  if (file.exists(sshPID)) {
    pid <- readLines(sshPID, n = -1)
    return(as.numeric(pid))
  } else {
    return(TRUE)
  }

}
