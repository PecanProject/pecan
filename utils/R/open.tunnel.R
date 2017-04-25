#' Title
#'
#' @param remote_host name of remote server to connect to (e.g. geo.bu.edu)
#' @param tunnel_dir  directory to store tunnel file in, typically from settings$host
#' @param user        username on remote_host
#' @param password    password on remote_host
#'
#' @return
#' @export
#'
#' @examples
open_tunnel <- function(remote_host,tunnel_dir = "~/.pecan/tunnel/",user=NULL,password=NULL){
  
  ## make sure local tunnel directory exists
  dir.create(tunnel_dir)
  
  ## get username if not provided
  if(is.null(user)){
    user <- readline("Username:: ")
  }
  
  ## get password if not provided
  if(is.null(password)){
    password <- getPass()
  }
  
  sshTunnel   <- file.path(tunnel_dir,"tunnel")
  sshPID      <- file.path(tunnel_dir,"pid")
  sshPassFile <- file.path(tunnel_dir,"password")
  
  if(file.exists(sshTunnel)){
    print("Tunnel already exists. If tunnel is not working try calling kill.tunnel then reopening")
    return(TRUE)
  }
  
  ## write password to temporary file
  write(password,file = sshPassFile)

#  start <- system(paste0("ssh -nN -o ControlMaster=yes -o ControlPath=",sshTunnel," -l ",user," ",remote_host),wait = FALSE,input = password)
#  Sys.sleep(5)
#  end <- system2("send",password)
  
  stat <- system(paste("~/pecan/web/sshtunnel.sh",remote_host,user,tunnel_dir),wait=FALSE)
  
  if(file.exists(sshPassFile)){
    file.remove(sshPassFile)
    return(FALSE)
  }  
  
  if(file.exists(sshPID)){
    pid <- readLines(sshPID,n = -1)
    return(as.numeric(pid))
  } else {
    return(TRUE)
  }
  
}