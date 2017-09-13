##' @export
##' @name kill.tunnel
##' @title kill.tunnel
##' @description kill tunnel to remote machine
##' @author Rob Kooper
kill.tunnel <- function(settings,exe=TRUE,data=TRUE) {
  if (exe && !is.null(settings$host$tunnel)) {
    pidfile <- file.path(dirname(settings$host$tunnel), "pid")
    pid <- readLines(pidfile)
    print(paste("Killing tunnel with PID", pid))
    tools::pskill(pid)
    file.remove(pidfile)
  }
  if (data && !is.null(settings$host$data_tunnel)) {
    pidfile <- file.path(dirname(settings$host$data_tunnel), "pid")
    pid <- readLines(pidfile)
    print(paste("Killing tunnel with PID", pid))
    tools::pskill(pid)
    file.remove(pidfile)
  }
} # kill.tunnel
