##' @export
##' @name kill.tunnel
##' @description kill tunnel to remote machine
##' @author Rob Kooper
kill.tunnel <- function() {
  if (exists("settings") && !is.null(settings$host$tunnel)) {
    pidfile <- file.path(dirname(settings$host$tunnel), "pid")
    pid <- readLines(pidfile)
    print(paste("Killing tunnel with PID", pid))
    tools::pskill(pid)
    file.remove(pidfile)
  }
}