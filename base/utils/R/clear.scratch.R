
#' Removes previous model run output from worker node local scratch directories on EBI-CLUSTER
#'
#' @author Shawn Serbin
#' @param settings list of PEcAn settings. Only \code{settings$host$name} is used
#' @return nothing
#' @export
#' @examples
#' \dontrun{
#' clear.scratch(settings)
#' }
clear.scratch <- function(settings) {
  
  ### Setup script
  clear.scratch <- system.file("clear.scratch.sh", package = "PEcAn.utils")
  host  <- settings$host
  nodes <- paste0("all.q@compute-0-", seq(0, 24, 1), ".local")
  
  if (any(grep("cluster", host$name))) {
    for (i in nodes) {
      print(" ")
      print(paste("----- Removing output on node: ", i, sep = ""))
      system(paste0("ssh -T ", settings$host$name, " qlogin -q ", i, " < ", clear.scratch))
      print(" ")
    }  ### End of for loop
    
  } else {
    print("---- No output to delete.  Output host is not EBI-CLUSTER ----")
  }  ### End of if/else
} # clear.scratch
