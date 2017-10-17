#--------------------------------------------------------------------------------#
# Functions used to write STATUS used by history
#--------------------------------------------------------------------------------#
##' @export
##' @name status.start
##' @title status.start
##' @description PEcAn workflow status tracking: start module
##' @author Rob Kooper
status.start <- function(name) {
  if (exists("settings")) {
    cat(paste(name, format(Sys.time(), "%F %T"), sep = "\t"), file = file.path(settings$outdir, 
                                                                               "STATUS"), append = TRUE)
  }
}

##' @name status.end
##' @title status.end
##' @description PEcAn workflow status tracking: end module
##' @author Rob Kooper
##' @export
status.end <- function(status = "DONE") {
  if (exists("settings")) {
    cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep = "\t"), file = file.path(settings$outdir, 
                                                                                           "STATUS"), append = TRUE)
  }
}

##' @name status.skip
##' @title status.skip
##' @description PEcAn workflow status tracking: skip module
##' @author Rob Kooper
##' @export
status.skip <- function(name) {
  if (exists("settings")) {
    cat(paste(name, 
              format(Sys.time(), "%F %T"), "", 
              format(Sys.time(), "%F %T"), 
              "SKIPPED", "\n", sep = "\t"), 
        file = file.path(settings$outdir, "STATUS"), 
        append = TRUE)
  }
} # status.skip

##' @name status.check
##' @title status.check
##' @description PEcAn workflow status tracking: check module status
##' @author Rob Kooper
##' @export
status.check <- function(name) {
  if (!exists("settings")) 
    return(0)
  status.file <- file.path(settings$outdir, "STATUS")
  if (!file.exists(status.file)) {
    return(0)
  }
  status.data <- utils::read.table(status.file, row.names = 1, header = FALSE, sep = "\t",
                            quote = "", fill = TRUE)
  if (!name %in% row.names(status.data)) {
    return(0)
  }
  status.data[name, ]
  if (is.na(status.data[name, 3])) {
    PEcAn.logger::logger.warn("UNKNOWN STATUS FOR", name)
    return(0)
  }
  if (status.data[name, 3] == "DONE") {
    return(1)
  }
  if (status.data[name, 3] == "ERROR") {
    return(-1)
  }
  return(0)
} # status.check
