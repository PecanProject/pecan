#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

.utils.logger <- new.env() 
.utils.logger$filename <- NA
.utils.logger$console  <- TRUE
.utils.logger$stderr   <- TRUE
.utils.logger$quit     <- FALSE
.utils.logger$level    <- 0
.utils.logger$width    <- ifelse(getOption("width")<10, getOption("width"), getOption("width")-5)

##' Prints a debug message.
##' 
##' This function will print a debug message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.debug("variable", 5)
##' }
logger.debug <- function(msg, ...) {
	logger.message("DEBUG", msg, ...)
}

##' Prints an informational message.
##' 
##' This function will print an informational message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.info("PEcAn version 1.2")
##' }
logger.info <- function(msg, ...) {
	logger.message("INFO", msg, ...)
}

##' Prints a warning message.
##' 
##' This function will print a warning message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.warn("detected NA values")
##' }
logger.warn <- function(msg, ...) {
	logger.message("WARN", msg, ...)
}

##' Prints an error message.
##' 
##' This function will print an error message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.error("system did not converge")
##' }
logger.error <- function(msg, ...) {
	logger.message("ERROR", msg, ...)
}

##' Prints an severe message and stops execution.
##' 
##' This function will print a message and stop execution of the code. This
##' should only be used if the application should terminate. 
##' 
##' set \code{\link{logger.setQuitOnSevere(FALSE)}}. To avoid terminating
##' the session. This is set by default to TRUE if interactive or running
##' inside Rstudio.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.severe("missing parameters")
##' }
logger.severe <- function(msg, ...) {
	logger.message("SEVERE", msg, ...)

	# run option
	error <- getOption("error")
	if (!is.null(error)) {
		eval(error)
	}

	# quit if not interactive, otherwise use stop
	if (.utils.logger$quit) {
     	quit(save="no", status=1)
    } else {
		stop(paste(msg, ...))
    }
}

##' Prints a message at a certain log level.
##' 
##' This function will print a message. This is the function that is responsible for
##' the actual printing of the message.
##'
##' This is a place holder and will be later filled in with a more complex logging set
##' @param level the level of the message (DEBUG, INFO, WARN, ERROR)
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.message("DEBUG", "variable", 5)
##' }
logger.message <- function(level, msg, ...) {
	if (logger.getLevelNumber(level) >= .utils.logger$level) {
		dump.frames(dumpto="dump.log")
		calls <- names(dump.log)
	    func <- sub("\\(.*", "", tail(calls[-(which(substr(calls, 0, 3) == "log"))], 1))
	    if (length(func) == 0) {
	    	func <- "console"
	    }
                
                stamp.text <- sprintf("%s %-6s [%s] :", Sys.time(), level, func)
                long.msg <- paste(c(msg, ...), collapse=" ")
                if(nchar(long.msg) > 20){
                    new.msg <- paste("\n", strwrap(long.msg, width=.utils.logger$width, indent=2, exdent=2), collapse = " ")
                } else {
                    new.msg <- long.msg
                }
                text <- paste(stamp.text, new.msg, "\n")

		if (.utils.logger$console) {
			if (.utils.logger$stderr) {
				cat(text, file=stderr())
			} else {
				cat(text, file=stdout())
			}

		}
		if (!is.na(.utils.logger$filename)) {
			cat(text, file=.utils.logger$filename, append=TRUE)
		}
	}
}

##' Configure logging level.
##' 
##' This will configure the logger level. This allows to turn DEBUG, INFO,
##' WARN and ERROR messages on and off.
##'
##' @param level the level of the message (ALL, DEBUG, INFO, WARN, ERROR, OFF)
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setLevel("DEBUG")
##' }
logger.setLevel <- function(level) {
	.utils.logger$level = logger.getLevelNumber(level)
}

##' Returns numeric value for string
##'
##' Given the string representation this will return the numeric value
##' ALL   =  0
##' DEBUG = 10
##' INFO  = 20
##' WARN  = 30
##' ERROR = 40
##' ALL   = 99
##'
##' @return level the level of the message
##' @author Rob Kooper
logger.getLevelNumber <- function(level) {
	if (toupper(level) == "ALL") {
		return(0)
	} else if (toupper(level) == "DEBUG") {
		return(10)
	} else if (toupper(level) == "INFO") {
		return(20)
	} else if (toupper(level) == "WARN") {
		return(30)
	} else if (toupper(level) == "ERROR") {
		return(40)
	} else if (toupper(level) == "SEVERE") {
		return(40)
	} else if (toupper(level) == "OFF") {
		return(60)
	} else {
		logger.warn(level, " is not a valid value, setting level to INFO")
		return(logger.getLevelNumber("INFO"))
	}	
}

##' Get configured logging level.
##' 
##' This will return the current level configured of the logging messages
##'
##' @return level the level of the message (ALL, DEBUG, INFO, WARN, ERROR, OFF)
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.getLevel()
##' }
logger.getLevel <- function() {
	if (.utils.logger$level < 10) {
		return("ALL")
	} else if (.utils.logger$level < 20) {
		return("DEBUG")
	} else if (.utils.logger$level < 30) {
		return("INFO")
	} else if (.utils.logger$level < 40) {
		return("WARN")
	} else if (.utils.logger$level < 50) {
		return("ERROR")
	} else if (.utils.logger$level < 60) {
		return("SEVERE")
	} else {
		return("OFF")
	}
}

##' Configure logging to console.
##' 
##' Should the logging to be printed to the console or not.
##'
##' @param console set to true to print logging to console.
##' @param stderr set to true (default) to use stderr instead of stdout for logging
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setUseConsole(TRUE)
##' }
logger.setUseConsole <- function(console, stderr=TRUE) {
	.utils.logger$console <- console
	.utils.logger$stderr <- stderr
}

##' Configure logging output filename.
##' 
##' The name of the file where the logging information should be written to.
##'
##' @param filename the file to send the log messages to (or NA to not write to file)
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setOutputFile("pecan.log")
##' }
logger.setOutputFile <- function(filename) {
	.utils.logger$filename <- filename
}

##' Configure whether severe should quit.
##' 
##' The default is for a non-interactive session to quit. Setting this to false is
##' especially useful for running tests when placed in \code{inst/tests/test.<fn>.R}, 
##' but is not passed from \code{tests/run.all.R}.
##'
##' @param severeQuits should R quit on a severe error.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setQuitOnSevere(FALSE)
##' }
logger.setQuitOnSevere <- function(severeQuits) {
	.utils.logger$quit = severeQuits
}

##' Configure the number of chars per line
##' 
##' The default is for 60 chars per line. Setting this to any value will
##' wrap the line when printing a message at that many chars.
##'
##' @param width number of chars to print before wrapping to next line.
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{
##' logger.setWidth(70)
##' }
logger.setWidth <- function(width) {
  .utils.logger$width = width
}
