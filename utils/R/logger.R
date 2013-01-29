#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

.local <- new.env() 
.local$filename <- NA
.local$console  <- TRUE
.local$DEBUG    <- TRUE
.local$INFO     <- TRUE
.local$WARN     <- TRUE
.local$ERROR    <- TRUE

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
	if (.local[[level]]) {
		dump.frames(dumpto="dump.log")
		calls <- names(dump.log)
	    func <- sub("\\(.*\\)", "", tail(calls[-(which(substr(calls, 0, 3) == "log"))], 1))
	    if (length(func) == 0) {
	    	func <- "console"
	    }
		text <- sprintf("%s %-5s [%s] : %s\n", Sys.time(), level, func, paste(msg, ...))
		if (.local$console) {
			cat(text)
		}
		if (!is.na(.local$filename)) {
			cat(text, file=.local$filename, append=TRUE)
		}
	}
}

##' Configure logging level.
##' 
##' This will configure the logger level. This allows to turn DEBUG, INFO,
##' WARN and ERROR messages on and off.
##'
##' @param level the level of the message (DEBUG, INFO, WARN, ERROR)
##' @param enable wheter or not the messages should be printed at this level.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.enable("DEBUG", TRUE)
##' }
logger.enable <- function(level, enable=TRUE) {
	if (toupper(level) == "DEBUG") {
		.local$DEBUG = enable
	} else if (toupper(level) == "INFO") {
		.local$INFO = enable
	} else if (toupper(level) == "WARN") {
		.local$WARN = enable
	} else if (toupper(level) == "ERROR") {
		.local$ERROR = enable
	} else {
		logger.warn("Could not set level", level)
	}
}

##' Configure logging output.
##' 
##' This will configure the logger.
##'
##' @param filename the file to send the log messages to (or NA to not write to file)
##' @param console set to true if the logger should write to the console
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.output(console=FALSE)
##' }
logger.output <- function(filename=.local$filename, console=.local$console) {
	.local$filename <- filename
	.local$console  <- console
}
