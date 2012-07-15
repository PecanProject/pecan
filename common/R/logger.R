log.variables <- list("filename"="", "console"=TRUE, "DEBUG"=TRUE, "INFO"=TRUE, "WARN"=TRUE, "ERROR"=TRUE)

##' Prints a debug message.
##' 
##' This function will print a debug message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' log.debug("variable", 5)
log.debug <- function(msg, ...) {
	log.message("DEBUG", msg, ...)
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
##' log.info("PEcAn version 1.2")
log.info <- function(msg, ...) {
	log.message("INFO", msg, ...)
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
##' log.warn("detected NA values")
log.warn <- function(msg, ...) {
	log.message("WARN", msg, ...)
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
##' log.error("system did not converge")
log.error <- function(msg, ...) {
	log.message("ERROR", msg, ...)
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
##' log.message("DEBUG", "variable", 5)
log.message <- function(level, msg, ...) {
	if (log.variables[[level]]) {
		text <- sprintf("%-5s [%s] : %s", level, Sys.time(), paste(msg, ...))
		if (log.variables$console) {
			print(text)
		}
		if (log.variables$filename != "") {
			cat(text, file=log.variables$filename, append=TRUE)
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
##' log.enable("DEBUG", TRUE)
log.enable <- function(level, enable=TRUE) {
	if (level == "DEBUG") {
		log.variables$DEBUG <<- enable
	} else if (level == "INFO") {
		log.variables$INFO <<- enable
	} else if (level == "WARN") {
		log.variables$WARN <<- enable
	} else if (level == "ERROR") {
		log.variables$ERROR <<- enable
	} else {
		log.warn("Could not set level", level)
	}
}

##' Configure logging output.
##' 
##' This will configure the logger.
##'
##' @param filename the file to send the log messages to.
##' @param console set to true if the logger should write to the console
##' @export
##' @author Rob Kooper
##' @examples
##' log.output(console=FALSE)
log.output <- function(filename=log.variables$filename, console=log.variables$console) {
	log.variables$console <<- console
	log.variables$filename <<- filename
}
