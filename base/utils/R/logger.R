logger_deprecated <- function() {
    warning('Logger functions have moved from PEcAn.utils to PEcAn.logger.', 
            'This usage is deprecated')
}

#' Logger functions (imported temporarily from PEcAn.logger)
#' 
#' @importFrom PEcAn.logger logger.debug
#' @export
logger.debug <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.debug(...)
}

#' @importFrom PEcAn.logger logger.info
#' @export
logger.info <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.info(...)
}

#' @importFrom PEcAn.logger logger.warn
#' @export
logger.warn <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.warn(...)
}

#' @importFrom PEcAn.logger logger.error
#' @export
logger.error <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.error(...)
}

#' @importFrom PEcAn.logger logger.severe
#' @export
logger.severe <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.severe(...)
}

#' @importFrom PEcAn.logger logger.setLevel
#' @export
logger.setLevel <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.setLevel(...)
}

#' @importFrom PEcAn.logger logger.getLevel
#' @export
logger.getLevel <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.getLevel(...)
}

#' @importFrom PEcAn.logger logger.setOutputFile
#' @export
logger.setOutputFile <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.setOutputFile(...)
}

#' @importFrom PEcAn.logger logger.setQuitOnSevere
#' @export
logger.setQuitOnSevere <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.setQuitOnSevere(...)
}

#' @importFrom PEcAn.logger logger.setWidth
#' @export
logger.setWidth <- function(...) {
    logger_deprecated()
    PEcAn.logger::logger.setWidth(...)
}
