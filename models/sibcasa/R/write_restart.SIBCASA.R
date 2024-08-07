#' Write restart template for SDA
#'
#' Write restart files for model
#'
#' @author Alexey Shiklomanov
#'
#' @param start.time   Time of current assimilation step
#' @param stop.time    Time of next assimilation step
#' @param new.state    Analysis state matrix returned by \code{sda.enkf}
#' @inheritParams read_restart.SIBCASA
#'
#' @export
write_restart.SIBCASA <- function(outdir,
                                  runid,
                                  start.time,
                                  stop.time,
                                  settings,
                                  new.state) {
  PEcAn.logger::logger.severe("NOT IMPLEMENTED")
}
