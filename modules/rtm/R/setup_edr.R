#' Setup EDR run
#'
#' Using an existing ED2IN file as a template, create a new ED2IN and history 
#' file configured for running EDR.
#'
#' @param ed2in ED2IN list object (see [PEcAn.ED2::read_ed2in]).
#' @param output_dir Directory in which run files will be stored
#' @param datetime Date time object (or compliant string) at which to run EDR. 
#' Defaults to 12 noon on start date in ED2IN.
#' @param ... Additional arguments passed on to `PEcAn.ED2::modify_ed2in`
#' @return Path to EDR-configured ED2IN file.
#' @author Alexey Shiklomanov
#' @export
setup_edr <- function(ed2in, output_dir,
                      datetime = ISOdatetime(ed2in[["IYEARA"]],
                                             ed2in[["IMONTHA"]],
                                             ed2in[["IDATEA"]],
                                             12, 00, 00, tz = "UTC"),
                      ...) {

  hour <- as.numeric(strftime(datetime, "%H", tz = "UTC"))
  if (hour < 8 | hour > 17) {
    PEcAn.logger::logger.warn(
      "Starting hour ", hour,
      " is not between 8am and 5pm. ",
      "It is generally a good idea to run EDR during the day to get good results."
    )
  }

  dir.create(output_dir, showWarnings = FALSE)
  nextday <- as.POSIXct(datetime, tz = "UTC") + 86400   # Add one day

  history_prefix <- EDR.preprocess.history(
    history.path = dirname(ed2in$SFILOUT),
    history.prefix = basename(ed2in$SFILOUT),
    datetime = datetime,
    output.path = output_dir
  )

  ed2in_edr <- PEcAn.ED2::modify_ed2in(
    ed2in,
    start_date = datetime,
    end_date = nextday,
    output_dir = output_dir,
    run_dir = output_dir,
    runtype = "HISTORY",
    SFILIN = history_prefix,
    ...
  )

  PEcAn.ED2::check_ed2in(ed2in_edr)

  ed2in_edr_path <- file.path(output_dir, "ED2IN")
  PEcAn.ED2::write_ed2in(ed2in_edr, ed2in_edr_path)

  ed2in_edr_path
}
