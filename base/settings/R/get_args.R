#' Get Args
#'
#' Used in web/workflow.R to parse command line arguments.
#' See also https://github.com/PecanProject/pecan/pull/2626.
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{./web/workflow.R -h}
get_args <- function () {
    option_list = list(
      optparse::make_option(
        c("-s", "--settings"),
        default = ifelse(Sys.getenv("PECAN_SETTINGS") != "", 
                         Sys.getenv("PECAN_SETTINGS"), "pecan.xml"),
        type = "character",
        help = "Settings XML file",
        metavar = "FILE",
      ),
      optparse::make_option(
        c("-c", "--continue"),
        default = FALSE,
        action = "store_true",
        type = "logical",
        help = "Continue processing",
      )
    )

    parser <- optparse::OptionParser(option_list = option_list)
    args <- optparse::parse_args(parser)

    if (!file.exists(args$settings)) {
        optparse::print_help(parser)
        stop(sprintf('--settings "%s" not a valid file\n', args$settings))
    }

    return(invisible(args))
}