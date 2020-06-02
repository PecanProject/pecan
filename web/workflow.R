#!/usr/bin/env Rscript

suppressMessages(library("optparse"))
suppressMessages(library("R.utils"))
suppressMessages(library("XML"))
#library(PEcAn.all)
#library(PEcAn.utils)
#library(RCurl)

# --------------------------------------------------
get_args <- function () {
    option_list = list(
      make_option(
        c("-s", "--settings"),
        default = ifelse(Sys.getenv("PECAN_SETTINGS") != "", 
                         Sys.getenv("PECAN_SETTINGS"), "pecan.xml"),
        type = "character",
        help = "Settings XML file",
        metavar = "FILE",
      )
    )

    parser = OptionParser(option_list = option_list)
    args = parse_args(parser)

    if (!file.exists(args$settings)) {
        print_help(parser)
        stop(sprintf('--settings "%s" not a valid file\n', args$settings))
    }

    return(invisible(args))
}

# --------------------------------------------------
main <- function () {
    args = get_args()
    print(args)
    print(read.settings(args$settings))
}

# --------------------------------------------------
read.settings <- function (filename) {
    if (!file.exists(filename)) {
        stop(sprintf('"%s" not a valid file\n', filename))
    }

    settings = XML::xmlToList(XML::xmlParse(filename))
    #settings = expandMultiSettings(as.Settings(XML::xmlToList(filename)))
    return(invisible(settings))
}

main()
