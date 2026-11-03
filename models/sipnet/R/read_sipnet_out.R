#' Read `sipnet.out` file to `data.frame`
#'
#' @param sipnet_out_file Path to `sipnet.out` file
#'
#' @return `data.frame` of SIPNET output
read_sipnet_out <- function(sipnet_out_file) {
  # SIPNET v1 had a "Notes" comment line before the header; v2 removed it.
  # if the first line starts with "year", there is no Notes line.
  first_line <- readLines(sipnet_out_file, n = 1)
  skip_n <- if (grepl("^year", first_line)) 0 else 1
  # Temporary workaround until
  # https://github.com/PecanProject/sipnet/issues/304 is resolved.
  sipnet_output <- tryCatch({
    utils::read.table(sipnet_out_file, header = TRUE, skip = skip_n, sep = "")
  }, error = function(err) {
    PEcAn.logger::logger.warn(
      "Failed to read using `read.table`. ",
      "Trying to parse output manually."
    )
    raw_lines <- readLines(sipnet_out_file)
    raw_header <- raw_lines[[1 + skip_n]]
    raw_body <- utils::tail(raw_lines, -(1 + skip_n))
    # SIPNET output is right-aligned with the column names in the header.
    # We use this to figure out where the numbers end if there are no spaces.
    token_matches <- gregexpr("\\S+", raw_header, perl = TRUE)
    proc_header <- regmatches(raw_header, token_matches)[[1]]
    col_ends <- token_matches[[1]] + attr(token_matches[[1]], "match.length") - 1
    col_starts <- c(1, utils::head(col_ends, -1) + 1)
    col_widths <- col_ends - col_starts + 1
    result <- utils::read.fwf(
      textConnection(raw_body),
      widths = col_widths,
      col.names = proc_header,
      na.strings = c("nan", "-nan")
    )
    result[] <- lapply(result, as.numeric)
    result
  })
  sipnet_output
}
