#' Subset a Sipnet clim file line-by-line without parsing columns
#'
#' Ignores lines before the target date, reads and writes wanted lines as
#' character without parsing any columns, and exits without touching lines after
#' the end date. This is faster than reading the entire table and subsetting,
#' especially when taking small subsets of large files that live on slow disks.
#'
#' If the input file is fixed-width (as files produced by met2model.SIPNET are),
#' gets a further speedup by seeking directly to the byte offset for start_day.
#' Variable-width clim files do not get this speedup (because file
#' position needs to be determined by scanning for end-of-line markers),
#' but they still benefit from skipping all lines after stop_day.
#'
#' Only works for files with a constant timestep, and only copies whole days,
#' i.e. if you have hourly data the output length will be a multiple of 24.
#'
#' The step size and line length are detected by reading the first `n_head` lines
#' of the original file; this number is tunable with less being likely faster
#' and more being more likely to detect variation, but probably doesn't need to
#' be more than one day.
#'
#' TODO: To match split.inputs.SIPNET, the end date is exclusive:
#' stop_day is the date to stop _before_, not the last day to include in the
#' output. This is confusing and differs from most of the rest of PEcAn --
#' how committed to this definition are we?
#'
#' @param clim_in path to a Sipnet clim file
#' @param clim_out path to write the subset
#' @param start_day date of first day to write
#' @param stop_day date to stop _before_ writing. See details
#' @param n_head Number of lines of `clim_in` to read to determine file format
#'
subset_met_lines <- function(clim_in, clim_out, start_day, stop_day,
                             n_head = 10) {
  start_day <- as.Date(start_day)
  stop_day <- as.Date(stop_day)

  con <- file(clim_in, "r")
  on.exit(close(con), add = TRUE)

  first_lines <- peek(con, n = n_head)
  bytes_in_head <- seek(con)

  n_cols <- length(
    scan(text = first_lines[[1]], what = double(), nmax = 14, quiet = TRUE)
  )
  if (n_cols == 14) { # v1 format: 14 cols starting with unused location index
    col_offset <- 1
    col_names <- c("", "year", "doy", "hour", "step", rep("", 9))
  } else { # v2 format: 12 cols, year first
    col_offset = 0
    col_names = c("year", "doy", "hour", "step", rep("", 8))
  }
  first_vals <- utils::read.table(
    text = first_lines,
    colClasses = "double",
    col.names = col_names,
    header = FALSE
  )

  input_start_day <- as.Date(
    paste(first_vals$year[[1]], first_vals$doy[[1]]),
    format = "%Y %j"
  )
  chars_per_line <- unique(nchar(first_lines))

  # variable step size => can't calculate n rows
  # TODO might not be too bad to loosen to "every day must have same number of
  # steps", e.g. 8hr day + 16hr night is still 2 rows/day
  steps_per_day <- unique(round(1 / first_vals$step))
  stopifnot(length(steps_per_day) == 1)

  # Insist (well, "insist") on whole days.
  # TODO try harder here?
  # Supporting partial days would be doable, just tedious to implement.
  if (first_vals$hour[[1]] != 0) {
    PEcAn.logger::logger.warn(
      "File", clim_in, "starts partway through a day, so output will too"
    )
  }

  lines_skip <- steps_per_day * as.numeric(start_day - input_start_day)
  lines_read <- steps_per_day * as.numeric(stop_day - start_day)

  if (length(chars_per_line) == 1) {
    # Fast path for fixed-width files: seek all the way to start point
    # w/o scanning intervening bytes
    bytes_per_line <- bytes_in_head / length(first_lines)
    bytes_skip <- bytes_per_line * lines_skip
    seek(con, bytes_skip, origin = "start")
    raw_txt <- readLines(con, n = lines_read)
    stopifnot(
      nchar(raw_txt[[1]]) == chars_per_line,
      nchar(raw_txt[[length(raw_txt)]]) == chars_per_line
    )
  } else {
    # less fast -- but still pretty quick -- path:
    # Have to scan to count EOLs, but need not parse fields within lines
    # and can stop immediately on reaching end of subset
    raw_txt <- readLines(con, n = lines_read + lines_skip) |>
      utils::tail(lines_read)
  }

  raw_txt |>
    check_start_end(
      d1 = start_day,
      d2 = stop_day - lubridate::days(1),
      n = lines_read,
      offset = col_offset
    ) |>
    writeLines(clim_out)

  NULL
}

# Read lines from an open file, then push them back so next read() will take
# them first
peek <- function(con, n = 1) {
  stopifnot(isOpen(con, "r"))
  l <- readLines(con, n = n)
  pushBack(l, con)

  l
}

# Read day and time from a raw line
# Input looks like "2016\t1\t0\t0.125..." (sipnet v2)
# or "0\t2016\t1\t0\t0.125..." (sipnet v1),
# either one possibly with additional whitespace around the tabs
read_date <- function(line, offset = 0) {
  vals <- scan(text = line, what = double(), nmax = 2 + offset, quiet = TRUE)
  yr <- vals[[1 + offset]]
  doy <- vals[[2 + offset]]
  stopifnot(is.finite(yr), doy > 0, doy <= 366)
  as.Date(paste(yr, doy), format = "%Y %j")
}

# Stream lines through, stop if they don't start and end on the expected days.
# Note: Here d2 is last day *in* data, not stop-by date
# Note: n only checks number of rows; does not inspect dates
# or ordering or even nonemptiness of the lines in the middle
check_start_end <- function(lines, d1, d2, n, offset = 0) {
  stopifnot(
    read_date(lines[[1]], offset = offset) == d1,
    read_date(lines[[length(lines)]], offset = offset) == d2,
    length(lines) == n
  )

  lines
}