#' Read ED2IN file to named list
#'
#' Parse an ED2IN file to a named list, such that `NL%MYTAG = 'value'` gets 
#' converted to `list(MYTAG = 'value')`.
#'
#' @param filename Full path to ED2IN file
#' @return Named list of the 
#' @export
read_ed2in <- function(filename) {
  raw_file <- readLines(filename)

  # Extract tag-value pairs
  ed2in_tag_rxp <- paste0(
    "^[[:blank:]]*",              # Initial whitespace (does not start with a `!` comment)
    "NL%([[:graph:]]+)",          # Capture namelist tag (1)
    "[[:blank:]]+=[[:blank:]]*",  # Equals, with optional surrounding whitespace
    "(",                          # Begin value capture (2)
    "[[:digit:].-]+(,[[:blank:]]*[[:digit:].-]+)*",   # Number, or number list
    "|",                          # ...or...
    "@.*?@",                      # Old substitution tag (e.g. @MYVALUE@)
    "|",                          # ...or...
    "'[[:graph:][:blank:]]*'",    # Quoted string, or list of strings
    ")",                          # End value capture
    "[[:blank:]]*!?.*$"           # Trailing whitespace and possible comments
  )

  tag_lines <- grep(ed2in_tag_rxp, raw_file, perl = TRUE)
  sub_file <- raw_file[tag_lines]
  tags <- gsub(ed2in_tag_rxp, "\\1", sub_file, perl = TRUE)
  values <- gsub(ed2in_tag_rxp, "\\2", sub_file, perl = TRUE)

  # Extract comments. They will be stored in the object attributes.
  all_lines <- seq_along(raw_file)
  comment_linenos <- all_lines[!all_lines %in% tag_lines]
  comment_values <- raw_file[comment_linenos]

  # Convert to a list to allow storing of multiple data types
  values_list <- as.list(values)

  numeric_values <- !is.na(suppressWarnings(as.numeric(values))) |
    grepl("^@.*?@$", values)    # Unquoted old substitutions are numeric
  values_list[numeric_values] <- lapply(values_list[numeric_values], as.numeric)
  # NOTE: This should throw a warning if any old substitution tags are present

  # Convert values that are a list of numbers to a numeric vector
  numlist_values <- grep(
    "[[:digit:].-]+(,[[:blank:]]*[[:digit:].-]+)+",
    values
  )
  values_list[numlist_values] <- lapply(
    values_list[numlist_values],
    function(x) as.numeric(strsplit(x, split = ",")[[1]])
  )

  # Convert values that are a list of strings to a character vector
  charlist_values <- grep("'.*?'(,'.*?')+", values)
  values_list[charlist_values] <- lapply(
    values_list[charlist_values],
    function(x) strsplit(x, split = ",")[[1]]
  )

  # Remove extra quoting of strings
  quoted_values <- grep("'.*?'", values)
  values_list[quoted_values] <- lapply(
    values_list[quoted_values],
    gsub,
    pattern = "'",
    replacement = ""
  )

  structure(
    values_list,
    names = tags,
    class = c("ed2in", "list"),
    comment_linenos = comment_linenos,
    comment_values = comment_values,
    value_linenos = tag_lines
  )
}

#' Print method for `ed2in`
#'
#' Sets attributes to `NULL` before printing, so the output isn't as messy.
#'
#' @inheritParams base::print
#' 
#' @export
print.ed2in <- function(x, ...) {
  attributes(x) <- attributes(x)["names"]
  print.default(x, ...)
}

#' Check if object is `ed2in`
#'
#' Simple test if object inheirts from class `"ed2in"`.
#'
#' @param x Object to be tested
#' @export
is.ed2in <- function(x) {
  inherits(x, "ed2in")
}
