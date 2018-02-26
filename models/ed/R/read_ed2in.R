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
  tag_lines <- grep(ed2in_tag_rxp, raw_file, perl = TRUE)
  tags <- gsub(ed2in_tag_rxp, "\\1", sub_file, perl = TRUE)
  values <- gsub(ed2in_tag_rxp, "\\2", sub_file, perl = TRUE)

  # Convert to a list to allow storing of multiple data types
  values_list <- as.list(values)
  names(values_list) <- tags

  numeric_values <- !is.na(suppressWarnings(as.numeric(values)))
  values_list[numeric_values] <- lapply(values_list[numeric_values], as.numeric)

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

  values_list
}

#' Regular expressions for ED2IN tags and values
#'
#' Regular expressions used to match ED2IN tags (first captured group) and 
#' values (second captured group).
ed2in_tag_rxp <- paste0(
  "^[[:blank:]]*NL%([[:graph:]]+)[[:blank:]]+=",
  "[[:blank:]]*([[:digit:].-]+(,[[:blank:]]*[[:digit:].-]+)*",
  "|'[[:graph:][:blank:]]*')[[:blank:]]*!?.*"
)
