# Shared helpers: agricultural CLASS list, missing-crop checks, gap-fill exemptions.
# Subclass fill skips X and YP; ADOY fill skips X and I (YP still receives ADOY).

parse_cli_gapfill_year <- function(argv) {
  if (!length(argv)) {
    return(list(year = NA_integer_, cli_year = FALSE))
  }
  for (t in argv) {
    y <- suppressWarnings(as.integer(t))
    if (!is.na(y) && y >= 1990L && y <= 2100L) {
      return(list(year = y, cli_year = TRUE))
    }
  }
  list(year = NA_integer_, cli_year = FALSE)
}

load_ag_class_vector <- function(path_crop_lookup_csv) {
  crop_lookup <- readr::read_csv(path_crop_lookup_csv, show_col_types = FALSE) %>%
    dplyr::mutate(
      is_agricultural = tolower(trimws(as.character(is_agricultural))) == "true",
      CLASS = trimws(as.character(CLASS))
    )
  crop_lookup %>%
    dplyr::filter(is_agricultural) %>%
    dplyr::distinct(CLASS) %>%
    dplyr::filter(!is.na(CLASS), CLASS != "") %>%
    dplyr::pull(CLASS)
}

is_missing_landiq_crop <- function(class_chr) {
  class_chr <- trimws(as.character(class_chr))
  is.na(class_chr) | class_chr == "" | class_chr %in% c("U", "X")
}

subclass_gapfill_exempt_classes <- function() {
  c("X", "YP")
}

adoy_gapfill_exempt_classes <- function() {
  c("X", "I")
}

is_missing_subclass <- function(subclass_chr) {
  subclass_chr <- trimws(as.character(subclass_chr))
  is.na(subclass_chr) | subclass_chr == "" | subclass_chr == "**"
}

needs_subclass_gapfill <- function(class_chr, subclass_chr, ag_classes) {
  class_chr <- trimws(as.character(class_chr))
  subclass_chr <- trimws(as.character(subclass_chr))
  in_ag <- class_chr %in% ag_classes
  exempt <- class_chr %in% subclass_gapfill_exempt_classes()
  missing_sub <- is_missing_subclass(subclass_chr)
  in_ag & !exempt & missing_sub
}
