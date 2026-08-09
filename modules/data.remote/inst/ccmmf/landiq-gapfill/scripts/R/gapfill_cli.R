# Shared helpers: agricultural CLASS list, missing-crop checks, gap-fill exemptions.
# Subclass fill skips X and YP; ADOY fill skips X and I (YP still receives ADOY).

parse_cli_gapfill_year <- function(argv) {
  yrs <- parse_cli_gapfill_years(argv)
  if (!length(yrs)) {
    return(list(year = NA_integer_, cli_year = FALSE))
  }
  list(year = yrs[[1L]], cli_year = TRUE)
}

#' Parse calendar years from CLI tokens: `2023`, `2023,2024`, or `2023 2024`.
parse_cli_gapfill_years <- function(argv) {
  if (!length(argv)) {
    return(integer(0))
  }
  tokens <- unlist(strsplit(paste(argv, collapse = ","), ",", fixed = TRUE), use.names = FALSE)
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) {
    return(integer(0))
  }
  years <- integer(0)
  for (t in tokens) {
    if (grepl("^[0-9]{4}-[0-9]{4}$", t)) {
      a <- as.integer(substr(t, 1L, 4L))
      b <- as.integer(substr(t, 6L, 9L))
      if (is.na(a) || is.na(b) || b < a) {
        stop("Invalid year range: ", t)
      }
      years <- c(years, seq.int(a, b))
    } else {
      y <- suppressWarnings(as.integer(t))
      if (is.na(y) || y < 1990L || y > 2100L) {
        stop("Invalid year token: ", t, " (use YYYY, YYYY,YYYY, or YYYY-YYYY)")
      }
      years <- c(years, y)
    }
  }
  sort(unique(as.integer(years)))
}

#' Years for crop/adoy/merge/qc: CLI first, then LANDIQ_GAPFILL_RUN_YEARS / GAPFILL_YEAR.
resolve_cli_gapfill_years <- function(argv, cmd, required = TRUE) {
  years <- parse_cli_gapfill_years(argv)
  if (!length(years)) {
    env_list <- trimws(Sys.getenv("LANDIQ_GAPFILL_RUN_YEARS", ""))
    if (nzchar(env_list)) {
      years <- parse_cli_gapfill_years(env_list)
    }
  }
  if (!length(years)) {
    y <- suppressWarnings(as.integer(Sys.getenv("GAPFILL_YEAR", "")))
    if (!is.na(y)) {
      years <- y
    }
  }
  if (!length(years) && isTRUE(required)) {
    stop(
      "Command '", cmd, "' requires year(s): e.g. '", cmd, " 2023,2024' ",
      "(or set LANDIQ_GAPFILL_RUN_YEARS)"
    )
  }
  years
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

#' CLI dispatcher for landiq-gapfill steps (used by scripts/gapfill.R).
#'
#' Independent commands (each does one job):
#'   cdl-landiq-probs        rebuild CDL x LandIQ probability tables
#'   crop YEARS              season-2 crop identity gap-fill (YYYY or YYYY,YYYY)
#'   adoy-ref                rebuild ADOY reference tables
#'   adoy YEARS              ADOY gap-fill (YYYY or YYYY,YYYY)
#'   merge [YEARS]           join crop+ADOY fills into $LANDIQ_GAPFILLED
#'   qc [YEARS]              provenance tallies for the given years
gapfill_main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  argv <- as.character(argv)
  if (!length(argv) || argv[[1L]] %in% c("-h", "--help", "help")) {
    message(
      "Usage: Rscript gapfill.R <cmd> [years]\n",
      "  Years: 2023 | 2023,2024 | 2023 2024 | 2023-2024\n",
      "  cdl-landiq-probs   rebuild CDL x LandIQ probability tables\n",
      "  crop <YEARS>       crop identity gap-fill\n",
      "  adoy-ref           rebuild ADOY reference tables\n",
      "  adoy <YEARS>       ADOY gap-fill\n",
      "  merge [YEARS]      join crop+ADOY fills into gap-filled table\n",
      "  qc [YEARS]         provenance QC summary\n",
      "  (merge/qc years: CLI, else LANDIQ_GAPFILL_RUN_YEARS)\n",
      "  COVER: use scripts/R/cover_crop_landiq.R (not a gapfill.R command)"
    )
    return(invisible(NULL))
  }

  cmd <- tolower(trimws(argv[[1L]]))
  rest <- if (length(argv) > 1L) argv[-1L] else character()

  if (cmd == "emission") {
    stop("Renamed: use 'cdl-landiq-probs' (was 'emission')")
  }
  if (cmd == "product") {
    stop("Renamed: use 'merge' (was 'product')")
  }
  if (cmd == "cover") {
    stop(
      "COVER is not a gapfill.R command. Run: ",
      "Rscript \"$LANDIQ_GAPFILL_ROOT/scripts/R/cover_crop_landiq.R\""
    )
  }
  if (cmd %in% c("ensure-tables", "shared-tables")) {
    stop(
      "Removed '", cmd, "': use 'cdl-landiq-probs' and/or 'adoy-ref' ",
      "(or run_gapfill.sh --cdl-landiq-probs / --adoy-ref)"
    )
  }

  switch(
    cmd,
    `cdl-landiq-probs` = {
      ensure_emission_tables(force = TRUE)
    },
    crop = {
      years <- resolve_cli_gapfill_years(rest, cmd, required = TRUE)
      for (y in years) {
        run_gapfill(y)
      }
    },
    `adoy-ref` = {
      ensure_adoy_reference(force = TRUE)
    },
    adoy = {
      years <- resolve_cli_gapfill_years(rest, cmd, required = TRUE)
      for (y in years) {
        run_adoy_gapfill(y)
      }
    },
    merge = {
      years <- resolve_cli_gapfill_years(rest, cmd, required = TRUE)
      build_landiq_product(years = years)
    },
    qc = {
      years <- resolve_cli_gapfill_years(rest, cmd, required = TRUE)
      qc_gapfill_product(years = years)
    },
    stop(
      "Unknown gapfill command: ", cmd,
      " (use cdl-landiq-probs|crop|adoy-ref|adoy|merge|qc; COVER: cover_crop_landiq.R)"
    )
  )
  invisible(TRUE)
}
