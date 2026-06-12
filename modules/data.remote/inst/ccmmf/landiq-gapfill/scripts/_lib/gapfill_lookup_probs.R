# Normalize CDL x LandIQ lookup masses into conditional probability tables.

#' Resolve emission lookup source parquet (fraction or dominant weighting).
emission_lookup_source_path <- function(
    suffix = landiq_lookup_suffix(),
    weighting = tolower(trimws(Sys.getenv("CDL_LANDIQ_LOOKUP_WEIGHTING", "fraction")))) {
  if (!weighting %in% c("fraction", "dominant")) {
    stop("CDL_LANDIQ_LOOKUP_WEIGHTING must be fraction or dominant; got: ", weighting)
  }
  paths <- emission_lookup_paths(suffix)
  if (identical(weighting, "dominant")) paths$dominant else paths$mass
}

#' Build P(CDL | CLASS) and P(CDL | CLASS::SUBCLASS) tables from lookup masses.
#'
#' @return Invisibly returns paths to prob_class and prob_sub parquets.
build_emission_prob_tables <- function(
    suffix = landiq_lookup_suffix(),
    weighting = tolower(trimws(Sys.getenv("CDL_LANDIQ_LOOKUP_WEIGHTING", "fraction")))) {
  out_dir <- path_outputs()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  path_src <- emission_lookup_source_path(suffix, weighting)
  if (!file.exists(path_src)) {
    stop("Missing lookup parquet: ", path_src, "\nRun build_emission_lookup() first.")
  }

  path_prob_class <- file.path(out_dir, sprintf("cdl_prob_by_class_%s.parquet", suffix))
  path_prob_sub <- file.path(out_dir, sprintf("cdl_prob_by_subclass_%s.parquet", suffix))
  ag_class_vector <- load_ag_class_vector(path_crop_lookup_csv())

  message("Reading lookup (", weighting, "): ", path_src)
  mass <- arrow::read_parquet(path_src, as_data_frame = TRUE)
  if (identical(weighting, "dominant")) {
    if (!"n_parcel_years" %in% names(mass)) {
      stop("Dominant lookup parquet missing column n_parcel_years: ", path_src)
    }
    mass <- mass %>% dplyr::rename(mass = n_parcel_years)
  }

  required <- c("truth_key", "CLASS", "SUBCLASS", "cdl_code", "mass")
  miss <- setdiff(required, names(mass))
  if (length(miss) > 0L) {
    stop("Lookup parquet missing required columns: ", paste(miss, collapse = ", "))
  }

  codes_train <- sort(unique(as.integer(mass$cdl_code)))
  n_class <- length(ag_class_vector)
  n_code <- length(codes_train)

  mass_class <- mass %>%
    dplyr::group_by(CLASS, cdl_code) %>%
    dplyr::summarise(mass = sum(mass, na.rm = TRUE), .groups = "drop")

  E <- matrix(
    1e-6,
    nrow = n_class,
    ncol = n_code,
    dimnames = list(ag_class_vector, as.character(codes_train))
  )
  for (k in seq_len(nrow(mass_class))) {
    r <- mass_class$CLASS[k]
    cc <- as.character(mass_class$cdl_code[k])
    if (r %in% rownames(E) && cc %in% colnames(E)) {
      E[r, cc] <- E[r, cc] + mass_class$mass[k]
    }
  }
  row_tot <- rowSums(E)
  bad <- !is.finite(row_tot) | row_tot <= 0
  if (any(bad)) {
    E[bad, ] <- 1 / n_code
    row_tot <- rowSums(E)
  }
  E <- E / row_tot

  prob_class_tbl <- tibble::tibble(
    CLASS = rep(rownames(E), times = ncol(E)),
    cdl_code = rep(as.integer(colnames(E)), each = nrow(E)),
    prob = as.numeric(E)
  )
  arrow::write_parquet(prob_class_tbl, path_prob_class)
  message("Wrote ", path_prob_class, " (", n_class, " classes x ", n_code, " codes)")

  prob_sub_tbl <- mass %>%
    dplyr::group_by(truth_key, cdl_code) %>%
    dplyr::summarise(
      mass = sum(mass, na.rm = TRUE),
      cdl_name = dplyr::first(cdl_name),
      .groups = "drop"
    ) %>%
    dplyr::group_by(truth_key) %>%
    dplyr::mutate(prob = mass / sum(mass)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      truth_key,
      cdl_code = as.integer(cdl_code),
      cdl_name,
      prob
    )

  arrow::write_parquet(prob_sub_tbl, path_prob_sub)
  message(
    "Wrote ", path_prob_sub,
    " (", dplyr::n_distinct(prob_sub_tbl$truth_key), " truth_keys, ",
    dplyr::n_distinct(prob_sub_tbl$cdl_code), " codes)"
  )

  invisible(list(prob_class = path_prob_class, prob_sub = path_prob_sub))
}
