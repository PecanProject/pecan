# Emission table cache, build orchestration, and runtime bundle loader.

emission_output_paths <- function(suffix = landiq_lookup_suffix()) {
  lookup <- emission_lookup_paths(suffix)
  out <- path_outputs()
  list(
    lookup = lookup$mass,
    lookup_dominant = lookup$dominant,
    prior = lookup$prior,
    prob_class = file.path(out, sprintf("cdl_prob_by_class_%s.parquet", suffix)),
    prob_sub = file.path(out, sprintf("cdl_prob_by_subclass_%s.parquet", suffix))
  )
}

emission_tables_cached <- function(suffix = landiq_lookup_suffix()) {
  paths <- emission_output_paths(suffix)
  all(file.exists(unlist(paths[c("prior", "prob_class", "prob_sub", "lookup")])))
}

#' Build or load cached CDL emission lookup + probability tables.
ensure_emission_tables <- function(force = NULL) {
  suffix <- landiq_lookup_suffix()
  if (is.null(force)) {
    force <- tolower(Sys.getenv("GAPFILL_REBUILD_EMISSION", "false")) %in% c("1", "true", "yes")
  }
  if (!force && emission_tables_cached(suffix)) {
    message("Emission tables cached (suffix=", suffix, "); skipping build")
    return(invisible(emission_output_paths(suffix)))
  }
  message("Building emission tables...")
  build_emission_lookup()
  build_emission_prob_tables()
  invisible(emission_output_paths(suffix))
}

build_emission_matrix <- function(prob_class, ag_class_vector) {
  codes_train <- sort(unique(prob_class$cdl_code))
  E <- matrix(
    0,
    nrow = length(ag_class_vector),
    ncol = length(codes_train),
    dimnames = list(ag_class_vector, as.character(codes_train))
  )
  for (k in seq_len(nrow(prob_class))) {
    r <- prob_class$CLASS[k]
    cc <- as.character(prob_class$cdl_code[k])
    if (r %in% rownames(E) && cc %in% colnames(E)) {
      E[r, cc] <- prob_class$prob[k]
    }
  }
  E
}

#' Load emission matrices and priors for gap-fill steps.
load_emission_bundle <- function() {
  paths <- emission_output_paths()
  if (!all(file.exists(unlist(paths[c("prob_class", "prob_sub", "prior")])))) {
    stop("Missing emission tables; run ensure_emission_tables() first.")
  }
  ag_class_vector <- load_ag_class_vector(path_crop_lookup_csv())
  prob_class <- arrow::read_parquet(paths$prob_class, as_data_frame = TRUE) %>%
    dplyr::mutate(
      CLASS = as.character(CLASS),
      cdl_code = as.integer(cdl_code),
      prob = as.numeric(prob)
    )
  sub_prob_long <- arrow::read_parquet(paths$prob_sub, as_data_frame = TRUE) %>%
    dplyr::transmute(
      truth_key = as.character(truth_key),
      obs_key = as.character(as.integer(cdl_code)),
      cdl_name = if ("cdl_name" %in% names(.)) as.character(cdl_name) else NA_character_,
      prob = as.numeric(prob)
    )
  class_sub_prior <- arrow::read_parquet(paths$prior, as_data_frame = TRUE)
  crop_lk <- load_landiq_crop_lookup(path_crop_lookup_csv())
  list(
    suffix = landiq_lookup_suffix(),
    paths = paths,
    ag_class_vector = ag_class_vector,
    E = build_emission_matrix(prob_class, ag_class_vector),
    prob_class = prob_class,
    sub_prob_long = sub_prob_long,
    class_sub_prior = class_sub_prior,
    crop_lk = crop_lk
  )
}
