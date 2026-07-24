build_cdl_fraction_panel <- function(cdl_gap_full, code_cols, parcel_ids = NULL) {
  cdl_gap <- cdl_gap_full %>%
    dplyr::mutate(
      parcel_id = trimws(as.character(parcel_id)),
      cdl_code = as.integer(cdl_code)
    ) %>%
    dplyr::filter(!is.na(cdl_code), !is.na(frac), frac > 0)

  if (!is.null(parcel_ids)) {
    parcel_ids <- trimws(as.character(parcel_ids))
    cdl_gap <- cdl_gap %>% dplyr::filter(parcel_id %in% parcel_ids)
  }

  F_long <- cdl_gap %>%
    dplyr::group_by(parcel_id, cdl_code) %>%
    dplyr::summarise(frac_sum = sum(frac, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(code_chr = as.character(cdl_code)) %>%
    dplyr::filter(code_chr %in% code_cols)

  if (nrow(F_long) == 0L) {
    return(NULL)
  }

  F_wide <- F_long %>%
    dplyr::select(parcel_id, code_chr, frac_sum) %>%
    tidyr::pivot_wider(names_from = code_chr, values_from = frac_sum, values_fill = 0)
  for (nm in setdiff(code_cols, names(F_wide))) {
    F_wide[[nm]] <- 0
  }
  F_wide <- F_wide %>% dplyr::select(parcel_id, dplyr::all_of(code_cols))

  F_mat_raw <- as.matrix(F_wide[, code_cols, drop = FALSE])
  native_mass <- rowSums(F_mat_raw)
  ok <- native_mass > 0 & is.finite(native_mass)
  if (!any(ok)) {
    return(NULL)
  }

  F_wide <- F_wide[ok, , drop = FALSE]
  F_mat <- F_mat_raw[ok, , drop = FALSE] / native_mass[ok]
  dom_idx <- max.col(F_mat, ties.method = "first")

  list(
    parcel_id = F_wide$parcel_id,
    F_mat = F_mat,
    native_mass = native_mass[ok],
    dominant_code = as.integer(code_cols[dom_idx])
  )
}

cdl_class_likelihood <- function(F_mat, E, ag_class_vector, cdl_class_obs = "fraction") {
  n_class <- length(ag_class_vector)
  F_panel <- F_mat
  if (identical(cdl_class_obs, "onehot")) {
    idx_dom <- max.col(F_panel, ties.method = "first")
    F_one <- matrix(0, nrow = nrow(F_panel), ncol = ncol(F_panel))
    F_one[cbind(seq_len(nrow(F_panel)), idx_dom)] <- 1
    F_panel <- F_one
  }
  L_mat <- F_panel %*% t(E)
  L_rs <- rowSums(L_mat)
  bad_L <- !is.finite(L_rs) | L_rs <= 0
  if (any(bad_L)) {
    L_mat[bad_L, ] <- 1 / n_class
    L_rs <- rowSums(L_mat)
  }
  L_mat / L_rs
}

map_class_from_cdl <- function(cdl_panel, E, ag_class_vector, cdl_class_obs = "fraction") {
  p_cdl <- cdl_class_likelihood(cdl_panel$F_mat, E, ag_class_vector, cdl_class_obs)
  ag_class_vector[max.col(p_cdl, ties.method = "first")]
}
