# County- and state-level CLASS transition matrices for full-year crop gap-fill.
# CSV layout: square matrix with ag CLASS labels as row/column names (see README).

county_matrix_stem <- function(county_name) {
  gsub("[^A-Za-z0-9_]+", "_", trimws(as.character(county_name)))
}

path_county_transition_dir <- function() {
  env <- trimws(Sys.getenv("COUNTY_TRANSITION_MATRICES_DIR", ""))
  if (nzchar(env)) {
    return(env)
  }
  stop(
    "Set COUNTY_TRANSITION_MATRICES_DIR to the county CLASS transition matrix ",
    "directory (source documentation/setup_env.sh; default under ",
    "landiq-gapfill/data/county_transition_matrices)."
  )
}

gapfill_transition_level <- function() {
  lvl <- tolower(trimws(Sys.getenv("GAPFILL_TRANSITION_LEVEL", "county")))
  if (!lvl %in% c("county", "state")) {
    stop("GAPFILL_TRANSITION_LEVEL must be county or state; got: ", lvl)
  }
  lvl
}

normalize_transition_matrix <- function(mat, ag_class_vector) {
  mat <- as.matrix(mat)
  storage.mode(mat) <- "double"

  missing_rows <- setdiff(ag_class_vector, rownames(mat))
  missing_cols <- setdiff(ag_class_vector, colnames(mat))
  if (length(missing_rows) > 0L || length(missing_cols) > 0L) {
    stop(
      "Transition matrix is missing required classes. Missing rows: ",
      paste(missing_rows, collapse = ", "),
      " | missing cols: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  out <- mat[ag_class_vector, ag_class_vector, drop = FALSE]
  row_sums <- rowSums(out)
  if (any(!is.finite(row_sums))) {
    stop("Transition matrix has non-finite row sums")
  }
  positive <- row_sums > 0
  if (any(positive)) {
    for (i in which(positive)) {
      out[i, ] <- out[i, ] / row_sums[i]
    }
  }
  out
}

load_transition_matrix_csv <- function(path_csv, ag_class_vector) {
  if (!file.exists(path_csv)) {
    stop("Transition matrix CSV not found: ", path_csv)
  }
  mat <- read.csv(
    path_csv,
    row.names = 1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  normalize_transition_matrix(mat, ag_class_vector)
}

load_county_transition_matrices <- function(dir, ag_class_vector) {
  if (!dir.exists(dir)) {
    stop("County transition matrix directory not found: ", dir)
  }
  # Prefer *_crop_matrix.csv (current Ananya training outputs); also accept
  # legacy *_transition_matrix.csv.
  files <- list.files(dir, pattern = "_crop_matrix\\.csv$", full.names = TRUE)
  if (length(files) == 0L) {
    files <- list.files(dir, pattern = "_transition_matrix\\.csv$", full.names = TRUE)
  }
  if (length(files) == 0L) {
    stop("No *_crop_matrix.csv or *_transition_matrix.csv files in: ", dir)
  }

  mats <- list()
  for (path_csv in files) {
    stem <- sub("_(crop|transition)_matrix\\.csv$", "", basename(path_csv))
    mats[[stem]] <- load_transition_matrix_csv(path_csv, ag_class_vector)
  }
  mats
}

compute_fwd_bwd_from_transition <- function(A, idx_lo, idx_hi, n_class) {
  n_panel <- length(idx_lo)
  p_fwd <- matrix(0, nrow = n_panel, ncol = n_class)
  p_bwd <- matrix(0, nrow = n_panel, ncol = n_class)
  A <- unname(as.matrix(A))

  for (r in seq_len(n_panel)) {
    ia <- idx_lo[r]
    ic <- idx_hi[r]
    if (!is.na(ia)) {
      pf <- A[ia, ]
      pf <- pmax(pf, 0)
      sp <- sum(pf)
      if (!is.finite(sp) || sp <= 0) {
        p_fwd[r, ] <- rep(1 / n_class, n_class)
      } else {
        p_fwd[r, ] <- pf / sp
      }
    }
    if (!is.na(ic)) {
      vb <- A[, ic]
      vb <- pmax(vb, 0)
      sb <- sum(vb)
      if (!is.finite(sb) || sb <= 0) {
        p_bwd[r, ] <- rep(1 / n_class, n_class)
      } else {
        p_bwd[r, ] <- vb / sb
      }
    }
  }
  list(p_fwd = p_fwd, p_bwd = p_bwd)
}

compute_fwd_bwd_by_county <- function(
    county_stems,
    idx_lo,
    idx_hi,
    county_mats,
    fallback_A,
    n_class) {
  n_panel <- length(idx_lo)
  p_fwd <- matrix(0, nrow = n_panel, ncol = n_class)
  p_bwd <- matrix(0, nrow = n_panel, ncol = n_class)
  matrix_stem_used <- rep(NA_character_, n_panel)

  use_stem <- county_stems
  missing <- is.na(use_stem) | !nzchar(use_stem) | !(use_stem %in% names(county_mats))
  if (any(missing)) {
    use_stem[missing] <- "__fallback__"
  }
  county_mats[["__fallback__"]] <- fallback_A

  for (stem in unique(use_stem)) {
    A <- county_mats[[stem]]
    rows <- which(use_stem == stem)
    part <- compute_fwd_bwd_from_transition(A, idx_lo[rows], idx_hi[rows], n_class)
    p_fwd[rows, ] <- part$p_fwd
    p_bwd[rows, ] <- part$p_bwd
    if (identical(stem, "__fallback__")) {
      matrix_stem_used[rows] <- NA_character_
    } else {
      matrix_stem_used[rows] <- stem
    }
  }

  list(
    p_fwd = p_fwd,
    p_bwd = p_bwd,
    county_matrix_stem = matrix_stem_used,
    n_county_specific = sum(!is.na(matrix_stem_used)),
    n_state_fallback = sum(is.na(matrix_stem_used))
  )
}
