# Helper functions for crop and tillage transition workflows.

#' Format consecutive parcel states as transitions
#'
#' Converts a parcel-by-time state table into consecutive `from` -> `to`
#' transitions. If a non-dominant probability column is present, transition
#' weights are reduced when either endpoint is uncertain.
#'
#' @param year_states Data frame containing parcel states through time.
#' @param id_col Name of the parcel identifier column.
#' @param time_col Name of the integer time column.
#' @param state_col Name of the state column.
#' @param non_dom_col Name of the non-dominant probability column. If absent,
#'   all rows are treated as fully dominant.
#' @param min_weight Minimum transition weight.
#'
#' @return A data frame containing the original columns plus `from`, `to`,
#'   `next_time`, `from_non_dom`, `to_non_dom`, and `weight`.
#' @export
make_transitions = function(year_states,
                            id_col = "parcel_id",
                            time_col = "year",
                            state_col = "state",
                            non_dom_col = "non_dom_prob",
                            min_weight = 0.05) {
  
  required_cols = c(id_col, time_col, state_col)
  missing_cols = setdiff(required_cols, names(year_states))
  
  if (length(missing_cols) > 0) {
    stop(
      "year_states is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!is.numeric(min_weight) || length(min_weight) != 1 ||
      is.na(min_weight) || min_weight < 0 || min_weight > 1) {
    stop("min_weight must be one numeric value between 0 and 1.")
  }
  
  transitions = as.data.frame(
    year_states,
    stringsAsFactors = FALSE
  )
  
  if (!(non_dom_col %in% names(transitions))) {
    transitions[[non_dom_col]] = 0
  }
  
  transitions[[non_dom_col]] = suppressWarnings(
    as.numeric(transitions[[non_dom_col]])
  )
  transitions[[non_dom_col]][is.na(transitions[[non_dom_col]])] = 0
  
  ord = order(
    transitions[[id_col]],
    transitions[[time_col]],
    na.last = TRUE
  )
  
  transitions = transitions[ord, , drop = FALSE]
  
  n = nrow(transitions)
  
  if (n == 0) {
    transitions$from = character(0)
    transitions$to = character(0)
    transitions$next_time = numeric(0)
    transitions$from_non_dom = numeric(0)
    transitions$to_non_dom = numeric(0)
    transitions$weight = numeric(0)
    return(transitions)
  }
  
  ids = as.character(transitions[[id_col]])
  states = as.character(transitions[[state_col]])
  times = transitions[[time_col]]
  non_dom = transitions[[non_dom_col]]
  
  same_next_id = rep(FALSE, n)
  
  if (n > 1) {
    same_next_id[seq_len(n - 1)] =
      !is.na(ids[seq_len(n - 1)]) &
      !is.na(ids[seq_len(n - 1) + 1]) &
      ids[seq_len(n - 1)] == ids[seq_len(n - 1) + 1]
  }
  
  to = rep(NA_character_, n)
  next_time = rep(NA, n)
  to_non_dom = rep(NA_real_, n)
  
  if (n > 1) {
    idx = which(same_next_id)
    
    to[idx] = states[idx + 1]
    next_time[idx] = times[idx + 1]
    to_non_dom[idx] = non_dom[idx + 1]
  }
  
  transitions$from = states
  transitions$to = to
  transitions$next_time = next_time
  transitions$from_non_dom = non_dom
  transitions$to_non_dom = to_non_dom
  
  keep =
    !is.na(transitions$from) &
    !is.na(transitions$to) &
    transitions$next_time == transitions[[time_col]] + 1
  
  transitions = transitions[keep, , drop = FALSE]
  
  transitions$weight = pmax(
    min_weight,
    (1 - transitions$from_non_dom) *
      (1 - transitions$to_non_dom)
  )
  
  rownames(transitions) = NULL
  transitions
}


#' Build a transition probability matrix
#'
#' Aggregates weighted `from` -> `to` transitions and normalizes each
#' non-empty row so that it sums to one.
#'
#' @param dt Data frame containing transition records.
#' @param states_all Character vector defining the complete state order.
#' @param from_col Name of the source-state column.
#' @param to_col Name of the destination-state column.
#' @param weight_col Name of the transition-weight column. If absent, each
#'   transition receives weight 1.
#'
#' @return A square numeric transition matrix with rows and columns ordered as
#'   `states_all`. States with no outgoing transitions remain zero rows.
#' @export
make_transition_matrix = function(dt,
                                  states_all,
                                  from_col = "from",
                                  to_col = "to",
                                  weight_col = "weight") {
  
  required_cols = c(from_col, to_col)
  missing_cols = setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0) {
    stop(
      "Transition data is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  states_all = as.character(states_all)
  
  if (length(states_all) == 0 || anyNA(states_all) ||
      anyDuplicated(states_all)) {
    stop("states_all must contain unique, non-missing state labels.")
  }
  
  transitions = as.data.frame(
    dt,
    stringsAsFactors = FALSE
  )
  
  if (!(weight_col %in% names(transitions))) {
    transitions[[weight_col]] = 1
  }
  
  from = as.character(transitions[[from_col]])
  to = as.character(transitions[[to_col]])
  weight = suppressWarnings(
    as.numeric(transitions[[weight_col]])
  )
  
  keep = !is.na(from) & !is.na(to)
  
  from = from[keep]
  to = to[keep]
  weight = weight[keep]
  
  if (length(from) == 0) {
    return(
      matrix(
        0,
        nrow = length(states_all),
        ncol = length(states_all),
        dimnames = list(states_all, states_all)
      )
    )
  }
  
  unknown_states = setdiff(
    unique(c(from, to)),
    states_all
  )
  
  if (length(unknown_states) > 0) {
    stop(
      "Transition data contains states not listed in states_all: ",
      paste(unknown_states, collapse = ", ")
    )
  }
  
  weighted = stats::aggregate(
    weight,
    by = list(
      from = from,
      to = to
    ),
    FUN = sum,
    na.rm = TRUE
  )
  
  names(weighted)[names(weighted) == "x"] = "N"
  
  prob_mat = matrix(
    0,
    nrow = length(states_all),
    ncol = length(states_all),
    dimnames = list(states_all, states_all)
  )
  
  row_index = match(weighted$from, states_all)
  col_index = match(weighted$to, states_all)
  
  prob_mat[cbind(row_index, col_index)] = weighted$N
  
  row_totals = rowSums(prob_mat)
  nonzero_rows = row_totals > 0
  
  prob_mat[nonzero_rows, ] =
    prob_mat[nonzero_rows, , drop = FALSE] /
    row_totals[nonzero_rows]
  
  stopifnot(all(rownames(prob_mat) == states_all))
  stopifnot(all(colnames(prob_mat) == states_all))
  stopifnot(
    all(abs(rowSums(prob_mat)[nonzero_rows] - 1) < 1e-10)
  )
  
  prob_mat
}


#' Build transition matrices by group
#'
#' Splits transition records by one or more grouping columns and builds one
#' transition matrix for each group.
#'
#' @param transitions Data frame containing transition records.
#' @param states_all Character vector defining the complete state order.
#' @param group_cols Character vector of grouping-column names.
#'
#' @return A named list of transition matrices.
#' @export
make_grouped_transition_matrices = function(transitions,
                                            states_all,
                                            group_cols) {
  
  if (length(group_cols) == 0) {
    stop("group_cols must contain at least one column name.")
  }
  
  missing_cols = setdiff(group_cols, names(transitions))
  
  if (length(missing_cols) > 0) {
    stop(
      "transitions is missing grouping columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  group_values = lapply(
    group_cols,
    function(col) as.character(transitions[[col]])
  )
  
  group_key = do.call(
    paste,
    c(group_values, sep = "__")
  )
  
  transition_groups = split(
    transitions,
    group_key,
    drop = TRUE
  )
  
  lapply(
    transition_groups,
    make_transition_matrix,
    states_all = states_all
  )
}
