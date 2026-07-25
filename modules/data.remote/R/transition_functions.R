#1. takes a file and puts it into a consistent transition format to make a matrix  
make_transitions = function(year_states, id_col = "parcel_id", time_col = "year", state_col = "state",
                            non_dom_col = "non_dom_prob", min_weight = 0.05) {
  
  dt = copy(as.data.table(year_states))
  
  setnames(dt, id_col, "id")
  setnames(dt, time_col, "time")
  setnames(dt, state_col, "state")
  
  if (non_dom_col %in% names(dt)) {
    setnames(dt, non_dom_col, "non_dom_prob")
  } else {
    dt[, non_dom_prob := 0]}
  
  setorder(dt, id, time)
  
  dt[, `:=`(
    from = state,
    to = shift(state, type = "lead"),
    next_time = shift(time, type = "lead"),
    from_non_dom = non_dom_prob,
    to_non_dom = shift(non_dom_prob, type = "lead")
  ), by = id]
  
  transitions = dt[
    !is.na(from) &
      !is.na(to) &
      next_time == time + 1]
  
  transitions[, weight := pmax(
    min_weight,
    (1 - from_non_dom) * (1 - to_non_dom))]
  
  setnames(transitions, "id", id_col)
  setnames(transitions, "time", time_col)
  
  return(transitions)
}

#2. creates one transition matrix, used to help make grouped transition matrices in the next function  
make_transition_matrix = function(dt, states_all, from_col = "from", to_col = "to", weight_col = "weight") {
  
  dt = copy(as.data.table(dt))
  
  setnames(dt, from_col, "from")
  setnames(dt, to_col, "to")
  
  if (weight_col %in% names(dt)) {
    setnames(dt, weight_col, "weight")
  } else {
    dt[, weight := 1]}
  
  transitions_weighted = dt[
    !is.na(from) & !is.na(to),
    .(N = sum(weight, na.rm = TRUE)),
    by = .(from, to)]
  
  if (nrow(transitions_weighted) == 0) {
    empty_mat = matrix(
      0,
      nrow = length(states_all),
      ncol = length(states_all),
      dimnames = list(states_all, states_all))
    return(empty_mat)}
  
  tmat_counts = dcast(transitions_weighted, from ~ to, value.var = "N", fill = 0)
  
  ## add missing columns
  missing_cols = setdiff(states_all, colnames(tmat_counts))
  for (mc in missing_cols) {
    tmat_counts[[mc]] = 0}
  
  ## add missing rows
  missing_rows = setdiff(states_all, tmat_counts$from)
  if (length(missing_rows) > 0) {
    zero_rows = data.table(from = missing_rows)
    for (s in states_all) {
      zero_rows[[s]] = 0}
    tmat_counts = rbind(tmat_counts, zero_rows, fill = TRUE)}
  
  ## order rows/cols
  tmat_counts[, ord := match(from, states_all)]
  setorder(tmat_counts, ord)
  tmat_counts[, ord := NULL]
  tmat_counts = tmat_counts[, c("from", states_all), with = FALSE]
  
  ## convert to probability matrix
  rn = tmat_counts$from
  prob_mat = as.matrix(tmat_counts[, ..states_all])
  storage.mode(prob_mat) = "double"
  
  row_totals = rowSums(prob_mat)
  tmat_final = prob_mat
  
  tmat_final[row_totals > 0, ] =
    prob_mat[row_totals > 0, ] / row_totals[row_totals > 0]
  
  tmat_final[row_totals == 0, ] = 0
  
  rownames(tmat_final) = rn
  colnames(tmat_final) = states_all
  
  stopifnot(all(rownames(tmat_final) == states_all))
  stopifnot(all(colnames(tmat_final) == states_all))
  stopifnot(all(abs(rowSums(tmat_final)[row_totals > 0] - 1) < 1e-10))
  
  return(tmat_final)}

#3. creates multiple matrices by desired group (crop class, county, etc)
    #avoids having to use split(by=...)
make_grouped_transition_matrices = function(transitions, states_all, group_cols) {
  
  transition_groups = split(transitions, by = group_cols, keep.by = TRUE)
  
  transition_mats = lapply(transition_groups, make_transition_matrix, states_all = states_all)
  
  return(transition_mats)}
