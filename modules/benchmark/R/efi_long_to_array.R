#' Reshape EFI Long Format DataFrame to Matrix of Ensemble Members
#'
#' Takes an EFI long format dataset (containing scenario, datetime/time, site_id/site,
#' parameter/ensemble/member, variable, prediction/value) and reshapes it into a matrix
#' of predictions with rows corresponding to timesteps and columns corresponding to ensemble members.
#'
#' @param df data.frame in EFI long format.
#' @param var character string specifying the target variable to filter (optional).
#' @param site character string specifying the target site_id to filter (optional).
#'
#' @importFrom tidyr pivot_wider
#' @return A matrix with timesteps as rows and ensemble members as columns, with a `time` attribute (POSIXct vector).
#' @export
efi_long_to_array <- function(df, var = NULL, site = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    PEcAn.logger::logger.severe("Input dataframe to efi_long_to_array is NULL or empty.")
  }

  # Standardize variable column name
  var_col <- intersect(c("variable", "var"), names(df))[1]
  if (!is.na(var_col)) {
    if (!is.null(var)) {
      df <- df[df[[var_col]] == var, , drop = FALSE]
    }
    if (length(unique(df[[var_col]])) > 1) {
      PEcAn.logger::logger.severe("Input dataframe contains multiple variables. Please specify target 'var' or filter to a single variable.")
    }
  }

  # Standardize site column name
  site_col <- intersect(c("site_id", "site"), names(df))[1]
  if (!is.na(site_col)) {
    if (!is.null(site)) {
      df <- df[df[[site_col]] == site, , drop = FALSE]
    }
    if (length(unique(df[[site_col]])) > 1) {
      PEcAn.logger::logger.severe("Input dataframe contains multiple sites. Please specify target 'site' or filter to a single site.")
    }
  }

  if (nrow(df) == 0) {
    PEcAn.logger::logger.severe("No matching rows found after filtering by variable/site.")
  }

  # Standardize time column name
  time_col <- intersect(c("datetime", "time", "date"), names(df))[1]
  if (is.na(time_col)) {
    PEcAn.logger::logger.severe("Missing time column in EFI long dataframe (expected 'datetime', 'time', or 'date').")
  }

  # Standardize ensemble member column name
  param_col <- intersect(c("parameter", "ensemble", "member", "param"), names(df))[1]
  if (is.na(param_col)) {
    PEcAn.logger::logger.severe("Missing ensemble member column in EFI long dataframe (expected 'parameter', 'ensemble', or 'member').")
  }

  # Standardize prediction column name
  pred_col <- intersect(c("prediction", "value", "val"), names(df))[1]
  if (is.na(pred_col)) {
    PEcAn.logger::logger.severe("Missing prediction column in EFI long dataframe (expected 'prediction' or 'value').")
  }

  # Pivot wide keeping every member
  wide_df <- tidyr::pivot_wider(
    df,
    id_cols = dplyr::all_of(time_col),
    names_from = dplyr::all_of(param_col),
    values_from = dplyr::all_of(pred_col),
    names_prefix = "member_"
  )

  time_vec <- as.POSIXct(wide_df[[time_col]], tz = "UTC")
  val_df <- wide_df[, -1, drop = FALSE]
  ens_mat <- apply(val_df, 2, function(x) as.numeric(unlist(x)))
  if (!is.matrix(ens_mat)) {
    ens_mat <- matrix(ens_mat, nrow = nrow(val_df), ncol = ncol(val_df))
    colnames(ens_mat) <- names(val_df)
  }
  attr(ens_mat, "time") <- time_vec

  return(ens_mat)
}
