# Helper functions used by fertilization.R and ncc.R to produce
# nitrogen and carbon management predictions.

#' Normalize crop names
#'
#' Standardizes crop names before matching across LandIQ and fertilization
#' reference tables.
#'
#' @param s Character vector of crop names.
#'
#' @return A character vector of normalized crop names.
#' @export
normalize_name = function(s) {
  s |>
    tolower() |>
    stringr::str_replace_all("\\(.*?\\)", "") |>
    stringr::str_replace_all("grouped for remote sensing only", "") |>
    stringr::str_replace_all("\\bor\\b", "") |>
    stringr::str_replace_all("\\band\\b", "") |>
    stringr::str_replace_all("&", "") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish()
}


#' Parse candidate crop names
#'
#' Splits candidate crop labels and retains names that match a supplied set of
#' known crop names.
#'
#' @param s A single character string containing one or more candidate names.
#' @param known Character vector of known crop names.
#'
#' @return A character vector of unique matching crop names.
#' @export
parse_candidates = function(s, known) {
  if (is.na(s) || nchar(s) == 0) {
    return(character(0))
  }
  
  parts = stringr::str_split(s, " ?/ ?")[[1]] |>
    stringr::str_squish()
  
  prefix = if (stringr::str_detect(parts[1], ",")) {
    stringr::str_extract(parts[1], "^[^,]+, ")
  } else {
    ""
  }
  
  result = character()
  
  for (p in parts) {
    if (tolower(p) %in% tolower(known)) {
      result = c(result, p)
    } else if (nchar(prefix) > 0) {
      combined = paste0(prefix, p)
      
      if (tolower(combined) %in% tolower(known)) {
        result = c(result, combined)
      }
    }
  }
  
  unique(result)
}


#' Normalize crop lookup keys
#'
#' Converts crop labels to lowercase, replaces ampersands with "and", removes
#' punctuation, and collapses repeated whitespace.
#'
#' @param x Character vector of crop labels.
#'
#' @return A character vector of normalized crop lookup keys.
#' @export
normalize_crop_key = function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&", "and") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish()
}


#' Classify PFTs into annual or perennial families
#'
#' @param pft Character vector of plant functional type labels.
#'
#' @return A character vector containing `"annual"`, `"perennial"`, or `NA`.
#' @export
pft_family = function(pft) {
  dplyr::case_when(
    pft %in% c("row", "hay", "rice") ~ "annual",
    pft == "woody" ~ "perennial",
    TRUE ~ NA_character_
  )
}


#' Build the LandIQ-to-MAGiC crop lookup
#'
#' Builds a lookup connecting LandIQ crop codes to the crop categories used in
#' the MAGiC scenario files.
#'
#' @param lookup_path Path to the LandIQ crop-code lookup CSV.
#'
#' @return A tibble with LandIQ codes and matched MAGiC scenario crop labels.
#' @importFrom rlang .data
#' @export
build_ncc_crop_lookup = function(lookup_path) {
  if (!file.exists(lookup_path)) {
    PEcAn.logger::logger.severe(
      "LandIQ lookup table not found: ",
      lookup_path
    )
  }
  
  code_map = readr::read_csv(
    lookup_path,
    show_col_types = FALSE
  )
  
  required_lookup_cols = c(
    "CLASS",
    "SUBCLASS",
    "CLASS_desc",
    "SUBCLASS_desc",
    "crops_included"
  )
  
  missing_lookup_cols = setdiff(
    required_lookup_cols,
    names(code_map)
  )
  
  if (length(missing_lookup_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Lookup table missing columns: ",
      paste(missing_lookup_cols, collapse = ", ")
    )
  }
  
  code_map = code_map |>
    dplyr::mutate(
      CLASS = as.character(.data$CLASS),
      SUBCLASS = as.character(.data$SUBCLASS),
      code = paste0(.data$CLASS, .data$SUBCLASS),
      desc_key = normalize_crop_key(
        paste(
          .data$CLASS_desc,
          .data$SUBCLASS_desc,
          .data$crops_included
        )
      )
    )
  
  crop_lookup = code_map |>
    dplyr::mutate(
      scenario_crop = dplyr::case_when(
        # berries / strawberries
        .data$CLASS == "T" & .data$SUBCLASS %in% c("19", "28") ~
          "All Other Berries",
        .data$CLASS == "T" & .data$SUBCLASS == "20" ~
          "Strawberries (Fresh Market)",
        
        # almonds / nuts
        .data$CLASS == "D" & .data$SUBCLASS == "12" ~
          "Almonds",
        .data$CLASS == "D" & .data$SUBCLASS %in% c("13", "14", "17") ~
          "All Other Nut Crops",
        
        # pome / stone / other deciduous fruit
        .data$CLASS == "D" & .data$SUBCLASS %in% c("1", "6") ~
          "Pome Fruit",
        .data$CLASS == "D" &
          .data$SUBCLASS %in% c("2", "3", "5", "7", "8", "16") ~
          "Stone Fruit",
        .data$CLASS == "D" ~
          "All Other Fruit Crops",
        
        # matches scenario transition-state map where C -> Citrus
        .data$CLASS == "C" ~
          "Citrus",
        
        # grapes
        .data$CLASS == "V" & .data$SUBCLASS == "1" ~
          "Grapes, Table",
        .data$CLASS == "V" & .data$SUBCLASS == "2" ~
          "Grapes, Wine",
        .data$CLASS == "V" & .data$SUBCLASS == "3" ~
          "Grapes Dried, Raisins",
        .data$CLASS == "V" ~
          "Grapes, Wine",
        
        # fallow
        .data$CLASS == "X" ~
          "Fallow",
        
        # broad annual groups
        .data$CLASS %in% c("F", "P") ~
          "All Other Field Crops (Incl. Pasture /Rangeland)",
        .data$CLASS %in% c("G", "R", "T") ~
          "Annual Cropland",
        
        TRUE ~ NA_character_
      ),
      scenario_crop_key = normalize_crop_key(.data$scenario_crop)
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "code",
          "CLASS",
          "SUBCLASS",
          "CLASS_desc",
          "SUBCLASS_desc",
          "scenario_crop",
          "scenario_crop_key"
        )
      )
    ) |>
    dplyr::distinct()
  
  dup_codes = crop_lookup |>
    dplyr::count(.data$code) |>
    dplyr::filter(.data$n > 1)
  
  if (nrow(dup_codes) > 0) {
    PEcAn.logger::logger.warn(
      sprintf(
        paste0(
          "Some LandIQ codes map to multiple scenario crops ",
          "(%d duplicates found): %s"
        ),
        nrow(dup_codes),
        paste(dup_codes$code, collapse = ", ")
      )
    )
  }
  
  crop_lookup
}


#' Read projected county crop data
#'
#' Reads a projected county CSV, validates required columns, filters to the
#' requested years, and standardizes fields used by the fertilization and NCC
#' workflows.
#'
#' @param fn Path to a projected county CSV file.
#' @param years Integer vector of years to retain.
#' @param scenario_value Character string identifying the management scenario.
#'
#' @return A tibble of standardized projected crop records.
#' @export
read_projected_county = function(fn, years, scenario_value) {
  PEcAn.logger::logger.info(
    "Reading: ",
    basename(fn)
  )
  
  dt = readr::read_csv(
    fn,
    show_col_types = FALSE
  )
  
  if (!"season" %in% names(dt)) {
    dt$season = 0L
  }
  
  required_cols = c(
    "parcel_id",
    "county",
    "year",
    "CLASS",
    "SUBCLASS",
    "PFT",
    "planting_date",
    "ACRES"
  )
  
  missing_cols = setdiff(
    required_cols,
    names(dt)
  )
  
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Projected file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "\nFile: ",
      fn,
      "\nAvailable columns: ",
      paste(names(dt), collapse = ", ")
    )
  }
  
  dt |>
    dplyr::filter(
      .data$year %in% .env$years,
      !is.na(.data$CLASS),
      !is.na(.data$SUBCLASS),
      !is.na(.data$PFT),
      !is.na(.data$planting_date),
      !is.na(.data$ACRES),
      .data$ACRES > 0
    ) |>
    dplyr::transmute(
      parcel_id = as.integer(.data$parcel_id),
      county = as.character(.data$county),
      year = as.integer(.data$year),
      season = as.integer(
        dplyr::coalesce(.data$season, 0L)
      ),
      anchor = as.Date(.data$planting_date),
      code = paste0(
        as.character(.data$CLASS),
        as.character(.data$SUBCLASS)
      ),
      PFT = as.character(.data$PFT),
      ACRES = as.numeric(.data$ACRES),
      scenario = .env$scenario_value
    )
}




#' Write a parcel batch to parquet
#'
#' Filters a data frame to a supplied set of parcel IDs and writes the result as
#' one parquet shard.
#'
#' @param pids Parcel IDs to write.
#' @param df Data frame containing parcel-level output.
#' @param out_path Directory where the parquet shard will be written.
#' @param codec Parquet compression codec. Defaults to `"ZSTD"`.
#'
#' @return The written file path, or `NULL` when the shard is empty.
#' @export
write_batch = function(pids, df, out_path, codec = "ZSTD") {
  shard = df |>
    dplyr::filter(.data$parcel_id %in% pids)
  
  if (nrow(shard) == 0) {
    return(NULL)
  }
  
  pid_min = min(
    shard[["parcel_id"]],
    na.rm = TRUE
  )
  
  pid_max = max(
    shard[["parcel_id"]],
    na.rm = TRUE
  )
  
  fn = file.path(
    out_path,
    sprintf("%d_%d.parquet", pid_min, pid_max)
  )
  
  arrow::write_parquet(
    shard,
    fn,
    compression = codec
  )
  
  fn
}


#' Assign NCC events across ensemble members
#'
#' Randomly orders parcels within scenario/county/year/crop groups and retains
#' enough parcels to meet each group's compost-acre target.
#'
#' @param design_targets Data frame containing parcel design rows and compost
#'   acreage targets.
#' @param n_ensemble Number of ensemble members to generate.
#'
#' @return A tibble containing assigned NCC events for all ensemble members.
#' @export
assign_events = function(design_targets, n_ensemble) {
  group_cols = c(
    "scenario",
    "county",
    "year",
    "scenario_crop_key"
  )
  
  out_list = vector(
    "list",
    n_ensemble
  )
  
  for (e in seq_len(n_ensemble)) {
    dt_e = design_targets |>
      dplyr::mutate(
        rand = stats::runif(dplyr::n())
      ) |>
      dplyr::arrange(
        dplyr::across(dplyr::all_of(group_cols)),
        .data$rand
      ) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(group_cols))
      ) |>
      dplyr::mutate(
        cum_acres = cumsum(.data$ACRES),
        prev_cum_acres = dplyr::lag(
          .data$cum_acres,
          default = 0
        )
      ) |>
      dplyr::filter(
        .data$cum_acres <= .data$target_compost_acres |
          .data$prev_cum_acres < .data$target_compost_acres
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        ensemble_member = e,
        ens_id = sprintf("ens_%03d", e)
      )
    
    out_list[[e]] = dt_e
  }
  
  dplyr::bind_rows(out_list)
}


#' Standardize prediction keys
#'
#' Standardizes parcel, year, class, subclass, and crop-code fields used when
#' joining management outputs to projected crop files.
#'
#' @param df A prediction data frame.
#'
#' @return The input data frame with standardized key columns and `crop_code`.
#' @keywords internal
standardize_prediction_keys = function(df) {
  required = c(
    "parcel_id",
    "year",
    "CLASS",
    "SUBCLASS"
  )
  
  missing_required = setdiff(
    required,
    names(df)
  )
  
  if (length(missing_required) > 0) {
    stop(
      "Prediction file missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  df = df |>
    dplyr::mutate(
      parcel_id = as.character(.data$parcel_id),
      year = as.integer(.data$year),
      CLASS = as.character(.data$CLASS),
      SUBCLASS = as.character(.data$SUBCLASS),
      SUBCLASS = dplyr::if_else(
        is.na(.data$SUBCLASS) |
          .data$SUBCLASS == "" |
          .data$SUBCLASS == "NA",
        NA_character_,
        .data$SUBCLASS
      )
    )
  
  if ("code" %in% names(df)) {
    df = df |>
      dplyr::mutate(
        code = as.character(.data$code),
        crop_code = dplyr::if_else(
          !is.na(.data$code) & .data$code != "",
          .data$code,
          paste0(
            .data$CLASS,
            dplyr::coalesce(.data$SUBCLASS, "")
          )
        )
      )
  } else {
    df = df |>
      dplyr::mutate(
        crop_code = paste0(
          .data$CLASS,
          dplyr::coalesce(.data$SUBCLASS, "")
        )
      )
  }
  
  df
}
