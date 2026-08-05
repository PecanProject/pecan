
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

parse_candidates = function(s, known) {
  if (is.na(s) || nchar(s) == 0) return(character(0))
  
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

normalize_crop_key = function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&", "and") |>
    stringr::str_replace_all("[[:punct:]]", " ") |>
    stringr::str_squish()
}

pft_family = function(pft) {
  dplyr::case_when(
    pft %in% c("row", "hay", "rice") ~ "annual",
    pft == "woody" ~ "perennial",
    TRUE ~ NA_character_
  )
}

build_ncc_crop_lookup = function(lookup_path) {
  if (!file.exists(lookup_path)) {
    PEcAn.logger::logger.severe("LandIQ lookup table not found: ", lookup_path)
  }
  
  code_map = readr::read_csv(lookup_path, show_col_types = FALSE) |>
    dplyr::mutate(
      CLASS = as.character(.data$CLASS), 
      SUBCLASS = as.character(.data$SUBCLASS),
      code = paste0(.data$CLASS, .data$SUBCLASS),
      desc_key = normalize_crop_key(paste(.data$CLASS_desc, .data$SUBCLASS_desc, .data$crops_included))
    )
  
  required_lookup_cols = c("CLASS", "SUBCLASS", "CLASS_desc", "SUBCLASS_desc")
  missing_lookup_cols = setdiff(required_lookup_cols, names(code_map))
  
  if (length(missing_lookup_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Lookup table missing columns: ",
      paste(missing_lookup_cols, collapse = ", ")
    )
  }
  
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
        .data$CLASS == "D" & .data$SUBCLASS %in% c("2", "3", "5", "7", "8", "16") ~
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
      "code", "CLASS", "SUBCLASS", "CLASS_desc", "SUBCLASS_desc", "scenario_crop", "scenario_crop_key"
    ) |>
    dplyr::distinct()
  
  dup_codes = crop_lookup |>
    dplyr::count(.data$code) |>
    dplyr::filter(.data$n > 1)
  
  if (nrow(dup_codes) > 0) {
    PEcAn.logger::logger.warn(
      sprintf("Some LandIQ codes map to multiple scenario crops (%d duplicates found): %s",
              nrow(dup_codes), paste(dup_codes$code, collapse = ", "))
    )
  }
  
  crop_lookup
}

read_projected_county = function(fn) {
  PEcAn.logger::logger.info("Reading: ", basename(fn))
  
  dt = readr::read_csv(fn, show_col_types = FALSE)
  
  if (!"season" %in% names(dt)) { dt$season = 0L }
  
  required_cols = c("parcel_id", "county", "year", "CLASS", "SUBCLASS", "PFT", "planting_date", "ACRES")
  missing_cols  = setdiff(required_cols, names(dt))
  
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "Projected file is missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nFile: ", fn, 
      "\nAvailable columns: ", paste(names(dt), collapse = ", ")
    )
  }
  
  dt |>
    dplyr::filter(
      .data$year %in% config[["years"]],
      !is.na(.data$CLASS),
      !is.na(.data$SUBCLASS),
      !is.na(.data$PFT),
      !is.na(.data$planting_date),
      !is.na(.data$ACRES),
      .data$ACRES > 0
    ) |>
    dplyr::transmute(
      parcel_id = as.integer(.data$parcel_id),
      county    = as.character(.data$county),
      year      = as.integer(.data$year),
      season    = as.integer(dplyr::coalesce(.data$season, 0L)),
      anchor    = as.Date(.data$planting_date),
      code      = paste0(as.character(.data$CLASS), as.character(.data$SUBCLASS)),
      PFT       = as.character(.data$PFT),
      ACRES     = as.numeric(.data$ACRES),
      scenario  = config[["scenario"]]
    )
}

write_batch = function(pids, df, out_path, codec = "ZSTD") {
  shard = df |> dplyr::filter(.data$parcel_id %in% pids)
  
  if (nrow(shard) == 0) return(NULL) # Guard against empty shards
  
  pid_min = min(shard[["parcel_id"]], na.rm = TRUE)
  pid_max = max(shard[["parcel_id"]], na.rm = TRUE)
  
  fn = file.path(out_path, sprintf("%d_%d.parquet", pid_min, pid_max))
  arrow::write_parquet(shard, fn, compression = codec)
  
  return(fn)
}

assign_events = function(design_targets, n_ensemble) {
  dt = data.table::as.data.table(design_targets)
  out_list = vector("list", n_ensemble)
  group_cols = c("scenario", "county", "year", "scenario_crop_key")
  
  for (e in seq_len(n_ensemble)) {
    dt_e = data.table::copy(dt)
    dt_e[, rand := runif(.N)]
    data.table::setorderv(dt_e, c(group_cols, "rand"))
    
    dt_e[, cum_acres := cumsum(ACRES), by = group_cols]
    dt_e[, prev_cum_acres := data.table::shift(cum_acres, fill = 0), by = group_cols]
    
    dt_e = dt_e[cum_acres <= target_compost_acres | prev_cum_acres < target_compost_acres]
    dt_e[, ensemble_member := e]
    dt_e[, ens_id := sprintf("ens_%03d", e)]
    
    out_list[[e]] = dt_e
  }
  
  dplyr::as_tibble(data.table::rbindlist(out_list, use.names = TRUE, fill = TRUE))
}