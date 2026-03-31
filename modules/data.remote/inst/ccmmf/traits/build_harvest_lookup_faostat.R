# Build long-format harvest lookup from FAOSTAT-style crop statistics plus LandIQ codes.
# Joins each FAOSTAT item to LandIQ crop_desc, aggregates DRYAD-style variables at subclass,
# then class x PFT, then PFT, then global, and derives harvest removal fractions (mass-balanced
# AGB using HI and yield, or clamped yield). Row levels follow coalesce(subclass, class, pft,
# placeholder, global) so pool_calculations_from_lookup.R can fall back the same way as
# build_harvest_lookup.R. If the Excel file is missing, uses HARVEST_PFT_SUMMARY_CSV only.
#
# Main inputs: CCMMF_MANAGEMENT; HARVEST_FAOSTAT_XLSX (columns item, variable, value);
#   HARVEST_PFT_SUMMARY_CSV for PFT-only fallback; LandIQ_cropCode_lookup_table.csv.
# Main outputs: plant_traits/harvest_lookup_long_faostat.rds and .csv (does not overwrite harvest_lookup_long.rds).
# How to run: Rscript scripts/traits/build_harvest_lookup_faostat.R from repo root (set CCMMF_MANAGEMENT).
# Workflow: optional harvest lookup for events; set HARVEST_LOOKUP_RDS to this RDS in make_events_statewide.R.

#### Load packages

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(tidyr)
})

#### Paths and constants

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
landiq_lookup_csv <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
plant_traits_dir <- file.path(path_management, "plant_traits")
out_rds <- file.path(plant_traits_dir, "harvest_lookup_long_faostat.rds")
out_csv <- file.path(plant_traits_dir, "harvest_lookup_long_faostat.csv")

faostat_xlsx <- Sys.getenv(
  "HARVEST_FAOSTAT_XLSX",
  "/projectnb/dietzelab/mkim/Harvest Data/new_output.xlsx"
)
pft_summary_csv <- Sys.getenv(
  "HARVEST_PFT_SUMMARY_CSV",
  "/projectnb/dietzelab/mkim/Harvest Data/harvest_pools_output.csv"
)

rs_root_shoot <- 0.20

harvest_param_names <- c("AGB_REMOVED", "AGB_LITTER", "BGB_REMOVED", "BGB_LITTER")

placeholder_means_wide <- tibble::tribble(
  ~PFT,                  ~AGB_REMOVED, ~AGB_LITTER, ~BGB_REMOVED, ~BGB_LITTER,
  "row",                       0.80,       0.20,         0.00,        1.00,
  "rice",                      0.80,       0.20,         0.00,        1.00,
  "hay",                       0.75,       0.15,         0.00,        0.00,
  "woody",                     0.15,       0.015,        0.00,        0.00,
  "woody_destructive",         0.80,       0.20,         0.00,        1.00
)

req_var_names <- c(
  "Maximum_Above_ground_biomass_kg_DM_ha",
  "Mean_Grain_yield_kg_DM_ha",
  "HI",
  "CR_removed_pc",
  "Mean_Above_ground_biomass_kg_DM_ha",
  "Yield_kg_DM_ha",
  "Mean_Harvest_index",
  "Ratio_residues_removed_from_field"
)

#### LandIQ load and FAOSTAT item to crop_desc mapping

# Agricultural LandIQ rows only; need PFT (same as build_planting_lookup / harvest).
load_landiq_mapping <- function(path = landiq_lookup_csv) {
  d <- as.data.frame(fread(path))
  d %>%
    mutate(SUBCLASS = as.character(SUBCLASS)) %>%
    filter(is_agricultural == TRUE, !is.na(PFT)) %>%
    transmute(class = CLASS, subclass = SUBCLASS, crop_desc = SUBCLASS_desc, class_desc = CLASS_desc, PFT = PFT)
}

norm_item <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[!nzchar(x) | is.na(x)] <- NA_character_
  x
}

# Exact LandIQ SUBCLASS_desc strings for FAOSTAT items that do not match automatically.
manual_item_to_crop <- tibble::tibble(
  item_clean = norm_item(c(
    "maize", "rice, paddy", "soybeans", "wheat", "barley", "oats", "sunflower seed",
    "rapeseed", "cotton", "sugar beets", "potatoes", "tomatoes", "bananas",
    "cassava", "coconuts", "triticale", "lentils", "sesame seed", "tea",
    "tobacco, unmanufactured", "vetches", "alfalfa"
  )),
  crop_desc = c(
    "Corn (field & sweet)",
    "Rice",
    "Beans (dry)",
    "Wheat",
    "Barley",
    "Oats",
    "Sunflowers",
    "Safflower",
    "Cotton",
    "Sugar beets",
    "Potatoes",
    "Tomatoes (processing)",
    "Miscellaneous deciduous",
    "Miscellaneous field",
    "Miscellaneous deciduous",
    "Miscellaneous grain and hay",
    "Beans (dry)",
    "Castor beans",
    "Miscellaneous field",
    "Miscellaneous field",
    "Beans (dry)",
    "Alfalfa and alfalfa mixtures"
  )
)

map_item_to_landiq <- function(item_clean, codes_tbl, manual_tbl) {
  if (is.na(item_clean)) {
    return(tibble::tibble(
      crop_desc = NA_character_, class = NA_character_,
      subclass = NA_character_, PFT = NA_character_
    ))
  }
  hit <- manual_tbl %>% dplyr::filter(.data$item_clean == item_clean)
  if (nrow(hit) == 1L) {
    m <- codes_tbl %>% dplyr::filter(.data$crop_desc == hit$crop_desc[1])
    if (nrow(m) >= 1L) {
      return(m %>% dplyr::slice(1) %>% dplyr::select(crop_desc, class, subclass, PFT))
    }
  }
  hits <- codes_tbl %>% dplyr::filter(.data$norm_crop == item_clean)
  if (nrow(hits) == 1L) {
    return(hits %>% dplyr::select(crop_desc, class, subclass, PFT))
  }
  if (nrow(hits) > 1L) {
    return(hits %>% dplyr::arrange(nchar(.data$crop_desc)) %>% dplyr::slice(1) %>%
      dplyr::select(crop_desc, class, subclass, PFT))
  }
  hits <- codes_tbl %>% dplyr::filter(stringr::str_detect(.data$norm_crop, stringr::fixed(item_clean)))
  if (nrow(hits) >= 1L) {
    return(hits %>% dplyr::arrange(nchar(.data$crop_desc)) %>% dplyr::slice(1) %>%
      dplyr::select(crop_desc, class, subclass, PFT))
  }
  words <- unlist(stringr::str_split(item_clean, "\\s+"))
  words <- words[nzchar(words)]
  if (length(words) > 0L) {
    patt <- paste0("\\b(", paste(stringr::str_escape(words), collapse = "|"), ")\\b")
    hits <- codes_tbl %>% dplyr::filter(stringr::str_detect(.data$norm_crop, patt))
    if (nrow(hits) >= 1L) {
      return(hits %>% dplyr::arrange(nchar(.data$crop_desc)) %>% dplyr::slice(1) %>%
        dplyr::select(crop_desc, class, subclass, PFT))
    }
  }
  tibble::tibble(
    crop_desc = NA_character_, class = NA_character_,
    subclass = NA_character_, PFT = NA_character_
  )
}

#### Harvest fractions from wide trait row (HI, yield, residue)

harvest_fractions_from_wide <- function(df_wide, pft_for_placeholder, cr_fallback) {
  for (cn in req_var_names) {
    if (!cn %in% names(df_wide)) {
      df_wide[[cn]] <- NA_real_
    }
  }
  cr_use <- suppressWarnings(as.numeric(df_wide$CR_removed_pc[1]))
  if (is.na(cr_use)) {
    cr_use <- cr_fallback
  }
  cr <- dplyr::coalesce(
    suppressWarnings(as.numeric(df_wide$CR_removed_pc[1])),
    suppressWarnings(as.numeric(df_wide$Ratio_residues_removed_from_field[1])),
    cr_use
  )
  if (is.na(cr) || !is.finite(cr)) {
    cr <- 0
  }
  cr <- pmin(pmax(cr, 0), 100)

  agb <- suppressWarnings(as.numeric(df_wide$Mean_Above_ground_biomass_kg_DM_ha[1]))
  if (is.na(agb) || !is.finite(agb) || agb <= 0) {
    agb <- suppressWarnings(as.numeric(df_wide$Maximum_Above_ground_biomass_kg_DM_ha[1]))
  }
  hi_raw <- dplyr::coalesce(
    suppressWarnings(as.numeric(df_wide$HI[1])),
    suppressWarnings(as.numeric(df_wide$Mean_Harvest_index[1]))
  )
  yield_obs <- dplyr::coalesce(
    suppressWarnings(as.numeric(df_wide$Yield_kg_DM_ha[1])),
    suppressWarnings(as.numeric(df_wide$Mean_Grain_yield_kg_DM_ha[1]))
  )

  yield_use <- NA_real_
  residue <- NA_real_
  if (!is.na(hi_raw) && is.finite(hi_raw)) {
    hi_c <- pmin(pmax(hi_raw, 0), 1)
    yield_use <- hi_c * agb
    residue <- agb - yield_use
  } else if (!is.na(yield_obs) && is.finite(yield_obs) && !is.na(agb) && agb > 0) {
    yield_use <- pmin(pmax(yield_obs, 0), agb)
    residue <- agb - yield_use
  }

  ph <- placeholder_means_wide %>% dplyr::filter(.data$PFT == pft_for_placeholder)
  if (nrow(ph) != 1L) {
    ph <- placeholder_means_wide %>% dplyr::filter(.data$PFT == "row")
  }

  if (is.na(agb) || !is.finite(agb) || agb <= 0 || is.na(yield_use) || is.na(residue)) {
    return(c(
      AGB_REMOVED = ph$AGB_REMOVED[1], AGB_LITTER = ph$AGB_LITTER[1],
      BGB_REMOVED = ph$BGB_REMOVED[1], BGB_LITTER = ph$BGB_LITTER[1],
      source = "placeholder"
    ))
  }

  agb_rm <- yield_use + residue * (cr / 100)
  agb_lit <- residue * (1 - cr / 100)
  c(
    AGB_REMOVED = agb_rm / agb, AGB_LITTER = agb_lit / agb,
    BGB_REMOVED = 0, BGB_LITTER = 1,
    source = "calculated"
  )
}

agg_to_fraction_row <- function(agg_slice, pft_label, cr_fb) {
  if (nrow(agg_slice) == 0L) {
    return(NULL)
  }
  wide <- agg_slice %>%
    dplyr::select(variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)
  v <- harvest_fractions_from_wide(wide, pft_label, cr_fb)
  tibble::as_tibble(as.list(v))
}

#### Read FAOSTAT Excel and PFT summary CSV fallback

read_faostat_excel <- function(path_xlsx) {
  if (!nzchar(path_xlsx) || !file.exists(path_xlsx)) {
    return(NULL)
  }
  tryCatch(
    readxl::read_excel(path_xlsx),
    error = function(e) {
      warning("Could not read HARVEST_FAOSTAT_XLSX: ", conditionMessage(e))
      NULL
    }
  )
}

try_pft_summary_csv <- function(path_csv) {
  if (!nzchar(path_csv) || !file.exists(path_csv)) {
    return(tibble::tibble())
  }
  raw <- readr::read_csv(path_csv, show_col_types = FALSE)
  pc_cols <- c("AGB_REMOVED_pc", "AGB_LITTER_pc", "BGB_REMOVED_pc", "BGB_LITTER_pc")
  if (!all(pc_cols %in% names(raw))) {
    stop("HARVEST_PFT_SUMMARY_CSV must contain columns: ", paste(pc_cols, collapse = ", "))
  }
  raw %>%
    dplyr::mutate(
      AGB_REMOVED = .data$AGB_REMOVED_pc / 100,
      AGB_LITTER = .data$AGB_LITTER_pc / 100,
      BGB_REMOVED = .data$BGB_REMOVED_pc / 100,
      BGB_LITTER = .data$BGB_LITTER_pc / 100,
      n_crops = if ("n_crops" %in% names(raw)) as.integer(.data$n_crops) else 0L,
      source = if ("source" %in% names(raw)) as.character(.data$source) else "csv"
    ) %>%
    dplyr::select(PFT, AGB_REMOVED, AGB_LITTER, BGB_REMOVED, BGB_LITTER, n_crops, source)
}

merge_pft_with_placeholders <- function(summary_tbl) {
  ph <- placeholder_means_wide %>% dplyr::mutate(PFT = as.character(PFT))
  if (nrow(summary_tbl) == 0) {
    return(ph %>% dplyr::mutate(n_crops = 0L, source = "placeholder"))
  }
  keyed <- summary_tbl %>%
    dplyr::mutate(PFT = as.character(PFT)) %>%
    dplyr::rename(
      AGB_REMOVED_f = AGB_REMOVED,
      AGB_LITTER_f = AGB_LITTER,
      BGB_REMOVED_f = BGB_REMOVED,
      BGB_LITTER_f = BGB_LITTER
    )
  ph %>%
    dplyr::left_join(keyed, by = "PFT") %>%
    dplyr::mutate(
      AGB_REMOVED = dplyr::coalesce(.data$AGB_REMOVED_f, .data$AGB_REMOVED),
      AGB_LITTER = dplyr::coalesce(.data$AGB_LITTER_f, .data$AGB_LITTER),
      BGB_REMOVED = dplyr::coalesce(.data$BGB_REMOVED_f, .data$BGB_REMOVED),
      BGB_LITTER = dplyr::coalesce(.data$BGB_LITTER_f, .data$BGB_LITTER),
      n_crops = dplyr::coalesce(as.integer(.data$n_crops), 0L),
      source = dplyr::coalesce(.data$source, "placeholder")
    ) %>%
    dplyr::select(PFT, AGB_REMOVED, AGB_LITTER, BGB_REMOVED, BGB_LITTER, n_crops, source)
}

#### LandIQ subclass and class frames (woody vs woody_destructive)

# Same woody / woody_destructive duplication as build_harvest_lookup.R.
landiq_subclass_class_frames <- function(mapping_df) {
  subclass_valid <- mapping_df %>% dplyr::distinct(class, subclass, crop_desc, PFT)
  woody_sub <- subclass_valid %>% dplyr::filter(PFT == "woody") %>% dplyr::mutate(PFT = "woody_destructive")
  subclass_valid <- dplyr::bind_rows(subclass_valid, woody_sub)
  class_valid <- mapping_df %>%
    dplyr::distinct(level_id = class, class_desc, PFT) %>%
    dplyr::mutate(class = level_id, crop_desc = class_desc, subclass = NA_character_) %>%
    dplyr::select(level_id, class, subclass, crop_desc, PFT)
  woody_cls <- class_valid %>% dplyr::filter(PFT == "woody") %>% dplyr::mutate(PFT = "woody_destructive")
  class_valid <- dplyr::bind_rows(class_valid, woody_cls)
  list(subclass_valid = subclass_valid, class_valid = class_valid)
}

# Long harvest params from wide PFT table (CSV path or internal); same shape as build_harvest_lookup join.
pft_wide_to_placeholders_long <- function(pft_wide_tbl) {
  pft_wide_tbl %>%
    tidyr::pivot_longer(
      c(AGB_REMOVED, AGB_LITTER, BGB_REMOVED, BGB_LITTER),
      names_to = "param",
      values_to = "mean_obs"
    ) %>%
    dplyr::mutate(
      sd_obs = NA_real_,
      n_obs = as.integer(ifelse(.data$source == "placeholder" | is.na(.data$n_crops), 0L, .data$n_crops))
    ) %>%
    dplyr::select(PFT, param, mean_obs, sd_obs, n_obs)
}

# Stack subclass / class / pft / global from PFT-only long rows (CSV fallback = build_harvest_lookup.R layout).
harvest_long_from_pft_placeholders <- function(mapping_df, placeholders_long) {
  skel <- landiq_subclass_class_frames(mapping_df)
  tbl_sub <- skel$subclass_valid %>%
    dplyr::left_join(placeholders_long, by = "PFT", relationship = "many-to-many") %>%
    dplyr::mutate(level = "subclass")
  tbl_class <- skel$class_valid %>%
    dplyr::select(-level_id) %>%
    dplyr::left_join(placeholders_long, by = "PFT", relationship = "many-to-many") %>%
    dplyr::mutate(level = "class")
  tbl_pft <- placeholders_long %>%
    dplyr::mutate(level = "pft", class = NA_character_, subclass = NA_character_, crop_desc = NA_character_)
  tbl_global <- placeholders_long %>%
    dplyr::group_by(param) %>%
    dplyr::summarise(mean_obs = mean(mean_obs, na.rm = TRUE), sd_obs = NA_real_, n_obs = 0L, .groups = "drop") %>%
    dplyr::mutate(
      level = "global",
      class = NA_character_,
      subclass = NA_character_,
      crop_desc = NA_character_,
      PFT = NA_character_
    )
  dplyr::bind_rows(tbl_sub, tbl_class, tbl_pft, tbl_global) %>%
    dplyr::relocate(level, PFT, class, subclass, crop_desc, param, mean_obs, sd_obs, n_obs) %>%
    dplyr::arrange(level, PFT, class, subclass, param)
}

write_harvest_lookup_outputs <- function(harvest_lookup_long, rds_path, csv_path) {
  saveRDS(harvest_lookup_long, rds_path)
  readr::write_csv(harvest_lookup_long, csv_path)
  cat("Wrote ", rds_path, "\n", sep = "")
  cat("Wrote ", csv_path, "\n", sep = "")
}

#### Create output directory

dir.create(plant_traits_dir, recursive = TRUE, showWarnings = FALSE)

#### Load LandIQ

cat("Loading LandIQ mapping (agricultural classes only)...\n")
mapping <- load_landiq_mapping(landiq_lookup_csv)
codes <- mapping %>%
  dplyr::distinct(crop_desc, class, subclass, PFT) %>%
  dplyr::mutate(norm_crop = norm_item(.data$crop_desc))

harvest_tbl <- read_faostat_excel(faostat_xlsx)
use_excel <- !is.null(harvest_tbl) &&
  all(c("item", "variable", "value") %in% names(harvest_tbl))

if (!use_excel) {
  cat("No usable FAOSTAT Excel; using PFT summary CSV path.\n")
  summary_csv <- try_pft_summary_csv(pft_summary_csv)
  if (nrow(summary_csv) == 0) {
    stop("Need HARVEST_FAOSTAT_XLSX with item/variable/value or HARVEST_PFT_SUMMARY_CSV.")
  }
  pft_wide <- merge_pft_with_placeholders(summary_csv)
  pl <- pft_wide_to_placeholders_long(pft_wide)
  harvest_lookup_long <- harvest_long_from_pft_placeholders(mapping, pl)
  write_harvest_lookup_outputs(harvest_lookup_long, out_rds, out_csv)
  quit(save = "no", status = 0)
}

cat("Joining FAOSTAT items to LandIQ crop_desc (planting-style identity)...\n")

cr_fb <- NA_real_
if ("original_crop" %in% names(harvest_tbl)) {
  cr_fb <- harvest_tbl %>%
    dplyr::filter(.data$original_crop == "All_crops", .data$variable == "CR_removed_pc") %>%
    dplyr::summarise(v = mean(as.numeric(.data$value), na.rm = TRUE)) %>%
    dplyr::pull(v)
}

traits_long <- harvest_tbl %>%
  dplyr::mutate(
    item_clean = norm_item(.data$item),
    value_num = suppressWarnings(as.numeric(.data$value))
  ) %>%
  dplyr::filter(!is.na(.data$item_clean), !is.na(.data$variable))

uniq_items <- unique(traits_long$item_clean)
item_rows <- vector("list", length(uniq_items))
for (ii in seq_along(uniq_items)) {
  item_rows[[ii]] <- map_item_to_landiq(uniq_items[ii], codes, manual_item_to_crop) %>%
    dplyr::mutate(item_clean = uniq_items[ii])
}
item_map <- dplyr::bind_rows(item_rows)

n_mapped <- sum(!is.na(item_map$class))
cat("  Mapped ", n_mapped, " of ", nrow(item_map), " distinct items to a LandIQ subclass.\n", sep = "")

harvest_join <- traits_long %>%
  dplyr::left_join(item_map, by = "item_clean") %>%
  dplyr::filter(!is.na(.data$class), !is.na(.data$subclass))

cat("  Trait rows after LandIQ join: ", nrow(harvest_join), "\n", sep = "")

if (nrow(harvest_join) == 0L) {
  stop(
    "No FAOSTAT rows joined to LandIQ (check item names vs SUBCLASS_desc and manual_item_to_crop). ",
    "Or use HARVEST_PFT_SUMMARY_CSV only by removing/renaming the Excel path."
  )
}

#### Summarize at subclass (mean per variable, like TRY to subclass)

sub_agg <- harvest_join %>%
  dplyr::group_by(class, subclass, crop_desc, PFT, variable) %>%
  dplyr::summarise(
    value = mean(.data$value_num, na.rm = TRUE),
    n_obs = dplyr::n(),
    .groups = "drop"
  )

sub_keys <- sub_agg %>%
  dplyr::distinct(class, subclass, crop_desc, PFT)

cat("Computing harvest fractions per subclass key...\n")
sub_frac_list <- vector("list", nrow(sub_keys))
for (i in seq_len(nrow(sub_keys))) {
  k <- sub_keys[i, , drop = FALSE]
  sl <- sub_agg %>%
    dplyr::filter(
      .data$class == k$class[1], .data$subclass == k$subclass[1],
      .data$crop_desc == k$crop_desc[1], .data$PFT == k$PFT[1]
    )
  fr <- agg_to_fraction_row(sl, k$PFT[1], cr_fb)
  if (!is.null(fr)) {
    sub_frac_list[[i]] <- dplyr::bind_cols(k, fr)
  }
}
frac_sub <- dplyr::bind_rows(sub_frac_list)

#### Summarize at class x PFT (re-aggregate raw rows, like planting class level)

class_agg <- harvest_join %>%
  dplyr::group_by(class, PFT, variable) %>%
  dplyr::summarise(value = mean(.data$value_num, na.rm = TRUE), n_obs = dplyr::n(), .groups = "drop")

class_keys <- class_agg %>% dplyr::distinct(class, PFT)

cat("Computing harvest fractions per class x PFT...\n")
class_frac_list <- vector("list", nrow(class_keys))
for (i in seq_len(nrow(class_keys))) {
  k <- class_keys[i, , drop = FALSE]
  sl <- class_agg %>%
    dplyr::filter(.data$class == k$class[1], .data$PFT == k$PFT[1])
  fr <- agg_to_fraction_row(sl, k$PFT[1], cr_fb)
  if (!is.null(fr)) {
    class_frac_list[[i]] <- dplyr::bind_cols(
      tibble::tibble(class = k$class[1], PFT = k$PFT[1]),
      fr
    )
  }
}
frac_class <- dplyr::bind_rows(class_frac_list)

#### Summarize at PFT and global

pft_agg <- harvest_join %>%
  dplyr::group_by(PFT, variable) %>%
  dplyr::summarise(value = mean(.data$value_num, na.rm = TRUE), n_obs = dplyr::n(), .groups = "drop")

pft_keys <- pft_agg %>% dplyr::distinct(PFT)

cat("Computing harvest fractions per PFT...\n")
pft_frac_list <- vector("list", nrow(pft_keys))
for (i in seq_len(nrow(pft_keys))) {
  pf <- pft_keys$PFT[i]
  sl <- pft_agg %>% dplyr::filter(.data$PFT == pf)
  fr <- agg_to_fraction_row(sl, pf, cr_fb)
  if (!is.null(fr)) {
    pft_frac_list[[i]] <- dplyr::bind_cols(tibble::tibble(PFT = pf), fr)
  }
}
frac_pft <- dplyr::bind_rows(pft_frac_list)

glob_agg <- harvest_join %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(value = mean(.data$value_num, na.rm = TRUE), .groups = "drop")
dom_pft <- names(sort(table(harvest_join$PFT), decreasing = TRUE))[1]
if (is.na(dom_pft)) {
  dom_pft <- "row"
}
fr_g <- agg_to_fraction_row(
  glob_agg %>% dplyr::transmute(variable, value),
  dom_pft,
  cr_fb
)
if (is.null(fr_g) || nrow(fr_g) == 0L) {
  stop("Global harvest fraction row is empty; check FAOSTAT variables.")
}
fr_g_num <- function(nm) suppressWarnings(as.numeric(fr_g[[nm]][1]))

ph_wide <- placeholder_means_wide %>% dplyr::mutate(PFT = as.character(PFT))
ph_long <- ph_wide %>%
  tidyr::pivot_longer(c(AGB_REMOVED, AGB_LITTER, BGB_REMOVED, BGB_LITTER),
    names_to = "param", values_to = "ph"
  )

frac_sub_l <- frac_sub %>%
  tidyr::pivot_longer(c(AGB_REMOVED, AGB_LITTER, BGB_REMOVED, BGB_LITTER),
    names_to = "param", values_to = "v_sub"
  ) %>%
  dplyr::select(class, subclass, crop_desc, PFT, param, v_sub)
frac_class_l <- frac_class %>%
  tidyr::pivot_longer(c(AGB_REMOVED, AGB_LITTER, BGB_REMOVED, BGB_LITTER),
    names_to = "param", values_to = "v_class"
  ) %>%
  dplyr::select(class, PFT, param, v_class)
frac_pft_l <- frac_pft %>%
  tidyr::pivot_longer(c(AGB_REMOVED, AGB_LITTER, BGB_REMOVED, BGB_LITTER),
    names_to = "param", values_to = "v_pft"
  ) %>%
  dplyr::select(PFT, param, v_pft)

g_vals <- tibble::tibble(
  param = harvest_param_names,
  v_glob = c(
    fr_g_num("AGB_REMOVED"), fr_g_num("AGB_LITTER"),
    fr_g_num("BGB_REMOVED"), fr_g_num("BGB_LITTER")
  )
)

#### Assemble long lookup (coalesce subclass, class, pft, placeholder, global)

skel <- landiq_subclass_class_frames(mapping)

tbl_sub <- skel$subclass_valid %>%
  tidyr::crossing(param = harvest_param_names) %>%
  dplyr::left_join(frac_sub_l, by = c("class", "subclass", "crop_desc", "PFT", "param")) %>%
  dplyr::left_join(frac_class_l, by = c("class", "PFT", "param")) %>%
  dplyr::left_join(frac_pft_l, by = c("PFT", "param")) %>%
  dplyr::left_join(g_vals, by = "param") %>%
  dplyr::left_join(ph_long, by = c("PFT", "param")) %>%
  dplyr::mutate(
    # Placeholder before global so woody_destructive does not take the global cereal mean.
    mean_obs = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$v_sub)),
      suppressWarnings(as.numeric(.data$v_class)),
      suppressWarnings(as.numeric(.data$v_pft)),
      suppressWarnings(as.numeric(.data$ph)),
      suppressWarnings(as.numeric(.data$v_glob))
    ),
    src = dplyr::case_when(
      !is.na(.data$v_sub) ~ "subclass",
      !is.na(.data$v_class) ~ "class",
      !is.na(.data$v_pft) ~ "pft",
      !is.na(.data$v_glob) ~ "global",
      TRUE ~ "placeholder"
    ),
    sd_obs = NA_real_,
    n_obs = 0L,
    level = "subclass"
  ) %>%
  dplyr::select(level, PFT, class, subclass, crop_desc, param, mean_obs, sd_obs, n_obs, src)

tbl_class <- skel$class_valid %>%
  dplyr::select(-level_id) %>%
  tidyr::crossing(param = harvest_param_names) %>%
  dplyr::left_join(frac_class_l, by = c("class", "PFT", "param")) %>%
  dplyr::left_join(frac_pft_l, by = c("PFT", "param")) %>%
  dplyr::left_join(g_vals, by = "param") %>%
  dplyr::left_join(ph_long, by = c("PFT", "param")) %>%
  dplyr::mutate(
    mean_obs = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$v_class)),
      suppressWarnings(as.numeric(.data$v_pft)),
      suppressWarnings(as.numeric(.data$ph)),
      suppressWarnings(as.numeric(.data$v_glob))
    ),
    src = dplyr::case_when(
      !is.na(.data$v_class) ~ "class",
      !is.na(.data$v_pft) ~ "pft",
      !is.na(.data$v_glob) ~ "global",
      TRUE ~ "placeholder"
    ),
    sd_obs = NA_real_,
    n_obs = 0L,
    level = "class"
  ) %>%
  dplyr::select(level, PFT, class, subclass, crop_desc, param, mean_obs, sd_obs, n_obs, src)

tbl_pft <- tidyr::crossing(
  PFT = unique(ph_wide$PFT),
  param = harvest_param_names
) %>%
  dplyr::left_join(frac_pft_l, by = c("PFT", "param")) %>%
  dplyr::left_join(g_vals, by = "param") %>%
  dplyr::left_join(ph_long, by = c("PFT", "param")) %>%
  dplyr::mutate(
    mean_obs = dplyr::coalesce(
      suppressWarnings(as.numeric(.data$v_pft)),
      suppressWarnings(as.numeric(.data$ph)),
      suppressWarnings(as.numeric(.data$v_glob))
    ),
    sd_obs = NA_real_,
    n_obs = 0L,
    level = "pft",
    class = NA_character_,
    subclass = NA_character_,
    crop_desc = NA_character_
  ) %>%
  dplyr::select(level, PFT, class, subclass, crop_desc, param, mean_obs, sd_obs, n_obs)

tbl_global <- tibble::tibble(
  param = harvest_param_names,
  mean_obs = c(
    fr_g_num("AGB_REMOVED"), fr_g_num("AGB_LITTER"),
    fr_g_num("BGB_REMOVED"), fr_g_num("BGB_LITTER")
  ),
  sd_obs = NA_real_,
  n_obs = 0L,
  level = "global",
  class = NA_character_,
  subclass = NA_character_,
  crop_desc = NA_character_,
  PFT = NA_character_
) %>%
  dplyr::select(level, PFT, class, subclass, crop_desc, param, mean_obs, sd_obs, n_obs)

harvest_lookup_long <- dplyr::bind_rows(
  tbl_sub %>% dplyr::select(-src),
  tbl_class %>% dplyr::select(-src),
  tbl_pft,
  tbl_global
) %>%
  dplyr::relocate(level, PFT, class, subclass, crop_desc, param, mean_obs, sd_obs, n_obs) %>%
  dplyr::arrange(level, PFT, class, subclass, param)

#### Write RDS and CSV

write_harvest_lookup_outputs(harvest_lookup_long, out_rds, out_csv)
