# Pool calculations from pre-built TRY and harvest lookup tables for SIPNET
# planting and harvest rows. Maps LandIQ crop code and PFT to C and N pools at
# planting (LAI from MSLSP EVI via lai_from_mslsp.R) and to removal
# fractions at harvest. Trait lookup order: subclass > class > PFT > global.
#
# Main inputs: planting_lookup_long.rds, harvest_lookup_long.rds,
# LandIQ_cropCode_lookup_table.csv
#
# Main outputs: Tibbles from initialize_planting() and initialize_harvest_from_lookup() (no writes here).
# Used by make_events_statewide.R for statewide planting events.

#### Load packages

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
})

#### Paths and configuration

path_management     <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
plant_traits_dir    <- file.path(path_management, "plant_traits")
planting_lookup_rds <- file.path(plant_traits_dir, "planting_lookup_long.rds")
harvest_lookup_rds  <- file.path(plant_traits_dir, "harvest_lookup_long.rds")
landiq_lookup_csv   <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")

# Prefer sibling lai_from_mslsp.R in this traits package (training / PEcAn tree).
.traits_dir <- tryCatch(
  dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1L]), mustWork = FALSE)),
  error = function(e) NA_character_
)
if (is.na(.traits_dir) || !nzchar(.traits_dir) || !file.exists(file.path(.traits_dir, "lai_from_mslsp.R"))) {
  .traits_dir <- trimws(Sys.getenv("CCMMF_CODE", ""))
  if (nzchar(.traits_dir)) {
    .traits_dir <- file.path(.traits_dir, "traits")
  }
}
if (!file.exists(file.path(.traits_dir, "lai_from_mslsp.R"))) {
  stop(
    "Missing lai_from_mslsp.R. Set CCMMF_CODE to inst/ccmmf, or source pool_calculations ",
    "via Rscript from the traits/ directory."
  )
}
source(file.path(.traits_dir, "lai_from_mslsp.R"))

sla_pooled_key <- "SLA_POOLED"

# Infix "default if missing": x %||% y returns y when x is NULL or length-0, else x
# (same idea as ?? in some languages). We use it because trait lookup records do not
# always include every optional field (e.g. n_obs, sd_obs); for diagnostics we still
# want stable columns, so we fall back to NA_* instead of NULL or branching everywhere.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#### LandIQ crop table (agricultural rows, key = CLASS pasted with SUBCLASS)

load_landiq_mapping <- function(path = landiq_lookup_csv) {
  d <- as.data.frame(fread(path))
  d %>%
    filter(is_agricultural == TRUE) %>%
    mutate(CLASS = as.character(CLASS),
           SUBCLASS = as.character(SUBCLASS),
           key = paste0(CLASS, SUBCLASS)) %>%
    relocate(CLASS, SUBCLASS, CLASS_desc, SUBCLASS_desc, PFT, key)
}

#### Named-vector indexes from RDS long tables (fast lookup by string key)
#
# load_trait_lookup() splits the long table by level (subclass, class, pft, global).
# Each call below gets one of those pieces. Keys are built in the same order
# get_trait_record tries them: subclass, class, pft, global.

make_trait_index <- function(lookup_df) {
  lookup_df <- as.data.frame(lookup_df)
  cols <- c(
    "mean_obs", "sd_obs", "n_obs", "n_species", "n_datasets",
    "mean_species", "sd_species_mean",
    "mean_dataset", "sd_dataset_mean",
    "replicates_median", "n_replicates_nonNA", "errorRisk_max",
    "TraitName"
  )
  # Drop any name not in the table (the planting RDS does not have every column listed above).
  cols <- intersect(cols, names(lookup_df))
  # No data rows: return empty indexes. Otherwise the tests below can guess wrong on empty vectors.
  if (nrow(lookup_df) == 0L) {
    idx <- lapply(cols, function(cc) stats::setNames(lookup_df[[cc]], character(0)))
    names(idx) <- cols
    return(idx)
  }
  if ("subclass" %in% names(lookup_df) && !all(is.na(lookup_df$subclass))) {
    # Subclass: class+subclass|TraitKey (harvest includes PFT in this key; traits do not)
    key <- paste0(lookup_df$class, lookup_df$subclass, "|", lookup_df$TraitKey)
  } else if ("class" %in% names(lookup_df) && !all(is.na(lookup_df$class))) {
    # Class-level rows: class|PFT|TraitKey or class|TraitKey
    has_pft <- "PFT" %in% names(lookup_df)
    use_pft <- if (has_pft) !is.na(lookup_df$PFT) & nzchar(trimws(as.character(lookup_df$PFT))) else rep(FALSE, nrow(lookup_df))
    lookup_key <- ifelse(use_pft, paste0(lookup_df$class, "|", lookup_df$PFT), dplyr::coalesce(lookup_df$class, "GLOBAL"))
    key <- paste0(lookup_key, "|", lookup_df$TraitKey)
  } else if ("PFT" %in% names(lookup_df) && "class" %in% names(lookup_df) && all(is.na(lookup_df$class)) && !all(is.na(lookup_df$PFT))) {
    # PFT-level rows: PFT|TraitKey
    key <- paste0(lookup_df$PFT, "|", lookup_df$TraitKey)
  } else if ("PFT" %in% names(lookup_df) && all(is.na(lookup_df$PFT))) {
    # Global rows: GLOBAL|TraitKey
    key <- paste0("GLOBAL|", lookup_df$TraitKey)
  } else {
    stop("make_trait_index: unrecognized row layout (check level column in RDS)", call. = FALSE)
  }
  idx <- lapply(cols, function(cc) stats::setNames(lookup_df[[cc]], key))
  names(idx) <- cols
  idx
}

make_harvest_index <- function(lookup_df) {
  lookup_df <- as.data.frame(lookup_df)
  cols <- c("mean_obs", "sd_obs", "n_obs")
  cols <- intersect(cols, names(lookup_df))
  # No data rows: return empty indexes (same reason as make_trait_index).
  if (nrow(lookup_df) == 0L) {
    idx <- lapply(cols, function(cc) stats::setNames(lookup_df[[cc]], character(0)))
    names(idx) <- cols
    return(idx)
  }
  if ("subclass" %in% names(lookup_df) && !all(is.na(lookup_df$subclass))) {
    key <- paste0(lookup_df$class, lookup_df$subclass, "|", lookup_df$PFT, "|", lookup_df$param)
  } else if ("class" %in% names(lookup_df) && !all(is.na(lookup_df$class))) {
    key <- paste0(lookup_df$class, "|", lookup_df$PFT, "|", lookup_df$param)
  } else if ("PFT" %in% names(lookup_df) && "class" %in% names(lookup_df) && all(is.na(lookup_df$class)) && !all(is.na(lookup_df$PFT))) {
    key <- paste0(lookup_df$PFT, "|", lookup_df$param)
  } else if ("PFT" %in% names(lookup_df) && all(is.na(lookup_df$PFT))) {
    key <- paste0("GLOBAL|", lookup_df$param)
  } else {
    stop("make_harvest_index: unrecognized row layout (check level column in RDS)", call. = FALSE)
  }
  idx <- lapply(cols, function(cc) stats::setNames(lookup_df[[cc]], key))
  names(idx) <- cols
  idx
}

# One row from an index built above; NULL if key missing or mean_obs is NA.
lookup_index_fetch <- function(idx, key) {
  if (is.null(key)) return(NULL)
  v <- idx$mean_obs[key]
  if (length(v) == 0 || is.na(v[[1]])) return(NULL)
  out <- list()
  for (nm in names(idx)) out[[nm]] <- idx[[nm]][key][[1]]
  out
}

#### Load planting and harvest RDS; build all indexes and LandIQ mapping

load_trait_lookup <- function(path = planting_lookup_rds,
                              harvest_path = harvest_lookup_rds,
                              landiq_path = landiq_lookup_csv) {
  df <- readRDS(path)
  harvest_df <- readRDS(harvest_path)
  list(
    lookup = df,
    idx_subclass = make_trait_index(df %>% filter(.data$level == "subclass")),
    idx_class    = make_trait_index(df %>% filter(.data$level == "class")),
    idx_pft      = make_trait_index(df %>% filter(.data$level == "pft")),
    idx_global   = make_trait_index(df %>% filter(.data$level == "global")),
    harvest_lookup = harvest_df,
    idx_harvest_subclass = make_harvest_index(harvest_df %>% filter(.data$level == "subclass")),
    idx_harvest_class    = make_harvest_index(harvest_df %>% filter(.data$level == "class")),
    idx_harvest_pft      = make_harvest_index(harvest_df %>% filter(.data$level == "pft")),
    idx_harvest_global   = make_harvest_index(harvest_df %>% filter(.data$level == "global")),
    mapping = load_landiq_mapping(landiq_path)
  )
}

#### LandIQ code or CLASS+SUBCLASS to crop fields used in keys

# Returns class, subclass, crop_desc, class_desc, pft from LandIQ code (e.g. T19).
get_group_class_from_code <- function(code, mapping_df) {
  row <- mapping_df %>% filter(.data$key == !!code)
  if (nrow(row) == 0) return(list(class = NA_character_, subclass = NA_character_, crop_desc = NA_character_, class_desc = NA_character_, pft = NA_character_))
  pft <- if ("PFT" %in% names(row)) row$PFT[1] else NA_character_
  list(class = row$CLASS[1], subclass = row$SUBCLASS[1], crop_desc = row$SUBCLASS_desc[1], class_desc = row$CLASS_desc[1], pft = pft)
}

# Same structure as get_group_class_from_code but look up by class and subclass.
get_group_class_from_class_subclass <- function(class, subclass, mapping_df) {
  if (is.null(mapping_df)) return(list(class = NA_character_, subclass = NA_character_, crop_desc = NA_character_, class_desc = NA_character_, pft = NA_character_))
  class <- trimws(as.character(class))[1]
  subclass <- as.character(subclass)[1]
  if (is.na(class) || is.na(subclass) || !nzchar(class) || !nzchar(subclass)) return(list(class = NA_character_, subclass = NA_character_, crop_desc = NA_character_, class_desc = NA_character_, pft = NA_character_))
  row <- mapping_df %>% filter(.data$CLASS == !!class, .data$SUBCLASS == !!subclass)
  if (nrow(row) == 0) return(list(class = class, subclass = subclass, crop_desc = NA_character_, class_desc = NA_character_, pft = NA_character_))
  pft <- if ("PFT" %in% names(row)) row$PFT[1] else NA_character_
  list(class = row$CLASS[1], subclass = row$SUBCLASS[1], crop_desc = row$SUBCLASS_desc[1], class_desc = row$CLASS_desc[1], pft = pft)
}

as_trait_key <- function(trait_id_or_key) {
  if (is.character(trait_id_or_key)) return(trait_id_or_key)
  as.character(as.numeric(trait_id_or_key))
}

#### Trait and harvest values with subclass then class then pft then global fallback

get_trait_record <- function(lk, subclass, class, trait_id_or_key, pft = NULL) {
  tkey <- as_trait_key(trait_id_or_key)
  k_sub <- paste0(class, subclass, "|", tkey)
  if (is.null(pft) && !is.null(lk$mapping) && "PFT" %in% names(lk$mapping)) {
    row <- lk$mapping %>% filter(.data$CLASS == !!class, .data$SUBCLASS == !!subclass)
    pft <- if (nrow(row) > 0) row$PFT[1] else NA_character_
  }
  k_cls <- if (!is.na(pft) && nzchar(pft)) paste0(class, "|", pft, "|", tkey) else paste0(class, "|", tkey)
  k_pft <- if (!is.na(pft) && nzchar(pft)) paste0(pft, "|", tkey) else NULL
  k_glb <- paste0("GLOBAL|", tkey)

  r <- lookup_index_fetch(lk$idx_subclass, k_sub)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "subclass")))
  r <- lookup_index_fetch(lk$idx_class, k_cls)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "class")))
  if (!is.null(k_pft) && !is.null(lk$idx_pft)) {
    r <- lookup_index_fetch(lk$idx_pft, k_pft)
    if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "pft")))
  }
  r <- lookup_index_fetch(lk$idx_global, k_glb)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "global")))
  list(value = NA_real_, src = "none")
}

# Harvest fallback: subclass -> class -> pft -> global (same as trait lookup).
# lookup_pft: row/rice/hay/woody/woody_destructive (maps from parcel PFT + destructive).
get_harvest_param <- function(lk, subclass, class, lookup_pft, param) {
  k_sub <- paste0(class, subclass, "|", lookup_pft, "|", param)
  k_cls <- paste0(class, "|", lookup_pft, "|", param)
  k_pft <- paste0(lookup_pft, "|", param)
  k_glb <- paste0("GLOBAL|", param)

  r <- lookup_index_fetch(lk$idx_harvest_subclass, k_sub)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "subclass")))
  r <- lookup_index_fetch(lk$idx_harvest_class, k_cls)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "class")))
  r <- lookup_index_fetch(lk$idx_harvest_pft, k_pft)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "pft")))
  r <- lookup_index_fetch(lk$idx_harvest_global, k_glb)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, src = "global")))
  list(value = NA_real_, src = "none")
}

derive_CN_coarse <- function(lk, subclass, class) {
  CN_root  <- get_trait_record(lk, subclass, class, 1055)$value
  CN_fine  <- get_trait_record(lk, subclass, class, 2057)$value
  f_fine   <- get_trait_record(lk, subclass, class, 2005)$value
  f_coarse <- get_trait_record(lk, subclass, class, 1534)$value

  if (all(!is.na(c(CN_root, CN_fine, f_fine, f_coarse)))) {
    f_root <- f_fine + f_coarse
    if (!is.na(f_root) && f_root > 0) {
      f_fine_root <- f_fine / f_root
      f_coarse_root <- f_coarse / f_root
      denom <- (1 / CN_root) - (f_fine_root / CN_fine)
      if (!is.na(denom) && denom > 0) {
        CN_coarse <- f_coarse_root / denom
        if (is.finite(CN_coarse) && CN_coarse > 0) return(CN_coarse)
      }
    }
  }
  if (!is.na(CN_root)) return(CN_root)
  if (!is.na(CN_fine)) return(CN_fine)
  CN_stem <- get_trait_record(lk, subclass, class, 165)$value
  if (!is.na(CN_stem)) return(CN_stem)
  CN_leaf <- get_trait_record(lk, subclass, class, 146)$value
  if (!is.na(CN_leaf)) return(CN_leaf)
  NA_real_
}

#### Planting: core (LandIQ code plus numeric LAI to C and N pools)

planting_pools_from_lookup <- function(ID, DATE, code, LAI, PFT, lk,
                                       diagnostics = FALSE) {
  gc <- get_group_class_from_code(code, lk$mapping)
  subclass <- gc$subclass
  class <- gc$class
  crop_desc <- gc$crop_desc
  class_desc <- gc$class_desc
  pft <- gc$pft

  sla_rec <- get_trait_record(lk, subclass, class, sla_pooled_key, pft = pft)
  SLA <- sla_rec$value
  if (is.na(SLA) || SLA <= 0) {
    out <- tibble(
      LOC = ID, DATE = DATE, CLASS_SUBCLASS = code, class = class, subclass = subclass, crop_desc = crop_desc, CLASS_DESC = class_desc,
      PFT = PFT, LAI = LAI,
      C_LEAF = NA_real_, C_STEM = NA_real_, C_FINEROOT = NA_real_, C_COARSEROOT = NA_real_,
      N_LEAF = NA_real_, N_STEM = NA_real_, N_FINEROOT = NA_real_, N_COARSEROOT = NA_real_,
      ENSEMBLE_SIZE = 1L
    )
    if (diagnostics) {
      out$sla_src <- sla_rec$src
      out$sla_n_obs <- sla_rec$n_obs %||% NA_integer_
      out$sla_sd_obs <- sla_rec$sd_obs %||% NA_real_
    }
    return(out)
  }

  M_leaf_kgm2 <- LAI / SLA

  r3441 <- get_trait_record(lk, subclass, class, 3441, pft = pft)
  r128  <- get_trait_record(lk, subclass, class, 128, pft = pft)
  r1534 <- get_trait_record(lk, subclass, class, 1534, pft = pft)
  r2005 <- get_trait_record(lk, subclass, class, 2005, pft = pft)

  M_leaf_gplant <- r3441$value
  M_stem_gplant <- r128$value
  f_coarse <- r1534$value
  f_fine   <- r2005$value

  f_root <- NA_real_
  if (!is.na(f_coarse) || !is.na(f_fine)) f_root <- dplyr::coalesce(f_coarse, 0) + dplyr::coalesce(f_fine, 0)

  F_stem <- F_fineRoot <- F_coarseRoot <- NA_real_
  if (!any(is.na(c(M_leaf_gplant, M_stem_gplant, f_root))) && f_root > 0 && f_root < 1) {
    M_plant_gplant <- (M_leaf_gplant + M_stem_gplant) / (1 - f_root)
    M_fine_gplant  <- if (!is.na(f_fine))   f_fine   * M_plant_gplant else NA_real_
    M_coarse_gplant<- if (!is.na(f_coarse)) f_coarse * M_plant_gplant else NA_real_
    F_stem <- M_stem_gplant / M_leaf_gplant
    F_fineRoot <- if (!is.na(M_fine_gplant)) M_fine_gplant / M_leaf_gplant else NA_real_
    F_coarseRoot <- if (!is.na(M_coarse_gplant)) M_coarse_gplant / M_leaf_gplant else NA_real_
  } else if (!any(is.na(c(M_leaf_gplant, M_stem_gplant)))) {
    F_stem <- M_stem_gplant / M_leaf_gplant
  }

  M_stem_kgm2   <- if (!is.na(F_stem)       && !is.na(M_leaf_kgm2)) F_stem       * M_leaf_kgm2 else NA_real_
  M_fine_kgm2   <- if (!is.na(F_fineRoot)   && !is.na(M_leaf_kgm2)) F_fineRoot   * M_leaf_kgm2 else NA_real_
  M_coarse_kgm2 <- if (!is.na(F_coarseRoot) && !is.na(M_leaf_kgm2)) F_coarseRoot * M_leaf_kgm2 else NA_real_

  C_leaf <- M_leaf_kgm2 * 0.47
  C_stem <- M_stem_kgm2 * 0.47
  C_fineroot <- M_fine_kgm2 * 0.47
  C_coarseroot <- M_coarse_kgm2 * 0.50

  r14   <- get_trait_record(lk, subclass, class, 14, pft = pft)
  r146  <- get_trait_record(lk, subclass, class, 146, pft = pft)
  r165  <- get_trait_record(lk, subclass, class, 165, pft = pft)
  r1055 <- get_trait_record(lk, subclass, class, 1055, pft = pft)
  r2057 <- get_trait_record(lk, subclass, class, 2057, pft = pft)

  Nleaf_mass_mgg <- r14$value
  CN_leaf <- r146$value
  CN_stem <- r165$value
  CN_root <- r1055$value
  CN_fine <- r2057$value

  if (!is.na(Nleaf_mass_mgg) && !is.na(M_leaf_kgm2)) {
    N_leaf <- M_leaf_kgm2 * (Nleaf_mass_mgg * 1e-6)
  } else if (!is.na(CN_leaf) && !is.na(C_leaf)) {
    N_leaf <- C_leaf / CN_leaf
  } else {
    N_leaf <- NA_real_
  }

  if (!is.na(C_stem) && !is.na(CN_stem)) {
    N_stem <- C_stem / CN_stem
  } else if (!is.na(C_stem) && !is.na(CN_root)) {
    N_stem <- C_stem / CN_root
  } else if (!is.na(C_stem) && !is.na(CN_leaf)) {
    N_stem <- C_stem / CN_leaf
  } else {
    N_stem <- NA_real_
  }

  CN_fine_use <- CN_fine
  if (is.na(CN_fine_use)) CN_fine_use <- CN_root
  if (is.na(CN_fine_use)) CN_fine_use <- CN_stem
  if (is.na(CN_fine_use)) CN_fine_use <- CN_leaf
  N_fineroot <- if (!is.na(C_fineroot) && !is.na(CN_fine_use)) C_fineroot / CN_fine_use else NA_real_

  CN_coarse <- derive_CN_coarse(lk, subclass, class)
  if (is.na(CN_coarse)) {
    if (!is.na(CN_root)) CN_coarse <- CN_root
    else if (!is.na(CN_stem)) CN_coarse <- CN_stem
    else if (!is.na(CN_leaf)) CN_coarse <- CN_leaf
  }
  N_coarseroot <- if (!is.na(C_coarseroot) && !is.na(CN_coarse)) C_coarseroot / CN_coarse else NA_real_

  out <- tibble(
    LOC = ID, DATE = DATE, CLASS_SUBCLASS = code, class = class, subclass = subclass, crop_desc = crop_desc, CLASS_DESC = class_desc,
    PFT = PFT, LAI = LAI,
    C_LEAF = C_leaf, C_STEM = C_stem, C_FINEROOT = C_fineroot, C_COARSEROOT = C_coarseroot,
    N_LEAF = N_leaf, N_STEM = N_stem, N_FINEROOT = N_fineroot, N_COARSEROOT = N_coarseroot,
    ENSEMBLE_SIZE = 1L
  )

  if (diagnostics) {
    out$sla_src <- sla_rec$src
    out$sla_n_obs <- sla_rec$n_obs %||% NA_integer_
    out$sla_sd_obs <- sla_rec$sd_obs %||% NA_real_
    out$src_14   <- r14$src
    out$src_3441 <- r3441$src
    out$src_128  <- r128$src
    out$src_2005 <- r2005$src
    out$src_1534 <- r1534$src
    out$src_1055 <- r1055$src
    out$src_2057 <- r2057$src
    out$src_146  <- r146$src
    out$src_165  <- r165$src
    out$used_class_any  <- any(c(out$src_14, out$src_3441, out$src_128, out$src_2005, out$src_1534,
                                 out$src_1055, out$src_2057, out$src_146, out$src_165) == "class")
    out$used_pft_any    <- any(c(out$src_14, out$src_3441, out$src_128, out$src_2005, out$src_1534,
                                 out$src_1055, out$src_2057, out$src_146, out$src_165) == "pft")
    out$used_global_any <- any(c(out$src_14, out$src_3441, out$src_128, out$src_2005, out$src_1534,
                                 out$src_1055, out$src_2057, out$src_146, out$src_165) == "global")
  }
  out
}

#### Planting: initialize_planting (fixed LAI or MSLSP EVI)

# If LAI is a finite number, use it and ignore mslsp_EVImax and mslsp_EVIamp.
# Else require both EVI fields and compute LAI with compute_lai_from_mslsp().
# LandIQ key: if class and subclass are both non-empty, code is paste0(class, subclass).
# Otherwise code must be set. Non-empty class overrides the class used in
# compute_lai_from_mslsp when the caller passed only a code string (optional CLASS column).

initialize_planting <- function(
    ID, DATE, PFT, lk,
    code = NULL,
    class = NA_character_,
    subclass = NA_character_,
    LAI = NA_real_,
    mslsp_EVImax = NULL,
    mslsp_EVIamp = NULL,
    diagnostics = FALSE) {

  cls <- trimws(as.character(class)[1])
  sub <- as.character(subclass)[1]
  code_in <- if (is.null(code)) "" else trimws(as.character(code)[1])

  if (!is.na(cls) && nzchar(cls) && !is.na(sub) && nzchar(sub)) {
    code_chr <- paste0(cls, sub)
  } else if (nzchar(code_in)) {
    code_chr <- code_in
  } else {
    stop("initialize_planting: provide LandIQ code or both class and subclass.")
  }

  lai_vec <- suppressWarnings(as.numeric(LAI))
  use_explicit_lai <- length(lai_vec) > 0L && is.finite(lai_vec[[1]]) && !is.na(lai_vec[[1]])

  if (use_explicit_lai) {
    return(planting_pools_from_lookup(
      ID = ID, DATE = DATE, code = code_chr, LAI = lai_vec[[1]], PFT = PFT, lk = lk, diagnostics = diagnostics
    ))
  }

  if (is.null(mslsp_EVImax) || is.null(mslsp_EVIamp)) {
    stop("initialize_planting: provide finite LAI, or both mslsp_EVImax and mslsp_EVIamp.")
  }
  mx <- suppressWarnings(as.numeric(mslsp_EVImax)[1])
  ma <- suppressWarnings(as.numeric(mslsp_EVIamp)[1])
  if (is.na(mx) || is.na(ma)) {
    stop("initialize_planting: mslsp_EVImax and mslsp_EVIamp must be non-NA when LAI is not given.")
  }

  class_for_lai <- cls
  if (is.na(class_for_lai) || !nzchar(class_for_lai)) {
    class_for_lai <- get_group_class_from_code(code_chr, lk$mapping)$class
  }

  lai_diag <- compute_lai_from_mslsp(
    mslsp_EVImax = mx,
    mslsp_EVIamp = ma,
    pft = PFT,
    class = class_for_lai,
    diagnostics = TRUE
  )

  out <- planting_pools_from_lookup(
    ID = ID, DATE = DATE, code = code_chr, LAI = lai_diag$LAI, PFT = PFT, lk = lk, diagnostics = diagnostics
  )
  if (diagnostics) {
    out$lai_rule_id <- lai_diag$lai_rule_id
    out$lai_evi_field_used <- lai_diag$lai_evi_field_used
    out$lai_evi_value_used <- lai_diag$lai_evi_value_used
    out$lai_k <- lai_diag$lai_k
    out$lai_a <- lai_diag$lai_a
    out$lai_b <- lai_diag$lai_b
    out$lai_min <- lai_diag$lai_min
    out$lai_max <- lai_diag$lai_max
  }
  out
}

#### Harvest: removal fractions from lookup

initialize_harvest_from_lookup <- function(ID, DATE, code, PFT, lk, destructive = FALSE) {
  gc <- get_group_class_from_code(code, lk$mapping)
  subclass <- gc$subclass
  class <- gc$class
  crop_desc <- gc$crop_desc
  class_desc <- gc$class_desc

  # Map parcel PFT to lookup PFT: rice/row->row, hay->hay, woody->woody or woody_destructive
  lookup_pft <- dplyr::case_when(
    PFT %in% c("rice", "row")    ~ "row",
    PFT == "hay"                 ~ "hay",
    PFT == "woody" & destructive ~ "woody_destructive",
    PFT == "woody"               ~ "woody",
    TRUE                         ~ "skip"
  )
  if (lookup_pft == "skip") return(NULL)

  r_agb_rem <- get_harvest_param(lk, subclass, class, lookup_pft, "AGB_REMOVED")
  r_agb_lit <- get_harvest_param(lk, subclass, class, lookup_pft, "AGB_LITTER")
  r_bgb_rem <- get_harvest_param(lk, subclass, class, lookup_pft, "BGB_REMOVED")
  r_bgb_lit <- get_harvest_param(lk, subclass, class, lookup_pft, "BGB_LITTER")

  tibble(
    LOC = ID, DATE = DATE, CLASS_SUBCLASS = code, class = class, subclass = subclass, crop_desc = crop_desc, CLASS_DESC = class_desc,
    PFT = PFT,
    AGB_REMOVED = r_agb_rem$value, AGB_LITTER = r_agb_lit$value,
    BGB_REMOVED = r_bgb_rem$value, BGB_LITTER = r_bgb_lit$value,
    ENSEMBLE_SIZE = 1L
  )
}

