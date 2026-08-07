# Pool calculations from planting and harvest lookup CSVs for SIPNET events.
#
# Planting fallback: TRY subclass > TRY class > lit subclass > lit class >
# TRY PFT > default PFT. Fine/coarse root splits come from the lookup
# (including source=default); this file does not invent numeric defaults.
#
# Inputs: planting_lookup.csv, harvest_lookup.csv, LandIQ_cropCode_lookup_table.csv.
# Used by make_events_statewide.R (no writes here).

#### Load packages

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
})

#### Paths and configuration

path_management     <- Sys.getenv("MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
plant_traits_dir    <- file.path(path_management, "plant_traits")
planting_lookup_csv <- file.path(plant_traits_dir, "planting_lookup.csv")
harvest_lookup_csv <- file.path(plant_traits_dir, "harvest_lookup.csv")
landiq_lookup_csv <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")

# local=TRUE so compute_lai_from_mslsp lands in the same env as this script
# (events load this via source(..., local = pool_env)).
source(
  file.path(path_management, "scripts/traits/lai_from_mslsp.R"),
  local = TRUE
)

sla_key <- "SLA"


# Infix "default if missing": x %||% y returns y when x is NULL or length-0, else x
# (same idea as ?? in some languages). We use it because trait lookup records do not
# always include every optional field (e.g. n_obs, sd_obs); for diagnostics we still
# want stable columns, so we fall back to NA_* instead of NULL or branching everywhere.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#### LandIQ crop table (2021 RS legend only; key = CLASS pasted with SUBCLASS)

# Downstream LandIQ products harmonize all years to the Nov 2021 DWR RS legend.
# Keep legend_year == 2021 only. Allow CLASS+** when crops_included is set
# (C** citrus, V** vineyards after harmonization).
load_landiq_mapping <- function(path = landiq_lookup_csv) {
  d <- as.data.frame(fread(path))
  d %>%
    mutate(
      CLASS = as.character(CLASS),
      SUBCLASS = as.character(SUBCLASS),
      SUBCLASS_desc = as.character(SUBCLASS_desc),
      PFT = as.character(PFT),
      legend_year = suppressWarnings(as.integer(legend_year)),
      crops_included = if ("crops_included" %in% names(.)) {
        as.character(crops_included)
      } else {
        NA_character_
      },
      crops_included = dplyr::coalesce(crops_included, "")
    ) %>%
    filter(
      legend_year == 2021L,
      is_agricultural == TRUE,
      !is.na(PFT),
      PFT != "other",
      !grepl(
        "idle|not cropped|new lands prepped",
        SUBCLASS_desc,
        ignore.case = TRUE
      ),
      # Drop empty ** placeholders; keep rolled-up crops (C**, V**)
      SUBCLASS != "**" | nzchar(trimws(crops_included))
    ) %>%
    mutate(key = paste0(CLASS, SUBCLASS)) %>%
    relocate(CLASS, SUBCLASS, CLASS_desc, SUBCLASS_desc, PFT, key)
}

#### Named-vector indexes from lookup long tables (fast lookup by string key)
#
# load_trait_lookup() splits by level and builds lit indexes from source=literature
# rows in planting_lookup.csv. get_trait_record: TRY subclass > TRY class >
# lit subclass > lit class > TRY PFT > default PFT.

make_trait_index <- function(lookup_df) {
  lookup_df <- normalize_lookup_keys(as.data.frame(lookup_df))
  cols <- c("mean_obs", "sd_obs", "n_obs", "n_species", "n_datasets", "TraitName")
  cols <- intersect(cols, names(lookup_df))
  if (nrow(lookup_df) == 0L) {
    idx <- lapply(cols, function(cc) stats::setNames(lookup_df[[cc]], character(0)))
    names(idx) <- cols
    return(idx)
  }
  lvl <- unique(as.character(lookup_df$level))
  if (length(lvl) != 1L || !lvl %in% c("subclass", "class", "pft")) {
    stop("make_trait_index: expected a single level in {subclass, class, pft}", call. = FALSE)
  }
  if (identical(lvl, "subclass")) {
    key <- paste0(lookup_df$class, lookup_df$subclass, "|", lookup_df$TraitKey)
  } else if (identical(lvl, "class")) {
    has_pft <- "PFT" %in% names(lookup_df)
    use_pft <- if (has_pft) !is.na(lookup_df$PFT) & nzchar(trimws(as.character(lookup_df$PFT))) else rep(FALSE, nrow(lookup_df))
    lookup_key <- ifelse(use_pft, paste0(lookup_df$class, "|", lookup_df$PFT), as.character(lookup_df$class))
    key <- paste0(lookup_key, "|", lookup_df$TraitKey)
  } else {
    key <- paste0(lookup_df$PFT, "|", lookup_df$TraitKey)
  }
  idx <- lapply(cols, function(cc) stats::setNames(lookup_df[[cc]], key))
  names(idx) <- cols
  idx
}

# Key fragment for orchard-clearing vs routine harvest (logical column on CSV).
harvest_dest_key <- function(destructive) {
  d <- as.logical(destructive)
  ifelse(!is.na(d) & d, "TRUE", "FALSE")
}

make_harvest_index <- function(lookup_df) {
  lookup_df <- normalize_lookup_keys(as.data.frame(lookup_df))
  cols <- c("mean_obs", "sd_obs", "n_obs", "source")
  cols <- intersect(cols, names(lookup_df))
  if (nrow(lookup_df) == 0L) {
    idx <- lapply(cols, function(cc) stats::setNames(lookup_df[[cc]], character(0)))
    names(idx) <- cols
    return(idx)
  }
  lvl <- unique(as.character(lookup_df$level))
  if (length(lvl) != 1L || !lvl %in% c("subclass", "class", "pft")) {
    stop("make_harvest_index: expected a single level in {subclass, class, pft}", call. = FALSE)
  }
  dest <- harvest_dest_key(lookup_df$destructive)
  if (identical(lvl, "subclass")) {
    key <- paste0(lookup_df$class, lookup_df$subclass, "|", lookup_df$PFT, "|", dest, "|", lookup_df$param)
  } else if (identical(lvl, "class")) {
    key <- paste0(lookup_df$class, "|", lookup_df$PFT, "|", dest, "|", lookup_df$param)
  } else {
    key <- paste0(lookup_df$PFT, "|", dest, "|", lookup_df$param)
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

#### Literature / default / TRY row filters and lit indexes from planting CSV

is_literature_row <- function(df) {
  n <- nrow(df)
  if (n == 0L) return(logical(0))
  if (!("source" %in% names(df))) return(rep(FALSE, n))
  tolower(trimws(as.character(dplyr::coalesce(df$source, "")))) == "literature"
}

is_default_row <- function(df) {
  n <- nrow(df)
  if (n == 0L) return(logical(0))
  if (!("source" %in% names(df))) return(rep(FALSE, n))
  tolower(trimws(as.character(dplyr::coalesce(df$source, "")))) == "default"
}

is_try_row <- function(df) {
  n <- nrow(df)
  if (n == 0L) return(logical(0))
  !is_literature_row(df) & !is_default_row(df)
}

# CSV round-trip turns NA class/subclass into ""; treat blanks as missing for keys.
blank_to_na_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
  x
}

normalize_lookup_keys <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if ("class" %in% names(df)) df$class <- blank_to_na_chr(df$class)
  if ("subclass" %in% names(df)) df$subclass <- blank_to_na_chr(df$subclass)
  if ("PFT" %in% names(df)) df$PFT <- blank_to_na_chr(df$PFT)
  if ("TraitKey" %in% names(df)) df$TraitKey <- as.character(df$TraitKey)
  if ("param" %in% names(df)) df$param <- as.character(df$param)
  if ("destructive" %in% names(df)) {
    df$destructive <- as.logical(df$destructive)
    df$destructive[is.na(df$destructive)] <- FALSE
  } else {
    df$destructive <- FALSE
  }
  df
}

make_lit_trait_indexes_from_lookup <- function(df) {
  empty <- list(
    idx_lit_subclass = list(mean_obs = stats::setNames(numeric(0), character(0))),
    idx_lit_class = list(mean_obs = stats::setNames(numeric(0), character(0))),
    lit = NULL
  )
  if (is.null(df) || !("level" %in% names(df))) return(empty)
  lit <- df %>%
    filter(is_literature_row(.), !is.na(.data$mean_obs), .data$level %in% c("subclass", "class"))
  sub <- lit %>% filter(.data$level == "subclass")
  cls <- lit %>% filter(.data$level == "class")
  if (nrow(sub) == 0L && nrow(cls) == 0L) return(empty)

  idx_sub <- if (nrow(sub)) {
    sub %>%
      mutate(key = paste0(.data$class, .data$subclass, "|", .data$TraitKey)) %>%
      { list(mean_obs = stats::setNames(.$mean_obs, .$key)) }
  } else {
    list(mean_obs = stats::setNames(numeric(0), character(0)))
  }
  idx_cls <- if (nrow(cls)) {
    cls %>%
      mutate(key = paste0(.data$class, "|", .data$TraitKey)) %>%
      { list(mean_obs = stats::setNames(.$mean_obs, .$key)) }
  } else {
    list(mean_obs = stats::setNames(numeric(0), character(0)))
  }
  list(idx_lit_subclass = idx_sub, idx_lit_class = idx_cls, lit = bind_rows(sub, cls))
}

#### Load planting and harvest CSV lookups; build all indexes and LandIQ mapping

load_trait_lookup <- function(path = planting_lookup_csv,
                              harvest_path = harvest_lookup_csv,
                              landiq_path = landiq_lookup_csv) {
  df <- as.data.frame(data.table::fread(
    path,
    colClasses = list(character = c("class", "subclass", "PFT", "level", "source"))
  )) %>%
    normalize_lookup_keys()
  harvest_df <- as.data.frame(data.table::fread(
    harvest_path,
    colClasses = list(character = c("class", "subclass", "PFT", "level", "source"))
  )) %>%
    normalize_lookup_keys()
  lit_idx <- make_lit_trait_indexes_from_lookup(df)
  try_df <- df %>% filter(is_try_row(.))
  default_df <- df %>% filter(is_default_row(.), .data$level == "pft")
  list(
    lookup = df,
    idx_subclass = make_trait_index(try_df %>% filter(.data$level == "subclass")),
    idx_class    = make_trait_index(try_df %>% filter(.data$level == "class")),
    idx_pft      = make_trait_index(try_df %>% filter(.data$level == "pft")),
    idx_default_pft = make_trait_index(default_df),
    idx_lit_subclass = lit_idx$idx_lit_subclass,
    idx_lit_class    = lit_idx$idx_lit_class,
    lit = lit_idx$lit,
    harvest_lookup = harvest_df,
    idx_harvest_subclass = make_harvest_index(harvest_df %>% filter(.data$level == "subclass")),
    idx_harvest_class    = make_harvest_index(harvest_df %>% filter(.data$level == "class")),
    idx_harvest_pft      = make_harvest_index(harvest_df %>% filter(.data$level == "pft")),
    mapping = load_landiq_mapping(landiq_path)
  )
}

#### LandIQ code to crop fields used in keys

# Returns class, subclass, crop_desc, class_desc, pft from LandIQ code (e.g. T19).
get_group_class_from_code <- function(code, mapping_df) {
  row <- mapping_df %>% filter(.data$key == !!code)
  if (nrow(row) == 0) return(list(class = NA_character_, subclass = NA_character_, crop_desc = NA_character_, class_desc = NA_character_, pft = NA_character_))
  pft <- if ("PFT" %in% names(row)) row$PFT[1] else NA_character_
  list(class = row$CLASS[1], subclass = row$SUBCLASS[1], crop_desc = row$SUBCLASS_desc[1], class_desc = row$CLASS_desc[1], pft = pft)
}

as_trait_key <- function(trait_id_or_key) {
  if (is.character(trait_id_or_key)) return(trait_id_or_key)
  as.character(as.numeric(trait_id_or_key))
}

#### Unit helpers (expected units after lookup build / lit value_as_used)
#
# SLA: m2/kg (= mm2/mg). LAI is m2 leaf / m2 ground -> M_leaf = LAI/SLA in kg/m2.
# 110 LWR, 136 stem/plant, 470 RMF, 2005/1534: mass fractions in [0,1] (g/g).
# 9 RS, 1019: mass ratios (g/g).
# 14 leaf N: mg N / g DM -> kg N / kg DM = value * 1e-3.
# 146/165/1055/2057: C:N mass ratios (g/g), dimensionless.

# Plant mass fraction in g/g. Accepts percent 1-100 when still on % scale.
as_mass_fraction <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) return(NA_real_)
  v <- as.numeric(x[[1]])
  if (!is.finite(v) || v <= 0) return(NA_real_)
  if (v > 1 && v <= 100) v <- v / 100
  if (v <= 0 || v > 1) return(NA_real_)
  v
}

# Root:shoot (and similar) mass ratio g/g.
as_mass_ratio <- function(x, min_ok = 0.01, max_ok = 10) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) return(NA_real_)
  v <- as.numeric(x[[1]])
  if (!is.finite(v) || v < min_ok || v > max_ok) return(NA_real_)
  v
}

# SLA in m2/kg (reject LMA-like inverses and non-positive).
as_sla_m2_kg <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) return(NA_real_)
  v <- as.numeric(x[[1]])
  if (!is.finite(v) || v <= 0 || v > 200) return(NA_real_)
  v
}

# Leaf N mg/g -> kg N / kg DM.
leaf_n_mg_g_to_kg_kg <- function(n_mg_g) {
  if (is.null(n_mg_g) || length(n_mg_g) == 0L || is.na(n_mg_g[[1]])) return(NA_real_)
  v <- as.numeric(n_mg_g[[1]])
  if (!is.finite(v) || v <= 0 || v > 100) return(NA_real_)
  v * 1e-3
}

#### Trait lookup: TRY subclass > TRY class > lit subclass > lit class > TRY PFT > default PFT

get_trait_record <- function(lk, subclass, class, trait_id_or_key, pft = NULL) {
  tkey <- as_trait_key(trait_id_or_key)
  k_sub <- paste0(class, subclass, "|", tkey)
  if (is.null(pft) && !is.null(lk$mapping) && "PFT" %in% names(lk$mapping)) {
    row <- lk$mapping %>% filter(.data$CLASS == !!class, .data$SUBCLASS == !!subclass)
    pft <- if (nrow(row) > 0) row$PFT[1] else NA_character_
  }
  k_cls_try <- if (!is.na(pft) && nzchar(pft)) paste0(class, "|", pft, "|", tkey) else paste0(class, "|", tkey)
  k_cls_lit <- paste0(class, "|", tkey)
  k_pft <- if (!is.na(pft) && nzchar(pft)) paste0(pft, "|", tkey) else NULL

  r <- lookup_index_fetch(lk$idx_subclass, k_sub)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, level = "subclass", source = "try", src = "subclass")))
  r <- lookup_index_fetch(lk$idx_class, k_cls_try)
  if (!is.null(r)) return(c(r, list(value = r$mean_obs, level = "class", source = "try", src = "class")))
  if (!is.null(lk$idx_lit_subclass)) {
    r <- lookup_index_fetch(lk$idx_lit_subclass, k_sub)
    if (!is.null(r)) return(c(r, list(value = r$mean_obs, level = "subclass", source = "literature", src = "subclass")))
  }
  if (!is.null(lk$idx_lit_class)) {
    r <- lookup_index_fetch(lk$idx_lit_class, k_cls_lit)
    if (!is.null(r)) return(c(r, list(value = r$mean_obs, level = "class", source = "literature", src = "class")))
  }
  if (!is.null(k_pft) && !is.null(lk$idx_pft)) {
    r <- lookup_index_fetch(lk$idx_pft, k_pft)
    if (!is.null(r)) return(c(r, list(value = r$mean_obs, level = "pft", source = "try", src = "pft")))
  }
  if (!is.null(k_pft) && !is.null(lk$idx_default_pft)) {
    r <- lookup_index_fetch(lk$idx_default_pft, k_pft)
    if (!is.null(r)) return(c(r, list(value = r$mean_obs, level = "pft", source = "default", src = "pft")))
  }
  list(value = NA_real_, level = "none", source = NA_character_, src = "none")
}

# Harvest fallback: subclass -> class -> pft (keyed by PFT + destructive).
# PFT is LandIQ only (hay/rice/row/woody); orchard clearing uses destructive=TRUE.
get_harvest_param <- function(lk, subclass, class, lookup_pft, param,
                              destructive = FALSE) {
  dest <- harvest_dest_key(destructive)
  k_sub <- paste0(class, subclass, "|", lookup_pft, "|", dest, "|", param)
  k_cls <- paste0(class, "|", lookup_pft, "|", dest, "|", param)
  k_pft <- paste0(lookup_pft, "|", dest, "|", param)

  r <- lookup_index_fetch(lk$idx_harvest_subclass, k_sub)
  if (!is.null(r)) {
    return(c(r, list(value = r$mean_obs, level = "subclass", source = r$source %||% NA_character_, src = "subclass")))
  }
  r <- lookup_index_fetch(lk$idx_harvest_class, k_cls)
  if (!is.null(r)) {
    return(c(r, list(value = r$mean_obs, level = "class", source = r$source %||% NA_character_, src = "class")))
  }
  r <- lookup_index_fetch(lk$idx_harvest_pft, k_pft)
  if (!is.null(r)) {
    return(c(r, list(value = r$mean_obs, level = "pft", source = r$source %||% NA_character_, src = "pft")))
  }
  list(value = NA_real_, level = "none", source = NA_character_, src = "none")
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
#
# Units:
#   LAI [-] m2/m2; SLA [m2/kg]; organ masses [kg/m2]; C/N pools [kg/m2]
#   C_leaf/stem/fine = 0.47 * mass; C_coarse = 0.50 * mass
#   Stem:leaf from 136 (stem/plant) and/or 110 (LWR) with RS(9) or RMF(470)
#   2005/1534 used as fine:coarse shares of M_root (not f*M_plant)

planting_pools_from_lookup <- function(ID, DATE, code, LAI, PFT, lk,
                                       diagnostics = FALSE) {
  gc <- get_group_class_from_code(code, lk$mapping)
  subclass <- gc$subclass
  class <- gc$class
  crop_desc <- gc$crop_desc
  class_desc <- gc$class_desc
  pft <- gc$pft

  sla_rec <- get_trait_record(lk, subclass, class, sla_key, pft = pft)
  SLA <- as_sla_m2_kg(sla_rec$value)
  if (is.na(SLA)) {
    out <- tibble(
      LOC = ID, DATE = DATE, CLASS_SUBCLASS = code, class = class, subclass = subclass, crop_desc = crop_desc, CLASS_DESC = class_desc,
      PFT = PFT, LAI = LAI,
      C_LEAF = NA_real_, C_STEM = NA_real_, C_FINEROOT = NA_real_, C_COARSEROOT = NA_real_,
      N_LEAF = NA_real_, N_STEM = NA_real_, N_FINEROOT = NA_real_, N_COARSEROOT = NA_real_,
      ENSEMBLE_SIZE = 1L
    )
    if (diagnostics) {
      out$sla_src <- sla_rec$src
      out$sla_source <- sla_rec$source
      out$sla_n_obs <- sla_rec$n_obs %||% NA_integer_
      out$sla_sd_obs <- sla_rec$sd_obs %||% NA_real_
    }
    return(out)
  }

  # kg DM / m2
  M_leaf_kgm2 <- as.numeric(LAI) / SLA

  r110  <- get_trait_record(lk, subclass, class, 110, pft = pft)
  r136  <- get_trait_record(lk, subclass, class, 136, pft = pft)
  r9    <- get_trait_record(lk, subclass, class, 9, pft = pft)
  r470  <- get_trait_record(lk, subclass, class, 470, pft = pft)
  r1534 <- get_trait_record(lk, subclass, class, 1534, pft = pft)
  r2005 <- get_trait_record(lk, subclass, class, 2005, pft = pft)
  r1019 <- get_trait_record(lk, subclass, class, 1019, pft = pft)

  lwr <- as_mass_fraction(r110$value)   # leaf / plant (g/g)
  smf <- as_mass_fraction(r136$value)   # stem / plant (g/g)
  rs  <- as_mass_ratio(r9$value)        # root / shoot (g/g)
  rmf <- as_mass_fraction(r470$value)   # root / plant (g/g)
  if (is.na(rmf) && !is.na(rs)) rmf <- rs / (1 + rs)
  if (is.na(rs) && !is.na(rmf) && rmf < 1) rs <- rmf / (1 - rmf)

  # Fill missing plant-level fine/coarse from RMF + the other fraction, or from
  # 1019 (coarse/fine mass ratio). Only when values are finite and positive.
  f2005_val <- as_mass_fraction(r2005$value)
  f1534_val <- as_mass_fraction(r1534$value)
  r1019_val <- as_mass_ratio(r1019$value)
  if (is.na(f2005_val) && is.na(f1534_val) && !is.na(r1019_val) && r1019_val > 0 && !is.na(rmf)) {
    fine_of_root <- 1 / (1 + r1019_val)
    f2005_val <- rmf * fine_of_root
    f1534_val <- rmf * (1 - fine_of_root)
    base1019 <- if (is.null(r1019$src) || identical(r1019$src, "none")) "derived" else r1019$src
    r2005$src <- paste0(base1019, "+1019")
    r1534$src <- paste0(base1019, "+1019")
  } else if (!is.na(rmf)) {
    if (is.na(f2005_val) && !is.na(f1534_val) && f1534_val < rmf) {
      f2005_val <- rmf - f1534_val
      base <- if (is.null(r1534$src) || identical(r1534$src, "none")) "derived" else r1534$src
      r2005$src <- paste0(base, "+RMF-1534")
    } else if (is.na(f1534_val) && !is.na(f2005_val) && f2005_val < rmf) {
      f1534_val <- rmf - f2005_val
      base <- if (is.null(r2005$src) || identical(r2005$src, "none")) "derived" else r2005$src
      r1534$src <- paste0(base, "+RMF-2005")
    }
  }

  # Stem:leaf  (kg/kg). Prefer measured 136+110; else close mass balance with RMF.
  alpha <- NA_real_
  if (!is.na(smf) && !is.na(lwr) && lwr > 0) {
    alpha <- smf / lwr
  } else if (!is.na(smf) && !is.na(rmf) && (1 - smf - rmf) > 0) {
    lwr_imp <- 1 - smf - rmf
    alpha <- smf / lwr_imp
    if (is.na(lwr)) lwr <- lwr_imp
  } else if (!is.na(lwr) && !is.na(rmf) && (1 - lwr - rmf) > 0) {
    smf_imp <- 1 - lwr - rmf
    alpha <- smf_imp / lwr
    if (is.na(smf)) smf <- smf_imp
  }

  M_stem_kgm2 <- if (!is.na(alpha) && is.finite(alpha) && alpha >= 0) alpha * M_leaf_kgm2 else NA_real_
  M_shoot_kgm2 <- if (!is.na(M_stem_kgm2)) M_leaf_kgm2 + M_stem_kgm2 else NA_real_

  M_root_kgm2 <- NA_real_
  if (!is.na(M_shoot_kgm2) && !is.na(rs)) {
    M_root_kgm2 <- rs * M_shoot_kgm2
  } else if (!is.na(lwr) && lwr > 0 && !is.na(rmf)) {
    M_plant_kgm2 <- M_leaf_kgm2 / lwr
    M_root_kgm2 <- rmf * M_plant_kgm2
  }

  # SIPNET plant = leaf + stem + root (no fruit). Keep RS: M_root = RS * shoot.
  M_plant_kgm2 <- if (!is.na(M_shoot_kgm2) && !is.na(M_root_kgm2)) {
    M_shoot_kgm2 + M_root_kgm2
  } else {
    NA_real_
  }

  # 2005/1534 are recorded as whole-plant fractions in TRY/lit, but applying
  # them as f * M_plant re-sizes roots and breaks RS. Use them only as the
  # relative fine:coarse split of M_root (normalize over the root pool).
  # Prefer derived f2005_val/f1534_val (may come from 1019+RMF).
  f_fine   <- f2005_val
  f_coarse <- f1534_val
  if (!is.na(f_coarse) && is.finite(f_coarse) && f_coarse == 0) f_coarse <- 0

  # If still missing plant fractions but 1019 is known, use it directly as the
  # root-pool split (does not require RMF).
  if ((is.na(f_fine) || is.na(f_coarse) || (dplyr::coalesce(f_fine, 0) + dplyr::coalesce(f_coarse, 0) <= 0)) &&
      !is.na(r1019_val) && r1019_val > 0) {
    f_fine <- 1
    f_coarse <- r1019_val
    base1019 <- if (is.null(r1019$src) || identical(r1019$src, "none")) "derived" else r1019$src
    r2005$src <- paste0(base1019, "+1019_split")
    r1534$src <- paste0(base1019, "+1019_split")
  }

  M_fine_kgm2 <- M_coarse_kgm2 <- NA_real_
  root_split_src <- NA_character_
  if (!is.na(M_root_kgm2)) {
    f_fine_part <- dplyr::coalesce(f_fine, 0)
    f_coarse_part <- dplyr::coalesce(f_coarse, 0)
    root_frac_sum <- f_fine_part + f_coarse_part
    if (root_frac_sum > 0) {
      fine_share <- f_fine_part / root_frac_sum
      coarse_share <- f_coarse_part / root_frac_sum
      root_split_src <- if (identical(r2005$source, "default") || identical(r1534$source, "default")) {
        "default"
      } else {
        "lookup"
      }
      M_fine_kgm2 <- fine_share * M_root_kgm2
      M_coarse_kgm2 <- coarse_share * M_root_kgm2
    }
    # No hardcoded fine/coarse invent here: missing 2005/1534 after the lookup
    # chain (including source=default PFT rows) leaves root pools NA.
  }

  C_leaf <- M_leaf_kgm2 * 0.47
  C_stem <- if (!is.na(M_stem_kgm2)) M_stem_kgm2 * 0.47 else NA_real_
  C_fineroot <- if (!is.na(M_fine_kgm2)) M_fine_kgm2 * 0.47 else NA_real_
  C_coarseroot <- if (!is.na(M_coarse_kgm2)) M_coarse_kgm2 * 0.50 else NA_real_

  r14   <- get_trait_record(lk, subclass, class, 14, pft = pft)
  r146  <- get_trait_record(lk, subclass, class, 146, pft = pft)
  r165  <- get_trait_record(lk, subclass, class, 165, pft = pft)
  r1055 <- get_trait_record(lk, subclass, class, 1055, pft = pft)
  r2057 <- get_trait_record(lk, subclass, class, 2057, pft = pft)

  Nleaf_frac <- leaf_n_mg_g_to_kg_kg(r14$value)  # kg N / kg DM
  CN_leaf <- r146$value
  CN_stem <- r165$value
  CN_root <- r1055$value
  CN_fine <- r2057$value

  if (!is.na(Nleaf_frac) && !is.na(M_leaf_kgm2)) {
    N_leaf <- M_leaf_kgm2 * Nleaf_frac
  } else if (!is.na(CN_leaf) && CN_leaf > 0 && !is.na(C_leaf)) {
    N_leaf <- C_leaf / CN_leaf
  } else {
    N_leaf <- NA_real_
  }

  if (!is.na(C_stem) && !is.na(CN_stem) && CN_stem > 0) {
    N_stem <- C_stem / CN_stem
  } else if (!is.na(C_stem) && !is.na(CN_root) && CN_root > 0) {
    N_stem <- C_stem / CN_root
  } else if (!is.na(C_stem) && !is.na(CN_leaf) && CN_leaf > 0) {
    N_stem <- C_stem / CN_leaf
  } else {
    N_stem <- NA_real_
  }

  CN_fine_use <- CN_fine
  if (is.na(CN_fine_use) || CN_fine_use <= 0) CN_fine_use <- CN_root
  if (is.na(CN_fine_use) || CN_fine_use <= 0) CN_fine_use <- CN_stem
  if (is.na(CN_fine_use) || CN_fine_use <= 0) CN_fine_use <- CN_leaf
  N_fineroot <- if (!is.na(C_fineroot) && !is.na(CN_fine_use) && CN_fine_use > 0) C_fineroot / CN_fine_use else NA_real_

  CN_coarse <- derive_CN_coarse(lk, subclass, class)
  if (is.na(CN_coarse) || CN_coarse <= 0) {
    if (!is.na(CN_root) && CN_root > 0) CN_coarse <- CN_root
    else if (!is.na(CN_stem) && CN_stem > 0) CN_coarse <- CN_stem
    else if (!is.na(CN_leaf) && CN_leaf > 0) CN_coarse <- CN_leaf
  }
  N_coarseroot <- if (!is.na(C_coarseroot) && !is.na(CN_coarse) && CN_coarse > 0) C_coarseroot / CN_coarse else NA_real_

  out <- tibble(
    LOC = ID, DATE = DATE, CLASS_SUBCLASS = code, class = class, subclass = subclass, crop_desc = crop_desc, CLASS_DESC = class_desc,
    PFT = PFT, LAI = LAI,
    C_LEAF = C_leaf, C_STEM = C_stem, C_FINEROOT = C_fineroot, C_COARSEROOT = C_coarseroot,
    N_LEAF = N_leaf, N_STEM = N_stem, N_FINEROOT = N_fineroot, N_COARSEROOT = N_coarseroot,
    ENSEMBLE_SIZE = 1L
  )

  if (diagnostics) {
    out$sla_src <- sla_rec$src
    out$sla_source <- sla_rec$source
    out$sla_n_obs <- sla_rec$n_obs %||% NA_integer_
    out$sla_sd_obs <- sla_rec$sd_obs %||% NA_real_
    out$src_14   <- r14$src
    out$src_110  <- r110$src
    out$src_136  <- r136$src
    out$src_2005 <- r2005$src
    out$src_1534 <- r1534$src
    out$src_1019 <- r1019$src
    out$src_9    <- r9$src
    out$src_470  <- r470$src
    out$src_1055 <- r1055$src
    out$src_2057 <- r2057$src
    out$src_146  <- r146$src
    out$src_165  <- r165$src
    out$source_14   <- r14$source
    out$source_110  <- r110$source
    out$source_136  <- r136$source
    out$source_2005 <- r2005$source
    out$source_1534 <- r1534$source
    out$source_1019 <- r1019$source
    out$source_9    <- r9$source
    out$source_470  <- r470$source
    out$source_1055 <- r1055$source
    out$source_2057 <- r2057$source
    out$source_146  <- r146$source
    out$source_165  <- r165$source
    out$root_split_src <- root_split_src
    out$used_default_split <- identical(root_split_src, "default") ||
      identical(r2005$source, "default") || identical(r1534$source, "default")
    out$alpha_stem_leaf <- alpha
    out$lwr_used <- lwr
    out$smf_used <- smf
    out$rs_used <- rs
    src_cols <- c(out$src_14, out$src_110, out$src_136, out$src_2005, out$src_1534, out$src_9,
                  out$src_470, out$src_1055, out$src_2057, out$src_146, out$src_165)
    source_cols <- c(out$source_14, out$source_110, out$source_136, out$source_2005, out$source_1534,
                     out$source_9, out$source_470, out$source_1055, out$source_2057, out$source_146, out$source_165)
    out$used_class_any <- any(src_cols == "class", na.rm = TRUE)
    out$used_lit_any   <- any(source_cols == "literature", na.rm = TRUE)
    out$used_pft_any   <- any(src_cols == "pft", na.rm = TRUE)
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

initialize_harvest_from_lookup <- function(ID, DATE, code, PFT, lk,
                                           destructive = FALSE,
                                           diagnostics = FALSE) {
  gc <- get_group_class_from_code(code, lk$mapping)
  subclass <- gc$subclass
  class <- gc$class
  crop_desc <- gc$crop_desc
  class_desc <- gc$class_desc

  # LandIQ PFTs only. Orchard clearing: PFT=woody + destructive=TRUE.
  lookup_pft <- dplyr::case_when(
    PFT == "rice"  ~ "rice",
    PFT == "row"   ~ "row",
    PFT == "hay"   ~ "hay",
    PFT == "woody" ~ "woody",
    TRUE           ~ "skip"
  )
  if (lookup_pft == "skip") return(NULL)
  # Clearing only defined for woody; ignore destructive on annual PFTs.
  dest <- isTRUE(destructive) && identical(lookup_pft, "woody")

  r_agb_rem <- get_harvest_param(lk, subclass, class, lookup_pft, "AGB_REMOVED", destructive = dest)
  r_agb_lit <- get_harvest_param(lk, subclass, class, lookup_pft, "AGB_LITTER", destructive = dest)
  r_bgb_rem <- get_harvest_param(lk, subclass, class, lookup_pft, "BGB_REMOVED", destructive = dest)
  r_bgb_lit <- get_harvest_param(lk, subclass, class, lookup_pft, "BGB_LITTER", destructive = dest)

  out <- tibble(
    LOC = ID, DATE = DATE, CLASS_SUBCLASS = code, class = class, subclass = subclass, crop_desc = crop_desc, CLASS_DESC = class_desc,
    PFT = PFT,
    AGB_REMOVED = r_agb_rem$value, AGB_LITTER = r_agb_lit$value,
    BGB_REMOVED = r_bgb_rem$value, BGB_LITTER = r_bgb_lit$value,
    ENSEMBLE_SIZE = 1L
  )
  if (diagnostics) {
    out$lookup_pft <- lookup_pft
    out$destructive <- dest
    out$src_agb_removed <- r_agb_rem$src
    out$src_agb_litter <- r_agb_lit$src
    out$src_bgb_removed <- r_bgb_rem$src
    out$src_bgb_litter <- r_bgb_lit$src
  }
  out
}

