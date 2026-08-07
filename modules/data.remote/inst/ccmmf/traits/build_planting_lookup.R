# Build planting trait lookup for SIPNET pool init.
#
# Inputs: TRY allocation files (plant_traits/TRY_allocation_traits/*.txt),
# curated lit rows (planting_sources/literature_allocation_traits.csv), LandIQ 2021.
# Output: plant_traits/planting_lookup.csv
#   levels subclass|class|pft; sources try|literature|default
#
# TRY species matching: only AccSpeciesName listed in LandIQ latin_names
# (no genus fallback / wild congeners).
#
# Fallback used by the pool: TRY subclass > TRY class > lit subclass > lit class >
# TRY PFT > default PFT (programmatic fine/coarse root split when missing).
#
#   Rscript scripts/traits/build_planting_lookup.R
# Env: CCMMF_MANAGEMENT, TRY_ALLOCATION_DIR

#### Load packages

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
})

#### Paths and outputs (CCMMF_MANAGEMENT and TRY_ALLOCATION_DIR override defaults)

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
try_allocation_dir <- Sys.getenv(
  "TRY_ALLOCATION_DIR",
  file.path(path_management, "plant_traits/TRY_allocation_traits")
)
landiq_lookup_csv <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
trait_lit_csv <- file.path(
  path_management, "plant_traits/planting_sources/literature_allocation_traits.csv"
)
plant_traits_dir <- file.path(path_management, "plant_traits")
out_csv <- file.path(plant_traits_dir, "planting_lookup.csv")
coverage_csv <- file.path(plant_traits_dir, "planting_lookup_coverage.csv")

#### Traits kept for pool_calculations_from_lookup.R

pool_trait_ids <- c(
  9L,    # root/shoot
  14L,   # leaf N (mg/g)
  110L,  # LWR
  136L,  # stem mass fraction
  146L,  # leaf C/N
  165L,  # stem C/N
  470L,  # RMF
  1019L, # coarse/fine root mass ratio
  1055L, # root C/N
  1534L, # coarse root mass fraction
  2005L, # fine root mass fraction
  2057L, # fine root C/N
  3115L, # SLA (petiole excluded)
  3116L, # SLA (petiole included)
  3117L  # SLA (petiole undefined)
)


#### Helper functions

# Agricultural LandIQ rows - 2021 RS legend only (matches harmonized products).
# Keep CLASS+** when crops_included is set (C** citrus, V** vineyards).
# latin_names (semicolon-separated) drives TRY species -> code matching.
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
      crops_included = dplyr::coalesce(crops_included, ""),
      latin_names = if ("latin_names" %in% names(.)) {
        as.character(dplyr::coalesce(latin_names, ""))
      } else {
        ""
      }
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
      SUBCLASS != "**" | nzchar(trimws(crops_included))
    ) %>%
    transmute(
      class = CLASS, subclass = SUBCLASS, crop_desc = SUBCLASS_desc,
      class_desc = CLASS_desc, PFT = PFT, latin_names = latin_names,
      landiq_code = paste0(CLASS, SUBCLASS)
    )
}


# Pass-through for pool traits (TRY StdValue already in target units).
# SLA only: keep rows whose UnitName is mm2/mg or m2/kg; drop unknown units.
normalize_units <- function(trait_id, value, unit_str) {
  if (is.na(value)) return(NA_real_)
  trait_id <- as.numeric(trait_id)
  value_num <- suppressWarnings(as.numeric(value))
  if (is.na(value_num)) return(NA_real_)

  if (trait_id %in% c(3115, 3116, 3117)) {
    if (is.na(unit_str) || !nzchar(unit_str)) return(NA_real_)
    if (grepl("mm2/mg|mm2 mg-1|mm\\^2 mg-1|m2/kg|m\\^2 kg-1", unit_str, ignore.case = TRUE)) {
      return(value_num)
    }
    return(NA_real_)
  }

  value_num
}


# Lowercase Genus + species key for cropcode matching ("Zea mays", "zea_mays" ->
# "zea mays"). Strips hybrid markers (x). One-token names kept as genus-only.
normalize_species_key <- function(species_vec) {
  s <- tolower(trimws(as.character(species_vec)))
  s <- gsub("_", " ", s)
  s <- gsub("\\s+[x\u00d7]\\s+", " ", s)
  s <- gsub("\\s+x$", "", s)
  s <- gsub("\\s+", " ", s)
  vapply(strsplit(s, " ", perl = TRUE), function(w) {
    w <- w[nzchar(w) & w != "x" & w != "\u00d7"]
    if (length(w) >= 2L) paste(w[1L], w[2L]) else if (length(w) == 1L) w[1L] else NA_character_
  }, character(1))
}


# Build species -> LandIQ code from mapping$latin_names.
# If a name is listed under more than one code (rare; e.g. plums/prunes), keep
# the first LandIQ row encountered so each TRY species maps to one subclass.
load_species_code_maps <- function(mapping) {
  stopifnot(all(c("landiq_code", "latin_names") %in% names(mapping)))

  sp_rows <- list()
  for (i in seq_len(nrow(mapping))) {
    code <- mapping$landiq_code[[i]]
    names_raw <- mapping$latin_names[[i]]
    if (!nzchar(trimws(names_raw))) next
    parts <- trimws(unlist(strsplit(names_raw, ";", fixed = TRUE)))
    parts <- parts[nzchar(parts)]
    for (nm in parts) {
      key <- normalize_species_key(nm)
      if (is.na(key) || !nzchar(key)) next
      sp_rows[[length(sp_rows) + 1L]] <- data.frame(
        sp_key = key, landiq_code = code, stringsAsFactors = FALSE
      )
    }
  }

  species_to_code <- character(0)
  if (length(sp_rows)) {
    sp_df <- bind_rows(sp_rows) %>% distinct(sp_key, landiq_code)
    sp_df <- sp_df %>% group_by(sp_key) %>% slice(1L) %>% ungroup()
    species_to_code <- setNames(sp_df$landiq_code, sp_df$sp_key)
  }

  list(species_to_code = species_to_code)
}


# Map TRY AccSpeciesName to LandIQ code only when the name is in latin_names.
map_species_to_code <- function(species_vec, species_to_code) {
  sp_key <- normalize_species_key(species_vec)
  unname(species_to_code[sp_key])
}


# Read TRY .txt: pool TraitIDs. Prefer StdValue/UnitName so values share TRY's
# units (we do not convert Orig* ourselves).
load_try_allocation_releases <- function(dir) {
  if (!dir.exists(dir)) {
    stop("TRY allocation dir not found: ", dir, call. = FALSE)
  }
  files <- list.files(dir, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0L) {
    stop("No TRY allocation .txt files in ", dir, call. = FALSE)
  }
  want <- c(
    "AccSpeciesName", "SpeciesName", "TraitID", "TraitName",
    "OrigValueStr", "OrigUnitStr", "UnitName", "StdValue",
    "DatasetID", "ErrorRisk"
  )
  cat("Loading TRY allocation releases (", length(files), " file(s))...\n", sep = "")
  pieces <- lapply(files, function(f) {
    cat("  ", basename(f), " ...\n", sep = "")
    hdr <- names(fread(f, sep = "\t", nrows = 0L, quote = ""))
    sel <- intersect(want, hdr)
    d <- as.data.frame(fread(
      f, sep = "\t", quote = "", select = sel,
      na.strings = c("", "NA"), showProgress = TRUE
    ))
    if (!"TraitID" %in% names(d)) return(NULL)
    d <- d[!is.na(d$TraitID) & nzchar(as.character(d$TraitID)), , drop = FALSE]
    if (nrow(d) == 0L) return(NULL)
    d$TraitID <- suppressWarnings(as.numeric(d$TraitID))
    d <- d[!is.na(d$TraitID) & d$TraitID %in% pool_trait_ids, , drop = FALSE]
    if (nrow(d) == 0L) return(NULL)
    if ("ErrorRisk" %in% names(d)) {
      d$ErrorRisk <- suppressWarnings(as.numeric(d$ErrorRisk))
    }
    if ("StdValue" %in% names(d)) {
      d$StdValue <- suppressWarnings(as.numeric(d$StdValue))
      d$OrigValueStr <- ifelse(!is.na(d$StdValue), as.character(d$StdValue), d$OrigValueStr)
    }
    if (nrow(d) == 0L) return(NULL)
    if ("UnitName" %in% names(d)) {
      d$OrigUnitStr <- ifelse(
        !is.na(d$UnitName) & nzchar(as.character(d$UnitName)),
        as.character(d$UnitName),
        d$OrigUnitStr
      )
    }
    if (!"AccSpeciesName" %in% names(d) && "SpeciesName" %in% names(d)) {
      d$AccSpeciesName <- d$SpeciesName
    }
    keep <- intersect(
      c(
        "AccSpeciesName", "TraitID", "TraitName", "OrigValueStr", "OrigUnitStr",
        "DatasetID", "ErrorRisk"
      ),
      names(d)
    )
    d[, keep, drop = FALSE]
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    stop("No pool TraitID rows found in TRY allocation .txt files under ", dir, call. = FALSE)
  }
  bind_rows(pieces)
}


# Last-resort fine/coarse root mass fractions (relative split of M_root after
# pool normalize). Rice/row (incl. berries etc. in row): nearly all fine.
# Woody/hay rely on TRY PFT coverage (no default).
make_planting_pft_defaults <- function() {
  split_rows <- function(pft, fine, coarse) {
    tibble::tibble(
      PFT = pft,
      TraitKey = c("2005", "1534"),
      TraitID = c(2005L, 1534L),
      TraitName = c("Fine root mass fraction", "Coarse root mass fraction"),
      mean_obs = c(fine, coarse)
    )
  }
  dplyr::bind_rows(
    lapply(c("rice", "row"), function(p) split_rows(p, 0.99, 0.01))
  ) %>%
    dplyr::mutate(
      level = "pft",
      source = "default",
      class = NA_character_,
      subclass = NA_character_,
      crop_desc = NA_character_,
      sd_obs = NA_real_,
      n_obs = 0L,
      n_species = NA_integer_,
      n_datasets = NA_integer_,
      mean_species = NA_real_,
      sd_species_mean = NA_real_,
      min_species_n_obs = NA_integer_,
      median_species_n_obs = NA_integer_,
      max_species_n_obs = NA_integer_,
      mean_dataset = NA_real_,
      sd_dataset_mean = NA_real_,
      min_dataset_n_obs = NA_integer_,
      median_dataset_n_obs = NA_integer_,
      max_dataset_n_obs = NA_integer_
    )
}

# Curated lit: planting_sources/literature_allocation_traits.csv
# (subclass|class, source=literature, pool TraitKeys including SLA).
load_lit_lookup_rows <- function(path = trait_lit_csv) {
  if (!file.exists(path)) {
    cat("Literature lookup CSV not found (skipping): ", path, "\n", sep = "")
    return(tibble())
  }
  as.data.frame(fread(path)) %>%
    mutate(
      level = as.character(level),
      source = as.character(dplyr::coalesce(source, "literature")),
      PFT = as.character(PFT),
      class = as.character(class),
      subclass = as.character(subclass),
      crop_desc = as.character(crop_desc),
      TraitKey = as.character(TraitKey),
      TraitID = suppressWarnings(as.numeric(TraitID)),
      TraitID = ifelse(TraitKey == "SLA", NA_real_, TraitID),
      TraitName = as.character(TraitName),
      mean_obs = suppressWarnings(as.numeric(mean_obs)),
      n_obs = suppressWarnings(as.integer(n_obs))
    ) %>%
    filter(
      level %in% c("subclass", "class"),
      !is.na(mean_obs),
      is.finite(mean_obs)
    )
}


# Aggregate observations at one fallback level (subclass / class|PFT / PFT).
# mean_obs is what the pool uses; species_/dataset_ columns are diagnostics only.
summarize_level <- function(df, level, level_id_col) {
  id_sym <- rlang::sym(level_id_col)

  obs <- df %>%
    group_by(!!id_sym, TraitKey, TraitID, TraitName) %>%
    summarise(
      mean_obs = mean(value_std, na.rm = TRUE),
      sd_obs   = sd(value_std, na.rm = TRUE),
      n_obs    = sum(!is.na(value_std)),
      n_species  = n_distinct(AccSpeciesName),
      n_datasets = n_distinct(DatasetID),
      .groups = "drop"
    ) %>%
    rename(level_id = !!id_sym)

  sp <- df %>%
    group_by(!!id_sym, TraitKey, TraitID, TraitName, AccSpeciesName) %>%
    summarise(
      mean_sp = mean(value_std, na.rm = TRUE),
      n_obs_sp = sum(!is.na(value_std)),
      .groups = "drop"
    )
  sp_sum <- sp %>%
    group_by(!!id_sym, TraitKey, TraitID, TraitName) %>%
    summarise(
      mean_species = mean(mean_sp, na.rm = TRUE),
      sd_species_mean = sd(mean_sp, na.rm = TRUE),
      min_species_n_obs = suppressWarnings(min(n_obs_sp, na.rm = TRUE)),
      median_species_n_obs = suppressWarnings(stats::median(n_obs_sp, na.rm = TRUE)),
      max_species_n_obs = suppressWarnings(max(n_obs_sp, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rename(level_id = !!id_sym)

  ds <- df %>%
    group_by(!!id_sym, TraitKey, TraitID, TraitName, DatasetID) %>%
    summarise(
      mean_ds = mean(value_std, na.rm = TRUE),
      n_obs_ds = sum(!is.na(value_std)),
      .groups = "drop"
    )
  ds_sum <- ds %>%
    group_by(!!id_sym, TraitKey, TraitID, TraitName) %>%
    summarise(
      mean_dataset = mean(mean_ds, na.rm = TRUE),
      sd_dataset_mean = sd(mean_ds, na.rm = TRUE),
      min_dataset_n_obs = suppressWarnings(min(n_obs_ds, na.rm = TRUE)),
      median_dataset_n_obs = suppressWarnings(stats::median(n_obs_ds, na.rm = TRUE)),
      max_dataset_n_obs = suppressWarnings(max(n_obs_ds, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rename(level_id = !!id_sym)

  obs %>%
    left_join(sp_sum, by = c("level_id", "TraitKey", "TraitID", "TraitName")) %>%
    left_join(ds_sum, by = c("level_id", "TraitKey", "TraitID", "TraitName")) %>%
    mutate(level = level) %>%
    relocate(level, level_id, TraitKey, TraitID, TraitName)
}


#### Create output directory

dir.create(plant_traits_dir, recursive = TRUE, showWarnings = FALSE)

#### Load LandIQ and TRY allocation releases

cat("Loading LandIQ mapping (2021 ag only)...\n")
mapping <- load_landiq_mapping(landiq_lookup_csv)

cat("Building species->code map from cropcode latin_names...\n")
code_maps <- load_species_code_maps(mapping)
cat("  species keys: ", length(code_maps$species_to_code), "\n", sep = "")

try_data <- load_try_allocation_releases(try_allocation_dir)
cat(
  "  TRY pool rows: ", nrow(try_data),
  "; TraitIDs: ", paste(sort(unique(try_data$TraitID)), collapse = ", "), "\n", sep = ""
)

#### Normalize TRY units and map species string to LandIQ code

# Strip invalid bytes from TRY string columns (occasional non-UTF8).
scrub_chr <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x[is.na(x)] <- NA_character_
  x
}

cat("Normalizing values + mapping species->LandIQ code...\n")
data_normalized <- try_data %>%
  mutate(
    AccSpeciesName = scrub_chr(AccSpeciesName),
    OrigValueStr = scrub_chr(OrigValueStr),
    OrigUnitStr = scrub_chr(OrigUnitStr),
    value_num = suppressWarnings(as.numeric(OrigValueStr)),
    value_std = mapply(normalize_units, TraitID, value_num, OrigUnitStr),
    is_sla = TraitID %in% c(3115L, 3116L, 3117L),
    TraitKey = ifelse(is_sla, "SLA", as.character(TraitID)),
    TraitID = ifelse(is_sla, NA_real_, TraitID),
    TraitName = ifelse(
      is_sla,
      "Leaf area per leaf dry mass (specific leaf area, SLA)",
      TraitName
    ),
    landiq_code = map_species_to_code(AccSpeciesName, code_maps$species_to_code),
    mapped = !is.na(landiq_code)
  ) %>%
  select(-is_sla) %>%
  filter(!is.na(value_std))

n_mapped <- sum(data_normalized$mapped)
n_unmapped <- sum(!data_normalized$mapped)
cat("Rows after normalization (pool traits): ", nrow(data_normalized),
    " (mapped: ", n_mapped, ", unmapped: ", n_unmapped, ")\n", sep = "")

#### Unique-species coverage vs 2021 LandIQ codes

sp_cov <- data_normalized %>%
  distinct(AccSpeciesName, landiq_code, mapped) %>%
  mutate(match_level = ifelse(mapped, "species", "unmapped"))

code_cov <- mapping %>%
  select(landiq_code, crop_desc, PFT, class, subclass, latin_names) %>%
  left_join(
    data_normalized %>%
      filter(mapped) %>%
      group_by(landiq_code) %>%
      summarise(
        n_obs = n(),
        n_species = n_distinct(AccSpeciesName),
        n_traits = n_distinct(TraitKey),
        .groups = "drop"
      ),
    by = "landiq_code"
  ) %>%
  mutate(
    n_obs = dplyr::coalesce(n_obs, 0L),
    n_species = dplyr::coalesce(n_species, 0L),
    n_traits = dplyr::coalesce(n_traits, 0L),
    has_latin = nzchar(trimws(dplyr::coalesce(latin_names, ""))),
    has_try = n_obs > 0L
  ) %>%
  arrange(desc(has_try), landiq_code)

write_csv(code_cov, coverage_csv)
cat("Wrote coverage table: ", coverage_csv, "\n", sep = "")
cat(
  "Unique AccSpeciesName: ", n_distinct(data_normalized$AccSpeciesName),
  " | species-mapped spp: ", sum(sp_cov$match_level == "species"),
  " | unmapped spp: ", sum(sp_cov$match_level == "unmapped"), "\n", sep = ""
)
cat(
  "LandIQ codes with TRY hits: ", sum(code_cov$has_try),
  " / ", nrow(code_cov),
  " | with latin but no TRY: ", sum(code_cov$has_latin & !code_cov$has_try),
  " | no latin (expected thin): ", sum(!code_cov$has_latin), "\n", sep = ""
)

code_to_meta <- mapping %>%
  select(landiq_code, class, subclass, crop_desc, PFT)

# Class/PFT means come only from species-matched rows (no genus extras)
mapped_df <- data_normalized %>%
  filter(mapped) %>%
  inner_join(code_to_meta, by = "landiq_code")

cat("Mapped rows joined to LandIQ: ", nrow(mapped_df), "\n", sep = "")

#### Build level-specific data frames

sub_df <- mapped_df %>% mutate(subclass_id = paste0(class, subclass))
class_df <- mapped_df %>% mutate(class_pft = paste0(class, "|", PFT))
pft_df <- mapped_df

#### Summarize traits at each fallback level

cat("Summarizing subclass level...\n")
tbl_sub <- summarize_level(sub_df, level = "subclass", level_id_col = "subclass_id")

cat("Summarizing class level (by class x PFT)...\n")
tbl_class <- summarize_level(class_df, level = "class", level_id_col = "class_pft")

cat("Summarizing PFT level...\n")
tbl_pft <- summarize_level(pft_df, level = "pft", level_id_col = "PFT")

#### Parse level_id and add LandIQ identity columns for the long output

tbl_sub_parsed <- tbl_sub %>%
  mutate(class = sub("^(.)(.*)$", "\\1", level_id),
         subclass = sub("^.(.*)$", "\\1", level_id)) %>%
  left_join(mapping %>% distinct(class, subclass, crop_desc, PFT), by = c("class", "subclass")) %>%
  select(-level_id)

tbl_class_parsed <- tbl_class %>%
  mutate(class = sub("^([^|]+)\\|(.*)$", "\\1", level_id),
         PFT = sub("^([^|]+)\\|(.*)$", "\\2", level_id)) %>%
  left_join(mapping %>% distinct(class, class_desc), by = "class") %>%
  mutate(subclass = NA_character_, crop_desc = class_desc) %>%
  select(-class_desc, -level_id)

tbl_pft_parsed <- tbl_pft %>%
  mutate(PFT = level_id, class = NA_character_, subclass = NA_character_, crop_desc = NA_character_) %>%
  select(-level_id)

planting_lookup_long <- bind_rows(tbl_sub_parsed, tbl_class_parsed, tbl_pft_parsed) %>%
  mutate(source = "try")

cat("Loading literature rows from ", basename(trait_lit_csv), "...\n", sep = "")
lit_rows <- load_lit_lookup_rows(trait_lit_csv)
cat(
  "  lit rows: ", nrow(lit_rows),
  " (subclass ", sum(lit_rows$level == "subclass"),
  ", class ", sum(lit_rows$level == "class"), ")\n", sep = ""
)

planting_lookup_long <- bind_rows(planting_lookup_long, lit_rows)

# PFT defaults only when TRY/lit has no PFT row for that TraitKey
has_pft <- planting_lookup_long %>%
  filter(.data$level == "pft", .data$source %in% c("try", "literature")) %>%
  mutate(TraitKey = as.character(.data$TraitKey)) %>%
  distinct(PFT, TraitKey)
pft_defaults_all <- make_planting_pft_defaults()
defaults <- pft_defaults_all %>%
  anti_join(has_pft, by = c("PFT", "TraitKey"))
cat(
  "  default PFT root-split rows: ", nrow(defaults),
  " (skipped ", nrow(pft_defaults_all) - nrow(defaults),
  " already covered by try/lit)\n", sep = ""
)

planting_lookup_long <- bind_rows(planting_lookup_long, defaults) %>%
  mutate(TraitKey = as.character(TraitKey)) %>%
  relocate(level, source, PFT, class, subclass, crop_desc) %>%
  arrange(level, PFT, class, subclass, TraitKey)

#### Write CSV

# quote so subclass "**" survives CSV round-trip
planting_lookup_long <- planting_lookup_long %>%
  mutate(class = as.character(class), subclass = as.character(subclass))
fwrite(planting_lookup_long, out_csv, quote = TRUE)

cat("Wrote ", out_csv, " (", nrow(planting_lookup_long), " rows)\n", sep = "")
cat("Levels: ", paste(names(table(planting_lookup_long$level)), table(planting_lookup_long$level), sep = "=", collapse = ", "), "\n", sep = "")
cat(
  "Sources: ",
  paste(names(table(planting_lookup_long$source)), table(planting_lookup_long$source), sep = "=", collapse = ", "),
  "\n", sep = ""
)
