# Build harvest rem/lit lookup for SIPNET harvest events.
#
# Inputs: subclass rem/lit (harvest_sources/harvest_fractions_long.csv;
# rebuild with write_harvest_fractions_long.R) and LandIQ crop-code mapping
# (LandIQ_cropCode_lookup_table.csv, 2021 ag legend).
# Output: plant_traits/harvest_lookup.csv
#   levels subclass|class|pft; sources ludemann|holos|swat|ipcc|literature|default
#   column destructive (FALSE/TRUE): orchard clearing is PFT=woody + destructive=TRUE
#
# Fallback used by the pool: subclass > class mean > PFT mean > default PFT.
#
#   Rscript scripts/traits/build_harvest_lookup.R
# Env: MANAGEMENT

#### Load packages

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tibble)
})

#### Paths and outputs (MANAGEMENT overrides default)

path_management <- Sys.getenv("MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
plant_traits_dir <- file.path(path_management, "plant_traits")
harvest_sources <- file.path(plant_traits_dir, "harvest_sources")
landiq_lookup_csv <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
fractions_long_csv <- file.path(harvest_sources, "harvest_fractions_long.csv")
out_csv <- file.path(plant_traits_dir, "harvest_lookup.csv")

#### Rem/lit params kept for initialize_harvest_from_lookup()

params <- c("AGB_REMOVED", "AGB_LITTER", "BGB_REMOVED", "BGB_LITTER")
ok_sources <- c("ludemann", "holos", "swat", "ipcc", "literature")

#### Helper functions

# Keep CLASS+** when crops_included is set (C** citrus, V** vineyards).
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
      SUBCLASS != "**" | nzchar(trimws(crops_included))
    ) %>%
    transmute(
      class = CLASS,
      subclass = SUBCLASS,
      crop_desc = SUBCLASS_desc,
      class_desc = as.character(CLASS_desc),
      PFT = PFT,
      landiq_code = paste0(CLASS, SUBCLASS)
    )
}

# Subclass rem/lit from harvest_sources/harvest_fractions_long.csv
load_fractions_long <- function(path = fractions_long_csv) {
  stopifnot(file.exists(path))
  raw <- as.data.frame(fread(
    path,
    colClasses = list(character = c("class", "subclass", "PFT", "source"))
  ))
  if (!("trait_key" %in% names(raw))) {
    stop("harvest_fractions_long.csv needs trait_key", call. = FALSE)
  }
  raw$param <- as.character(raw$trait_key)
  if ("value_as_used" %in% names(raw)) {
    raw$mean_obs <- suppressWarnings(as.numeric(raw$value_as_used))
  } else if ("value" %in% names(raw)) {
    raw$mean_obs <- suppressWarnings(as.numeric(raw$value))
  } else {
    stop("harvest_fractions_long.csv needs value_as_used or value", call. = FALSE)
  }
  raw %>%
    mutate(
      source = as.character(source),
      PFT = as.character(PFT),
      class = as.character(class),
      subclass = as.character(subclass),
      crop_desc = as.character(crop_desc),
      param = as.character(param),
      mean_obs = suppressWarnings(as.numeric(mean_obs)),
      n_obs = suppressWarnings(as.integer(dplyr::coalesce(n_obs, 1L)))
    ) %>%
    filter(
      !is.na(subclass), nzchar(subclass), subclass != "CLASS",
      param %in% params,
      !is.na(mean_obs),
      is.finite(mean_obs),
      source %in% ok_sources
    )
}

majority_source <- function(s) {
  s <- s[!is.na(s) & nzchar(as.character(s))]
  if (!length(s)) stop("aggregation hit empty source", call. = FALSE)
  names(sort(table(s), decreasing = TRUE))[1]
}

# PFT rem/lit used only when subclass/class/PFT means are missing.
# Orchard clearing: PFT=woody + destructive=TRUE.
make_harvest_pft_defaults <- function() {
  routine <- tibble::tribble(
    ~PFT, ~param, ~mean_obs,
    "row", "AGB_REMOVED", 0.8,
    "row", "AGB_LITTER", 0.2,
    "row", "BGB_REMOVED", 0.0,
    "row", "BGB_LITTER", 1.0,
    "rice", "AGB_REMOVED", 0.8,
    "rice", "AGB_LITTER", 0.2,
    "rice", "BGB_REMOVED", 0.0,
    "rice", "BGB_LITTER", 1.0,
    "hay", "AGB_REMOVED", 0.75,
    "hay", "AGB_LITTER", 0.15,
    "hay", "BGB_REMOVED", 0.0,
    "hay", "BGB_LITTER", 1.0,
    "woody", "AGB_REMOVED", 0.15,
    "woody", "AGB_LITTER", 0.015,
    "woody", "BGB_REMOVED", 0.0,
    "woody", "BGB_LITTER", 0.0
  ) %>%
    mutate(destructive = FALSE)

  clearing <- tibble::tribble(
    ~PFT, ~param, ~mean_obs,
    "woody", "AGB_REMOVED", 0.9,
    "woody", "AGB_LITTER", 0.1,
    "woody", "BGB_REMOVED", 0.5,
    "woody", "BGB_LITTER", 0.5
  ) %>%
    mutate(destructive = TRUE)

  dplyr::bind_rows(routine, clearing) %>%
    mutate(
      level = "pft",
      source = "default",
      class = NA_character_,
      subclass = NA_character_,
      crop_desc = NA_character_,
      sd_obs = NA_real_,
      n_obs = 0L
    ) %>%
    select(
      level, source, PFT, destructive, class, subclass, crop_desc,
      param, mean_obs, sd_obs, n_obs
    )
}

#### Load LandIQ + fractions long

cat("Loading LandIQ mapping (2021 ag only)...\n")
mapping <- load_landiq_mapping()
cat("  codes: ", nrow(mapping), "\n", sep = "")

cat("Loading ", basename(fractions_long_csv), "...\n", sep = "")
raw <- load_fractions_long()

#### Summarize at each fallback level (subclass / class / pft)

# Subclass: 2021 codes only; refresh crop_desc/PFT from mapping
cat("Summarizing subclass level...\n")
tbl_sub <- raw %>%
  mutate(landiq_code = paste0(class, subclass)) %>%
  filter(landiq_code %in% mapping$landiq_code) %>%
  left_join(
    mapping %>% select(landiq_code, crop_desc_m = crop_desc, PFT_m = PFT),
    by = "landiq_code"
  ) %>%
  mutate(
    crop_desc = dplyr::coalesce(crop_desc_m, crop_desc),
    PFT = dplyr::coalesce(PFT_m, PFT),
    level = "subclass",
    destructive = FALSE
  ) %>%
  group_by(level, PFT, destructive, class, subclass, crop_desc, param) %>%
  summarise(
    mean_obs = mean(mean_obs, na.rm = TRUE),
    n_obs = as.integer(sum(dplyr::coalesce(n_obs, 1L))),
    source = majority_source(source),
    .groups = "drop"
  ) %>%
  mutate(sd_obs = NA_real_) %>%
  select(
    level, source, PFT, destructive, class, subclass, crop_desc,
    param, mean_obs, sd_obs, n_obs
  )

cat("Summarizing class level (by class x PFT)...\n")
tbl_class <- tbl_sub %>%
  group_by(class, PFT, destructive, param) %>%
  summarise(
    mean_obs = mean(mean_obs, na.rm = TRUE),
    n_obs = as.integer(sum(dplyr::coalesce(n_obs, 1L))),
    source = majority_source(source),
    .groups = "drop"
  ) %>%
  left_join(mapping %>% distinct(class, class_desc), by = "class") %>%
  mutate(
    level = "class",
    subclass = NA_character_,
    crop_desc = dplyr::coalesce(class_desc, class),
    sd_obs = NA_real_
  ) %>%
  select(
    level, source, PFT, destructive, class, subclass, crop_desc,
    param, mean_obs, sd_obs, n_obs
  )

cat("Summarizing PFT level...\n")
tbl_pft <- tbl_sub %>%
  group_by(PFT, destructive, param) %>%
  summarise(
    mean_obs = mean(mean_obs, na.rm = TRUE),
    n_obs = as.integer(sum(dplyr::coalesce(n_obs, 1L))),
    source = majority_source(source),
    .groups = "drop"
  ) %>%
  mutate(
    level = "pft",
    class = NA_character_,
    subclass = NA_character_,
    crop_desc = NA_character_,
    sd_obs = NA_real_
  ) %>%
  select(
    level, source, PFT, destructive, class, subclass, crop_desc,
    param, mean_obs, sd_obs, n_obs
  )

harvest_lookup_long <- bind_rows(tbl_sub, tbl_class, tbl_pft)

# Defaults only when no PFT row exists for that PFT x destructive x param
has_pft <- harvest_lookup_long %>%
  filter(.data$level == "pft", .data$source != "default") %>%
  distinct(PFT, destructive, param)
pft_defaults_all <- make_harvest_pft_defaults()
defaults <- pft_defaults_all %>%
  anti_join(has_pft, by = c("PFT", "destructive", "param"))
cat(
  "  default PFT rem/lit rows: ", nrow(defaults),
  " (skipped ", nrow(pft_defaults_all) - nrow(defaults),
  " already covered)\n", sep = ""
)

harvest_lookup_long <- bind_rows(harvest_lookup_long, defaults) %>%
  filter(!is.na(mean_obs), is.finite(mean_obs), PFT != "other") %>%
  mutate(
    class = as.character(class),
    subclass = as.character(subclass),
    source = as.character(source),
    destructive = as.logical(destructive)
  ) %>%
  relocate(
    level, source, PFT, destructive, class, subclass, crop_desc,
    param, mean_obs, sd_obs, n_obs
  ) %>%
  arrange(level, PFT, destructive, class, subclass, param)

#### Write CSV

# quote so subclass "**" survives CSV round-trip
fwrite(harvest_lookup_long, out_csv, quote = TRUE)

cat("Wrote ", out_csv, " (", nrow(harvest_lookup_long), " rows)\n", sep = "")
cat(
  "Levels: ",
  paste(names(table(harvest_lookup_long$level)), table(harvest_lookup_long$level), sep = "=", collapse = ", "),
  "\n", sep = ""
)
cat(
  "Sources: ",
  paste(names(table(harvest_lookup_long$source)), table(harvest_lookup_long$source), sep = "=", collapse = ", "),
  "\n", sep = ""
)
cat(
  "destructive: ",
  paste(names(table(harvest_lookup_long$destructive)), table(harvest_lookup_long$destructive), sep = "=", collapse = ", "),
  "\n", sep = ""
)
