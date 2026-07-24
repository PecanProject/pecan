# Build long-format planting trait lookup for CCMMF / SIPNET-style pool init.
#
# Reads TRY master_data, maps each observation's species to a LandIQ SUBCLASS_desc
# crop group (genus lookup), converts trait values to common units, then keeps
# only rows that join to an agricultural LandIQ code with a PFT. Aggregates to
# four fallback levels used downstream: subclass (class+subclass), class crossed
# with PFT, PFT-only, and global. TRY SLA traits 3115-3117 are merged into one
# pooled SLA trait (SLA_POOLED) before aggregation.
#
# Writes plant_traits/planting_lookup_long.rds and planting_lookup_long.csv
# under CCMMF_MANAGEMENT (default: .../ccmmf/management).
#
# Run:
#   Rscript $CCMMF_CODE/traits/build_planting_lookup.R
# Env: CCMMF_MANAGEMENT, TRY_MASTER_DATA (path to master_data.RData with object master_data).

#### Load packages

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
})

#### Paths and outputs (CCMMF_MANAGEMENT and TRY_MASTER_DATA override defaults)

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
master_data_path <- Sys.getenv("TRY_MASTER_DATA", "/projectnb/dietzelab/mkim/TRYDataR/master_data.RData")
landiq_lookup_csv <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
plant_traits_dir <- file.path(path_management, "plant_traits")
out_rds <- file.path(plant_traits_dir, "planting_lookup_long.rds")
out_csv <- file.path(plant_traits_dir, "planting_lookup_long.csv")

#### SLA pool: TRY IDs 3115, 3116, 3117 combined into one pseudo-trait

sla_component_ids <- c(3115, 3116, 3117)
sla_pooled_key <- "SLA_POOLED"
sla_pooled_name <- "SLA (combined: 3115/3116/3117)"

#### Helper functions

# Agricultural LandIQ rows only; need PFT (same definition as build_harvest_lookup.R).
load_landiq_mapping <- function(path = landiq_lookup_csv) {
  d <- as.data.frame(fread(path))
  d %>%
    mutate(SUBCLASS = as.character(SUBCLASS)) %>%
    filter(is_agricultural == TRUE, !is.na(PFT)) %>%
    transmute(class = CLASS, subclass = SUBCLASS, crop_desc = SUBCLASS_desc, class_desc = CLASS_desc, PFT = PFT)
}


# TRY reports mixed units; this maps OrigUnitStr to the unit system we aggregate in.
normalize_units <- function(trait_id, value, unit_str) {
  if (is.na(value)) return(NA_real_)
  trait_id <- as.numeric(trait_id)
  value_num <- suppressWarnings(as.numeric(value))

  if (trait_id == 14) {
    if (is.na(unit_str) || unit_str == "") return(value_num)
    if (grepl("mg/g|mg g-1|mg N g-1|g/100g", unit_str, ignore.case = TRUE)) return(value_num)
    if (grepl("%", unit_str, ignore.case = TRUE)) return(value_num * 10)
    if (grepl("g/kg", unit_str, ignore.case = TRUE)) return(value_num)
    return(value_num)
  }

  if (trait_id %in% c(3115, 3116, 3117)) {
    if (is.na(unit_str) || unit_str == "") return(NA_real_)
    if (grepl("mm2/mg|mm2 mg-1|mm\\^2 mg-1", unit_str, ignore.case = TRUE)) return(value_num)
    if (grepl("cm2/g|cm2 g-1|cm\\^2 g-1", unit_str, ignore.case = TRUE)) return(value_num * 0.1)
    if (grepl("m2/kg|m\\^2 kg-1", unit_str, ignore.case = TRUE)) return(value_num)
    if (grepl("g/m2|g m-2|mg/dm2", unit_str, ignore.case = TRUE)) return(NA_real_)
    return(NA_real_)
  }

  if (trait_id %in% c(3441, 128, 3450, 3952, 3953)) {
    if (is.na(unit_str) || unit_str == "") return(value_num)
    if (grepl("kg", unit_str, ignore.case = TRUE)) return(value_num * 1000)
    if (grepl("mg", unit_str, ignore.case = TRUE)) return(value_num / 1000)
    return(value_num)
  }

  if (trait_id %in% c(1534, 2005)) {
    if (is.na(unit_str) || unit_str == "") return(value_num)
    if (grepl("%", unit_str, ignore.case = TRUE)) return(value_num / 100)
    if (grepl("g/g|g g-1", unit_str, ignore.case = TRUE)) return(value_num)
    return(value_num)
  }

  if (trait_id %in% c(146, 165, 2057, 1055)) return(value_num)
  if (is.na(unit_str) || unit_str == "") return(value_num)
  return(value_num)
}


# First word of AccSpeciesName is genus; value must match LandIQ SUBCLASS_desc text.
genus_to_group <- c(
  # Berries & small fruits
  "Ribes" = "Bush berries",
  "Rubus" = "Bush berries",
  "Vaccinium" = "Bush berries",
  "Fragaria" = "Strawberries",
  # Tree fruits
  "Malus" = "Apples",
  "Pyrus" = "Pears",
  "Vitis" = "Wine grapes",
  "Persea" = "Avocados",
  "Olea" = "Olives",
  "Juglans" = "Walnuts",
  "Pistacia" = "Pistachios",
  "Punica" = "Pomegranates",
  "Phoenix" = "Dates",
  # Grains & row crops
  "Zea" = "Corn, Sorghum or Sudan (grouped for RS only)",
  "Sorghum" = "Corn, Sorghum or Sudan (grouped for RS only)",
  "Triticum" = "Wheat",
  "Oryza" = "Rice",
  "Zizania" = "Wild rice",
  "Gossypium" = "Cotton",
  "Helianthus" = "Sunflowers",
  # Legumes & forages
  "Medicago" = "Alfalfa and alfalfa mixtures",
  "Vigna" = "Beans (dry)",
  "Glycine" = "Beans (dry)",
  "Phaseolus" = "Beans (dry)",
  # Vegetables
  "Spinacia" = "Spinach",
  "Lactuca" = "Lettuce (all types)",
  "Brassica" = "Cole crops (mixture of 22-25)",
  "Daucus" = "Carrots",
  "Cucurbita" = "Melons, squash, cucumbers",
  "Cucumis" = "Melons, squash, cucumbers",
  "Capsicum" = "Peppers",
  "Solanum" = "Potatoes",
  "Ipomoea" = "Sweet potatoes",
  "Allium" = "Onions & garlic",
  # Miscellaneous
  "Prunus" = "Miscellaneous deciduous",
  "Agrostis" = "Miscellaneous grasses",
  "Festuca" = "Miscellaneous grasses",
  "Lolium" = "Miscellaneous grasses",
  "Poa" = "Miscellaneous grasses",
  "Achillea" = "Mixed pasture",
  "Artemisia" = "Mixed pasture",
  "Carex" = "Mixed pasture",
  "Juncus" = "Mixed pasture",
  "Coffea" = "Miscellaneous field",
  "unknown" = "Miscellaneous field"
)

map_species_to_group <- function(species_vec) {
  genus <- sub("^(\\w+).*", "\\1", species_vec)
  group <- genus_to_group[genus]
  group[is.na(group)] <- "NA"
  unname(group)
}


# Per-level means plus species- and dataset-level diagnostics for the long lookup table.
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

#### Load LandIQ and TRY master_data

cat("Loading LandIQ mapping (is_agricultural only)...\n")
mapping <- load_landiq_mapping(landiq_lookup_csv)

cat("Loading master_data...\n")
obj <- load(master_data_path)
if (!("master_data" %in% obj)) {
  stop("Expected object 'master_data' in ", master_data_path)
}

#### Normalize TRY units and map species string to LandIQ crop_desc group

cat("Normalizing values + mapping species->group...\n")
data_normalized <- master_data %>%
  mutate(
    value_num = suppressWarnings(as.numeric(OrigValueStr)),
    value_std = mapply(normalize_units, TraitID, value_num, OrigUnitStr),
    TraitKey = as.character(TraitID),
    group = map_species_to_group(AccSpeciesName)
  ) %>%
  filter(!is.na(value_std))
cat("Rows after normalization (all traits): ", nrow(data_normalized), "\n", sep = "")


#### Restrict TRY rows to species that map into our LandIQ crop_desc list

group_to_codes <- mapping %>%
  select(crop_desc, class, subclass, PFT) %>%
  distinct()

sub_df <- data_normalized %>%
  filter(group != "NA") %>%
  inner_join(group_to_codes, by = c("group" = "crop_desc"))

# Same joined TRY rows; we summarize them four ways (different grouping keys below).
class_df <- sub_df
pft_df <- sub_df
glob_df <- sub_df %>%
  mutate(GLOBAL = "GLOBAL")

# Copy SLA component traits under a single TraitKey so SLA pools with one fallback chain.
sub_df_sla <- sub_df %>%
  filter(TraitID %in% sla_component_ids) %>%
  mutate(TraitKey = sla_pooled_key, TraitID = NA_real_, TraitName = sla_pooled_name)
class_df_sla <- class_df %>%
  filter(TraitID %in% sla_component_ids) %>%
  mutate(TraitKey = sla_pooled_key, TraitID = NA_real_, TraitName = sla_pooled_name)
pft_df_sla <- pft_df %>%
  filter(TraitID %in% sla_component_ids) %>%
  mutate(TraitKey = sla_pooled_key, TraitID = NA_real_, TraitName = sla_pooled_name)
glob_df_sla <- glob_df %>%
  filter(TraitID %in% sla_component_ids) %>%
  mutate(TraitKey = sla_pooled_key, TraitID = NA_real_, TraitName = sla_pooled_name)

#### Summarize traits at each fallback level (subclass, class x PFT, PFT, global; plus SLA pooled)

# Subclass key is first letter of class plus numeric subclass (matches LandIQ code layout).
sub_df <- sub_df %>% mutate(subclass_id = paste0(class, subclass))
sub_df_sla <- sub_df_sla %>% mutate(subclass_id = paste0(class, subclass))
cat("Summarizing subclass level...\n")
tbl_sub <- bind_rows(
  summarize_level(sub_df, level = "subclass", level_id_col = "subclass_id"),
  summarize_level(sub_df_sla, level = "subclass", level_id_col = "subclass_id")
)
# Class-level table is keyed by class and PFT together (one trait row per class|PFT).
class_df <- class_df %>% mutate(class_pft = paste0(class, "|", PFT))
class_df_sla <- class_df_sla %>% mutate(class_pft = paste0(class, "|", PFT))
cat("Summarizing class level (by class x PFT)...\n")
tbl_class <- bind_rows(
  summarize_level(class_df, level = "class", level_id_col = "class_pft"),
  summarize_level(class_df_sla, level = "class", level_id_col = "class_pft")
)
cat("Summarizing PFT level...\n")
tbl_pft <- bind_rows(
  summarize_level(pft_df, level = "pft", level_id_col = "PFT"),
  summarize_level(pft_df_sla, level = "pft", level_id_col = "PFT")
)
cat("Summarizing global level...\n")
tbl_global <- bind_rows(
  summarize_level(glob_df, level = "global", level_id_col = "GLOBAL"),
  summarize_level(glob_df_sla, level = "global", level_id_col = "GLOBAL")
)

#### Parse level_id and add LandIQ identity columns for the long output

# summarize_level() only keeps level_id plus trait stats. Below we split that id and
# left_join LandIQ so each row carries class, subclass, crop_desc, PFT (the same
# names pool_calculations uses). PFT-only and global levels have no LandIQ code;
# we set those columns to NA so bind_rows() stacks one consistent schema.

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
tbl_global_parsed <- tbl_global %>%
  mutate(class = NA_character_, subclass = NA_character_, crop_desc = NA_character_, PFT = NA_character_) %>%
  select(-level_id)

planting_lookup_long <- bind_rows(tbl_sub_parsed, tbl_class_parsed, tbl_pft_parsed, tbl_global_parsed) %>%
  relocate(level, PFT, class, subclass, crop_desc) %>%
  arrange(level, PFT, class, subclass, TraitID)


#### Write RDS and CSV

saveRDS(planting_lookup_long, out_rds)
write_csv(planting_lookup_long, out_csv)