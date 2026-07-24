# Build long-format harvest removal / litter fraction lookup for CCMMF.
#
# Starts from agricultural LandIQ codes with a PFT. Uses placeholder fractions
# by PFT (row, rice, hay, woody, woody_destructive) for AGB_REMOVED, AGB_LITTER,
# BGB_REMOVED, BGB_LITTER. Duplicates every woody row as woody_destructive so
# orchards can use a different harvest scenario. Stacks subclass, class, PFT,
# and global levels so pool_calculations can use the same fallback order as traits.
#
# Writes plant_traits/harvest_lookup_long.rds and .csv under CCMMF_MANAGEMENT.
#
# Run:
#   Rscript $CCMMF_CODE/traits/build_harvest_lookup.R
# Env: CCMMF_MANAGEMENT

#### Load packages

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

#### Paths and constants (override root with CCMMF_MANAGEMENT)

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
landiq_lookup_csv <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
plant_traits_dir <- file.path(path_management, "plant_traits")
out_rds <- file.path(plant_traits_dir, "harvest_lookup_long.rds")
out_csv <- file.path(plant_traits_dir, "harvest_lookup_long.csv")

# Agricultural LandIQ rows only; need PFT (same filter as build_planting_lookup.R).
load_landiq_mapping <- function(path = landiq_lookup_csv) {
  d <- as.data.frame(fread(path))
  d %>%
    mutate(SUBCLASS = as.character(SUBCLASS)) %>%
    filter(is_agricultural == TRUE, !is.na(PFT)) %>%
    transmute(class = CLASS, subclass = SUBCLASS, crop_desc = SUBCLASS_desc, class_desc = CLASS_desc, PFT = PFT)
}

# Until field-derived fractions exist, these are fixed means; sd_obs NA, n_obs 0.
placeholder_means <- tibble::tribble(
  ~PFT,                  ~AGB_REMOVED, ~AGB_LITTER, ~BGB_REMOVED, ~BGB_LITTER,
  "row",                       0.80,       0.20,         0.00,        1.00,
  "rice",                      0.80,       0.20,         0.00,        1.00,
  "hay",                       0.75,       0.15,         0.00,        0.00,
  "woody",                     0.15,       0.015,        0.00,        0.00,
  "woody_destructive",         0.80,       0.20,         0.00,        1.00
)

#### Build tables by level

dir.create(plant_traits_dir, recursive = TRUE, showWarnings = FALSE)

cat("Loading LandIQ mapping (agricultural classes only)...\n")
mapping <- load_landiq_mapping(landiq_lookup_csv)

subclass_valid <- mapping %>%
  distinct(class, subclass, crop_desc, PFT)

# Class level uses CLASS_desc as the human-readable label (subclass left blank).
class_valid <- mapping %>%
  distinct(level_id = class, class_desc, PFT) %>%
  mutate(class = level_id, crop_desc = class_desc, subclass = NA_character_) %>%
  select(level_id, class, subclass, crop_desc, PFT)

# Harvest lookup distinguishes full orchard removal (woody_destructive) from partial woody harvest.
woody_sub <- subclass_valid %>% filter(PFT == "woody") %>% mutate(PFT = "woody_destructive")
subclass_valid <- bind_rows(subclass_valid, woody_sub)

woody_cls <- class_valid %>% filter(PFT == "woody") %>% mutate(PFT = "woody_destructive")
class_valid <- bind_rows(class_valid, woody_cls)

placeholders <- placeholder_means %>%
  pivot_longer(-PFT, names_to = "param", values_to = "mean_obs") %>%
  mutate(sd_obs = NA_real_, n_obs = 0L)

tbl_sub <- subclass_valid %>%
  left_join(placeholders, by = "PFT", relationship = "many-to-many") %>%
  mutate(level = "subclass")

tbl_class <- class_valid %>%
  select(-level_id) %>%
  left_join(placeholders, by = "PFT", relationship = "many-to-many") %>%
  mutate(level = "class")

# PFT-only rows: no spatial code; used when subclass and class both miss.
tbl_pft <- placeholders %>%
  mutate(
    level = "pft",
    class = NA_character_,
    subclass = NA_character_,
    crop_desc = NA_character_
  )

# Global row per param: mean of the placeholder PFT values (still placeholders).
tbl_global <- placeholders %>%
  group_by(param) %>%
  summarise(
    mean_obs = mean(mean_obs, na.rm = TRUE),
    sd_obs = NA_real_,
    n_obs = 0L,
    .groups = "drop"
  ) %>%
  mutate(
    level = "global",
    class = NA_character_,
    subclass = NA_character_,
    crop_desc = NA_character_,
    PFT = NA_character_
  )

harvest_lookup_long <- bind_rows(tbl_sub, tbl_class, tbl_pft, tbl_global) %>%
  relocate(level, PFT, class, subclass, crop_desc, param, mean_obs, sd_obs, n_obs) %>%
  arrange(level, PFT, class, subclass, param)

#### Write outputs

saveRDS(harvest_lookup_long, out_rds)
write_csv(harvest_lookup_long, out_csv)