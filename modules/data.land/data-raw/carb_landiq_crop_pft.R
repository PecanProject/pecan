#!/usr/bin/env Rscript

# Build the carb_landiq_crop_pft packaged dataset from CLASS-level defaults
# and SUBCLASS-level overrides, joined against the LandIQ taxonomy.

library(dplyr, warn.conflicts = FALSE)

class_pft_group <- dplyr::tribble(
  ~crop_type, ~pft_group,
  "C",        "woody",
  "D",        "woody",
  "V",        "woody",
  "YP",       "woody",
  "F",        "row",
  "G",        "row",
  "P",        "row",
  "T",        "row",
  "R",        "rice",
  "I",        "idle",
  "S",        "semi-ag",
  "U",        "urban",
  "UC",       "urban",
  "UI",       "urban",
  "UL",       "urban",
  "UR",       "urban",
  "UV",       "urban",
  "NB",       "non-crop",
  "NC",       "non-crop",
  "NR",       "non-crop",
  "NV",       "non-crop",
  "NW",       "non-crop",
  "E",        "non-crop",
  "X",        "non-crop",
  "Z",        "non-crop"
)

subclass_pft_group <- dplyr::tribble(
  ~crop_type, ~crop_code, ~pft_group,
  "T",        "19",       "woody",
  "T",        "28",       "woody",
  "G",        "6",        "hay",
  "G",        "7",        "hay"
)

taxonomy <- readr::read_tsv(
  file.path("data-raw", "landiq_crop_mapping_codes.tsv"),
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"),
  progress = FALSE
) |>
  dplyr::transmute(
    crop_type = .data$CLASS,
    crop_code = .data$SUBCLASS,
    crop_desc = .data$subclass_name
  )

carb_landiq_crop_pft <- taxonomy |>
  dplyr::left_join(class_pft_group, by = "crop_type") |>
  dplyr::left_join(
    subclass_pft_group,
    by = c("crop_type", "crop_code"),
    suffix = c("_default", "_override")
  ) |>
  dplyr::mutate(
    pft_group = dplyr::coalesce(.data$pft_group_override, .data$pft_group_default),
    pecan_pft = dplyr::case_when(
      .data$pft_group == "woody" ~ "temperate.deciduous",
      .data$pft_group %in% c("row", "rice", "hay") ~ "grass",
      .data$pft_group %in% c("idle", "semi-ag", "urban", "non-crop") ~ "soil",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(
    "crop_type", "crop_code", "crop_desc", "pft_group", "pecan_pft"
  ) |>
  dplyr::arrange(.data$crop_type, .data$crop_code)

stopifnot(
  nrow(carb_landiq_crop_pft) == nrow(taxonomy),
  !anyDuplicated(carb_landiq_crop_pft[, c("crop_type", "crop_code")])
)

usethis::use_data(carb_landiq_crop_pft, overwrite = TRUE)
