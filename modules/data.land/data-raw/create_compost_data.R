#!/usr/bin/env Rscript
#
# builds the ca_compost_amendment packaged dataset from the raw
# compost TSV that ships in data-raw/. renames columns into the
# snake_case schema we expose downstream and adds the CalRecycle
# material_class taxonomy (14 CCR section 17852).

raw_path <- file.path("data-raw", "compost.tsv")

# map each raw material name to one of the CalRecycle classes
# (14 CCR section 17852). biosolids is empty in the current table.
material_to_class <- function(m) {
  s <- tolower(m)
  dplyr::case_when(
    grepl("grass", s) ~ "green",
    grepl("manure|alfalfa|blood|poultry|corn cob|corn stalk", s) ~ "ag",
    grepl("apple|coffee|fruit|vegetable", s) ~ "food",
    grepl("bark|sawdust|woodchip|newspaper|paper", s) ~ "wood",
    grepl("leaf|leaves|pine needle|straw", s) ~ "yard",
    TRUE ~ NA_character_
  )
}

PEcAn.logger::logger.info("Reading raw compost TSV: ", raw_path)
raw <- readr::read_tsv(raw_path, show_col_types = FALSE)

ca_compost_amendment <- raw |>
  dplyr::transmute(
    material       = .data$Material,
    material_class = material_to_class(.data$Material),
    cn_min       = .data$`C_MIN (C:N)`,
    cn_max       = .data$`C_MAX (C:N)`,
    cn_avg       = .data$`C_Avg (C:N)`,
    n_pct        = as.numeric(.data$`Total N (%)`),
    pan_pct      = as.numeric(.data$`4 week PAN (%)`),
    n_class      = .data$`LowerN/HigherN`,
    app_rate_min = .data$`RowsMIN_AppRate (lbs/acre)`,
    app_rate_max = .data$`RowsMAX_AppRate (lbs/acre)`,
    total_n_min_lbs_acre = .data$`RowsMIN_Total_N (lbs N/acre)`,
    total_n_max_lbs_acre = .data$`RowsMAX_Total_N (lbs N/acre)`,
    total_n_min_g_m2     = round(
      PEcAn.utils::ud_convert(.data$total_n_min_lbs_acre, "lb/acre", "g/m^2"), 3),
    total_n_max_g_m2     = round(
      PEcAn.utils::ud_convert(.data$total_n_max_lbs_acre, "lb/acre", "g/m^2"), 3),
    source       = trimws(.data$Source)
  )

unclassified <- ca_compost_amendment |>
  dplyr::filter(is.na(.data$material_class)) |>
  dplyr::pull(.data$material) |>
  unique()
if (length(unclassified) > 0) {
  PEcAn.logger::logger.warn(sprintf(
    "Unclassified materials: %s",
    paste(unclassified, collapse = ", ")
  ))
}

PEcAn.logger::logger.info(sprintf(
  "Harmonized %d compost materials", nrow(ca_compost_amendment)))

usethis::use_data(ca_compost_amendment, overwrite = TRUE)
