# Define DayCent default C:N ratios for manures
daycent_default_cn <- tibble::tribble(
  ~swat_name, ~cn_ratio, ~daycent_name, ~daycent_description, ~notes,
  "dairy_fr", 12.60, "DARY", "LRR dairy solid Inventory managed manure", NA,
  "beef_fr",  12.60, "BEEF", "LRR beef solid Inventory managed manure", NA,
  "veal_fr",  12.60, "BEEF", "LRR beef solid Inventory managed manure", "no separate parameter in DayCent, assigned as beef",
  "swine_fr", 13.45, "SWIN", "LRR swine solid dry Inventory managed manure", NA,
  "sheep_fr", 11.36, "SHEP", "LRR sheep solid Inventory managed manure", NA,
  "goat_fr",  11.36, "SHEP", "LRR sheep solid Inventory managed manure", "no separate parameter in DayCent, assigned as sheep",
  "horse_fr", 30.72, "HORS", "LRR horse solid Inventory managed manure", NA,
  "layer_fr", 17.4,  "POUL", "LRR poultry solid Inventory managed manure", "used for all poultry: layer, broiler, turkey, duck",
  "broil_fr", 17.4,  "POUL", "LRR poultry solid Inventory managed manure", "used for all poultry: layer, broiler, turkey, duck",
  "trkey_fr", 17.4,  "POUL", "LRR poultry solid Inventory managed manure", "used for all poultry: layer, broiler, turkey, duck",
  "duck_fr",  17.4,  "POUL", "LRR poultry solid Inventory managed manure", "used for all poultry: layer, broiler, turkey, duck",
  "ceap_p_n", 11.98, "BEEF/SHEP", "Composite: beef + sheep", "composite, mean of beef and sheep",
  "ceap_p_p", 11.98, "BEEF/SHEP", "Composite: beef + sheep", "composite, mean of beef and sheep",
  "ceap_h_n", 12.60, "DARY", "LRR dairy solid Inventory managed manure", "composite, assigned as dairy",
  "ceap_h_p", 12.60, "DARY", "LRR dairy solid Inventory managed manure", "composite, assigned as dairy"
)  

convert_swat_fert_table_to_pkg_df <- function() {
  fertilizer.frt <- "https://raw.githubusercontent.com/swat-model/swatplus/refs/heads/main/data/Osu_1hru/fertilizer.frt"
  readr::read_table(
    file = fertilizer.frt,
    skip = 1,
    col_types = readr::cols(.default = readr::col_character())
  ) |>
    dplyr::filter(
      !name %in% c("elem_n", "elem_p", "p") |
      !stringr::str_starts(name, "ceap")
    ) |>
    dplyr::mutate(dplyr::across(c(min_n, min_p, org_n, org_p, nh3_n), as.numeric)) |>
    dplyr::rename(
      fraction_mineral_n = min_n,
      fraction_organic_n = org_n,
      fraction_nh3_n = nh3_n
    ) |>
    dplyr::left_join(daycent_default_cn, by = c("name" = "swat_name")) |>
    dplyr::mutate(
      description = ifelse(name == "org_compost", "OrganicCompost", description),
      name = ifelse(name == "anh_nh3", "anhydrous_ammonia", name),
      # SWAT's fertilizer.frt defines fraction_nh3_n as a fraction of the total mineral N
      # But this seems unintuitive
      # defining fraction_nh3_n relative to total mass
      fraction_nh3_n = fraction_mineral_n * fraction_nh3_n,
      fraction_no3_n = fraction_mineral_n - fraction_nh3_n,
      cn_ratio = dplyr::case_when(
        !is.na(cn_ratio) ~ cn_ratio,
        fraction_organic_n > 0 ~ NA_real_,
        TRUE ~ 0 # if no organic N, C:N defined as 0
      ),
      fraction_c = ifelse(!is.na(cn_ratio) & fraction_organic_n > 0,
                         cn_ratio * fraction_organic_n, 
                         0)
    ) |>
    dplyr::select(name, description, fraction_mineral_n, fraction_nh3_n, 
           fraction_no3_n, fraction_organic_n, fraction_c, cn_ratio)
}

custom_fertilizers <- tibble::tribble(
  ~name, ~description, ~fraction_mineral_n, ~fraction_nh3_n, ~fraction_no3_n, ~fraction_organic_n, ~fraction_c, ~cn_ratio,
  "manure", "Generic mixed animal manure", 0.0138, 0.0137, 0.0001, 0.02, 0.24, 12,
  "ammonium_nitrate", "Ammonium nitrate", 0.33, 0.17, 0.16, 0.0, 0.0, NA
)
fertilizer_composition_data <- dplyr::bind_rows(
  convert_swat_fert_table_to_pkg_df(),
  custom_fertilizers
)

usethis::use_data(fertilizer_composition_data, overwrite = TRUE)

