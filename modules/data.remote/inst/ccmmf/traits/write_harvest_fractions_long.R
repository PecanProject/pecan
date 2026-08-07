# Build harvest_sources/harvest_fractions_long.csv from woody lit + HI diagnostics.
#
# Upstream: woody_harvest_fractions.csv, harvest_faostat_curated_* diagnostics/
# provenance, harvest_item_landiq.csv, LandIQ 2021. Citations name Ludemann /
# SWAT / Holos / IPCC / papers (not the curated xlsx). Subclass rows only;
# class/PFT rollups happen in build_harvest_lookup.R.
#
#   Rscript scripts/traits/write_harvest_fractions_long.R

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
hs <- file.path(path_management, "plant_traits/harvest_sources")
landiq_path <- file.path(path_management, "LandIQ_cropCode_lookup_table.csv")
woody_csv <- file.path(hs, "woody_harvest_fractions.csv")
diag_csv <- file.path(hs, "harvest_faostat_curated_wide_diagnostics.csv")
prov_csv <- file.path(hs, "harvest_faostat_curated_provenance.csv")
item_map_csv <- file.path(hs, "harvest_item_landiq.csv")
out_csv <- file.path(hs, "harvest_fractions_long.csv")

params <- c("AGB_REMOVED", "AGB_LITTER", "BGB_REMOVED", "BGB_LITTER")

cite_ludemann <- "Ludemann et al. Dryad Combined_crop_data (doi:10.5061/dryad.n2z34tn0x)"
url_ludemann <- "https://doi.org/10.5061/dryad.n2z34tn0x"
cite_swat <- "Neitsch et al. SWAT Theoretical Documentation Appendix A"
url_swat <- "https://swat.tamu.edu/media/69419/Appendix-A.pdf"
cite_holos <- "AAFC Holos CropFactors.csv (https://github.com/holos-aafc/Holos)"
url_holos <- "https://github.com/holos-aafc/Holos"
cite_ipcc <- "IPCC 2019 Refinement Vol.4 Ch.11 Table 11.1A"
url_ipcc <- "https://www.ipcc-nggip.iges.or.jp/public/2019rf/vol4.html"

# source labels match citation names (not opaque "dryad")
classify_hi_source <- function(value_source) {
  s <- tolower(as.character(dplyr::coalesce(value_source, "")))
  dplyr::case_when(
    grepl("ludemann|dryad", s) ~ "ludemann",
    grepl("holos", s) ~ "holos",
    grepl("swat", s) ~ "swat",
    grepl("ipcc", s) ~ "ipcc",
    TRUE ~ "ludemann"
  )
}

citation_for <- function(src) {
  dplyr::case_when(
    src == "ludemann" ~ cite_ludemann,
    src == "swat" ~ cite_swat,
    src == "holos" ~ cite_holos,
    src == "ipcc" ~ cite_ipcc,
    TRUE ~ cite_ludemann
  )
}

url_for <- function(src, website) {
  w <- as.character(website)
  dplyr::case_when(
    !is.na(w) & nzchar(w) ~ w,
    src == "ludemann" ~ url_ludemann,
    src == "swat" ~ url_swat,
    src == "holos" ~ url_holos,
    src == "ipcc" ~ url_ipcc,
    TRUE ~ url_ludemann
  )
}

mapping <- as.data.frame(fread(landiq_path)) %>%
  mutate(
    CLASS = as.character(CLASS),
    SUBCLASS = as.character(SUBCLASS),
    SUBCLASS_desc = as.character(SUBCLASS_desc),
    CLASS_desc = as.character(CLASS_desc),
    PFT = as.character(PFT),
    legend_year = suppressWarnings(as.integer(legend_year)),
    crops_included = dplyr::coalesce(as.character(crops_included), "")
  ) %>%
  filter(
    legend_year == 2021L,
    is_agricultural == TRUE,
    !is.na(PFT),
    PFT != "other",
    !grepl("idle|not cropped|new lands prepped", SUBCLASS_desc, ignore.case = TRUE),
    SUBCLASS != "**" | nzchar(trimws(crops_included))
  ) %>%
  transmute(
    class = CLASS,
    subclass = SUBCLASS,
    crop_desc = SUBCLASS_desc,
    class_desc = CLASS_desc,
    PFT = PFT,
    landiq_code = paste0(CLASS, SUBCLASS)
  )

# --- HI provenance (prefer Mean_Harvest_index / HI) ---
prov <- as.data.frame(fread(prov_csv)) %>%
  filter(variable %in% c("Mean_Harvest_index", "HI", "HI_use")) %>%
  mutate(item_l = tolower(as.character(item))) %>%
  arrange(item_l, match(variable, c("HI_use", "Mean_Harvest_index", "HI"))) %>%
  group_by(item_l) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    item_l,
    item_prov = as.character(item),
    value_source = as.character(value_source),
    website = as.character(website_of_source_of_collated_data),
    source = classify_hi_source(value_source)
  )

diag <- as.data.frame(fread(diag_csv)) %>%
  filter(calc_ready == TRUE) %>%
  mutate(
    item = as.character(item),
    item_l = tolower(item)
  )

# --- Aggregation / mapping rules (live in this script, not in CSVs) ---

# HI inventory fill priority when several items hit one LandIQ subclass
# (matches curate_harvest_faostat_sheet.R: Ludemann -> IPCC -> SWAT -> Holos).
hi_source_rank <- c(ludemann = 1L, ipcc = 2L, swat = 3L, holos = 4L)
src_rank <- function(s) {
  r <- unname(hi_source_rank[as.character(s)])
  dplyr::if_else(is.na(r), 9L, as.integer(r))
}

# Inventory item name -> LandIQ crop_desc (aliases + Holos gap-fill crops).
# CSV harvest_item_landiq.csv holds the primary curated map; these are extras.
item_to_crop_desc <- tibble::tribble(
  ~item_l, ~crop_desc,
  # FAOSTAT / Dryad name aliases
  "rice, paddy", "Rice",
  "maize", "Corn, Sorghum or Sudan (grouped for RS only)",
  "beans, dry", "Beans (dry)",
  "sugar beet", "Sugar beets",
  "sugar beets", "Sugar beets",
  "soybeans", "Beans (dry)",
  # Holos CropFactors items -> nearest 2021 LandIQ subclass
  "hay", "Miscellaneous grasses",
  "maize forage", "Corn, Sorghum or Sudan (grouped for RS only)",
  "peas, dry", "Beans (dry)",
  "broad beans and horse beans, dry", "Beans (dry)",
  "mustard seed", "Miscellaneous field",
  "buckwheat", "Miscellaneous field",
  "canary seed", "Miscellaneous field",
  "forage seed", "Miscellaneous grasses"
)

# Root/tuber crops: copy AG rem/lit onto BGB
root_like_pattern <- "potato|sweet potato|sugar beet|carrot|onion|garlic|cassava|turnip"

item_map <- as.data.frame(fread(item_map_csv)) %>%
  mutate(
    item_l = tolower(as.character(item)),
    class = as.character(class),
    subclass = as.character(subclass),
    crop_desc = as.character(crop_desc),
    PFT = as.character(PFT)
  )

liq_nonwoody <- mapping %>%
  filter(PFT != "woody") %>%
  mutate(item_l = tolower(crop_desc))

map_rows <- bind_rows(
  item_map %>% select(item_l, crop_desc, class, subclass, PFT),
  item_to_crop_desc %>%
    inner_join(liq_nonwoody %>% select(crop_desc, class, subclass, PFT), by = "crop_desc"),
  liq_nonwoody %>% select(item_l, crop_desc, class, subclass, PFT)
) %>%
  distinct(item_l, .keep_all = TRUE)

matched <- diag %>%
  inner_join(map_rows, by = "item_l") %>%
  left_join(prov, by = "item_l") %>%
  mutate(
    source = dplyr::coalesce(source, "ludemann"),
    root_like = str_detect(tolower(crop_desc), root_like_pattern),
    AGB_REMOVED = as.numeric(AGB_REMOVED_calc),
    AGB_LITTER = as.numeric(AGB_LITTER_calc),
    BGB_REMOVED = dplyr::if_else(root_like, AGB_REMOVED, 0),
    BGB_LITTER = dplyr::if_else(root_like, AGB_LITTER, 1),
    crop_group = paste0("annual_", PFT),
    citation = citation_for(source),
    url = url_for(source, website),
    derivation_note = paste0(
      "HI->rem/lit (item=", item, "); rem=HI+(1-HI)*CR/100, lit=(1-HI)*(1-CR/100)",
      dplyr::if_else(root_like, "; root-like: BGB rem/lit copy AG", ""),
      dplyr::if_else(
        !is.na(value_source) & nzchar(value_source),
        paste0("; HI from: ", value_source),
        ""
      )
    )
  )

annual_wide <- matched %>%
  mutate(rank = src_rank(source)) %>%
  group_by(class, subclass, crop_desc, PFT, crop_group) %>%
  filter(rank == min(rank, na.rm = TRUE)) %>%
  summarise(
    AGB_REMOVED = mean(AGB_REMOVED, na.rm = TRUE),
    AGB_LITTER = mean(AGB_LITTER, na.rm = TRUE),
    BGB_REMOVED = mean(BGB_REMOVED, na.rm = TRUE),
    BGB_LITTER = mean(BGB_LITTER, na.rm = TRUE),
    source = source[[1]],
    citation = citation_for(source[[1]]),
    url = {
      u <- url[!is.na(url) & nzchar(url)]
      if (length(u)) u[[1]] else url_for(source[[1]], NA_character_)
    },
    derivation_note = paste(unique(derivation_note), collapse = "; "),
    n_obs = as.integer(n()),
    .groups = "drop"
  ) %>%
  mutate(
    landiq_code = paste0(class, subclass)
  ) %>%
  filter(landiq_code %in% mapping$landiq_code) %>%
  select(-landiq_code)

# --- Woody literature (subclass crops only; no CLASS rollups in this CSV) ---
# One row per LandIQ class+subclass: if several studies share a code (e.g. C**/V**),
# mean the rem/lit fractions and keep the real citations (do not invent values).
woody_sub <- as.data.frame(fread(woody_csv)) %>%
  filter(
    crop_desc != "ORCHARD_OR_VINEYARD_REMOVAL",
    subclass != "PFT",
    subclass != "CLASS"
  ) %>%
  mutate(
    class = as.character(class),
    subclass = as.character(subclass),
    PFT = "woody",
    source = "literature",
    crop_group = dplyr::coalesce(as.character(woody_group), "woody_lit"),
    landiq_code = paste0(class, subclass)
  ) %>%
  filter(landiq_code %in% mapping$landiq_code) %>%
  group_by(class, subclass, PFT, landiq_code) %>%
  summarise(
    AGB_REMOVED = mean(as.numeric(AGB_REMOVED), na.rm = TRUE),
    AGB_LITTER = mean(as.numeric(AGB_LITTER), na.rm = TRUE),
    BGB_REMOVED = mean(as.numeric(BGB_REMOVED), na.rm = TRUE),
    BGB_LITTER = mean(as.numeric(BGB_LITTER), na.rm = TRUE),
    across(
      any_of(c(
        "standing_AG_C_Mg_ha", "fruit_C_Mg_ha", "veg_annual_C_Mg_ha",
        "wood_C_Mg_ha", "root_C_Mg_ha"
      )),
      ~ if (all(is.na(.x))) NA_real_ else mean(as.numeric(.x), na.rm = TRUE)
    ),
    crop_group = {
      g <- woody_group[!is.na(woody_group) & nzchar(as.character(woody_group))]
      if (!length(g)) "woody_lit" else as.character(g[[1]])
    },
    citation = paste(unique(primary_citation[!is.na(primary_citation) & nzchar(primary_citation)]), collapse = "; "),
    url = {
      u <- url[!is.na(url) & nzchar(url)]
      if (!length(u)) NA_character_ else u[[1]]
    },
    derivation_note = {
      notes <- unique(method_note[!is.na(method_note) & nzchar(method_note)])
      n <- n()
      base <- paste(notes, collapse = "; ")
      if (n > 1L) {
        paste0(
          "Mean of ", n, " woody literature rows for this LandIQ code. ",
          base
        )
      } else {
        base
      }
    },
    n_obs = as.integer(n()),
    source = "literature",
    .groups = "drop"
  ) %>%
  left_join(
    mapping %>% select(landiq_code, crop_desc_m = crop_desc),
    by = "landiq_code"
  ) %>%
  mutate(crop_desc = crop_desc_m) %>%
  select(-crop_desc_m, -landiq_code)

pivot_to_long <- function(df) {
  df %>%
    pivot_longer(all_of(params), names_to = "trait_key", values_to = "value") %>%
    filter(!is.na(value), is.finite(value)) %>%
    mutate(
      unit = "fraction",
      value_as_used = value,
      year = NA_integer_,
      geo_note = NA_character_,
      n_sites = NA_integer_,
      confidence = "medium",
      status = "recorded",
      n_obs = as.integer(dplyr::coalesce(n_obs, 1L))
    )
}

c_cols <- c(
  "standing_AG_C_Mg_ha", "fruit_C_Mg_ha", "veg_annual_C_Mg_ha",
  "wood_C_Mg_ha", "root_C_Mg_ha"
)

annual_long <- pivot_to_long(annual_wide) %>%
  mutate(
    standing_AG_C_Mg_ha = NA_real_,
    fruit_C_Mg_ha = NA_real_,
    veg_annual_C_Mg_ha = NA_real_,
    wood_C_Mg_ha = NA_real_,
    root_C_Mg_ha = NA_real_
  )

woody_long <- pivot_to_long(woody_sub)

# Ensure C columns exist
for (cc in c_cols) {
  if (!cc %in% names(woody_long)) woody_long[[cc]] <- NA_real_
  if (!cc %in% names(annual_long)) annual_long[[cc]] <- NA_real_
}

# Planting-like column order (no level; class rollups happen in the lookup builder)
out <- bind_rows(annual_long, woody_long) %>%
  select(
    PFT, class, subclass, crop_desc, crop_group,
    trait_key, value, unit, value_as_used, source,
    citation, url, year, geo_note, n_obs, n_sites, confidence,
    derivation_note, status,
    all_of(c_cols)
  ) %>%
  arrange(source, class, subclass, trait_key)

# quote so subclass "**" survives
fwrite(out, out_csv, quote = TRUE)
cat("Wrote ", nrow(out), " rows to ", out_csv, "\n", sep = "")
cat("By source:\n")
print(table(out$source))
cat("\nNo citations should mention harvest_faostat_curated.xlsx:\n")
cat("  hits: ", sum(grepl("harvest_faostat_curated", out$citation, ignore.case = TRUE)), "\n", sep = "")
cat("Subclass codes: ", dplyr::n_distinct(paste0(out$class, out$subclass)), "\n", sep = "")
