#!/usr/bin/env Rscript
#
# builds bundled CA compost sampling distributions used by the
# sample_ca_compost_* functions. five tibbles in total. each one keeps a
# source field so the provenance travels with the data when the package
# loads.

# %C distribution. Bernard et al. 2023 (Front. Env. Sci., CA central coast
# vineyard) found 14 to 15% C; Sullivan et al. 2018 (OSU EM9217 proficiency
# program) median 15% TOC; CalRecycle generic roughly 20 to 23%. truncnorm
# with mean 20, sd 5, truncated 15 to 30 covers the literature range.
ca_compost_pct_c_distribution <- tibble::tibble(
  a      = 15,
  b      = 30,
  mean   = 20,
  sd     = 5,
  source = paste("Bernard et al. 2023 Front. Env. Sci.;",
                 "Sullivan, Bary, Miller & Brewer 2018 OSU EM9217;",
                 "CalRecycle finished compost characterization")
)

# C:N distribution. CDFA HSP white paper (Geisseler et al.) found CA
# finished compost bimodal at C:N <= 11 (manure dominant) vs > 11 (plant
# dominant); HSP 2026 requires applied C:N <= 25. truncnorm with mean 12,
# sd 4, truncated 8 to 25 covers both modes.
ca_compost_cn_distribution <- tibble::tibble(
  a      = 8,
  b      = 25,
  mean   = 12,
  sd     = 4,
  source = paste("CDFA Healthy Soils Program white paper (Geisseler et al.);",
                 "HSP 2026 Practice Guidelines")
)

# application rate envelope by PFT family, dry weight tons per acre. source:
# CDFA HSP white paper Table 2 + HSP 2026 Practice Guidelines, USDA NRCS
# Conservation Practice Standard 336 Soil Carbon Amendment (2022).
ca_compost_app_rate_envelope <- tibble::tibble(
  pft_family = c("annual", "perennial"),
  min_t_ac   = c(3, 2),
  max_t_ac   = c(8, 6),
  source     = "CDFA HSP white paper Table 2; HSP 2026; NRCS CPS 336"
)

# calendar window by PFT family, expressed as days before the cycle anchor
# (mslsp_OGI). annual: 7 to 28 days before the anchor (CDFA HSP Box 1,
# applied before planting). perennial: 60 to 180 days before the anchor
# (dormant season application; UC ANR orchard cost studies + Bernard et
# al. 2023 vineyard timing of Nov 9 and Jan 10).
ca_compost_calendar_window <- tibble::tibble(
  pft_family       = c("annual", "perennial"),
  offset_days_min  = c(7L, 60L),
  offset_days_max  = c(28L, 180L),
  source           = "CDFA HSP white paper Box 1; UC ANR cost studies; Bernard et al. 2023"
)

# material whitelist by PFT family. long format, one row per allowed pair.
# CalRecycle taxonomy from 14 CCR section 17852 + SB 1383 dominant
# feedstocks. biosolids excluded from food crops (regulatory). wood waste
# deferred since it has high C:N and immobilizes N
ca_compost_material_whitelist <- tibble::tibble(
  pft_family     = c("annual", "annual", "annual",
                     "perennial", "perennial", "perennial"),
  material_class = c("green", "food", "ag",
                     "green", "food", "yard"),
  source         = "14 CCR section 17852; CalRecycle SB 1383 dominant feedstocks"
)

usethis::use_data(ca_compost_pct_c_distribution,    overwrite = TRUE)
usethis::use_data(ca_compost_cn_distribution,       overwrite = TRUE)
usethis::use_data(ca_compost_app_rate_envelope,     overwrite = TRUE)
usethis::use_data(ca_compost_calendar_window,       overwrite = TRUE)
usethis::use_data(ca_compost_material_whitelist,    overwrite = TRUE)
