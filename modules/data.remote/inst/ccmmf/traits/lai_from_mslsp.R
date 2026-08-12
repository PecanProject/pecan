# Leaf area index (LAI) from MSLSP EVImax and EVIamp for CCMMF pool initialization.
#
# Mourad et al. (2020): LAI = (max(0, a*sqrt(k*EVI) - b))^2 with fixed a, b. The
# paper uses only the positive part of (a*sqrt(k*EVI) - b) before squaring; EVI is
# floored at 0. k reflects phenological timing (0.15 ~ early-season annuals,
# 0.50 ~ leaf-on on perennials). Row and rice use EVIamp (strong bare season;
# amplitude tracks the crop cycle). Hay and mature woody use EVImax (often green
# year-round; amplitude can be small while peak EVI still reflects canopy). Woody
# with LandIQ CLASS YP (young perennial) uses EVIamp.
#
# Why only CLASS, not SUBCLASS: the only LandIQ branch in this file is woody YP
# vs other woody. Row, rice, and hay do not use CLASS. We do not vary LAI by
# subclass (e.g. T19 vs T4) yet; that would belong in a richer rule table later.
# Pool functions pass CLASS from your data, or set it from CLASS_SUBCLASS using
# lk$mapping when you only supply a code string.
#
# Main entry: compute_lai_from_mslsp(mslsp_EVImax, mslsp_EVIamp, pft, class,
#   diagnostics=FALSE). Scalar LAI, or diagnostics=TRUE returns a list (LAI, lai_*).
#
# Downstream: sourced by pool_calculations_from_lookup.R for initialize_planting() (MSLSP path).
# Run alone: source this file from R (requires dplyr); no env vars for this module.
# Style: docs/SCRIPT_REFACTOR_PROMPT.md in ccmmf-phenology (ASCII, simple sections).

#### Load packages

suppressPackageStartupMessages({
  library(dplyr)
})

lai_mourad_a <- 2.92
lai_mourad_b <- 0.43
lai_default_min <- 0
lai_default_max <- Inf

compute_lai_from_mslsp <- function(mslsp_EVImax, mslsp_EVIamp,
                                   pft, class = NA_character_,
                                   diagnostics = FALSE) {
  if (!diagnostics) {
    na_out <- NA_real_
  } else {
    na_out <- list(
      LAI = NA_real_, lai_rule_id = NA_character_, lai_evi_field_used = NA_character_,
      lai_evi_value_used = NA_real_, lai_k = NA_real_, lai_a = NA_real_, lai_b = NA_real_,
      lai_min = NA_real_, lai_max = NA_real_
    )
  }

  if (length(pft) != 1L || is.na(pft)) return(na_out)
  pft_lc <- tolower(trimws(as.character(pft)))
  if (!nzchar(pft_lc)) return(na_out)

  if (length(class) != 1L || is.na(class)) {
    class_uc <- NA_character_
  } else {
    class_uc <- toupper(trimws(as.character(class)))
    if (!nzchar(class_uc)) class_uc <- NA_character_
  }

  rules <- tibble(pft_lc = pft_lc, class_uc = class_uc) %>%
    mutate(
      rule_id = case_when(
        pft_lc == "row" ~ "row_amp",
        pft_lc == "rice" ~ "rice_amp",
        pft_lc == "hay" ~ "hay_max",
        pft_lc == "woody" & !is.na(class_uc) & class_uc == "YP" ~ "woody_yp_amp",
        pft_lc == "woody" ~ "woody_max",
        TRUE ~ NA_character_
      ),
      k = case_when(
        pft_lc %in% c("row", "rice") ~ 0.15,
        pft_lc %in% c("hay", "woody") ~ 0.50,
        TRUE ~ NA_real_
      ),
      evi_field = case_when(
        pft_lc %in% c("row", "rice") ~ "EVIamp",
        pft_lc == "hay" ~ "EVImax",
        pft_lc == "woody" & !is.na(class_uc) & class_uc == "YP" ~ "EVIamp",
        pft_lc == "woody" ~ "EVImax",
        TRUE ~ NA_character_
      )
    )

  if (is.na(rules$rule_id[1])) return(na_out)

  evi_used <- if (rules$evi_field[1] == "EVIamp") mslsp_EVIamp else mslsp_EVImax
  evi_num <- as.numeric(evi_used)
  k <- rules$k[1]
  a <- lai_mourad_a
  b <- lai_mourad_b
  lai <- NA_real_
  if (!is.na(evi_num) && !is.na(k)) {
    evi_num <- pmax(0, evi_num)
    term <- a * sqrt(k * evi_num) - b
    lai <- pmax(0, term)^2
    lai <- pmax(lai_default_min, pmin(lai_default_max, lai))
  }

  if (!diagnostics) {
    return(lai)
  }
  list(
    LAI = lai,
    lai_rule_id = rules$rule_id[1],
    lai_evi_field_used = rules$evi_field[1],
    lai_evi_value_used = as.numeric(evi_used),
    lai_k = k,
    lai_a = a,
    lai_b = b,
    lai_min = lai_default_min,
    lai_max = lai_default_max
  )
}
