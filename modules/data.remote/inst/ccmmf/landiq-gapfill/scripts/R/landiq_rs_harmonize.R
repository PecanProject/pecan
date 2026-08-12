# Harmonize LandIQ SUBCLASS to the Nov 2021 DWR RS legend (harmonized_SUBCLASS).
# CLASS is unchanged across legend vintages; only SUBCLASS is remapped.
# legend_year = DWR legend vintage when (CLASS, SUBCLASS) is stored in deliveries / parquet.
# calendar_year selects which legend_year rows apply (<=2020 -> 2016, >=2021 -> 2021).
# CDL disambiguation for grouped RS codes (T31, D16): LandIQ_grouped_subclass_cdl_split.csv

#' Calendar year -> DWR legend_year used for stored LandIQ codes.
landiq_calendar_legend_year <- function(year) {
  yr <- as.integer(year)[1]
  if (is.na(yr)) {
    return(NA_integer_)
  }
  if (yr <= 2014L) {
    return(2014L)
  }
  if (yr <= 2020L) {
    return(2016L)
  }
  2021L
}

load_landiq_crop_lookup <- function(path_lookup_csv) {
  if (dir.exists(path_lookup_csv)) {
    path_lookup <- file.path(path_lookup_csv, "LandIQ_cropCode_lookup_table.csv")
  } else {
    path_lookup <- path_lookup_csv
  }
  if (!file.exists(path_lookup)) {
    stop("Missing LandIQ crop lookup: ", path_lookup)
  }
  raw <- readr::read_csv(path_lookup, show_col_types = FALSE) %>%
    dplyr::mutate(
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS)),
      legend_year = suppressWarnings(as.integer(.data$legend_year)),
      harmonized_SUBCLASS = trimws(as.character(.data$harmonized_SUBCLASS))
    )
  merge_tbl <- raw %>%
    dplyr::distinct(CLASS, SUBCLASS, legend_year, .keep_all = TRUE) %>%
    dplyr::transmute(
      CLASS,
      SUBCLASS_from = SUBCLASS,
      legend_year,
      harmonized_SUBCLASS
    )
  sp <- load_landiq_grouped_subclass_cdl_split(dirname(path_lookup))
  list(lookup = raw, merge = merge_tbl, split = sp$split)
}

load_landiq_grouped_subclass_cdl_split <- function(path_lookup_dir) {
  path_split <- file.path(path_lookup_dir, "LandIQ_grouped_subclass_cdl_split.csv")
  if (!file.exists(path_split)) {
    return(list(split = dplyr::tibble()))
  }
  raw <- readr::read_csv(path_split, show_col_types = FALSE) %>%
    dplyr::mutate(
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS)),
      cdl_code = suppressWarnings(as.integer(cdl_code)),
      cdl_split_priority = suppressWarnings(as.integer(cdl_split_priority)),
      harmonized_SUBCLASS = trimws(as.character(.data$harmonized_SUBCLASS))
    )
  split_tbl <- raw %>%
    dplyr::transmute(
      CLASS,
      SUBCLASS_from = SUBCLASS,
      cdl_code,
      harmonized_SUBCLASS,
      priority = dplyr::coalesce(cdl_split_priority, 99L)
    ) %>%
    dplyr::arrange(priority)
  list(split = split_tbl)
}

#' Map stored SUBCLASS to harmonized (2021 RS) codes using each row's calendar year.
harmonize_landiq_subclass_by_year <- function(df, merge_tbl, year_col = "year") {
  if (!year_col %in% names(df)) {
    stop("harmonize_landiq_subclass_by_year requires column: ", year_col)
  }
  yrs <- sort(unique(as.integer(df[[year_col]])))
  dplyr::bind_rows(lapply(yrs, function(yr) {
    part <- df[as.integer(df[[year_col]]) == yr, , drop = FALSE]
    apply_landiq_subclass_merge(part, merge_tbl, calendar_year = yr)
  }))
}

apply_landiq_subclass_merge <- function(df, merge_tbl, calendar_year = NULL) {
  df <- df %>%
    dplyr::mutate(
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS))
    )
  keyed <- merge_tbl %>%
    dplyr::distinct(CLASS, SUBCLASS_from, .keep_all = TRUE) %>%
    dplyr::group_by(CLASS, SUBCLASS_from) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
  if (!is.null(calendar_year)) {
    ly <- landiq_calendar_legend_year(calendar_year)
    era_tbl <- merge_tbl %>%
      dplyr::filter(legend_year == ly) %>%
      dplyr::distinct(CLASS, SUBCLASS_from, .keep_all = TRUE)
    out <- df %>%
      dplyr::left_join(era_tbl, by = c("CLASS", "SUBCLASS" = "SUBCLASS_from"))
    miss <- is.na(out$harmonized_SUBCLASS) | out$harmonized_SUBCLASS == ""
    if (any(miss)) {
      out <- out %>%
        dplyr::left_join(
          keyed %>%
            dplyr::transmute(
              CLASS,
              SUBCLASS_from,
              harmonized_SUBCLASS_fb = harmonized_SUBCLASS
            ),
          by = c("CLASS", "SUBCLASS" = "SUBCLASS_from")
        ) %>%
        dplyr::mutate(
          harmonized_SUBCLASS = dplyr::if_else(
            miss,
            harmonized_SUBCLASS_fb,
            harmonized_SUBCLASS
          )
        ) %>%
        dplyr::select(-harmonized_SUBCLASS_fb)
    }
  } else {
    out <- df %>%
      dplyr::left_join(keyed, by = c("CLASS", "SUBCLASS" = "SUBCLASS_from"))
  }
  out %>%
    dplyr::mutate(
      SUBCLASS = dplyr::if_else(
        !is.na(harmonized_SUBCLASS) & harmonized_SUBCLASS != "",
        harmonized_SUBCLASS,
        SUBCLASS
      )
    ) %>%
    dplyr::select(-dplyr::any_of(c("harmonized_SUBCLASS", "legend_year")))
}

apply_landiq_subclass_split_by_cdl <- function(joined_df, split_tbl) {
  if (nrow(split_tbl) == 0L) {
    return(joined_df)
  }
  split_rules <- split_tbl %>%
    dplyr::filter(cdl_code > 0L)

  split_defaults <- split_tbl %>%
    dplyr::filter(cdl_code == 0L) %>%
    dplyr::select(CLASS, SUBCLASS_from, SUBCLASS_default = harmonized_SUBCLASS)

  out <- joined_df %>%
    dplyr::mutate(
      CLASS = trimws(as.character(CLASS)),
      SUBCLASS = trimws(as.character(SUBCLASS)),
      cdl_code = as.integer(cdl_code)
    )

  keys <- split_rules %>%
    dplyr::distinct(CLASS, SUBCLASS_from)

  for (i in seq_len(nrow(keys))) {
    cl <- keys$CLASS[i]
    sc_from <- keys$SUBCLASS_from[i]
    rules_i <- split_rules %>%
      dplyr::filter(CLASS == cl, SUBCLASS_from == sc_from) %>%
      dplyr::arrange(priority)
    idx <- out$CLASS == cl & out$SUBCLASS == sc_from
    if (!any(idx)) {
      next
    }
    for (j in seq_len(nrow(rules_i))) {
      code_j <- rules_i$cdl_code[j]
      sub_j <- rules_i$harmonized_SUBCLASS[j]
      hit <- idx & out$cdl_code == code_j
      out$SUBCLASS[hit] <- sub_j
      idx <- idx & !hit
    }
    if (any(idx)) {
      def_sub <- split_defaults %>%
        dplyr::filter(CLASS == cl, SUBCLASS_from == sc_from) %>%
        dplyr::pull(SUBCLASS_default)
      if (length(def_sub) == 1L) {
        out$SUBCLASS[idx] <- def_sub
      }
    }
  }

  out
}

landiq_truth_keys <- function(df) {
  df %>%
    dplyr::mutate(
      SUBCLASS = dplyr::if_else(
        is.na(SUBCLASS) | SUBCLASS == "",
        "**",
        SUBCLASS
      ),
      truth_key = paste(CLASS, SUBCLASS, sep = "::")
    )
}
