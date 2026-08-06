# Summarize gap-fill outcomes in the product parquet (provenance counts per year).

.qc_mode_label <- function(mode) {
  if (identical(mode, "full")) "full-year" else "within-year"
}

.qc_pct <- function(n, denom) {
  if (is.na(denom) || denom <= 0L) {
    return(NA_real_)
  }
  100 * n / denom
}

.qc_is_subclass_gapfilled <- function(subclass_source) {
  src <- trimws(as.character(subclass_source))
  !src %in% c(
    "observed", "OBSERVED", "absent",
    "X/I/YP (no subclass)", "vineyard_fallback"
  )
}

.qc_subclass_gapfill_table <- function(s2, n_s2) {
  gap <- s2[.qc_is_subclass_gapfilled(s2$subclass_source), , drop = FALSE]
  if (nrow(gap) == 0L) {
    return(data.frame(
      CLASS = character(),
      SUBCLASS = character(),
      subclass_source = character(),
      n = integer(),
      pct_of_gapfilled = numeric(),
      pct_of_season2 = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  n_gap <- nrow(gap)
  gap$CLASS <- trimws(as.character(gap$CLASS))
  gap$SUBCLASS <- trimws(as.character(gap$SUBCLASS))
  gap$CLASS[is.na(gap$CLASS) | gap$CLASS == ""] <- "(missing)"
  gap$SUBCLASS[is.na(gap$SUBCLASS) | gap$SUBCLASS == ""] <- "**"

  tab <- gap %>%
    dplyr::count(CLASS, SUBCLASS, subclass_source, name = "n", sort = TRUE) %>%
    dplyr::mutate(
      pct_of_gapfilled = round(.qc_pct(n, n_gap), 2),
      pct_of_season2 = round(.qc_pct(n, n_s2), 2)
    )

  tab
}

.qc_subclass_gapfill_aggregate <- function(subclass_tab, n_s2) {
  if (nrow(subclass_tab) == 0L) {
    return(data.frame(
      CLASS = character(),
      SUBCLASS = character(),
      n = integer(),
      pct_of_gapfilled = numeric(),
      pct_of_season2 = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  n_gap <- sum(subclass_tab$n)
  subclass_tab %>%
    dplyr::group_by(CLASS, SUBCLASS) %>%
    dplyr::summarize(n = sum(n), .groups = "drop") %>%
    dplyr::mutate(
      pct_of_gapfilled = round(.qc_pct(n, n_gap), 2),
      pct_of_season2 = round(.qc_pct(n, n_s2), 2)
    ) %>%
    dplyr::arrange(dplyr::desc(n))
}

.qc_provenance_table <- function(x, label_col = "source") {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- "(missing)"
  tab <- sort(table(x), decreasing = TRUE)
  n <- as.integer(tab)
  denom <- sum(n)
  data.frame(
    source = names(tab),
    n = n,
    pct = round(.qc_pct(n, denom), 2),
    stringsAsFactors = FALSE
  )
}

.qc_summarize_year <- function(ds, year) {
  year <- as.integer(year)[1L]
  mode <- resolve_gapfill_mode(year)

  s2 <- ds %>%
    dplyr::filter(year == !!year, season == 2L) %>%
    dplyr::collect()

  if (nrow(s2) == 0L) {
    stop("QC: year ", year, " has no season-2 rows in the product.")
  }

  n_s2 <- nrow(s2)
  n_parcels <- dplyr::n_distinct(s2$parcel_id)

  subclass_tab <- .qc_provenance_table(s2$subclass_source)
  adoy_tab <- .qc_provenance_table(s2$adoy_source)

  subclass_gap_tab <- .qc_subclass_gapfill_table(s2, n_s2)
  subclass_gap <- sum(subclass_gap_tab$n, na.rm = TRUE)
  adoy_gap <- sum(
    adoy_tab$n[!adoy_tab$source %in% c("observed", "OBSERVED", "not_applicable", "absent")],
    na.rm = TRUE
  )

  season_tab <- ds %>%
    dplyr::filter(year == !!year) %>%
    dplyr::count(season) %>%
    dplyr::collect() %>%
    dplyr::arrange(season)

  list(
    year = year,
    mode = mode,
    mode_label = .qc_mode_label(mode),
    n_season2_rows = n_s2,
    n_parcels_season2 = n_parcels,
    n_subclass_gapfilled = subclass_gap,
    pct_subclass_gapfilled = round(.qc_pct(subclass_gap, n_s2), 2),
    n_adoy_gapfilled = adoy_gap,
    pct_adoy_gapfilled = round(.qc_pct(adoy_gap, n_s2), 2),
    subclass_tab = subclass_tab,
    subclass_gap_tab = subclass_gap_tab,
    adoy_tab = adoy_tab,
    season_tab = season_tab
  )
}

.qc_write_markdown_table <- function(df, con) {
  if (nrow(df) == 0L) {
    writeLines(c("| source | n | % |", "|---|---:|---:|", "| (none) | 0 | 0 |"), con)
    return(invisible())
  }
  lines <- c(
    "| source | n | % |",
    "|---|---:|---:|",
    vapply(
      seq_len(nrow(df)),
      function(i) {
        sprintf("| %s | %s | %s |", df$source[[i]], df$n[[i]], df$pct[[i]])
      },
      character(1)
    )
  )
  writeLines(lines, con)
}

.qc_write_subclass_md_table <- function(df, con, max_rows = 25L) {
  if (nrow(df) == 0L) {
    writeLines(
      c("| CLASS | SUBCLASS | n | % gap-filled | % season 2 |", "|---|---|---:|---:|---:|", "| (none) | -- | 0 | 0 | 0 |"),
      con
    )
    return(invisible())
  }
  show <- utils::head(df, max_rows)
  lines <- c(
    "| CLASS | SUBCLASS | n | % gap-filled | % season 2 |",
    "|---|---|---:|---:|---:|",
    vapply(
      seq_len(nrow(show)),
      function(i) {
        sprintf(
          "| %s | %s | %s | %s | %s |",
          show$CLASS[[i]], show$SUBCLASS[[i]], show$n[[i]],
          show$pct_of_gapfilled[[i]], show$pct_of_season2[[i]]
        )
      },
      character(1)
    )
  )
  if (nrow(df) > max_rows) {
    lines <- c(lines, "", paste0("_Showing top ", max_rows, " of ", nrow(df), " -- see CSV for full list._"))
  }
  writeLines(lines, con)
}

write_qc_gapfill_report <- function(summaries, path_md, path_csv, product_path) {
  years <- vapply(summaries, function(s) s$year, integer(1))
  summary_df <- do.call(
    rbind,
    lapply(summaries, function(s) {
      data.frame(
        year = s$year,
        mode = s$mode_label,
        n_season2_rows = s$n_season2_rows,
        n_parcels_season2 = s$n_parcels_season2,
        n_subclass_gapfilled = s$n_subclass_gapfilled,
        pct_subclass_gapfilled = s$pct_subclass_gapfilled,
        n_adoy_gapfilled = s$n_adoy_gapfilled,
        pct_adoy_gapfilled = s$pct_adoy_gapfilled,
        stringsAsFactors = FALSE
      )
    })
  )

  prov_long <- do.call(
    rbind,
    c(
      lapply(summaries, function(s) {
        cbind(year = s$year, metric = "subclass_source", s$subclass_tab)
      }),
      lapply(summaries, function(s) {
        cbind(year = s$year, metric = "adoy_source", s$adoy_tab)
      })
    )
  )

  subclass_long <- do.call(
    rbind,
    lapply(summaries, function(s) {
      if (nrow(s$subclass_gap_tab) == 0L) {
        return(NULL)
      }
      cbind(year = s$year, s$subclass_gap_tab)
    })
  )
  if (is.null(subclass_long)) {
    subclass_long <- data.frame(
      year = integer(),
      CLASS = character(),
      SUBCLASS = character(),
      subclass_source = character(),
      n = integer(),
      pct_of_gapfilled = numeric(),
      pct_of_season2 = numeric(),
      stringsAsFactors = FALSE
    )
  }

  readr::write_csv(summary_df, path_csv)
  readr::write_csv(prov_long, sub("\\.csv$", "_provenance.csv", path_csv))
  readr::write_csv(subclass_long, sub("\\.csv$", "_subclass.csv", path_csv))

  con <- file(path_md, open = "wt")
  on.exit(close(con), add = TRUE)
  writeLines(
    c(
      "# Gap-fill QC report",
      "",
      paste0("- Product: `", product_path, "`"),
      paste0("- Years checked: ", paste(years, collapse = ", ")),
      paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      ""
    ),
    con
  )

  for (s in summaries) {
    writeLines(
      c(
        paste0("## ", s$year, " (", s$mode_label, ")"),
        "",
        paste0(
          "Season 2 rows: **", format(s$n_season2_rows, big.mark = ","), "**",
          " (", format(s$n_parcels_season2, big.mark = ","), " parcels)"
        ),
        paste0(
          "Gap-filled subclass: **", format(s$n_subclass_gapfilled, big.mark = ","),
          "** (", s$pct_subclass_gapfilled, "% of season 2)"
        ),
        paste0(
          "Gap-filled ADOY: **", format(s$n_adoy_gapfilled, big.mark = ","),
          "** (", s$pct_adoy_gapfilled, "% of season 2)"
        ),
        "",
        "### Rows per season",
        ""
      ),
      con
    )
    season_lines <- c(
      "| season | n |",
      "|---|---:|",
      vapply(
        seq_len(nrow(s$season_tab)),
        function(i) sprintf("| %s | %s |", s$season_tab$season[[i]], s$season_tab$n[[i]]),
        character(1)
      )
    )
    writeLines(season_lines, con)
    writeLines(c("", "### Crop identity (`subclass_source`, season 2)", ""), con)
    .qc_write_markdown_table(s$subclass_tab, con)
    writeLines(c("", "### ADOY (`adoy_source`, season 2)", ""), con)
    .qc_write_markdown_table(s$adoy_tab, con)
    subclass_agg <- .qc_subclass_gapfill_aggregate(s$subclass_gap_tab, s$n_season2_rows)
    writeLines(
      c(
        "",
        "### Gap-filled crops (season 2, by CLASS / SUBCLASS)",
        "",
        "Which crop types account for the most subclass gap-fill. Full detail (including ",
        "`subclass_source`) is in `qc_gapfill_summary_subclass.csv`.",
        ""
      ),
      con
    )
    .qc_write_subclass_md_table(subclass_agg, con)
    writeLines("", con)
  }

  invisible(list(summary = summary_df, provenance = prov_long, subclass = subclass_long))
}

qc_gapfill_product <- function(years = resolve_gapfill_run_years()) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) {
    stop("QC: no years to check.")
  }

  product_path <- file.path(landiq_product_root(), "crops_all_years.parq")
  if (!file.exists(product_path)) {
    stop("QC: product not found: ", product_path)
  }

  out_dir <- path_outputs()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path_md <- file.path(out_dir, "qc_gapfill_report.md")
  path_csv <- file.path(out_dir, "qc_gapfill_summary.csv")

  message("=== Gap-fill QC ===")
  message("Product: ", product_path)
  message("Years: ", paste(years, collapse = ", "))

  ds <- arrow::open_dataset(product_path)
  product_years <- ds %>%
    dplyr::distinct(year) %>%
    dplyr::collect() %>%
    dplyr::pull(year)
  missing <- setdiff(years, product_years)
  if (length(missing) > 0L) {
    stop(
      "QC: product is missing requested year(s): ",
      paste(missing, collapse = ", ")
    )
  }

  summaries <- lapply(years, function(y) .qc_summarize_year(ds, y))

  res <- write_qc_gapfill_report(summaries, path_md, path_csv, product_path)
  message("Wrote ", path_md)
  message("Wrote ", path_csv)
  message("Wrote ", sub("\\.csv$", "_provenance.csv", path_csv))
  message("Wrote ", sub("\\.csv$", "_subclass.csv", path_csv))

  for (s in summaries) {
    message(
      "  year ", s$year, " (", s$mode_label, "): subclass gap-filled ",
      format(s$n_subclass_gapfilled, big.mark = ","), " (", s$pct_subclass_gapfilled,
      "%); ADOY gap-filled ", format(s$n_adoy_gapfilled, big.mark = ","), " (",
      s$pct_adoy_gapfilled, "%)"
    )
  }

  message("=== Gap-fill QC done ===")
  invisible(res)
}
