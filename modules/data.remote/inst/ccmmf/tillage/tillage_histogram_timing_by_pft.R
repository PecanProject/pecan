#!/usr/bin/env Rscript

# Histograms of when tillage events occur, faceted by PFT.
#
# Uses min_date from tillage_statewide_Y.parquet (NDTI minimum in each fallow window).
# Default: **all event rows** (several parcels have >1 fallow per file year). Set
# DEDUPE_PARCEL=1 for at most one row per parcel (earliest OGMn_date after sort).
#
# Env: MAP_YEAR, TILLAGE_PARQUET, CCMMF_MANAGEMENT, OUT_DIR,
#      BINWIDTH_DOY (default 14), DEDUPE_PARCEL (0|1)

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(ggplot2)
})

map_year <- suppressWarnings(as.integer(Sys.getenv("MAP_YEAR", "2020")))
if (is.na(map_year)) {
  map_year <- 2020L
}

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
path_tillage <- Sys.getenv(
  "TILLAGE_PARQUET",
  file.path(path_management, "event_files", sprintf("tillage_statewide_%d.parquet", map_year))
)
if (!file.exists(path_tillage)) {
  stop("Tillage parquet not found: ", path_tillage)
}

dedupe <- tolower(trimws(Sys.getenv("DEDUPE_PARCEL", "0"))) %in% c("1", "true", "yes")

dt <- as.data.table(arrow::read_parquet(path_tillage))
if ("event_type" %in% names(dt)) {
  dt <- dt[event_type == "tillage" | is.na(event_type)]
}
if (!all(c("min_date", "PFT") %in% names(dt))) {
  stop("Need columns min_date and PFT in ", path_tillage)
}

dt[, `:=`(
  parcel_id = as.character(parcel_id),
  min_date = as.Date(min_date),
  PFT = trimws(as.character(PFT))
)]
dt <- dt[!is.na(min_date)]
dt <- dt[nzchar(PFT) & !is.na(PFT)]

if (dedupe) {
  if ("OGMn_date" %in% names(dt)) {
    dt[, OGMn_date := as.Date(OGMn_date)]
    setorder(dt, parcel_id, OGMn_date, min_date)
  } else {
    setorder(dt, parcel_id, min_date)
  }
  n0 <- nrow(dt)
  dt <- unique(dt, by = "parcel_id")
  message("[tillage hist] Deduplicated by parcel: ", nrow(dt), " row(s) (from ", n0, ")")
} else {
  message("[tillage hist] All events: ", nrow(dt), " row(s)")
}

dt[, tillage_doy := as.integer(strftime(min_date, "%j"))]
dt[, tillage_month := as.integer(strftime(min_date, "%m"))]

binwidth <- suppressWarnings(as.numeric(Sys.getenv("BINWIDTH_DOY", "14")))
if (!is.finite(binwidth) || binwidth <= 0) {
  binwidth <- 14
}

out_dir <- Sys.getenv(
  "OUT_DIR",
  file.path(path_management, "figures", "tillage_histograms")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

slug <- if (dedupe) "deduped_parcel" else "all_events"
base <- file.path(
  out_dir,
  sprintf("tillage_timing_year=%d_%s", map_year, slug)
)

# --- DOY ---
p_doy <- ggplot2::ggplot(dt, ggplot2::aes(x = tillage_doy)) +
  ggplot2::geom_histogram(
    binwidth = binwidth,
    boundary = 0.5,
    closed = "left",
    fill = "steelblue4",
    color = "white",
    linewidth = 0.2
  ) +
  ggplot2::facet_wrap(~PFT, scales = "free_y", ncol = 2L) +
  ggplot2::labs(
    title = sprintf("Tillage event timing by PFT (%d)", map_year),
    subtitle = sprintf(
      "Day of year of min_date (NDTI min); bin width = %.0f d | %s | n = %s",
      binwidth,
      basename(path_tillage),
      format(nrow(dt), big.mark = ",")
    ),
    x = "Day of year",
    y = "Count"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, 400, by = 30), limits = c(0, 370)) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey92"),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9, lineheight = 1.2)
  )

path_doy <- paste0(base, "_metric=doy.png")
ggplot2::ggsave(path_doy, p_doy, width = 9, height = 7, dpi = 150, bg = "white")
message("Wrote ", path_doy)

# --- Calendar month (1–12) ---
p_mo <- ggplot2::ggplot(dt, ggplot2::aes(x = factor(tillage_month, levels = 1:12))) +
  ggplot2::geom_bar(fill = "darkseagreen4", color = "white", linewidth = 0.2) +
  ggplot2::facet_wrap(~PFT, scales = "free_y", ncol = 2L) +
  ggplot2::labs(
    title = sprintf("Tillage events by calendar month and PFT (%d)", map_year),
    subtitle = sprintf("Month of min_date | %s", basename(path_tillage)),
    x = "Month",
    y = "Count"
  ) +
  ggplot2::scale_x_discrete(
    labels = c(
      "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
      "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug",
      "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"
    )
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey92"),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 9)
  )

path_mo <- paste0(base, "_metric=month.png")
ggplot2::ggsave(path_mo, p_mo, width = 9, height = 7, dpi = 150, bg = "white")
message("Wrote ", path_mo)
