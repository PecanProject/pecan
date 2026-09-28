# Phenology events: SIPNET leafon / leafoff from MSLSP 50PCGI / 50PCGD
# (including gap-filled values). Perennials only (hay, woody). Annuals
# (row, rice) use planting / harvest dates, not leaf-on / leaf-off.

.phenology_event_cols <- c("event_type", "site_id", "date")

build_phenology_events <- function(matched, year, out_dir) {
  message("[phenology] Building events")
  if ("landiq_PFT" %in% names(matched)) {
    pft_l <- tolower(trimws(as.character(matched$landiq_PFT)))
    keep <- pft_l %in% c("hay", "woody")
    n_skip <- sum(!keep, na.rm = TRUE)
    if (n_skip > 0L) {
      matched <- matched[keep]
      message(
        "  Skipped phenology (not hay/woody; annuals and idle): ", n_skip
      )
    }
  }

  date_ok <- function(x) {
    s <- as.character(x)
    !is.na(s) & nzchar(s) & s != "NA" & s != "NaT"
  }

  leafon <- matched[date_ok(mslsp_50PCGI), .(
    event_type = "leafon",
    site_id = as.character(parcel_id),
    date = as.character(mslsp_50PCGI)
  )]
  leafoff <- matched[date_ok(mslsp_50PCGD), .(
    event_type = "leafoff",
    site_id = as.character(parcel_id),
    date = as.character(mslsp_50PCGD)
  )]
  pheno <- data.table::rbindlist(list(leafon, leafoff), use.names = TRUE)
  pheno <- keep_event_columns(pheno, .phenology_event_cols)
  message("  Phenology rows: ", nrow(pheno),
          " (leafon=", nrow(leafon), ", leafoff=", nrow(leafoff), ")")
  if (nrow(pheno)) {
    data.table::setorder(pheno, site_id, date, event_type)
  }

  write_event_outputs(
    pheno, out_dir, "phenology", year,
    json_builder = function(rows, i) {
      list(
        event_type = rows$event_type[i],
        date = rows$date[i]
      )
    }
  )
  invisible(pheno)
}
