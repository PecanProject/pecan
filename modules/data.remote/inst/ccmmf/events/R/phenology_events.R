# Phenology events: leaf-on / leaf-off dates from MSLSP 50PCGI / 50PCGD
# (including gap-filled values on the overlay).
# Includes young woody (YP): planting/harvest are not emitted for those stands.

build_phenology_events <- function(matched, year, out_dir) {
  message("[phenology] Building events")
  # Idle/fallow (PFT other) is not a crop phenology management event
  if ("landiq_PFT" %in% names(matched)) {
    is_other <- tolower(trimws(as.character(matched$landiq_PFT))) == "other"
    n_skip_other <- sum(is_other, na.rm = TRUE)
    if (n_skip_other > 0L) {
      matched <- matched[!is_other]
      message("  Skipped phenology (PFT other / idle-fallow): ", n_skip_other)
    }
  }
  # Prefer calendar year on the row when Peak is missing / gap-filled oddly
  # (YP / young woody are included here; planting and harvest skip them.)
  yr_fallback <- as.integer(year)
  pheno <- matched[, .(
    site_id = parcel_id,
    year = {
      yp <- suppressWarnings(lubridate::year(as.Date(mslsp_Peak)))
      data.table::fifelse(is.na(yp), yr_fallback, as.integer(yp))
    },
    leafonday = as.character(mslsp_50PCGI),
    leafoffday = as.character(mslsp_50PCGD),
    assigned_by = as.character(assigned_by),
    gapfill_date_source = as.character(gapfill_date_source)
  )]
  pheno <- pheno[
    !is.na(leafonday) & !is.na(leafoffday) &
      leafonday != "NA" & leafoffday != "NA" &
      nzchar(leafonday) & nzchar(leafoffday)
  ]
  message("  Phenology rows: ", nrow(pheno))
  data.table::setorder(pheno, site_id, year)
  pheno[, event_type := "phenology"]
  data.table::setcolorder(pheno, c("event_type", setdiff(names(pheno), "event_type")))

  write_event_outputs(
    pheno, out_dir, "phenology", year,
    json_builder = function(rows, i) {
      list(
        event_type = rows$event_type[i],
        year = rows$year[i],
        leafonday = rows$leafonday[i],
        leafoffday = rows$leafoffday[i],
        assigned_by = rows$assigned_by[i],
        gapfill_date_source = rows$gapfill_date_source[i]
      )
    }
  )
  invisible(pheno)
}
