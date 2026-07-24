# Phenology events: leaf-on / leaf-off dates from matched MSLSP columns.

build_phenology_events <- function(matched, year, out_dir) {
  message("[phenology] Building events")
  if ("assigned_by" %in% names(matched)) {
    matched <- matched[assigned_by == "matched"]
  }
  pheno <- matched[, .(
    site_id = parcel_id,
    year = lubridate::year(mslsp_Peak),
    leafonday = as.character(mslsp_50PCGI),
    leafoffday = as.character(mslsp_50PCGD)
  )]
  pheno <- pheno[!is.na(leafonday) & !is.na(leafoffday) & leafonday != "NA" & leafoffday != "NA"]
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
        leafoffday = rows$leafoffday[i]
      )
    }
  )
  invisible(pheno)
}
