## TODO:
## - integrate call into write.configs.SIPNET
## - parameterize planting allocation fractions
## - make sure files are written in correct output directory
## - map crops associated w/ planting and harvest --> PFTs; this will need to be handled separate from events.in
#' Write SIPNET events.in files from a PEcAn events.json
#'
#' Reads a single PEcAn events.json containing one or more site objects and
#' writes one SIPNET `events.in` file per site. Events are translated according to [SIPNET's `events.in`
#' specification](https://pecanproject.github.io/sipnet/parameters/#agronomic-events).
#' The writer expects inputs to already match the PEcAn MVP schema v0.1.0 naming and units where applicable.
#'
#' @details
#' - Supported `event_type` values: `tillage`, `planting`, `fertilization`,
#'   `irrigation`, `harvest`.
#' - Units translated from PEcAn standard_vars to SIPNET events.in specification:
#'   `kg/m^2` to `g/m^2`; irrigation `amount_mm` to `cm`.
#' - Planting allocation uses fixed internal parameters. Future work should use the same values
#'   that are written to `sipnet.parms` (e.g. after integrating this into `write.configs.SIPNET`)
#'
#' @param events_json character. Path to an `events.json` file containing an
#'   array of site objects with `site_id`, optional `pft`, and `events`.
#' @param outdir character. Output directory where per-site `events-<site>.in`
#'   files are written.
#'
#' @return Invisibly, a vector of files written.
#'
#' @examples
#' # Example with two events for a single site
#' tmp <- withr::local_tempfile(fileext = ".json")
#' site <- list(
#'     site_id = "EX1",
#'     events = list(
#'         list(event_type = "tillage", date = "2022-02-04", tillage_eff_0to1 = 0.2),
#'         list(event_type = "planting", date = "2022-02-19", leaf_c_kg_m2 = 0.01)
#'     )
#' )
#' jsonlite::write_json(list(site), tmp, auto_unbox = TRUE)
#' outdir <- withr::local_tempdir()
#' files <- write.events.SIPNET(tmp, outdir)
#' files
#'
#' @importFrom rlang %||%
#' @export
write.events.SIPNET <- function(events_json, outdir) {
    # Validate input JSON against PEcAn events schema
    PEcAn.data.land::validate_events_json(events_json)

    # TODO add overwrite argument
    x <- jsonlite::fromJSON(events_json, simplifyVector = FALSE)
    # allow a single site events.json that does not have a site_id
    site_objs <- if (!is.null(x$site_id)) list(x) else x
    files_written <- vector()

    leafAllocation <- 0.50
    woodAllocation <- 0.15
    fineRootAllocation <- 0.10
    coarseRootAllocation <- 0.25

    # Unit conversion helpers
    kg2g <- as.numeric(PEcAn.utils::ud_convert(1, "kg", "g")) # 1000
    mm2cm <- as.numeric(PEcAn.utils::ud_convert(1, "mm", "cm")) # 0.1

    # For each site, build event time series and write file
    for (site in site_objs) {
        sid <- site$site_id
        evs <- site$events
        # Order by date and build lines
        dates <- as.Date(vapply(evs, function(e) as.character(e$date), character(1)))
        ord <- order(dates)
        lines <- character()
        for (e in evs[ord]) {
            d <- as.Date(e$date)
            year <- as.integer(format(d, "%Y"))
            day <- as.integer(format(d, "%j"))
            type <- e$event_type
            if (type == "tillage") {
                f <- if (is.null(e$tillage_eff_0to1)) 0 else e$tillage_eff_0to1
                # TODO: consider validating up front against schema rather than here
                lines <- c(lines, sprintf("%d  %d  till  %s", year, day, f))
            } else if (type == "planting") {
                # infer total planted biomass from leaf pool and allocation fraction
                leaf_g <- as.numeric(if (is.null(e$leaf_c_kg_m2)) 0 else e$leaf_c_kg_m2) * kg2g
                total_g <- if (leafAllocation > 0) leaf_g / leafAllocation else leaf_g
                wood_g <- woodAllocation * total_g
                fr_g <- fineRootAllocation * total_g
                cr_g <- coarseRootAllocation * total_g
                lines <- c(lines, sprintf("%d  %d  plant  %s %s %s %s", year, day, leaf_g, wood_g, fr_g, cr_g))
            } else if (type == "fertilization") {
                orgN_g <- as.numeric(if (is.null(e$org_n_kg_m2)) 0 else e$org_n_kg_m2) * kg2g
                orgC_g <- as.numeric(if (is.null(e$org_c_kg_m2)) 0 else e$org_c_kg_m2) * kg2g
                nh4_g <- as.numeric(if (is.null(e$nh4_n_kg_m2)) 0 else e$nh4_n_kg_m2) * kg2g
                no3_g <- as.numeric(if (is.null(e$no3_n_kg_m2)) 0 else e$no3_n_kg_m2) * kg2g
                minN_g <- nh4_g + no3_g
                lines <- c(lines, sprintf("%d  %d  fert   %s %s %s", year, day, orgN_g, orgC_g, minN_g))
            } else if (type == "irrigation") {
                amt_cm <- as.numeric(if (is.null(e$amount_mm)) 0 else e$amount_mm) * mm2cm
                method_code <- if (is.null(e$method) || e$method == "soil") 1 else 0
                lines <- c(lines, sprintf("%d  %d  irrig  %s %s", year, day, amt_cm, method_code))
            } else if (type == "harvest") {
                abv_rem <- e$frac_above_removed_0to1 %||% 0
                blw_rem <- e$frac_below_removed_0to1 %||% 0
                abv_lit <- e$frac_above_to_litter_0to1 %||% (1.0 - abv_rem)
                blw_lit <- e$frac_below_to_litter_0to1 %||% (1.0 - blw_rem)
                lines <- c(
                    lines,
                    sprintf(
                        "%d  %d harv   %s %s %s %s",
                        year, day,
                        abv_rem, blw_rem,
                        abv_lit, blw_lit
                    )
                )
            }
        }
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
        fp <- file.path(outdir, sprintf("events-%s.in", sid))
        writeLines(lines, fp)
        files_written <- c(files_written, fp)
    }
    invisible(files_written)
}
