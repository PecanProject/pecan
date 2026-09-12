#' Copy final model state from all sites in a completed PEcAn workflow
#'
#' Finds the `restart.out` files in a PEcAn run directory, copies the last
#' restart from each run to the specified output directory,
#' and renames by site and ensemble ID.
#' This is useful when taking final state from one simulation as the starting
#' point for another one (e.g. forecasts that start from a current-observations
#' run).
#'
#' Paths look like either `ENS-00001-131976/restart.out` (in runs that are all
#' one segment) or `ENS-00004-131975/segments/segment_011/run/restart.out`
#' (in segmented runs).
#' Segmented runs will have a restart.out for each segment, in which case
#' we take the restart from the last segment in the run.
#' (Well, technically the one whose path sorts last, with our confidence that
#' this is also chonologically last coming from the segment numbering system).
#'
#' On copying, files are renamed by site ID and ensemble member. Segment number
#' is not retained. Thus the examples shown above would respectively become
#' `<dest_dir>/restart-131976-00001.out` and
#' `<dest_dir>.restart-131975-00004.out`.
#'
#' @param run_dir path to the run directory of a PEcAn workflow.
#'  This should be the directory listed in the workflow's `settings$rundir`;
#'  by convention this often ends in `output/run/`
#' @param dest_dir directory to which to copy one restart file per model run
#' @param overwrite logical: Replace existing destination files?
#'
#' @author Chris Black
#' @return Invisibly, a character vector of the filenames created in dest_dir.
#' @export
collect_restarts <- function(run_dir, dest_dir, overwrite = FALSE) {
  files <- run_dir |>
    list.files(
      pattern = "restart.out",
      recursive = TRUE,
      full.names = FALSE
    ) |>
    stringr::str_match(
      # captured groups: ens_num, site_id, segment_string
      # segment_string looks like "segments/segment_011/run/" if segmented,
      # just "/" if not.
      ".*ENS-(\\d+)-([^/]+)(/.*)restart.out"
    ) |>
    # first column of str_match result matrix is the whole path,
    # then one unnamed col per capture group
    as.data.frame() |>
    stats::setNames(nm = c("path", "ens_num", "site_id", "segment_string")) |>
    # Load-bearing assumption here: the segment string that sorts last is also
    # the segment that was run last and therefore contains the restart that has
    # truly end-of-run model state.
    dplyr::slice_max(
      .data$segment_string,
      by = c("site_id", "ens_num")
    ) |>
    dplyr::mutate(
      dest_file = file.path(
        .env$dest_dir,
        paste0("restart-", .data$site_id, "-", .data$ens_num, ".out")
      )
    )
  
  if (nrow(files) == 0) {
    PEcAn.logger::logger.error("No restarts found in run dir", run_dir)
    return(invisible(character()))
  }

  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  copy_status <- file.copy(
    from = file.path(run_dir, files$path),
    to = files$dest_file,
    overwrite = overwrite
  )
  if (!all(copy_status)) {
    PEcAn.logger::logger.error(
      "Not all files copied! failures:",
      toString(files$path[!copy_status])
    )
  }

  invisible(files$dest_file[copy_status])
}
