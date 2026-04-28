#' settings for whole run with many paths per input ->
#' settings for one ensemble member with one path per input
#'
#' @param settings single-site settings object (not Multisettings)
#' @param inputs named list of input indices (one row of the sample design)
subset_paths <- function(settings, path_nums) {
  for (input in names(path_nums)) {
    if (!is.list(settings$run$inputs[[input]])) {
      next
    }
    path_idx <- path_nums[[input]]
    all_paths <- settings$run$inputs[[input]]$path
    if (path_idx > length(all_paths)) {
      PEcAn.logger::logger.severe("No path at input ", sQuote(input), " index ", path_idx)
    }
    settings$run$inputs[[input]]$path <- all_paths[[path_idx]]
    # If we define a list of `source`s, also try to subset that (if the lengths
    # match). This is especially useful for processing events (because we store
    # the original JSON path in the `source`).
    if (!is.list(settings$run$inputs[[input]]$source)) {
      next
    }
    all_source_paths <- settings$run$inputs[[input]]$source
    n_source <- length(all_source_paths)
    n_path <- length(all_paths)
    if (n_source != n_path) {
      PEcAn.logger::logger.warn(sprintf(
        paste(
          "For input %s, number of paths (%d) ",
          "is not equal to number of sources (%d). ",
          "Assuming these are different things and therefore leaving sources as is."
        ),
        input, n_path, n_source
      ))
      next
    }
    settings$run$inputs[[input]]$source <- all_source_paths[[path_idx]]
  }
  settings
}

# TODO: We need a better, consistent implementation of this. However, this is
# OK as an example of a function that implements the capabilities needed for
# `write_segment_configs`.
crop2pft_example <- function(crop_code) {
  cls <- substr(crop_code, 1, 1)
  dplyr::case_when(
    crop_code == "P1" ~ "annual_crop",
    crop_code == "G2" ~ "annual_crop",
    cls == "D" ~ "temperate.deciduous",
    cls == "F" ~ "annual_crop",
    cls == "G" ~ "grass",
    cls == "P" ~ "grass",
    cls == "R" ~ "grass",
    cls == "T" ~ "annual_crop",
    is.na(crop_code) ~ "soil",
    TRUE ~ "UNKNOWN_PFT"
  )
}

write_segmented_configs.SIPNET <- function(settings, input_design = NULL, ...) {
  manifest_file <- file.path(settings$outdir, "runs_manifest.csv")
  if (!file.exists(manifest_file)) {
    PEcAn.logger::logger.severe("Could not find manifest file: ", manifest_file)
  }
  inputs_runs <- read.csv(manifest_file) |>
    dplyr::filter(.data$site_id == settings$run$site$id) |>
    # TODO the manifest should probably report these already...
    dplyr::mutate(
      ens_num = .data$run_id |>
        stringr::str_extract("ENS-(\\d+)", group = 1) |>
        as.integer()
    )
  if (!is.null(input_design)) {
    inputs_runs <- inputs_runs |>
      dplyr::left_join(
        input_design |> tibble::rowid_to_column("ens_num"),
        by = "ens_num",
        relationship = "many-to-one")
  }

  new_jobfiles <- character()

  for (i in seq_len(nrow(inputs_runs))) {
    new_jobfiles[[i]] <- write_segment_configs(settings, inputs_runs[i, ], ...)
  }
  invisible(new_jobfiles)
}

segment_dataframe <- function(run_settings) {
  events_json <- run_settings$run$inputs$events$source
  stopifnot(file.exists(events_json))

  crop_cycles <- PEcAn.data.land::events_to_crop_cycle_starts(events_json) |>
    dplyr::filter(.data$site_id == run_settings$run$site$id) |>
    dplyr::ungroup()

  run_start <- as.Date(run_settings$run$start.date)
  run_end <- as.Date(run_settings$run$end.date)

  # First, build segment data.frame just from the crop cycles. Note that we
  # already include the run end date (lead(..., default = run_end)).
  segments <- crop_cycles |>
    dplyr::rename(start_date = "date") |>
    dplyr::mutate(
      end_date = dplyr::lead(.data$start_date - 1, default = run_end),
    )

  # If we start *before* the first planting, add a segment for that.
  if (run_start < min(segments[["start_date"]])) {
    first_segment <- tibble::tibble(
      crop_cycle_id = 0,
      site_id = segments[["site_id"]][[1]],
      start_date = run_start,
      end_date = segments[["start_date"]][[1]] - 1,
      crop_code = NA_character_
    )
    segments <- dplyr::bind_rows(first_segment, segments)
  }

  # If the run start is *after* the first planting, clip to the start date.
  if (run_start > min(segments[["start_date"]])) {
    segments <- segments |>
      dplyr::filter(
        .data$start_date >= .env$run_start,
        # Also clip the end date to avoid edge cases where run_start == end_date
        .data$end_date > .env$run_start
      )
    if (nrow(segments) < 1) {
      PEcAn.logger::logger.severe(
        "Filtering resulted in no segments. ",
        "This is an invalid state; check settings and events.json."
      )
    }
    segments[1, "start_date"] <- run_start
  }

  segments |>
    dplyr::arrange(.data$start_date) |>
    dplyr::mutate(segment_id = sprintf("%03d", dplyr::row_number()))
}

write_segment_configs <- function(
  settings,
  run_row,
  crop2pft = crop2pft_example,
  replace_and_link = TRUE,
  force_rerun = FALSE
) {
  run_id <- run_row[["run_id"]]
  run_dir <- file.path(settings$rundir, run_id)
  run_modeloutdir <- file.path(settings$modeloutdir, run_id)
  run_settings <- subset_paths(settings, run_row)
  segment_rootdir <- file.path(run_dir, "segments")

  ens_samples_file <- file.path(
    run_settings$outdir,
    sprintf("ensemble.samples.%s.Rdata", run_settings$ensemble$ensemble.id)
  )
  stopifnot(file.exists(ens_samples_file))
  ensemble_samples <- PEcAn.utils::load_local(ens_samples_file)[["ens.samples"]]
  i_param <- run_row[["param"]] %||% 1
  run_traits <- lapply(ensemble_samples, \(dat) dat[i_param, ])

  segments <- segment_dataframe(run_settings) |>
    dplyr::mutate(
      pft = crop2pft(.data$crop_code),
      segment_dir = file.path(segment_rootdir, sprintf("segment_%s", .data$segment_id))
    )
  write.csv(segments, file = file.path(run_dir, "segments.csv"), row.names = FALSE)

  jobsh_files <- character()

  for (isegment in seq_len(nrow(segments))) {
    segment <- segments[isegment, ]
    dstart <- segment[["start_date"]]
    dend <- segment[["end_date"]]
    segment_dir <- segment[["segment_dir"]]

    if (force_rerun) {
      unlink(segment_dir, recursive = TRUE)
    }
    dir.create(segment_dir, showWarnings = FALSE, recursive = TRUE)

    runid_dummy <- "1"

    segment_inputs <- PEcAn.SIPNET::split_inputs.SIPNET(
      dstart,
      # NOTE: In split_inputs, end.time is *not* inclusive.
      # But `dend` *is* inclusive. So `+1` as a workaround here.
      (dend + 1),
      run_settings$run$inputs,
      overwrite = TRUE,
      outpath = segment_dir
    )

    # Segment-specific settings
    segment_outdir <- file.path(segment_dir, "out")
    dir.create(segment_outdir, showWarnings = FALSE, recursive = TRUE)
    segment_rundir <- file.path(segment_dir, "run")
    dir.create(segment_rundir, showWarnings = FALSE, recursive = TRUE)
    file.create(file.path(segment_rundir, "README.txt"))

    segment_rundir_withid <- file.path(segment_rundir, runid_dummy)
    dir.create(segment_rundir_withid, showWarnings = FALSE, recursive = TRUE)
    segment_outdir_withid <- file.path(segment_outdir, runid_dummy)
    dir.create(segment_outdir_withid, showWarnings = FALSE, recursive = TRUE)

    segment_settings <- run_settings
    segment_settings[["outdir"]] <- segment_outdir
    segment_settings[["modeloutdir"]] <- segment_outdir
    segment_settings[["rundir"]] <- segment_rundir
    segment_settings[[c("run", "start.date")]] <- dstart
    segment_settings[[c("run", "end.date")]] <- dend
    segment_settings[[c("run", "inputs")]] <- segment_inputs
    if (is.null(segment_settings[[c("model", "options")]])) {
      segment_settings[[c("model", "options")]] <- list()
    }

    if (isegment > 1) {
      # For isegment > 1, we restart from the *previous* segment's restart.out
      segment_settings[[c("model", "options", "RESTART_IN")]] <- restart_out
    }
    # ...and now, define a new restart.out for *this* segment
    restart_out <- file.path(segment_rundir, "restart.out")
    segment_settings[[c("model", "options", "RESTART_OUT")]] <- restart_out

    # trait.values must be a list of pfts, even though SIPNET takes only one
    # PFT as input. However, we can pass soil params through a "soil" PFT.
    # Here, if the segment PFT is soil, that's what we send. If the segment PFT
    # is anything else, we send that *and* the soil params.
    choose_pft <- unique(c(segment[["pft"]], "soil"))
    segment_traits <- run_traits[choose_pft]

    # Write dummy runs file
    writeLines(runid_dummy, file.path(segment_rundir, "runs.txt"))

    PEcAn.SIPNET::write.config.SIPNET(
      defaults = segment_settings[["pfts"]],
      trait.values = segment_traits,
      settings = segment_settings,
      run.id = runid_dummy
    )

    segment_jobsh <- file.path(segment_settings$rundir, runid_dummy, "job.sh")
    stopifnot(file.exists(segment_jobsh))
    jobsh_files <- c(jobsh_files, segment_jobsh)
  }

  # Now, get the run's jobsh file
  run_jobsh <- file.path(run_dir, "job.sh")
  target_sipnet_out <- file.path(run_modeloutdir, "sipnet.out")
  segmented_jobsh_file <- file.path(run_dir, "job_segmented.sh")
  segmented_jobsh_lines <- c(
    "#!/usr/bin/env bash",
    "",
    "# Redirect output",
    "exec 3>&1",
    paste("exec &>", shQuote(file.path(run_modeloutdir, "logfile.txt"))),
    "",
    "# Run model segments",
    paste("bash", jobsh_files),
    "",
    "# Concatenate sipnet out files",
    sprintf(
      "Rscript -e \"PEcAn.SIPNET::combine_sipnet_out(directory = %s, outfile = %s)\"",
      shQuote(segment_rootdir),
      shQuote(target_sipnet_out)
    ),
    "",
    "# Convert output to PEcAn standard",
    sprintf(
      "Rscript -e \"PEcAn.SIPNET::model2netcdf.SIPNET(%s)\"",
      paste(
        sprintf("outdir = %s", shQuote(run_modeloutdir)),
        sprintf("sitelat = %s", as.character(settings$run$site$lat)),
        sprintf("sitelon = %s", as.character(settings$run$site$lon)),
        sprintf("start_date = %s", shQuote(settings$run$start.date)),
        sprintf("end_date = %s", shQuote(settings$run$end.date)),
        sprintf("revision = %s", shQuote(settings$model$revision)),
        sep = ", "
      )
    )
  )
  writeLines(segmented_jobsh_lines, segmented_jobsh_file)
  if (replace_and_link) {
    run_jobsh_backup <- file.path(run_dir, "job_original.sh")
    file.rename(run_jobsh, run_jobsh_backup)
    file.symlink(segmented_jobsh_file, run_jobsh)
    Sys.chmod(run_jobsh, mode = "0755")
  }
  invisible(segmented_jobsh_file)
}
