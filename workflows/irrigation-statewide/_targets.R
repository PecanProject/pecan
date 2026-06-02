#!/usr/bin/env Rscript

library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

root_dir <- here::here("workflows/irrigation-statewide")
logdir <- file.path(root_dir, "_logs")
dir.create(logdir, showWarnings = FALSE, recursive = TRUE)

targets_config <- Sys.getenv("TAR_CONFIG", file.path(root_dir, "_targets.yaml"))
Sys.setenv(TAR_CONFIG = targets_config)

project <- Sys.getenv("TAR_PROJECT", "small")
config_base <- config::get(
  file = file.path(root_dir, "config.yml"),
  config = project
)
config_paths <- config::get(
  file = file.path(root_dir, "config_paths.yml"),
  config = Sys.getenv("IRRIGATION_PATHS_CONFIG", "default")
)
config <- config::merge(config_base, config_paths)

n_parcels <- config[["n_parcels"]]
batch_size <- config[["batch_size"]]
n_remote_workers <- config[["n_remote_workers"]]
n_local_workers <- as.integer(Sys.getenv("NSLOTS", 1))
exec_type <- config[["exec_type"]]
stopifnot(exec_type %in% c("cluster", "local"))
event_filename <- config[["event_filename"]]
n_irr_ensemble <- config[["n_irr_ensemble"]]

message(glue::glue(
  "PROJECT: {project}\n",
  "Running {n_parcels} parcels in batches of {batch_size} parcels each.\n",
  "Execution type: {exec_type} with ",
  if (exec_type == "local") {
    "{n_local_workers} workers.\n"
  } else {
    "{n_remote_workers} workers.\n"
  },
  "Output will be saved to ",
  "{file.path(config[['event_output_dir']], event_filename)}\n",
  "Targets output will be stored in ", tar_config_get("store")
))

ctrl_local <- crew_controller_local(
  name = "local",
  workers = n_local_workers
)

ctrl_sge <- crew_controller_sge(
  name = "sge",
  workers = n_remote_workers,
  # TLS causes weird allocator bugs. We're on an internal network, so no TLS is
  # probably fine.
  tls = crew::crew_tls(mode = "none"),
  options_cluster = crew_options_sge(
    log_output = logdir,
    script_lines = c(
      # Activate pixi
      'eval "$(pixi shell-hook -s bash)"',
      # Diagnostics
      "echo 'PIXI environment:'",
      "env | grep PIXI",
      "echo 'R .libPaths():'",
      "Rscript -e '.libPaths()'",
      # prevent arrow parallelism
      "export OMP_NUM_THREADS=1"
    )
  )
)

res_local <- tar_resources(
  crew = tar_resources_crew(controller = "local")
)
res_sge <- tar_resources(
  crew = tar_resources_crew(controller = "sge")
)

res_default <- if (exec_type == "local") {
  message("Running locally")
  res_local
} else if (exec_type == "cluster") {
  message("Running via SGE cluster")
  res_sge
} else {
  stop("Unknown exec_type ", shQuote(exec_type))
}

tar_option_set(
  controller = crew_controller_group(ctrl_local, ctrl_sge),
  resources = res_default,
  packages = c("ggplot2", "rlang", "PEcAn.data.land"),
  imports = c("PEcAn.data.land")
)

if (exec_type == "cluster") {
  tar_option_set(storage = "worker", retrieval = "worker")
}

tar_source(file.path(root_dir, "R"))

list(
  tar_target(crops_path, path.expand(config[["crops_path"]])),
  tar_target(mslsp_path, path.expand(config[["mslsp_path"]])),
  tar_target(cimis_etref_path, path.expand(config[["cimis_etref_path"]])),
  tar_target(chirps_precip_path, path.expand(config[["chirps_precip_path"]])),
  tar_target(ssurgo_weights_path, path.expand(config[["ssurgo_weights_path"]])),
  tar_target(ssurgo_gdb_path, path.expand(config[["ssurgo_gdb_path"]])),

  tar_target(event_output_dir, path.expand(config[["event_output_dir"]])),

  tar_target(validated_paths, {
    stopifnot(
      file.exists(crops_path),
      dir.exists(mslsp_path),
      length(list.files(mslsp_path, "\\.parquet")) == 7,
      dir.exists(cimis_etref_path),
      dir.exists(chirps_precip_path),
      file.exists(ssurgo_weights_path),
      dir.exists(ssurgo_gdb_path)
    )
    dir.create(event_output_dir, showWarnings = FALSE, recursive = TRUE)
    TRUE
  }),

  tar_target(parcel_ids, get_parcel_ids(crops_path, n_parcels)),

  tar_target(
    parcel_id_batches,
    split_into_batches(parcel_ids, batch_size),
    iteration = "list"
  ),

  tar_target(
    phenology,
    get_phenology(mslsp_path, parcel_id_batches),
    pattern = map(parcel_id_batches),
    format = "parquet"
  ),

  tar_target(
    etref,
    get_etref(cimis_etref_path, parcel_id_batches),
    pattern = map(parcel_id_batches),
    format = "parquet"
  ),

  tar_target(
    precip,
    get_precip(chirps_precip_path, parcel_id_batches),
    pattern = map(parcel_id_batches),
    format = "parquet"
  ),

  tar_target(
    crop_info,
    get_crop_info(crops_path, parcel_id_batches),
    pattern = map(parcel_id_batches),
    format = "parquet"
  ),

  tar_target(
    crops_with_soil,
    add_soil_awc(crop_info, ssurgo_weights_path, ssurgo_gdb_path),
    pattern = map(crop_info),
    format = "parquet"
  ),

  tar_target(
    complete_crop_timeseries,
    make_crop_timeseries(crops_with_soil, phenology, precip, etref),
    pattern = map(crops_with_soil, phenology, precip, etref),
    format = "parquet"
  ),

  tar_target(
    parcel_waterbalance,
    apply_water_balance(complete_crop_timeseries, "parcel_id"),
    pattern = map(complete_crop_timeseries),
    format = "parquet"
  ),

  tar_target(
    irr_events_df,
    make_event_df_parquet(
      file.path(event_output_dir, event_filename),
      parcel_waterbalance,
      n_ensemble = n_irr_ensemble,
      frac_uncertainty = 0.1
    ),
    pattern = map(parcel_waterbalance),
    format = "file"
  ),

  NULL
)
