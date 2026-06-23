# Generating `events.json` ensembles from management pipeline

This converts the raw outputs of the management/monitoring CCMMF pipeline into precisely structured parquet files. Then, it passes those inputs into the new `events_parquet_to_json` functionality to generate ensembles of `events.json` files that can be used for downstream simulations.

- `01a-clean-irrigation.R` --- Preprocess irrigation. This is handled separately because the raw data are really large (600M rows).
- `01b-clean-other-events.R` --- Preprocess remaining events. These are done together because they are much smaller.
- `02-events-to-json.R` --- Example of running
