# Generating `events.json` ensembles from management pipeline

This converts the raw outputs of the management/monitoring CCMMF pipeline into precisely structured parquet files. Then, it passes those inputs into the `events_parquet_to_json` function to generate ensembles of `events.json` files for downstream simulations.

- `01a-clean-irrigation.R` -- Preprocess irrigation. This is handled separately because the raw data are really large (600M rows).
- `01b-clean-other-events.R` -- Preprocess remaining events. These are done together because they are much smaller.
- `01c-clean-fertilization.R` -- Preprocess fertilization events. Reads both `workflows/fertilization-statewide/` (synthetic N) and `workflows/ncc-statewide/` (compost) raw outputs and writes them as one `fertilization.parquet` since SIPNET's FERT handler accumulates the org and mineral channels on same event_type
- `02-events-to-json.R` -- Example of running
