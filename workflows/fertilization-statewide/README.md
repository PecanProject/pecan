# Statewide fertilization events workflow

Builds an ensemble of synthetic N fertilization events for California ag parcels across 2016 and 2018 to 2023.

Run from PEcAn project root. Pick a profile (default, small, medium, all) with `FERT_PROJECT`:

```
FERT_PROJECT=default bash workflows/fertilization-statewide/run-statewide.sh
```

The three scripts run in order. `01-build-parcel-design.R` reads the LandIQ MSLSP matched product for each configured year, walks the Crop_types crosswalk to map each LandIQ CLASS+SUBCLASS code onto a per crop N rate envelope from `PEcAn.data.land::ca_n_application_rate`, and saves the resulting design table. Cycles whose code does not resolve to a rate get logged and dropped. `02-sample-n-rates.R` crosses the design with the ensemble dimension and draws annual N uniformly from each cycle's min/max envelope. `03-write-parquet.R` converts lb/acre to kg/m^2, splits between nh4 and no3 by the configured `nh4_no3_ratio`, partitions parcels into batches, and writes one parquet per batch named `<pid_min>_<pid_max>.parquet`. Compression is ZSTD when available, snappy otherwise.

Output lands at `/projectnb/dietzelab/ccmmf/usr/akash/event_files/fertilization/`. Intermediate `.rds` files live under `_staging/` next to the output and can be deleted after a successful run. `check-result.R` opens the dataset and prints schema, shard count, rows per ensemble member, rows per year, a sample of rows, and the total N range. `push-to-carb.sh` is the eventual `aws s3 sync` to `s3://carb/management/fertilization/v1.0/`.

The output columns are `parcel_id` (int), `ens_id` (string `fert_ens_NNN`), `date` (MSLSP `mslsp_OGI`), `nh4_n_kg_m2`, `no3_n_kg_m2`, `org_c_kg_m2` (zero here, compost lives in the ncc product), `org_n_kg_m2` (zero), `fert_subtype` (`"synthetic"`), `crop_code` (LandIQ CLASS+SUBCLASS), `PFT`. The rename to the schema the JSON converter consumes (`site_id`, `event_member_id`, hive partitioned by ensemble) happens in the downstream cleaner under `workflows/preprocess-event-parquet/`.
