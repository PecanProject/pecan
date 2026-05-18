# Statewide NCC (compost) events workflow

Builds an ensemble of compost amendment events for California ag parcels across 2016 and 2018 to 2023. Implements the monitoring framework's three nested conditional draws: probability of amendment per parcel year, then date conditional on amendment, then amount + material conditional on amendment. 2017 is skipped because LandIQ has no statewide survey for that year.

Run from the PEcAn project root. Pick a profile (default, small, medium, all) with `NCC_PROJECT`:

```
NCC_PROJECT=default bash workflows/ncc-statewide/run-statewide.sh
```

The three scripts run in order. `01-build-parcel-design.R` reads the LandIQ MSLSP matched product for each configured year, classifies each PFT into the annual or perennial family (compost rate depends on family, not crop), and saves the design table. Cycles whose PFT is outside that set get logged and dropped. `02-sample-ncc-events.R` expands the design across the ensemble dimension, runs the Bernoulli probability draw, and for cycles that fire pulls material, application rate, %C, C:N, and a date offset by calling the bundled samplers in `PEcAn.data.land::sample_ca_compost_*`. `03-write-parquet.R` computes `org_c_kg_m2 = app_rate * 0.2241702 * (pct_c / 100)` and `org_n_kg_m2 = org_c / cn_ratio`, partitions parcels into batches, and writes one parquet per batch named `<pid_min>_<pid_max>.parquet`. Compression is ZSTD when available, snappy otherwise.

Output lands at `/projectnb/dietzelab/ccmmf/usr/akash/event_files/ncc/`. Intermediate `.rds` files live under `_staging/` next to the output. `check-result.R` opens the dataset and prints schema, shard count, rows per ensemble member, rows per material, rows per year, a sample of rows, and the org_c / org_n ranges. `push-to-carb.sh` is the eventual `aws s3 sync` to `s3://carb/management/ncc/v1.0/`.

The output columns are `parcel_id` (int), `ens_id` (string `ncc_ens_NNN`), `date` (sampled within the per family calendar window), `material` (one of the CalRecycle classes: green, food, ag, yard), `org_c_kg_m2`, `org_n_kg_m2`, `nh4_n_kg_m2` (zero, compost N mineralizes through the soil pool in SIPNET), `no3_n_kg_m2` (zero), `ncc_subtype` (`"compost"`), `crop_code`, `PFT`. Only cycle ensemble rows where the probability draw fires get emitted, so the row count varies by `p_apply_default` in `config.yml`.

The bundled sampling distributions and per family lookups live in `PEcAn.data.land` (tables `ca_compost_pct_c_distribution`, `ca_compost_cn_distribution`, `ca_compost_app_rate_envelope`, `ca_compost_calendar_window`, `ca_compost_material_whitelist`) so the workflow only carries the probability knob in its own config. Defaults: %C 15 to 30 from the CDFA and CalRecycle characterizations of CA finished compost, C:N 8 to 25 from the CDFA HSP white paper, rates from CDFA HSP Table 2 plus NRCS CPS 336, materials from 14 CCR section 17852, calendar windows from UC ANR practice guidance.
