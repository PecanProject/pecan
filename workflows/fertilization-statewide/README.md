# Statewide fertilization events workflow

Builds an ensemble of synthetic N fertilization events for every California ag parcel in the LandIQ MSLSP matched product, for 2016 and 2018 to 2023. 2017 is skipped because LandIQ did not run a statewide survey that year. N rate envelopes come from `PEcAn.data.land::ca_n_application_rate`.

# Config

Everything tweakable lives in `config.yml`. Most setups only need to look at:

- `matched_dir`: the LandIQ MSLSP matched product directory
- `crosswalk_path`: the LandIQ to FREP to UC ANR crop name crosswalk TSV
- `output_dir`: where the parquet shards go
- `n_parcels`, `n_ensemble`, `batch_size`, `workers`: scale knobs per profile
- `nh4_fraction`: share of total synthetic N going to ammonium; the rest goes to nitrate (default 0.5 for a 50/50 split)

# Run

Pick a profile (`default`, `medium`, `all`) and run from the PEcAn project root:

```
FERT_PROJECT=default bash workflows/fertilization-statewide/run-statewide.sh
```

`default` is 1000 random parcels, single threaded. `all` is full statewide (~660k parcels), eight workers. The three R scripts chain together: 01 builds the design, 02 samples rates, 03 writes parquet. `check-result.R` reads the output back and prints a summary.

# Output

Parcel range sharded parquet at `<output_dir>/`. Columns: `parcel_id`, `ens_id` (`ens_NNN`, shared with ncc workflow), `date`, `nh4_n_kg_m2`, `no3_n_kg_m2`, `org_c_kg_m2` (zero), `org_n_kg_m2` (zero), `crop_code`.

Downstream, `workflows/preprocess-event-parquet/01c-clean-fertilization.R` renames `parcel_id` to `site_id` and `ens_id` to `event_member_id`, unions compost rows from `workflows/ncc-statewide/`, and writes JSON converter input.
