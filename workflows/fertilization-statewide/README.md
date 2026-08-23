# Statewide fertilization events workflow

Builds an ensemble of synthetic N fertilization events for California ag parcels in the harmonized CADWR Land Use crop map whose crop code resolves to an N rate, for 2016 and 2018 to 2023. 2017 is skipped because no statewide survey was flown that year. N rate envelopes come from `PEcAn.data.land::ca_n_application_rate`.

Source: California Department of Water Resources. (2016-2023). Statewide Crop Mapping. California Natural Resources Agency Open Data. https://data.cnra.ca.gov/dataset/statewide-crop-mapping

# Config

Configuration parameters live in `config.yml`. Most setups only need:

- `crops_path`: the harmonized CADWR Land Use crops parquet. Override with `CCMMF_CROPS_PATH`
- `phen_dir`: the gap-filled LandIQ to MSLSP match directory, the same product the ncc workflow reads. Override with `CCMMF_PHEN_DIR`
- `phen_glob`: file glob under `phen_dir` (default `assigned_year=*_gapfilled.parquet`). Override with `CCMMF_PHEN_GLOB`
- `phen_anchor_col`: phenology transition used as the anchor (default `mslsp_50PCGI`, leaf-on). Override with `CCMMF_PHEN_ANCHOR_COL`
- `crosswalk_path`: the CADWR to FREP to UC ANR crop name crosswalk TSV, versioned in this folder
- `output_dir`: output directory for parquet shards
- `n_parcels`, `n_ensemble`, `batch_size`, `workers`: settings per profile
- `nh4_fraction`: share of total synthetic N going to ammonium; the rest goes to nitrate (default 1, all ammonium)

# Run

Select a profile (`default`, `medium`, `all`) and run from the PEcAn project root:

```
FERT_PROJECT=default bash workflows/fertilization-statewide/run-statewide.sh
```

`default` is 1000 random parcels, single threaded. `all` is full statewide (~660k parcels), eight workers. The three R scripts chain together: 01 builds the design, 02 samples rates, 03 writes parquet. `check-result.R` reads the output back and prints a summary.

# Sampling design

| quantity | distribution | basis |
|---|---|---|
| annual N rate | uniform on `[min_n_lbs_acre, max_n_lbs_acre]` for the crop | `ca_n_application_rate` reports a guideline envelope, not a distribution. With no information beyond the bounds, uniform is the maximum entropy choice and avoids implying a central tendency the guidelines do not give |
| mineral N split | fixed, `nh4_fraction` of total N to ammonium and the remainder to nitrate, default 1 | SIPNET sums the two into a single mineral pool before the model sees them, so the split is invisible to the simulation. Defaulting to all ammonium avoids asserting a split we cannot justify without knowing the material mix |

Events carry no organic C or N: these are synthetic mineral fertilizer applications.

# Known limitations

- A crop cycle with no matched phenology row gets no anchor and is dropped. About 70%
  of crop cycles carry a matched row.
- The event date is the anchor itself; this workflow applies no offset. Dates outside
  the configured crop years occur because the phenology product itself assigns some
  transitions to an adjacent calendar year.
- `ca_n_application_rate` is an annual total per crop, so the whole season's N budget
  is applied as one event rather than split across pre-plant, side-dress and
  fertigation. The date is therefore a single anchored placeholder for a schedule, not
  a measured application date.
- Only crop codes present in the crosswalk resolve to an N rate envelope. Cycles whose
  code does not resolve are dropped and reported at run time.

# Output

Parcel range sharded parquet at `<output_dir>/`. Columns: `parcel_id`, `ens_id` (`ens_NNN`, shared with ncc workflow), `date`, `nh4_n_kg_m2`, `no3_n_kg_m2`, `org_c_kg_m2` (zero), `org_n_kg_m2` (zero), `crop_code`.

Downstream, `workflows/preprocess-event-parquet/01c-clean-fertilization.R` renames `parcel_id` to `site_id` and `ens_id` to `event_member_id`, unions compost rows from `workflows/ncc-statewide/`, and writes JSON converter input.
