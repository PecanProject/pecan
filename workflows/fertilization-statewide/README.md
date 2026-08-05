# Statewide fertilization events workflow

Builds an ensemble of synthetic N fertilization events for California ag parcels in the harmonized CADWR Land Use crop map whose crop code resolves to an N rate, for 2016 and 2018 to 2023. 2017 is skipped because no statewide survey was flown that year. N rate envelopes come from `PEcAn.data.land::ca_n_application_rate`.

Source: California Department of Water Resources. (2016-2023). Statewide Crop Mapping. California Natural Resources Agency Open Data. https://data.cnra.ca.gov/dataset/statewide-crop-mapping

# Config

Configuration parameters live in `config.yml`. Most setups only need:

- `crops_path`: the harmonized CADWR Land Use crops parquet
- `phen_dir`: the gap-filled phenology (green-up) directory
- `crosswalk_path`: the CADWR to FREP to UC ANR crop name crosswalk TSV
- `output_dir`: output directory for parquet shards
- `n_parcels`, `n_ensemble`, `batch_size`, `workers`: settings per profile
- `nh4_fraction`: share of total synthetic N going to ammonium; the rest goes to nitrate (default 0.5 for a 50/50 split)

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
| mineral N split | fixed, `nh4_fraction` of total N to ammonium and the remainder to nitrate, default 0.5 | project assumption; the guidelines report total N only. SIPNET sums the two into a single mineral pool, so the split affects the event record rather than the simulation |

Events carry no organic C or N: these are synthetic mineral fertilizer applications.

# Known limitations

- The phenology product has no season key, so crop cycles are matched to green-ups by
  rank: the nth cycle of a parcel-year takes the nth green-up. From 2018 on the product
  carries a second green-up for most double-crop parcels, so this resolves the majority
  of them. Where a parcel-year has fewer green-ups than cycles, the later cycles reuse
  the last available green-up. 2016 is the exception, carrying one green-up per parcel
  year, so its multi-season cycles all share an anchor.
- The event date is the green-up date itself; this workflow applies no offset. Dates
  outside the configured crop years occur because the phenology product itself assigns
  some green-ups to the previous calendar year.
- Only crop codes present in the crosswalk resolve to an N rate envelope. Cycles whose
  code does not resolve are dropped and reported at run time.

# Output

Parcel range sharded parquet at `<output_dir>/`. Columns: `parcel_id`, `ens_id` (`ens_NNN`, shared with ncc workflow), `date`, `nh4_n_kg_m2`, `no3_n_kg_m2`, `org_c_kg_m2` (zero), `org_n_kg_m2` (zero), `crop_code`.

Downstream, `workflows/preprocess-event-parquet/01c-clean-fertilization.R` renames `parcel_id` to `site_id` and `ens_id` to `event_member_id`, unions compost rows from `workflows/ncc-statewide/`, and writes JSON converter input.
