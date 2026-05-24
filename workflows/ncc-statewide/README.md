# Statewide compost (NCC) events workflow

NCC is project shorthand for organic amendments like compost and manure. This workflow builds an ensemble of compost events for every California ag parcel in the LandIQ MSLSP matched product, for 2016 and 2018 to 2023. 2017 is skipped because LandIQ did not run a statewide survey that year.

Material properties (application rate, %N, C:N, PAN) come from `PEcAn.data.land::ca_compost_amendment`.

# Config

Tweakable bits in `config.yml`:

- `matched_dir`: where LandIQ MSLSP matched product lives
- `output_dir`: where parquet shards go
- `n_parcels`, `n_ensemble`, `batch_size`, `workers`: scale knobs per profile
- `p_apply_default`: probability of compost per parcel year per ensemble member (default 0.10)

# Run

Pick a profile (`default`, `medium`, `all`) and run from PEcAn project root:

```
NCC_PROJECT=default bash workflows/ncc-statewide/run-statewide.sh
```

01 builds a design table and tags each PFT as annual (row, hay, rice) or perennial (woody). 02 runs Bernoulli gate, picks a material per fired row from `ca_compost_amendment` (constrained to CalRecycle classes that fit PFT family), draws app rate and C:N from that material's envelope. 03 does unit conversion, splits N into mineral and organic using PAN (clamped to zero for high C:N materials that immobilize), and writes parcel range sharded parquet.

`check-result.R` reads output back and prints a summary.

# Output columns

- `parcel_id`, `ens_id` (`ens_NNN`, shared with fertilization workflow), `date`
- `nh4_n_kg_m2`: PAN release fraction. Zero when `pan_pct` is negative (high C:N materials immobilize N instead of releasing it)
- `no3_n_kg_m2`: zero. Compost releases ammonium, nitrification happens later in soil pool
- `org_c_kg_m2`: total organic C from application. Does not depend on PAN; C is C regardless of N fate
- `org_n_kg_m2`: leftover organic N after PAN split
- `crop_code`: passthrough
- `material`: diagnostic column for `check-result.R` only. Cleaner strips it before union so it never reaches SIPNET

Downstream, `workflows/preprocess-event-parquet/01c-clean-fertilization.R` unions these with synthetic N rows from fertilization workflow and writes one `fertilization.parquet`. SIPNET handles both under same FERT event type.

# Compost timing

Anchor is MSLSP green up date (`mslsp_OGI`). For annuals that's close to but not exactly planting date (emergence lags planting by a few days to weeks). For perennials it's bud break. Date offset is uniform within a family window:

- annuals (row, hay, rice): 14 to 180 days before green up
- perennials (woody): 30 to 210 days before green up

These are working assumptions consistent with broad direction in literature (fall application for perennials, pre plant for annuals).
