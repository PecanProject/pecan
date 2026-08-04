# Statewide compost (NCC) events workflow

NCC is project shorthand for organic amendments like compost and manure. This workflow builds an ensemble of compost events for every California ag parcel in the harmonized CADWR Land Use crop map, for 2016 and 2018 to 2023. 2017 is skipped because no statewide survey was flown that year.

Source: California Department of Water Resources. (2016-2023). Statewide Crop Mapping. California Natural Resources Agency Open Data. https://data.cnra.ca.gov/dataset/statewide-crop-mapping

Material properties (%N, C:N, PAN, CalRecycle class) come from `PEcAn.data.land::ca_organic_amendment_properties`; application rates come from `PEcAn.data.land::ca_organic_amendment_app_rate`, which splits row crop and orchard rates. The two join on `material` and `source`. Annuals draw the `rows` rate, perennials the `trees` rate.

# Config

Configuration parameters in `config.yml`:

- `crops_path`: the harmonized CADWR Land Use crops parquet
- `phen_dir`: the gap-filled phenology (green-up) directory
- `cadwr_pfts_path`: the CADWR class/subclass to PFT map
- `output_dir`: output directory for parquet shards
- `n_parcels`, `n_ensemble`, `batch_size`, `workers`: settings per profile
- `p_apply_default`: probability of compost per parcel year per ensemble member (default 0.10)

# Run

Select a profile (`default`, `medium`, `all`) and run from PEcAn project root:

```
NCC_PROJECT=default bash workflows/ncc-statewide/run-statewide.sh
```

01 builds a design table and tags each PFT as annual (row, hay, rice) or perennial (woody). 02 runs Bernoulli gate, picks a material per fired row from the joined amendment tables (constrained to CalRecycle classes that fit PFT family, and to the crop structure that matches the family), draws app rate and C:N from that material's envelope. Materials are drawn uniformly, then a source is drawn uniformly among the materials reported by more than one source, so source disagreement enters the ensemble without biasing which material is selected. 03 converts units, carries all N as organic N, and writes parcel range sharded parquet.

`check-result.R` reads the output back, asserts the invariants listed under Checks, and
prints a summary.

# Sampling design

Every stochastic element is listed here with the distribution used and the reason for
it. Where a source reports only a range, the draw is uniform on that range: with no
information beyond the bounds, uniform is the maximum entropy choice, and it avoids
implying a central tendency the sources do not report. These are project assumptions,
not established distributions.

| quantity | distribution | basis |
|---|---|---|
| whether an application occurs | Bernoulli(`p_apply_default`), default 0.10 | scenario assumption; no statewide survey of organic amendment frequency is available. Drawn per crop cycle, see Known limitations |
| material | uniform over distinct eligible materials | no statewide data on the relative frequency of amendment types, so no material is favored |
| source, where a material is reported by more than one | uniform over that material's sources | keeps disagreement between sources in the ensemble instead of averaging it away |
| application rate | uniform on `[app_rate_min, app_rate_max]` | sources report an envelope, not a distribution |
| C:N | uniform on `[cn_min, cn_max]` | sources report an envelope, not a distribution |
| date offset before green-up | uniform on the family window, 14 to 180 days for annuals and 30 to 210 for perennials | working assumption consistent with the broad direction in the literature, fall application for perennials and pre plant for annuals |

Because applications precede green-up by up to 210 days, a crop cycle in year Y can carry
an event dated Y-1. Event dates therefore span one year earlier than the configured crop
years, and include 2017 even though 2017 has no crop cycles of its own.

Eligibility rules, applied before the material draw:

- Annuals draw the row crop rate, perennials the orchard rate, matching the
  `crop_structure` split in `ca_organic_amendment_app_rate`.
- `wood` class materials are excluded from annuals. Their C:N is high enough that net N
  immobilization is expected within a single row crop season, which SIPNET cannot
  represent since it has no immobilization flux.
- All other CalRecycle classes present in the data (`ag`, `food`, `yard`) are eligible
  for both families. A `material_class` outside this set raises an error rather than
  being silently excluded.

# Known limitations

- The application probability is drawn per crop cycle, so a parcel-year with two crop
  seasons receives two independent draws and an effective annual probability of
  `1-(1-p)^2`. Compost is more naturally a per parcel-year decision; this is open.
- The phenology product supplies one green-up per parcel-year with no season key, so in
  a multi-season parcel-year every cycle is anchored to the same date. In 2016 this
  affects 19,472 of 601,341 parcel-years, 3.2 percent.
- SIPNET has no immobilization flux, so a high C:N amendment that would show negative
  first year plant available N still yields small positive net mineralization.
- All organic C enters the single litter pool, so compost is not represented as more
  recalcitrant than fresh litter.

# Output columns

- `parcel_id`, `ens_id` (`ens_NNN`, shared with fertilization workflow), `date`
- `nh4_n_kg_m2`, `no3_n_kg_m2`: zero. These materials do not report mineral N present at application, so no mineral N is declared and SIPNET mineralizes the organic pool itself from the C:N supplied
- `org_c_kg_m2`: total organic C from the application, the application rate times the material C:N
- `org_n_kg_m2`: total N in the application, all carried as organic N
- `crop_code`: passthrough
- `material`: diagnostic column for `check-result.R` only. Cleaner strips it before union so it never reaches SIPNET

Downstream, `workflows/preprocess-event-parquet/01c-clean-fertilization.R` unions these with synthetic N rows from fertilization workflow and writes one `fertilization.parquet`. SIPNET handles both under same FERT event type.

# Compost timing

Anchor is the gap-filled green-up date (`leafonday`). For annuals that's close to but not exactly planting date (emergence lags planting by a few days to weeks). For perennials it's bud break. Date offset is uniform within a family window:

- annuals (row, hay, rice): 14 to 180 days before green up
- perennials (woody): 30 to 210 days before green up

These are working assumptions consistent with broad direction in literature (fall application for perennials, pre plant for annuals).
