# Statewide compost (NCC) events workflow

NCC is project shorthand for organic amendments like compost and manure. This workflow builds an ensemble of compost events for every California ag parcel in the harmonized CADWR Land Use crop map, for 2016 and 2018 to 2023. 2017 is skipped because no statewide survey was flown that year.

Source: California Department of Water Resources. (2016-2023). Statewide Crop Mapping. California Natural Resources Agency Open Data. https://data.cnra.ca.gov/dataset/statewide-crop-mapping

Material properties (%N, C:N, PAN, CalRecycle class) come from `PEcAn.data.land::ca_organic_amendment_properties`; application rates come from `PEcAn.data.land::ca_organic_amendment_app_rate`, which splits row crop and orchard rates. The two join on `material` and `source`. Which rate a PFT draws is set by `crop_structure` in `pft_timing`.

# Config

Configuration parameters in `config.yml`:

- `ccmmf_dir`: data root the input paths are relative to. Override with `CCMMF_DIR`
- `crops_path`: gap-filled LandIQ crops table. Override with `CCMMF_CROPS_PATH`
- `phen_dir`: gap-filled LandIQ to MSLSP match. Override with `CCMMF_PHEN_DIR`
- `phen_glob`: file glob under `phen_dir` (default `assigned_year=*_gapfilled.parquet`). Override with `CCMMF_PHEN_GLOB`
- `pft_lookup_path`: crop code to PFT table, the same one the monitoring products use. Override with `CCMMF_PFT_LOOKUP`
- `pft_timing`: per PFT, the anchor transition, signed offset window, and `crop_structure`. See Compost timing
- `output_dir`: output directory for parquet shards
- `n_parcels`, `n_ensemble`, `batch_size`, `workers`: settings per profile
- `p_apply_default`: probability of compost per crop cycle per ensemble member (default 0.10); see Sampling design

# Run

Select a profile (`default`, `medium`, `all`) and run from PEcAn project root:

```
NCC_PROJECT=default bash workflows/ncc-statewide/run-statewide.sh
```

01 builds a design table, resolving each crop cycle's PFT and the anchor date its PFT is timed against. 02 runs a Bernoulli gate, picks a material per fired row from the joined amendment tables using the application rate matching that PFT's `crop_structure`, and draws app rate, C:N and a signed date offset. Materials are drawn uniformly, then a source is drawn uniformly among the materials reported by more than one source, so source disagreement enters the ensemble without biasing which material is selected. 03 converts units, carries all N as organic N, and writes parcel range sharded parquet.

`check-result.R` reads the output back, asserts the invariants listed under Checks, and
prints a summary including the realized timing window per PFT.

# Sampling design

Every stochastic element is listed here with the distribution used and the reason for
it. Where a source reports only a range, the draw is uniform on that range: with no
information beyond the bounds, uniform is the maximum entropy choice, and it avoids
implying a central tendency the sources do not report. These are project assumptions,
not established distributions.

| quantity | distribution | basis |
|---|---|---|
| whether an application occurs | Bernoulli(`p_apply_default`), default 0.10 | scenario assumption; no statewide survey of organic amendment frequency is available. Drawn per crop cycle, see Known limitations |
| material | uniform over the 29 distinct materials | no statewide data on the relative frequency of amendment types, so no material is favored |
| source, where a material is reported by more than one | uniform over that material's sources | keeps disagreement between sources in the ensemble instead of averaging it away |
| application rate | uniform on `[app_rate_min, app_rate_max]` | sources report an envelope, not a distribution |
| C:N | uniform on `[cn_min, cn_max]` | sources report an envelope, not a distribution |
| date offset from the anchor | discrete uniform on `[offset_min, offset_max]` for the PFT, inclusive | working assumption per crop type, see Compost timing |

Offsets are signed, so an event can fall outside its anchor's calendar year in either
direction: row reaches 120 days before a planting date that may already sit in the prior
year, and hay reaches 14 days after a senescence date that may already sit in the
following one.
Event dates therefore span one year on each side of the configured crop years.

Every material is eligible for every `crop_structure`, with no screening by
`material_class` or C:N.

SIPNET represents the effect of a high C:N amendment itself. Litter breakdown is scaled by
`calcCNEffect(kCN, litterC, litterN) = kCN / (kCN + C:N)`, so a material that raises the
litter pool C:N decomposes, and releases N, more slowly. At `kCN = 80` wood chips at C:N
300 break down at roughly a quarter the rate of poultry litter at C:N 12.5.

# Compost timing

Compost is applied at different points of the season depending on the crop, so the anchor
is chosen per PFT rather than shared. Anchors are MSLSP phenology transitions carried on
the gap-filled LandIQ to MSLSP match, which keys them by `(parcel_id, year, season)`:

| PFT | anchor | transition means | offset window |
|---|---|---|---|
| row | `mslsp_OGI` | onset of greenness increase, 15%, used as planting | 120 to 90 days before |
| rice | `mslsp_OGI` | as above | 90 to 60 days before |
| woody | `mslsp_50PCGI` | 50% greenness increase, leaf-on | 30 days before to 14 days after |
| hay | `mslsp_OGD` | onset of greenness decrease, 10%, used as harvest | 0 to 14 days after |

The windows are working assumptions, not fitted values, and live in `config.yml` so they
can be revised without a code change. Rice is separated from row because applying labile
carbon to flooded, anaerobic soil raises different concerns than a dry seedbed.

# Known limitations

- The application probability is drawn per crop cycle, so a parcel-year with two crop
  seasons receives two independent draws and an effective annual probability of
  `1-(1-p)^2`. Compost is more naturally a per parcel-year decision; this is open.
- A crop cycle with no matched phenology row gets no anchor and is dropped with a count
  reported at run time. Against the gap-filled LandIQ table this is about 99% of cycles.
- `mslsp_50PCGI` resolves leaf-on well for deciduous orchards but not for vineyards or
  citrus. Measured 2020 interquartile ranges are 11 days for almond against 89 for
  vineyard and 77 for citrus, and the vineyard figure is from observed rather than filled
  retrievals. Citrus is evergreen, so its greenness transition is not a leaf-on; vineyard
  retrievals are affected by the interrow floor within a 30 m pixel. The woody window is
  narrower than that spread for those two classes.
- SIPNET has no immobilization flux, so a high C:N amendment that would show negative
  first year plant available N still yields small positive net mineralization.
- All organic C enters the single litter pool, so compost is not represented as more
  recalcitrant than fresh litter.

# Running outside the BU cluster

`config.yml` is plain YAML with no R expressions, so any parser can read it. Input paths
are relative to `ccmmf_dir` and are resolved at run time; `01` logs every resolved path,
so a run's log records the configuration it actually used.

Three inputs are needed. They are monitoring products, not distributed with this repo:

| config key | what it is |
|---|---|
| `crops_path` | gap-filled LandIQ crops table, one row per parcel, year and season |
| `phen_dir` | gap-filled LandIQ to MSLSP phenology match, keyed the same way |
| `pft_lookup_path` | crop code to PFT table |

If they sit under one directory in the layout `config.yml` describes, point `CCMMF_DIR` at
it and nothing else changes:

```
CCMMF_DIR=/my/ccmmf NCC_PROJECT=all bash workflows/ncc-statewide/run-statewide.sh
```

If the layout differs, override paths individually. These take precedence over
`ccmmf_dir`:

```
CCMMF_CROPS_PATH=/data/crops_all_years.parq \
CCMMF_PHEN_DIR=/data/phenology \
CCMMF_PFT_LOOKUP=/data/LandIQ_cropCode_lookup_table.csv \
CCMMF_NCC_OUT=/data/events \
NCC_PROJECT=all bash workflows/ncc-statewide/run-statewide.sh
```

`phen_glob` can be overridden with `CCMMF_PHEN_GLOB` if the phenology files are named
differently. Inputs are required rather than optional: if one does not resolve, the run
stops and names the variable to set.

# Output columns

- `parcel_id`, `ens_id` (`ens_NNN`, shared with fertilization workflow), `date`
- `nh4_n_kg_m2`, `no3_n_kg_m2`: zero. These materials do not report mineral N present at application, so no mineral N is declared and SIPNET mineralizes the organic pool itself from the C:N supplied
- `org_c_kg_m2`: total organic C from the application, the applied N mass times the material C:N
- `org_n_kg_m2`: total N in the application, all carried as organic N
- `crop_code`: passthrough
- `material`: diagnostic column for `check-result.R` only. Cleaner strips it before union so it never reaches SIPNET

Downstream, `workflows/preprocess-event-parquet/01c-clean-fertilization.R` unions these with synthetic N rows from fertilization workflow and writes one `fertilization.parquet`. SIPNET handles both under same FERT event type.
