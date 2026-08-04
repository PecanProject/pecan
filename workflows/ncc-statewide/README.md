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

`check-result.R` reads output back and prints a summary.

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
